#!/usr/bin/env python3
"""Convert <program language="r"> blocks to <console> blocks in PTX files.

Uses a persistent R subprocess:
- Context code is run silently (output suppressed) but state is accumulated
- Current block is run and its stdout is captured
- Errors in context are caught and don't crash the session
"""

import re
import subprocess
import sys
import os
import time
import threading
import queue

RSCRIPT = "/usr/bin/Rscript"

R_PREAMBLE = """\
.libPaths(c('/tmp/rlib2', .libPaths()))
suppressPackageStartupMessages({
  library(moderndive); library(nycflights23); library(dplyr); library(tibble)
  library(ggplot2); library(broom); library(knitr); library(stringr)
  library(janitor); library(readr); library(tidyr); library(forcats); library(scales)
})
options(width=80, tibble.width=80, tibble.print_max=20, tibble.print_min=10)
set.seed(2024)
"""

INFER_FUNCTIONS = [
    'specify', 'hypothesize', 'generate', 'calculate', 'visualize',
    'shade_p_value', 'shade_confidence_interval', 'get_p_value',
    'get_confidence_interval', 'rep_slice_sample', 'rep_sample_n',
]

SENTINEL = '__RDONE_SENTINEL_1234567890__'

def unescape_xml(text):
    text = text.replace('&lt;', '<')
    text = text.replace('&gt;', '>')
    text = text.replace('&amp;', '&')
    text = text.replace('&quot;', '"')
    text = text.replace("&apos;", "'")
    return text

def escape_xml(text):
    text = text.replace('&', '&amp;')
    text = text.replace('<', '&lt;')
    text = text.replace('>', '&gt;')
    return text

def wrap_for_context(raw_code):
    """Wrap code for silent context execution: suppress output and catch errors."""
    # Wrap library() to handle missing packages silently
    code = re.sub(r'\blibrary\s*\(([^)]+)\)',
                  r'try(suppressPackageStartupMessages(library(\1)), silent=TRUE)',
                  raw_code)
    # Capture output and catch errors
    return f'invisible(capture.output({{tryCatch({{\n{code}\n}}, error=function(e) invisible(NULL))}}, type="output"))'

class RSession:
    """Persistent R subprocess that maintains state across blocks."""
    
    def __init__(self):
        self.proc = subprocess.Popen(
            [RSCRIPT, '--no-init-file', '--vanilla', '-'],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
            text=True,
            bufsize=1
        )
        self._alive = True
        # Start preamble
        self._execute_silent(R_PREAMBLE)
    
    def _write(self, code):
        """Write code + sentinel to R stdin."""
        full = code.rstrip() + f'\ncat("{SENTINEL}\\n")\n'
        try:
            self.proc.stdin.write(full)
            self.proc.stdin.flush()
        except BrokenPipeError:
            self._alive = False
            raise RuntimeError("R session closed")
    
    def _read_until_sentinel(self, timeout=60):
        """Read stdout until sentinel, with timeout."""
        lines = []
        q = queue.Queue()
        
        def reader():
            while True:
                try:
                    line = self.proc.stdout.readline()
                except Exception:
                    q.put(None)
                    return
                if not line:
                    q.put(None)
                    return
                line = line.rstrip('\n')
                q.put(line)
                if line == SENTINEL:
                    return
        
        t = threading.Thread(target=reader, daemon=True)
        t.start()
        
        start = time.time()
        while True:
            elapsed = time.time() - start
            remaining = timeout - elapsed
            if remaining <= 0:
                raise TimeoutError("R timeout")
            try:
                item = q.get(timeout=remaining)
                if item is None:
                    self._alive = False
                    raise RuntimeError("R process ended")
                if item == SENTINEL:
                    break
                lines.append(item)
            except queue.Empty:
                raise TimeoutError("R timeout")
        
        return '\n'.join(lines)
    
    def _execute_silent(self, code, timeout=120):
        """Execute code silently (output suppressed)."""
        wrapped = wrap_for_context(code)
        self._write(wrapped)
        self._read_until_sentinel(timeout=timeout)
    
    def execute_and_capture(self, code, timeout=60):
        """Execute code and return its stdout. Wrapped in tryCatch so errors don't kill R."""
        wrapped = f"tryCatch({{\n{code}\n}}, error=function(e) invisible(NULL))"
        self._write(wrapped)
        return self._read_until_sentinel(timeout=timeout)
    
    @property
    def alive(self):
        return self._alive and self.proc.poll() is None
    
    def close(self):
        try:
            if self.alive:
                self.proc.stdin.close()
            self.proc.terminate()
            self.proc.wait(timeout=3)
        except Exception:
            pass


def should_skip_without_running(code_raw):
    """Return True to skip running this block."""
    code = code_raw.strip()
    if not code:
        return True
    
    lines = [l.strip() for l in code.split('\n') if l.strip() and not l.strip().startswith('#')]
    if not lines:
        return True
    
    if any(re.search(r'\bggplot\s*\(', l) for l in lines):
        return True
    if all(re.match(r'^(library|require)\s*\(', l) for l in lines):
        return True
    if any(re.search(r'\bView\s*\(', l) for l in lines):
        return True
    for fn in INFER_FUNCTIONS:
        if re.search(r'\b' + fn + r'\s*\(', code):
            return True
    if re.search(r'\bdygraph\b', code):
        return True
    return False


def format_console_block(block_code_xml, output_text, indent):
    """Format a <console> block."""
    child_ind = indent + '  '
    input_lines = block_code_xml.strip('\n')
    escaped_output = escape_xml(output_text.rstrip())
    return '\n'.join([
        f'{indent}<console prompt="&gt; ">',
        f'{child_ind}<input>',
        f'{input_lines}',
        f'{child_ind}</input>',
        f'{child_ind}<output>',
        f'{escaped_output}',
        f'{child_ind}</output>',
        f'{indent}</console>',
    ])


def process_file(filepath, chapter_preamble='', verbose=True):
    """Process a PTX file with a persistent R session."""
    with open(filepath, 'r', encoding='utf-8') as f:
        content = f.read()
    
    prog_pattern = re.compile(
        r'( *)<program language="r">\s*\n\s*<input>\n(.*?)\n\s*</input>\s*\n\s*</program>',
        re.DOTALL
    )
    
    matches = list(prog_pattern.finditer(content))
    if verbose:
        print(f"  Found {len(matches)} program blocks")
    
    session = RSession()
    
    # Apply chapter preamble
    if chapter_preamble.strip():
        try:
            session._execute_silent(chapter_preamble)
        except Exception as e:
            if verbose:
                print(f"  Warning: preamble error: {e}")
    
    replacements = []
    
    try:
        for i, match in enumerate(matches):
            indent = match.group(1)
            block_code_xml = match.group(2)
            original_block = match.group(0)
            raw_code = unescape_xml(block_code_xml)
            
            if not session.alive:
                session.close()
                session = RSession()
                if chapter_preamble.strip():
                    session._execute_silent(chapter_preamble)
            
            skip = should_skip_without_running(raw_code)
            
            if skip:
                try:
                    session._execute_silent(raw_code, timeout=30)
                except (TimeoutError, RuntimeError):
                    pass  # Context block failed - that's ok
                continue
            
            try:
                output = session.execute_and_capture(raw_code, timeout=60).strip()
                
                if output:
                    console = format_console_block(block_code_xml, output, indent)
                    replacements.append((original_block, console))
                    if verbose:
                        print(f"    Block {i+1}: CONVERTED ({len(output)} chars)")
                else:
                    if verbose:
                        print(f"    Block {i+1}: skipped (no output) - {repr(raw_code.strip()[:60])}")
            except TimeoutError:
                if verbose:
                    print(f"    Block {i+1}: timeout")
            except RuntimeError as e:
                if verbose:
                    print(f"    Block {i+1}: session died - {e}")
                session.close()
                session = RSession()
                if chapter_preamble.strip():
                    session._execute_silent(chapter_preamble)
            
            # Add to context silently
            if session.alive:
                try:
                    session._execute_silent(raw_code, timeout=30)
                except (TimeoutError, RuntimeError):
                    pass
    finally:
        session.close()
    
    new_content = content
    for orig, replacement in replacements:
        new_content = new_content.replace(orig, replacement, 1)
    
    return new_content


CHAPTER_PREAMBLES = {
    'ch_multiple_regression.ptx': """
UN_data_ch6 <- un_member_states_2024 |>
  select(country, life_expectancy_2022, fertility_rate_2022, income_group_2024) |>
  na.omit() |>
  rename(life_exp = life_expectancy_2022, fert_rate = fertility_rate_2022, income = income_group_2024) |>
  mutate(income = factor(income, levels = c("Low income", "Lower middle income", "Upper middle income", "High income")))
""",
    'ch_sampling.ptx': '',
    'ch_confidence_intervals.ptx': '',
    'ch_hypothesis_testing.ptx': '',
    'ch_inference_for_regression.ptx': '',
    'ch_tell_your_story.ptx': """
house_prices <- house_prices |> mutate(log10_price = log10(price), log10_size = log10(sqft_living))
""",
    'appendix_B_inference_examples.ptx': '',
    'appendix_C_tips_tricks.ptx': '',
}

FILES_PRIORITY = [
    'ch_multiple_regression.ptx',
    'ch_sampling.ptx',
    'ch_tell_your_story.ptx',
    'appendix_B_inference_examples.ptx',
    'appendix_C_tips_tricks.ptx',
    'ch_confidence_intervals.ptx',
    'ch_hypothesis_testing.ptx',
    'ch_inference_for_regression.ptx',
]

BASE_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'pretext', 'source')

def main():
    target_files = sys.argv[1:] if len(sys.argv) > 1 else FILES_PRIORITY
    
    for fname in target_files:
        filepath = os.path.join(BASE_DIR, fname)
        if not os.path.exists(filepath):
            print(f"File not found: {filepath}")
            continue
        
        print(f"\nProcessing {fname}...")
        preamble = CHAPTER_PREAMBLES.get(fname, '')
        new_content = process_file(filepath, preamble)
        
        with open(filepath, 'w', encoding='utf-8') as f:
            f.write(new_content)
        
        result = subprocess.run(['xmllint', '--noout', filepath],
                               capture_output=True, text=True)
        if result.returncode != 0:
            print(f"  XML VALIDATION FAILED:\n{result.stderr[:1000]}")
        else:
            nc = new_content.count('<console prompt="&gt; ">')
            print(f"  XML OK - {nc} console blocks created")

if __name__ == '__main__':
    main()
