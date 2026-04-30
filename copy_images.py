#!/usr/bin/env python3
"""
Copy and convert images to PreTeXt asset directories.
This script prepares images for the PreTeXt build process.

Images are copied to two locations:
  - pretext/assets/images/  (external assets, preserving subdirectory structure)
    Used for user-provided images referenced in PTX source files via
    <image source="images/..."/>. The external directory is configured as
    ../assets in publication/publication.ptx.
  - pretext/generated-assets/  (flat copy, legacy/fallback)
"""

import os
import shutil
import subprocess
from pathlib import Path

def main():
    # Define paths
    script_dir = Path(__file__).parent
    source_images_dir = script_dir / "images"
    # External assets directory: matches publication.ptx "external" setting (../assets
    # relative to source/main.ptx, i.e. pretext/assets/)
    pretext_external_images = script_dir / "pretext" / "assets" / "images"
    # Generated assets directory: matches publication.ptx "generated" setting
    pretext_generated = script_dir / "pretext" / "generated-assets"
    
    print("Preparing images for PreTeXt book...")
    print(f"Source: {source_images_dir}")
    print(f"External assets target: {pretext_external_images}")
    print(f"Generated assets target: {pretext_generated}")
    
    # Create target directories
    pretext_external_images.mkdir(parents=True, exist_ok=True)
    pretext_generated.mkdir(parents=True, exist_ok=True)
    
    # Check if ImageMagick convert is available
    convert_available = shutil.which("convert") is not None
    
    if convert_available:
        print("ImageMagick found - will convert EPS to PNG")
    else:
        print("ImageMagick not found - will only copy existing PNG files")
    
    images_copied = 0
    images_converted = 0
    
    # Copy existing PNG files:
    #   - to pretext/assets/images/ preserving subdirectory structure
    #   - to pretext/generated-assets/ (flat, for backwards compatibility)
    for png_file in source_images_dir.rglob("*.png"):
        # Skip defunct images
        if "defunct_images" in str(png_file):
            continue

        # Preserve subdirectory structure relative to source_images_dir
        rel_path = png_file.relative_to(source_images_dir)
        external_target = pretext_external_images / rel_path
        external_target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(png_file, external_target)

        # Also copy flat to generated-assets (legacy)
        flat_target = pretext_generated / png_file.name
        shutil.copy2(png_file, flat_target)

        images_copied += 1
        print(f"  Copied: {rel_path}")

    # Copy existing JPG/JPEG files with subdirectory structure preserved
    for jpg_file in source_images_dir.rglob("*.jpg"):
        if "defunct_images" in str(jpg_file):
            continue
        rel_path = jpg_file.relative_to(source_images_dir)
        external_target = pretext_external_images / rel_path
        external_target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(jpg_file, external_target)
        flat_target = pretext_generated / jpg_file.name
        shutil.copy2(jpg_file, flat_target)
        images_copied += 1
        print(f"  Copied: {rel_path}")

    for jpeg_file in source_images_dir.rglob("*.jpeg"):
        if "defunct_images" in str(jpeg_file):
            continue
        rel_path = jpeg_file.relative_to(source_images_dir)
        external_target = pretext_external_images / rel_path
        external_target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(jpeg_file, external_target)
        flat_target = pretext_generated / jpeg_file.name
        shutil.copy2(jpeg_file, flat_target)
        images_copied += 1
        print(f"  Copied: {rel_path}")
    
    # Convert EPS files to PNG if ImageMagick is available
    if convert_available:
        for eps_file in source_images_dir.rglob("*.eps"):
            # Get the base filename without extension
            base_name = eps_file.stem
            rel_dir = eps_file.relative_to(source_images_dir).parent
            external_target = pretext_external_images / rel_dir / f"{base_name}.png"
            external_target.parent.mkdir(parents=True, exist_ok=True)
            flat_target = pretext_generated / f"{base_name}.png"
            
            # Skip if PNG already exists in both targets
            if external_target.exists() and flat_target.exists():
                continue
            
            try:
                # Convert EPS to PNG using ImageMagick
                subprocess.run([
                    "convert",
                    "-density", "300",
                    "-quality", "90",
                    str(eps_file),
                    str(external_target)
                ], check=True, capture_output=True, text=True)
                shutil.copy2(external_target, flat_target)
                
                images_converted += 1
                print(f"  Converted: {rel_dir / base_name}.eps -> .png")
            except subprocess.CalledProcessError as e:
                print(f"  Warning: Failed to convert {eps_file.name}: {e}")
                continue
            except Exception as e:
                print(f"  Warning: Error processing {eps_file.name}: {e}")
                continue
    
    # Summary
    total_external = sum(1 for _ in pretext_external_images.rglob("*.png"))
    print(f"\nComplete!")
    print(f"  Copied: {images_copied} image files")
    print(f"  Converted: {images_converted} EPS files")
    print(f"  Total images in external assets: {total_external}")
    
    if images_copied == 0 and images_converted == 0:
        print("\nNote: No images were found in the source directory.")

    return 0

if __name__ == "__main__":
    exit(main())
