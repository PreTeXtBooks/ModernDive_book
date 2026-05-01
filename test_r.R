.libPaths(c('/tmp/rlib2', .libPaths()))
suppressPackageStartupMessages({
library(dplyr)
library(tibble)
library(ggplot2)
})
load('/tmp/flights.rda'); flights <- as_tibble(flights)
x <- flights |> filter(!dest == 'BTV' | dest == 'SEA')
print(x)
