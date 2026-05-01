#!/usr/bin/env Rscript
# Generate all figures needed for the PreTeXt book.
# This script produces PNG files in the ../images/ directory.
# Run from the pretext/ subdirectory or the repo root.

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tibble)
  library(nycflights23)
  library(moderndive)
  library(gapminder)
  library(patchwork)
  library(tidyr)
  library(readr)
  library(scales)
})

# Resolve the images output directory relative to this script's location
script_dir <- tryCatch(
  dirname(normalizePath(sys.frame(1)$ofile)),
  error = function(e) getwd()
)
# Allow override via environment variable (used in CI)
if (nchar(Sys.getenv("IMAGES_DIR")) > 0) {
  images_dir <- Sys.getenv("IMAGES_DIR")
} else {
  # Default: ../images relative to this script
  images_dir <- file.path(script_dir, "..", "images")
}
images_dir <- normalizePath(images_dir, mustWork = FALSE)
dir.create(images_dir, showWarnings = FALSE, recursive = TRUE)

save_fig <- function(plot, filename, width = 6, height = 4) {
  path <- file.path(images_dir, filename)
  ggsave(path, plot = plot, width = width, height = height, dpi = 150)
  message("Saved: ", filename)
}

set.seed(76)

# ============================================================
# Chapter 2: Data Visualization
# ============================================================

## 2.1 Gapminder scatter ----------------------------------------
gapminder_2007 <- gapminder |>
  filter(year == 2007) |>
  select(-year) |>
  rename(
    Country = country,
    Continent = continent,
    `Life Expectancy` = lifeExp,
    Population = pop,
    `GDP per Capita` = gdpPercap
  )

p_gapminder <- ggplot(
  data = gapminder_2007,
  mapping = aes(
    x = `GDP per Capita`,
    y = `Life Expectancy`,
    size = Population,
    color = Continent
  )
) +
  geom_point() +
  labs(x = "GDP per capita", y = "Life expectancy")
save_fig(p_gapminder, "fig-gapminder.png", width = 7, height = 4)

## 2.2 Envoy flights scatterplot (no alpha) ----------------------
envoy_flights <- flights |>
  filter(carrier == "MQ")

p_noalpha <- ggplot(data = envoy_flights,
                    mapping = aes(x = dep_delay, y = arr_delay)) +
  geom_point()
save_fig(p_noalpha, "fig-noalpha.png")

## 2.3 ggplot with no layers -------------------------------------
p_nolayers <- ggplot(data = envoy_flights,
                     mapping = aes(x = dep_delay, y = arr_delay))
save_fig(p_nolayers, "fig-nolayers.png")

## 2.4 Scatterplot with alpha = 0.2 ------------------------------
p_alpha <- ggplot(data = envoy_flights,
                  mapping = aes(x = dep_delay, y = arr_delay)) +
  geom_point(alpha = 0.2)
save_fig(p_alpha, "fig-alpha.png")

## 2.5 Regular vs. jittered scatterplot (side-by-side) ----------
jitter_example <- tibble(x = rep(0, 4), y = rep(0, 4))
p_jitter1 <- ggplot(data = jitter_example,
                    mapping = aes(x = x, y = y)) +
  geom_point() +
  coord_cartesian(xlim = c(-0.025, 0.025), ylim = c(-0.025, 0.025)) +
  labs(title = "Regular scatterplot")
p_jitter2 <- ggplot(data = jitter_example,
                    mapping = aes(x = x, y = y)) +
  geom_jitter(width = 0.01, height = 0.01) +
  coord_cartesian(xlim = c(-0.025, 0.025), ylim = c(-0.025, 0.025)) +
  labs(title = "Jittered scatterplot")
save_fig(p_jitter1 + p_jitter2, "fig-jitter-example-plot-1.png",
         width = 8, height = 4)

## 2.6 Jittered scatterplot (Envoy flights) ----------------------
p_jitter <- ggplot(data = envoy_flights,
                   mapping = aes(x = dep_delay, y = arr_delay)) +
  geom_jitter(width = 30, height = 30)
save_fig(p_jitter, "fig-jitter.png")

## 2.7 Time series – hourly wind speed Newark Jan 1-15 ----------
p_hourlytemp <- ggplot(
  data = early_january_2023_weather,
  mapping = aes(x = time_hour, y = wind_speed)
) +
  geom_line()
save_fig(p_hourlytemp, "fig-hourlytemp.png", width = 7, height = 3)

## 2.8 Wind speed on a horizontal line --------------------------
p_windline <- ggplot(data = weather,
                     mapping = aes(x = wind_speed, y = factor("A"))) +
  geom_point() +
  theme(
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  )
save_fig(p_windline, "fig-windspeed-on-line.png", width = 7, height = 2)

## 2.9 Histogram example -----------------------------------------
p_histex <- ggplot(data = weather, mapping = aes(x = wind_speed)) +
  geom_histogram(binwidth = 5, boundary = 40, color = "white")
save_fig(p_histex, "fig-histogramexample.png")

## 2.10 Weather histogram (default bins) -------------------------
p_wh <- ggplot(data = weather, mapping = aes(x = wind_speed)) +
  geom_histogram()
suppressMessages(save_fig(p_wh, "fig-weather-histogram.png"))

## 2.11 Weather histogram with white borders ---------------------
p_wh2 <- ggplot(data = weather, mapping = aes(x = wind_speed)) +
  geom_histogram(color = "white")
suppressMessages(save_fig(p_wh2, "fig-weather-histogram-2.png"))

## 2.12 Histogram bins comparison --------------------------------
hist_1 <- ggplot(data = weather, mapping = aes(x = wind_speed)) +
  geom_histogram(bins = 20, color = "white") +
  labs(title = "With 20 bins")
hist_2 <- ggplot(data = weather, mapping = aes(x = wind_speed)) +
  geom_histogram(binwidth = 5, color = "white") +
  labs(title = "With binwidth = 5 mph")
suppressMessages(save_fig(hist_1 + hist_2, "fig-hist-bins.png",
                          width = 8, height = 4))

## 2.13 Faceted histogram (3 rows) --------------------------------
p_facethist <- ggplot(data = weather, mapping = aes(x = wind_speed)) +
  geom_histogram(binwidth = 5, color = "white") +
  facet_wrap(~month)
suppressMessages(save_fig(p_facethist, "fig-facethistogram.png",
                          width = 8, height = 5))

## 2.14 Faceted histogram (4 rows) --------------------------------
p_facethist2 <- ggplot(data = weather, mapping = aes(x = wind_speed)) +
  geom_histogram(binwidth = 5, color = "white") +
  facet_wrap(~month, nrow = 4)
suppressMessages(save_fig(p_facethist2, "fig-facethistogram2.png",
                          width = 8, height = 6))

## 2.15 April wind speed jittered points -------------------------
cleaned_data <- weather |>
  filter(month == 4) |>
  filter(!is.na(wind_speed))

p_apr1 <- cleaned_data |>
  ggplot(mapping = aes(x = factor(month), y = wind_speed)) +
  labs(x = "") +
  geom_jitter(width = 0.075, height = 0.5, alpha = 0.1)
save_fig(p_apr1, "fig-apr1.png", width = 4, height = 4)

## patchwork_boxplot.png (built in 02-visualization.Rmd, save only if missing)
patchwork_path <- file.path(images_dir, "patchwork_boxplot.png")
if (!file.exists(patchwork_path)) {
  min_apr    <- min(cleaned_data$wind_speed)
  quartiles  <- quantile(cleaned_data$wind_speed, probs = c(0.25, 0.5, 0.75))
  max_apr    <- max(cleaned_data$wind_speed)

  base_plot <- cleaned_data |>
    ggplot(mapping = aes(x = factor(month), y = wind_speed)) +
    labs(x = "")

  boxplot_1 <- base_plot +
    geom_hline(
      yintercept = c(min_apr, quartiles[1], quartiles[2], quartiles[3], max_apr),
      linetype = "dashed"
    ) +
    geom_jitter(width = 0.075, height = 0.5, alpha = 0.1)

  boxplot_2 <- base_plot +
    geom_boxplot(outlier.shape = NA) +
    geom_hline(
      yintercept = c(min_apr, quartiles[1], quartiles[2], quartiles[3], max_apr),
      linetype = "dashed"
    ) +
    geom_jitter(width = 0.075, height = 0.5, alpha = 0.1)

  boxplot_3 <- base_plot + geom_boxplot()

  ggsave(patchwork_path, plot = boxplot_1 | boxplot_2 | boxplot_3,
         width = 9, height = 4, dpi = 150)
  message("Saved: patchwork_boxplot.png")
}

## 2.16 Invalid boxplot (month as numeric) -----------------------
p_badbox <- ggplot(data = weather,
                   mapping = aes(x = month, y = wind_speed)) +
  geom_boxplot()
suppressWarnings(save_fig(p_badbox, "fig-badbox.png"))

## 2.17 Side-by-side boxplot (month as factor) -------------------
p_monthtempbox <- ggplot(data = weather,
                         mapping = aes(x = factor(month), y = wind_speed)) +
  geom_boxplot()
save_fig(p_monthtempbox, "fig-monthtempbox.png", width = 7, height = 4)

## 2.18-2.19 Barplots (fruits) -----------------------------------
fruits         <- tibble(fruit = c("apple", "apple", "orange", "apple", "orange"))
fruits_counted <- tibble(fruit = c("apple", "orange"), number = c(3, 2))

p_geombar <- ggplot(data = fruits, mapping = aes(x = fruit)) +
  geom_bar()
save_fig(p_geombar, "fig-geombar.png", width = 4, height = 3)

p_geomcol <- ggplot(data = fruits_counted,
                    mapping = aes(x = fruit, y = number)) +
  geom_col()
save_fig(p_geomcol, "fig-geomcol.png", width = 4, height = 3)

## 2.20 Flights by carrier bar chart ----------------------------
p_flightsbar <- ggplot(data = flights, mapping = aes(x = carrier)) +
  geom_bar()
save_fig(p_flightsbar, "fig-flightsbar.png", width = 7, height = 4)

## 2.21 Carrier pie chart ---------------------------------------
p_carrierpie <- ggplot(flights, mapping = aes(x = factor(1), fill = carrier)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  guides(fill = guide_legend(keywidth = 0.8, keyheight = 0.8))
save_fig(p_carrierpie, "fig-carrierpie.png", width = 6, height = 5)

## 2.22 Stacked barplot (carrier fill = origin) -----------------
p_stacked <- ggplot(data = flights,
                    mapping = aes(x = carrier, fill = origin)) +
  geom_bar()
save_fig(p_stacked, "fig-flights-stacked-bar.png", width = 7, height = 4)

## 2.23 Stacked barplot using color instead of fill -------------
p_stacked_color <- ggplot(data = flights,
                           mapping = aes(x = carrier, color = origin)) +
  geom_bar()
save_fig(p_stacked_color, "fig-flights-stacked-bar-color.png",
         width = 7, height = 4)

## 2.24 Dodged barplot ------------------------------------------
p_dodged <- ggplot(data = flights,
                   mapping = aes(x = carrier, fill = origin)) +
  geom_bar(position = "dodge")
save_fig(p_dodged, "fig-flights-dodged-bar-color.png", width = 7, height = 4)

## 2.25 Faceted barplot (carrier by origin) ---------------------
p_facetbar <- ggplot(data = flights, mapping = aes(x = carrier)) +
  geom_bar() +
  facet_wrap(~origin, ncol = 1)
save_fig(p_facetbar, "fig-facet-bar-vert.png", width = 7, height = 7)

# ============================================================
# Chapter 3: Data Wrangling
# ============================================================

## gain histogram -----------------------------------------------
alaska_flights <- flights |>
  filter(carrier == "AS")

p_gain_hist <- alaska_flights |>
  mutate(gain = dep_delay - arr_delay) |>
  ggplot(mapping = aes(x = gain)) +
  geom_histogram(color = "white", bins = 20)
suppressMessages(save_fig(p_gain_hist, "fig-gain-hist.png"))

# ============================================================
# Chapter 4: Tidy Data
# ============================================================

drinks_smaller <- tibble(
  country = c("USA", "China", "Italy", "Saudi Arabia"),
  beer    = c(249, 79, 85, 0),
  spirit  = c(158, 192, 42, 5),
  wine    = c(84, 8, 237, 0)
)

## drinks-smaller barplot (before tidying) ----------------------
drinks_smaller_tidy <- drinks_smaller |>
  pivot_longer(
    cols = c(beer, spirit, wine),
    names_to = "type",
    values_to = "servings"
  )

p_drinks <- ggplot(drinks_smaller_tidy,
                   aes(x = country, y = servings, fill = type)) +
  geom_col(position = "dodge") +
  labs(x = "Country", y = "Servings")
save_fig(p_drinks, "fig-drinks-smaller.png", width = 6, height = 4)

## drinks-smaller-tidy barplot ----------------------------------
p_drinks_tidy <- ggplot(drinks_smaller_tidy,
                         aes(x = country, y = servings, fill = type)) +
  geom_col(position = "dodge") +
  labs(x = "Country", y = "Servings")
save_fig(p_drinks_tidy, "fig-drinks-smaller-tidy-barplot.png",
         width = 6, height = 4)

## Guatemala democracy time series (using local data/dem_score.csv) -----------
# Determine the repository root (one level above this script)
repo_root <- dirname(images_dir)
dem_csv <- file.path(repo_root, "data", "dem_score.csv")
if (file.exists(dem_csv)) {
  dem_score <- readr::read_csv(dem_csv, show_col_types = FALSE)
  guat_dem <- dem_score |>
    filter(country == "Guatemala")

  guat_dem_tidy <- guat_dem |>
    pivot_longer(
      cols = -country,
      names_to = "year",
      values_to = "democracy_score",
      names_transform = list(year = as.integer)
    )

  p_guat <- ggplot(
    data = guat_dem_tidy,
    mapping = aes(x = year, y = democracy_score)
  ) +
    geom_line() +
    labs(x = "Year", y = "Democracy Score")
  save_fig(p_guat, "fig-guat-dem-tidy.png", width = 6, height = 4)
} else {
  message("Note: data/dem_score.csv not found; skipping fig-guat-dem-tidy.png")
}

# ============================================================
# Appendix A: Statistical Background (normal curve figures)
# ============================================================

## Normal curves comparison ------------------------------------
x_vals <- seq(-4, 4, length.out = 500)
normal_data <- bind_rows(
  tibble(x = x_vals, y = dnorm(x_vals, 0, 0.5),
         Distribution = "mean=0, sd=0.5"),
  tibble(x = x_vals, y = dnorm(x_vals, 0, 1),
         Distribution = "mean=0, sd=1"),
  tibble(x = x_vals, y = dnorm(x_vals, 0, 2),
         Distribution = "mean=0, sd=2"),
  tibble(x = x_vals, y = dnorm(x_vals, -2, 1),
         Distribution = "mean=-2, sd=1")
)
p_normal2 <- ggplot(normal_data, aes(x = x, y = y,
                                     color = Distribution,
                                     linetype = Distribution)) +
  geom_line(linewidth = 0.8) +
  labs(x = "x", y = "Density") +
  theme(legend.position = "bottom")
save_fig(p_normal2, "fig-normal-curves2.png", width = 7, height = 4)

## Rule of thumb figure ----------------------------------------
df_norm <- tibble(x = x_vals, y = dnorm(x_vals))

p_rot <- ggplot(df_norm, aes(x = x, y = y)) +
  geom_line() +
  geom_area(data = df_norm |> filter(x >= -3, x <= 3),
            aes(x = x, y = y), fill = "grey80", alpha = 0.5) +
  geom_area(data = df_norm |> filter(x >= -2, x <= 2),
            aes(x = x, y = y), fill = "grey60", alpha = 0.5) +
  geom_area(data = df_norm |> filter(x >= -1, x <= 1),
            aes(x = x, y = y), fill = "grey40", alpha = 0.5) +
  annotate("text", x = 0, y = 0.2, label = "68%", size = 4) +
  annotate("text", x = 0, y = 0.1, label = "95%", size = 4) +
  annotate("text", x = 0, y = 0.04, label = "99.7%", size = 4) +
  labs(x = "Standard deviations from mean", y = "Density")
save_fig(p_rot, "fig-normal-rule-of-thumb.png", width = 6, height = 4)

message("\nAll figures generated successfully in: ", images_dir)
