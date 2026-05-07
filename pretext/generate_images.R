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
  library(infer)
  library(gridExtra)
  library(GGally)
  library(fivethirtyeight)
  library(mvtnorm)
  library(ISLR2)
  library(ggrepel)
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
all_points <- tibble(
  domain = seq(from = -10, to = 25, by = 0.01),
  `mu = 5, sigma = 2` = dnorm(x = domain, mean = 5, sd = 2),
  `mu = 5, sigma = 5` = dnorm(x = domain, mean = 5, sd = 5),
  `mu = 15, sigma = 2` = dnorm(x = domain, mean = 15, sd = 2)
) |>
  pivot_longer(
    cols = -domain,
    names_to = "Distribution",
    values_to = "value"
  ) |>
  mutate(
    Distribution = factor(
      Distribution,
      levels = c(
        "mu = 5, sigma = 2",
        "mu = 5, sigma = 5",
        "mu = 15, sigma = 2"
      )
    )
  )

for_labels <- all_points |>
  filter(
    between(domain, 3.795, 3.805) & Distribution == "mu = 5, sigma = 2" |
      between(domain, 0.005, 0.0105) & Distribution == "mu = 5, sigma = 5" |
      between(domain, 16.005, 16.015) & Distribution == "mu = 15, sigma = 2"
  )

p_normal2 <- ggplot(all_points, aes(x = domain, y = value, linetype = Distribution)) +
  geom_line() +
  geom_label_repel(
    data = for_labels,
    aes(label = Distribution),
    nudge_x = c(-1, -2.1, 1)
  ) +
  theme_light() +
  scale_linetype_manual(values = c("solid", "dotted", "longdash")) +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none"
  )
save_fig(p_normal2, "fig-normal-curves2.png", width = 7, height = 4)

## Rule of thumb figure ----------------------------------------
shade_3_sd <- function(x) {
  y <- dnorm(x, mean = 0, sd = 1)
  y[x <= -3 | x >= 3] <- NA
  y
}

shade_2_sd <- function(x) {
  y <- dnorm(x, mean = 0, sd = 1)
  y[x <= -1.96 | x >= 1.96] <- NA
  y
}

shade_1_sd <- function(x) {
  y <- dnorm(x, mean = 0, sd = 1)
  y[x <= -1 | x >= 1] <- NA
  y
}

labels <- tibble(
  x = c(-3.5, -2.5, -1.5, -0.5, 0.5, 1.5, 2.5, 3.5),
  label = c("0.15%", "2.35%", "13.5%", "34%", "34%", "13.5%", "2.35%", "0.15%"),
  y = rep(0.3, times = 8)
)

p_rot <- ggplot(data = tibble(x = c(-4, 4)), aes(x)) +
  geom_text(data = labels, aes(y = y, label = label)) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), n = 1000) +
  stat_function(fun = shade_3_sd, geom = "area", fill = "black", alpha = 0.25, n = 1000) +
  stat_function(fun = shade_2_sd, geom = "area", fill = "black", alpha = 0.25, n = 1000) +
  stat_function(fun = shade_1_sd, geom = "area", fill = "black", alpha = 0.25, n = 1000) +
  geom_vline(
    xintercept = c(-3, -1.96, -1, 0, 1, 1.96, 3),
    linetype = "dashed",
    alpha = 0.5
  ) +
  scale_x_continuous(breaks = seq(from = -3, to = 3, by = 1)) +
  labs(x = "z", y = "") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )
save_fig(p_rot, "fig-normal-rule-of-thumb.png", width = 6, height = 4)

## Middle 95% shaded normal curve --------------------------------
p_norm_shaded <- ggplot(NULL, aes(c(-4, 4))) +
  geom_area(stat = "function", fun = dnorm, fill = "grey100", xlim = c(-4, -2)) +
  geom_area(stat = "function", fun = dnorm, fill = "grey80", xlim = c(-2, 2)) +
  geom_area(stat = "function", fun = dnorm, fill = "grey100", xlim = c(2, 4)) +
  labs(x = "z", y = "") +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = NULL) +
  annotate("text", x = 2, y = -0.01, label = "q", color = "blue")
save_fig(p_norm_shaded, "fig-normal-curve-shaded-3.png", width = 6, height = 4)

# ============================================================
# Chapter 5: Regression
# ============================================================

## Data setup ---------------------------------------------------
un_data_ch5 <- un_member_states_2024 |>
  select(iso,
         life_exp = life_expectancy_2022,
         fert_rate = fertility_rate_2022,
         obes_rate = obesity_rate_2016) |>
  na.omit()

gapminder2022 <- un_member_states_2024 |>
  select(country, life_exp = life_expectancy_2022, continent, gdp_per_capita) |>
  na.omit()

## 5.1 Nine correlation coefficients ----------------------------
set.seed(76)
correlation <- c(-0.9999, -0.9, -0.75, -0.3, 0, 0.3, 0.75, 0.9, 0.9999)
n_sim <- 100
values <- NULL
for (i in seq_along(correlation)) {
  rho <- correlation[i]
  sigma <- matrix(c(5, rho * sqrt(50), rho * sqrt(50), 10), 2, 2)
  sim <- rmvnorm(
    n = n_sim,
    mean = c(20, 40),
    sigma = sigma
  ) |>
    as.data.frame() |>
    as_tibble() |>
    mutate(correlation = round(rho, 2))
  values <- bind_rows(values, sim)
}
p_correlation1 <- ggplot(data = values, mapping = aes(V1, V2)) +
  geom_point() +
  facet_wrap(~correlation, ncol = 3) +
  labs(x = "x", y = "y") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )
save_fig(p_correlation1, "fig-correlation1.png", width = 6, height = 5)

## 5.2 Scatterplot: fertility vs life expectancy ----------------
p_numxplot1 <- ggplot(un_data_ch5,
                      aes(x = life_exp, y = fert_rate)) +
  geom_point(alpha = 0.1) +
  labs(x = "Life Expectancy", y = "Fertility Rate")
save_fig(p_numxplot1, "fig-numxplot1.png")

## 5.3 Scatterplot with regression line -------------------------
p_numxplot3 <- ggplot(un_data_ch5, aes(x = life_exp, y = fert_rate)) +
  geom_point(alpha = 0.1) +
  labs(x = "Life Expectancy",
       y = "Fertility Rate",
       title = "Relationship of life expectancy and fertility rate") +
  geom_smooth(method = "lm", se = FALSE)
suppressMessages(save_fig(p_numxplot3, "fig-numxplot3.png"))

## 5.4 Scatterplot with annotated residual (Bosnia) -------------
demographics_model <- lm(fert_rate ~ life_exp, data = un_data_ch5)
bih_index <- which(un_data_ch5$iso == "BIH")
bih_pt    <- get_regression_points(demographics_model) |> slice(bih_index)
x_bih     <- bih_pt$life_exp
y_bih     <- bih_pt$fert_rate
yhat_bih  <- bih_pt$fert_rate_hat

p_numxplot4 <- ggplot(un_data_ch5, aes(x = life_exp, y = fert_rate)) +
  geom_point(color = "grey") +
  labs(x = "Life Expectancy", y = "Fertility Rate",
       title = "Relationship of Fertility Rate and Life Expectancy") +
  geom_smooth(method = "lm", se = FALSE) +
  annotate("point", x = x_bih, y = yhat_bih, col = "red", shape = 15, size = 4) +
  annotate("segment",
           x = x_bih, xend = x_bih, y = y_bih, yend = yhat_bih,
           color = "blue",
           arrow = arrow(type = "closed", length = unit(0.04, "npc"))) +
  annotate("point", x = x_bih, y = y_bih, col = "red", size = 4)
suppressMessages(save_fig(p_numxplot4, "fig-numxplot4.png"))

## 5.5 Life expectancy histogram --------------------------------
p_lifeexp2022hist <- ggplot(gapminder2022, aes(x = life_exp)) +
  geom_histogram(binwidth = 5, color = "white") +
  labs(x = "Life expectancy", y = "Number of countries",
       title = "Histogram of distribution of worldwide life expectancies")
suppressMessages(save_fig(p_lifeexp2022hist, "fig-lifeexp2022hist.png"))

## 5.6 Faceted histogram by continent ---------------------------
p_catxplot0b <- ggplot(gapminder2022, aes(x = life_exp)) +
  geom_histogram(binwidth = 5, color = "white") +
  labs(x = "Life expectancy", y = "Number of countries",
       title = "Histogram of distribution of worldwide life expectancies") +
  facet_wrap(~continent, nrow = 2)
suppressMessages(save_fig(p_catxplot0b, "fig-catxplot0b.png", width = 8, height = 5))

## 5.7 Boxplot by continent -------------------------------------
p_catxplot1 <- ggplot(gapminder2022, aes(x = continent, y = life_exp)) +
  geom_boxplot() +
  labs(x = "Continent", y = "Life expectancy",
       title = "Life expectancy by continent")
save_fig(p_catxplot1, "fig-catxplot1.png", width = 7, height = 4)

## 5.8 Best-fitting line: 4-panel residuals figure --------------
add_residual <- function(p, x, y, y_hat) {
  p +
    annotate("point", x = x, y = y, col = "red", size = 2) +
    annotate("point", x = x, y = y_hat, col = "red", shape = 15, size = 2) +
    annotate("segment",
             x = x, xend = x, y = y, yend = y_hat, color = "blue",
             arrow = arrow(type = "closed", length = unit(0.02, "npc")))
}

tcd_pt   <- get_regression_points(demographics_model) |>
  slice(which(un_data_ch5$iso == "TCD"))
ind_pt   <- get_regression_points(demographics_model) |>
  slice(which(un_data_ch5$iso == "IND"))
slb_pt   <- get_regression_points(demographics_model) |>
  slice(which(un_data_ch5$iso == "SLB"))

base_p <- ggplot(un_data_ch5, aes(x = life_exp, y = fert_rate)) +
  geom_point(size = 0.8, color = "grey") +
  labs(x = "Life Expectancy", y = "Fertility Rate") +
  geom_smooth(method = "lm", se = FALSE)

p5a <- add_residual(base_p, x_bih, y_bih, yhat_bih) +
  labs(title = "Bosnia and Herzegovina's residual") +
  theme(plot.title = element_text(size = 10))
p5b <- add_residual(
         add_residual(base_p, x_bih, y_bih, yhat_bih),
         tcd_pt$life_exp, tcd_pt$fert_rate, tcd_pt$fert_rate_hat) +
  labs(title = "Chad's residual added") +
  theme(plot.title = element_text(size = 10))
p5c <- add_residual(
         add_residual(
           add_residual(base_p, x_bih, y_bih, yhat_bih),
           tcd_pt$life_exp, tcd_pt$fert_rate, tcd_pt$fert_rate_hat),
         ind_pt$life_exp, ind_pt$fert_rate, ind_pt$fert_rate_hat) +
  labs(title = "India's residual added") +
  theme(plot.title = element_text(size = 10))
p5d_base <- add_residual(
              add_residual(
                add_residual(base_p, x_bih, y_bih, yhat_bih),
                tcd_pt$life_exp, tcd_pt$fert_rate, tcd_pt$fert_rate_hat),
              ind_pt$life_exp, ind_pt$fert_rate, ind_pt$fert_rate_hat)
p5d <- add_residual(p5d_base,
                    slb_pt$life_exp, slb_pt$fert_rate, slb_pt$fert_rate_hat) +
  labs(title = "Solomon Islands' residual added") +
  theme(plot.title = element_text(size = 10))

suppressMessages(
  save_fig(p5a + p5b + p5c + p5d + plot_layout(nrow = 2),
           "fig-best-fitting-line.png", width = 9, height = 6)
)

## 5.9 Three lines example --------------------------------------
example_data <- tibble(x = c(0, 0.5, 1), y = c(2, 1, 3))
p_three_lines <- ggplot(example_data, aes(x = x, y = y)) +
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
  geom_hline(yintercept = 2.5, col = "red", linetype = "dotted", linewidth = 1) +
  geom_abline(intercept = 2, slope = -1, col = "forestgreen",
              linetype = "dashed", linewidth = 1) +
  geom_point(size = 4)
suppressMessages(save_fig(p_three_lines, "fig-three-lines.png", width = 5, height = 4))

## 5.10 HDI vs Life Expectancy scatterplot ----------------------
p_hdi_lifeexp <- ggplot(data = un_member_states_2024,
                        aes(x = hdi_2022, y = life_expectancy_2022)) +
  geom_point() +
  labs(x = "Human Development Index (HDI)", y = "Life Expectancy")
save_fig(p_hdi_lifeexp, "fig-hdi-lifeexp.png")

## 5.11 HDI vs Fertility Rate scatterplot -----------------------
p_hdi_fertility <- ggplot(data = un_member_states_2024,
                          aes(x = hdi_2022, y = fertility_rate_2022)) +
  geom_point() +
  labs(x = "Human Development Index (HDI)", y = "Fertility Rate")
save_fig(p_hdi_fertility, "fig-hdi-fertility.png")

# ============================================================
# Chapter 6: Multiple Regression
# ============================================================

## Data setup ---------------------------------------------------
UN_data_ch6 <- un_member_states_2024 |>
  select(country,
         life_expectancy_2022,
         fertility_rate_2022,
         income_group_2024) |>
  na.omit() |>
  rename(life_exp = life_expectancy_2022,
         fert_rate = fertility_rate_2022,
         income = income_group_2024) |>
  mutate(income = factor(income,
                         levels = c("Low income", "Lower middle income",
                                    "Upper middle income", "High income")))

credit_ch6 <- Credit |>
  as_tibble() |>
  select(debt = Balance, credit_limit = Limit,
         income = Income, credit_rating = Rating, age = Age)

## 6.1 Colored scatterplot with interaction lines ---------------
p_numxcatxplot1 <- ggplot(UN_data_ch6,
                           aes(x = life_exp, y = fert_rate, color = income)) +
  geom_point() +
  labs(x = "Life Expectancy", y = "Fertility Rate", color = "Income group") +
  geom_smooth(method = "lm", se = FALSE)
suppressMessages(save_fig(p_numxcatxplot1, "numxcatxplot1.png", width = 7, height = 4))

## 6.2 Parallel slopes model ------------------------------------
p_numxcatx_parallel <- ggplot(UN_data_ch6,
                               aes(x = life_exp, y = fert_rate, color = income)) +
  geom_point() +
  labs(x = "Life expectancy", y = "Fertility rate", color = "Income group") +
  geom_parallel_slopes(se = FALSE)
save_fig(p_numxcatx_parallel, "numxcatx-parallel.png", width = 7, height = 4)

## 6.3 Side-by-side comparison of interaction vs parallel slopes -
interaction_plot <- ggplot(UN_data_ch6,
                            aes(x = life_exp, y = fert_rate, color = income)) +
  geom_point() +
  labs(x = "Life expectancy", y = "Fertility rate", color = "Income group") +
  geom_smooth(method = "lm", se = FALSE) +
  theme(legend.position = "none")
parallel_slopes_plot <- ggplot(UN_data_ch6,
                                aes(x = life_exp, y = fert_rate, color = income)) +
  geom_point() +
  labs(x = "Life expectancy", y = "Fertility rate", color = "Income group") +
  geom_parallel_slopes(se = FALSE) +
  theme(axis.title.y = element_blank())
suppressMessages(
  save_fig(interaction_plot + parallel_slopes_plot,
           "numxcatx-comparison.png", width = 10, height = 4)
)

## 6.4 Fitted values for two example countries ------------------
model_int_ch6 <- lm(fert_rate ~ income + life_exp + income:life_exp,
                    data = UN_data_ch6)
newpoints_ch6 <- get_regression_points(model_int_ch6,
                                       newdata = slice(UN_data_ch6, c(41, 102)))
p_fitted <- ggplot(UN_data_ch6,
                   aes(x = life_exp, y = fert_rate, color = income)) +
  geom_point() +
  labs(x = "Life expectancy", y = "Fertility rate", title = "Interaction model") +
  geom_smooth(method = "lm", se = FALSE) +
  geom_vline(data = newpoints_ch6,
             aes(xintercept = life_exp, col = income),
             linetype = "dashed", linewidth = 1, show.legend = FALSE) +
  geom_point(data = newpoints_ch6,
             aes(x = life_exp, y = fert_rate_hat),
             size = 3, show.legend = FALSE) +
  geom_point(data = newpoints_ch6,
             aes(x = life_exp, y = fert_rate),
             size = 3, show.legend = FALSE)
suppressMessages(save_fig(p_fitted, "fitted-values.png", width = 7, height = 5))

## 6.5 Credit card debt vs credit limit and income --------------
p_debt_vs_limit <- ggplot(credit_ch6, aes(x = credit_limit, y = debt)) +
  geom_point() +
  labs(x = "Credit limit (in $)", y = "Credit card debt (in $)",
       title = "Debt and credit limit") +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(limits = c(0, 2000))
p_debt_vs_income <- ggplot(credit_ch6, aes(x = income, y = debt)) +
  geom_point() +
  labs(x = "Income (in $1000)", y = "Credit card debt (in $)",
       title = "Debt and income") +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(limits = c(0, 2000)) +
  theme(axis.title.y = element_blank())
suppressMessages(
  save_fig(p_debt_vs_limit + p_debt_vs_income,
           "2numxplot1.png", width = 9, height = 4)
)

# ============================================================
# Chapter 8: Confidence Intervals
# ============================================================

## Output subdirectory for Ch8 images --------------------------
ch8_dir <- file.path(images_dir, "08-confidence-intervals")
dir.create(ch8_dir, showWarnings = FALSE, recursive = TRUE)

save_fig_ch8 <- function(plot, filename, width = 6, height = 4) {
  path <- file.path(ch8_dir, filename)
  ggsave(path, plot = plot, width = width, height = height, dpi = 150)
  message("Saved: 08-confidence-intervals/", filename)
}

## Data setup --------------------------------------------------
# Population parameters for the almonds bowl
mu_ch8    <- almonds_bowl |>
  summarize(mean(weight)) |>
  pull()
sigma_ch8 <- almonds_bowl |>
  summarize(pop_sd(weight)) |>
  pull()

num_almonds_sample_ch8 <- length(almonds_sample_100$weight)
se_xbar_ch8 <- sigma_ch8 / sqrt(num_almonds_sample_ch8)
sample_mean_ch8 <- mean(almonds_sample_100$weight)
lower_bound_ch8 <- sample_mean_ch8 - 1.96 * sigma_ch8 / sqrt(100)
upper_bound_ch8 <- sample_mean_ch8 + 1.96 * sigma_ch8 / sqrt(100)

## 8.1 Sample mean histogram + normal curve (redraw from Ch7) --
set.seed(2)
virtual_mean_weight_100_ch8 <- almonds_bowl |>
  rep_slice_sample(n = num_almonds_sample_ch8, replace = TRUE, reps = 1000) |>
  summarize(mean_weight = mean(weight), n = n())

p_sample_mean_100 <- ggplot(virtual_mean_weight_100_ch8, aes(x = mean_weight)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.01,
                 color = "white") +
  stat_function(fun = dnorm,
                args = list(mean = mu_ch8,
                            sd = sigma_ch8 / sqrt(num_almonds_sample_ch8)),
                col = "red") +
  labs(x = "Sample means with n=100") +
  annotate("point", x = sample_mean_ch8, y = 0, color = "blue") +
  annotate("point", x = mu_ch8, y = 0, color = "red") +
  annotate("text", x = mu_ch8, y = -1, label = "mu", parse = TRUE,
           color = "red") +
  annotate("text", x = sample_mean_ch8, y = -1,
           label = "bar(x)", parse = TRUE, color = "blue")
save_fig_ch8(p_sample_mean_100, "sample-mean-100-with-normal-redraw-1.png",
             width = 6, height = 4)

## 8.2 Three normal distributions -------------------------------
all_points_ch8 <- tibble(
  domain = seq(from = -10, to = 25, by = 0.01),
  `mu = 5, sigma = 2` = dnorm(x = domain, mean = 5, sd = 2),
  `mu = 5, sigma = 5` = dnorm(x = domain, mean = 5, sd = 5),
  `mu = 15, sigma = 2` = dnorm(x = domain, mean = 15, sd = 2)
) |>
  pivot_longer(
    cols = -domain,
    names_to = "Distribution",
    values_to = "value"
  ) |>
  mutate(
    Distribution = factor(
      Distribution,
      levels = c("mu = 5, sigma = 2", "mu = 5, sigma = 5", "mu = 15, sigma = 2")
    )
  )

for_labels_ch8 <- all_points_ch8 |>
  filter(
    (between(domain, 3.795, 3.805) & Distribution == "mu = 5, sigma = 2") |
    (between(domain, 0.005, 0.0105) & Distribution == "mu = 5, sigma = 5") |
    (between(domain, 16.005, 16.015) & Distribution == "mu = 15, sigma = 2")
  )

p_normal_curves <- all_points_ch8 |>
  ggplot(aes(x = domain, y = value, linetype = Distribution)) +
  geom_line() +
  ggrepel::geom_label_repel(data = for_labels_ch8, aes(label = Distribution),
                             nudge_x = c(-1, -2.1, 1)) +
  theme_light() +
  scale_linetype_manual(values = c("solid", "dotted", "longdash")) +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none"
  )
save_fig_ch8(p_normal_curves, "normal-curves-1.png", width = 7, height = 4)

## 8.3 Normal area within one standard deviation ---------------
p_norm_shaded_1a <- ggplot(data = data.frame(x = c(-4, 4)), aes(x)) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1)) +
  geom_area(stat = "function", fun = dnorm, fill = "grey100",
            xlim = c(-4, -1)) +
  geom_area(stat = "function", fun = dnorm, fill = "grey80",
            xlim = c(-1, 1)) +
  geom_area(stat = "function", fun = dnorm, fill = "grey100",
            xlim = c(1, 4)) +
  labs(x = "z", y = "") +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = c(-1, 1))
save_fig_ch8(p_norm_shaded_1a, "normal-curve-shaded-1a-1.png",
             width = 3, height = 4)

## 8.4 Normal area within two standard deviations --------------
p_norm_shaded_2a <- ggplot(data = data.frame(x = c(-4, 4)), aes(x)) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1)) +
  geom_area(stat = "function", fun = dnorm, fill = "grey100",
            xlim = c(-4, -2)) +
  geom_area(stat = "function", fun = dnorm, fill = "grey80",
            xlim = c(-2, 2)) +
  geom_area(stat = "function", fun = dnorm, fill = "grey100",
            xlim = c(2, 4)) +
  labs(x = "z", y = "") +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = c(-2, 2))
save_fig_ch8(p_norm_shaded_2a, "normal-curve-shaded-2a-1.png",
             width = 3, height = 4)

## 8.5 Normal density curve for sample mean weight of almonds --
p_norm_curve_1 <- ggplot(data = data.frame(x = c(3.5, 3.8)), aes(x)) +
  stat_function(fun = dnorm,
                args = list(mean = mu_ch8,
                            sd = sigma_ch8 / sqrt(num_almonds_sample_ch8)),
                col = "red") +
  ylab("") +
  scale_y_continuous(breaks = NULL) +
  labs(x = "Sample means with n=100") +
  annotate("point", x = sample_mean_ch8, y = 0, color = "blue") +
  annotate("point", x = mu_ch8, y = 0, color = "red") +
  annotate("text", x = mu_ch8, y = -1,
           label = paste0("mu == ", round(mu_ch8, 2)), parse = TRUE,
           color = "red") +
  annotate("text", x = sample_mean_ch8, y = -1,
           label = paste0("bar(x) == ", round(sample_mean_ch8, 2)),
           parse = TRUE, color = "blue") +
  geom_hline(yintercept = 0, col = "red", lty = 2)
save_fig_ch8(p_norm_curve_1, "normal-curve-1-1.png", width = 6, height = 4)

## 8.6 Normal density curve showing CI interval -----------------
df_ci_ch8 <- data.frame(
  x1 = lower_bound_ch8, x2 = upper_bound_ch8, y1 = 0, y2 = 0
)
p_norm_curve_2 <- ggplot(data = data.frame(x = c(3.5, 3.8)), aes(x)) +
  stat_function(fun = dnorm,
                args = list(mean = mu_ch8,
                            sd = sigma_ch8 / sqrt(num_almonds_sample_ch8)),
                col = "red") +
  ylab("") +
  scale_y_continuous(breaks = NULL) +
  labs(title = "The Sampling Distribution of the Sample Mean",
       x = "Sample mean weights") +
  geom_point(aes(x = sample_mean_ch8, y = 0), color = "blue") +
  geom_point(aes(x = mu_ch8, y = 0), color = "red") +
  annotate(geom = "text", x = mu_ch8, y = -1,
           label = "\u03BC", color = "red") +
  annotate(geom = "text", x = sample_mean_ch8, y = -1,
           label = "x\u0305", color = "blue") +
  geom_hline(yintercept = 0, col = "red", lty = 2) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2),
               data = df_ci_ch8, col = "blue")
save_fig_ch8(p_norm_curve_2, "normal-curve-2-1.png", width = 6, height = 4)

## 8.7 Standard normal and two t-distributions -----------------
p_t_curve_1 <- ggplot(data = data.frame(x = c(-4, 4)), aes(x)) +
  stat_function(fun = dnorm, args = list(), col = "black") +
  ylab("") +
  stat_function(fun = dt, args = list(df = 2), col = "blue",
                linetype = "dotted") +
  ylab("") +
  stat_function(fun = dt, args = list(df = 10), col = "red",
                linetype = "dashed") +
  ylab("") +
  scale_y_continuous(breaks = NULL) +
  labs(x = "The standard normal and two t-distributions")
save_fig_ch8(p_t_curve_1, "t-curve-1-1.png", width = 6, height = 4)

## 8.8 100 confidence intervals for almond mean ----------------
set.seed(202)
almond_mean_cis_ch8 <- almonds_bowl |>
  rep_sample_n(size = 100, reps = 100, replace = FALSE) |>
  summarize(sample_mean = mean(weight), sample_sd = sd(weight),
            size = n()) |>
  mutate(
    lower_bound = sample_mean - qt(.975, size - 1) * sample_sd / sqrt(size),
    upper_bound = sample_mean + qt(.975, size - 1) * sample_sd / sqrt(size),
    captured = lower_bound <= mu_ch8 & upper_bound >= mu_ch8
  )

p_almond_cis <- ggplot(almond_mean_cis_ch8) +
  geom_segment(aes(
    y = replicate, yend = replicate,
    x = lower_bound, xend = upper_bound,
    alpha = factor(captured, levels = c("TRUE", "FALSE"))
  )) +
  labs(
    x = expression("Sample mean weight of almonds"),
    y = "Confidence interval number",
    alpha = "Captured"
  ) +
  geom_vline(xintercept = mu_ch8, color = "red") +
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank()
  )
save_fig_ch8(p_almond_cis, "almond-mean-cis-1.png",
             width = 6, height = 5)

## 8.9 Normal curve with shaded middle 0.95 area ---------------
p_norm_shaded_3a <- ggplot(data = data.frame(x = c(-4, 4)), aes(x)) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1)) +
  geom_area(stat = "function", fun = dnorm, fill = "grey100",
            xlim = c(-4, -1.96)) +
  geom_area(stat = "function", fun = dnorm, fill = "grey80",
            xlim = c(-1.96, 1.96)) +
  geom_area(stat = "function", fun = dnorm, fill = "grey100",
            xlim = c(1.96, 4)) +
  labs(x = "z", y = "") +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = NULL) +
  geom_point(aes(x = 0, y = 0), color = "red") +
  geom_point(aes(x = -1.96, y = 0), color = "red") +
  geom_point(aes(x = 1.96, y = 0), color = "red") +
  annotate(geom = "text", x = -1.96, y = -0.03, label = "-q", color = "red") +
  annotate(geom = "text", x = 1.96, y = -0.03, label = "q", color = "red") +
  annotate(geom = "text", x = 0, y = -0.04, label = "0", color = "red")
save_fig_ch8(p_norm_shaded_3a, "normal-curve-shaded-3a-1.png",
             width = 3, height = 4)

## 8.10 Bootstrap sample vs original sample histograms ---------
set.seed(202)
boot_sample_ch8 <- almonds_sample_100 |>
  ungroup() |>
  select(-replicate) |>
  rep_sample_n(size = 100, replace = TRUE, reps = 1)

almonds_clean_ch8 <- almonds_sample_100 |>
  ungroup() |>
  select(-replicate)

p_boot_orig_1 <- ggplot(boot_sample_ch8, aes(x = weight)) +
  geom_histogram(binwidth = 0.1, color = "white") +
  labs(title = "Resample of 100 almonds' weights") +
  scale_x_continuous(limits = c(2.85, 4.15),
                     breaks = seq(2.85, 4.15, 0.1)) +
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, 5))
p_boot_orig_2 <- ggplot(almonds_clean_ch8, aes(x = weight)) +
  geom_histogram(binwidth = 0.1, color = "white") +
  labs(title = "Original sample of 100 almonds' weights") +
  scale_x_continuous(limits = c(2.85, 4.15),
                     breaks = seq(2.85, 4.15, 0.1)) +
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, 5))
save_fig_ch8(
  p_boot_orig_1 + p_boot_orig_2 + plot_layout(ncol = 1, guides = "collect"),
  "origandresample-1.png", width = 7, height = 6
)

## 8.11 Distribution of 35 bootstrap sample means --------------
set.seed(20)
bootstrap_samples_35_ch8 <- almonds_clean_ch8 |>
  rep_sample_n(size = 100, replace = TRUE, reps = 35)

boot_means_35_ch8 <- bootstrap_samples_35_ch8 |>
  summarize(mean_weight = mean(weight))

p_resampling_35 <- ggplot(boot_means_35_ch8, aes(x = mean_weight)) +
  geom_histogram(binwidth = 0.01, color = "white") +
  labs(x = "sample mean weight in grams")
save_fig_ch8(p_resampling_35, "resampling-35-1.png", width = 6, height = 4)

## 8.12 Histogram of 1000 bootstrap sample means ---------------
set.seed(20)
boot_means_1000_ch8 <- almonds_clean_ch8 |>
  rep_sample_n(size = 100, replace = TRUE, reps = 1000) |>
  summarize(mean_weight = mean(weight))

p_one_thousand_means <- ggplot(boot_means_1000_ch8, aes(x = mean_weight)) +
  geom_histogram(binwidth = 0.01, color = "white") +
  labs(x = "sample mean weight in grams")
suppressMessages(
  save_fig_ch8(p_one_thousand_means, "one-thousand-sample-means-1.png",
               width = 6, height = 4)
)

## 8.13 Bootstrap distribution via infer -----------------------
set.seed(20)
bootstrap_means_ch8 <- almonds_clean_ch8 |>
  specify(response = weight) |>
  generate(reps = 1000) |>
  calculate(stat = "mean")

p_boot_dist_infer <- visualize(bootstrap_means_ch8)
save_fig_ch8(p_boot_dist_infer, "bootstrap-distribution-infer-1.png",
             width = 6, height = 4)

## 8.14 Percentile CI visualization ----------------------------
percentile_ci_ch8 <- bootstrap_means_ch8 |>
  get_confidence_interval(level = 0.95, type = "percentile")

p_percentile_ci <- visualize(bootstrap_means_ch8) +
  shade_confidence_interval(endpoints = percentile_ci_ch8)
save_fig_ch8(p_percentile_ci, "percentile-ci-viz-1.png",
             width = 6, height = 4)

## 8.15 Standard-error CI visualization -----------------------
x_bar_ch8 <- almonds_clean_ch8 |>
  specify(response = weight) |>
  calculate(stat = "mean")

standard_error_ci_ch8 <- bootstrap_means_ch8 |>
  get_confidence_interval(type = "se", point_estimate = x_bar_ch8,
                          level = 0.95)

p_se_ci <- visualize(bootstrap_means_ch8) +
  shade_confidence_interval(endpoints = standard_error_ci_ch8)
save_fig_ch8(p_se_ci, "se-ci-viz-1.png", width = 6, height = 4)

## 8.16 Mythbusters yawning bootstrap distribution -------------
set.seed(42)
bootstrap_dist_yawning_ch8 <- mythbusters_yawn |>
  specify(formula = yawn ~ group, success = "yes") |>
  generate(reps = 1000, type = "bootstrap") |>
  calculate(stat = "diff in props", order = c("seed", "control"))

p_boot_myth <- visualize(bootstrap_dist_yawning_ch8) +
  geom_vline(xintercept = 0)
save_fig_ch8(p_boot_myth, "bootstrap-distribution-mythbusters-1.png",
             width = 6, height = 4)

## 8.17 Mythbusters: percentile + SE confidence intervals ------
myth_ci_percentile_ch8 <- bootstrap_dist_yawning_ch8 |>
  get_confidence_interval(type = "percentile", level = 0.95)

obs_diff_props_ch8 <- mythbusters_yawn |>
  specify(formula = yawn ~ group, success = "yes") |>
  calculate(stat = "diff in props", order = c("seed", "control"))

myth_ci_se_ch8 <- bootstrap_dist_yawning_ch8 |>
  get_confidence_interval(type = "se", point_estimate = obs_diff_props_ch8,
                          level = 0.95)

p_boot_myth_ci <- visualize(bootstrap_dist_yawning_ch8) +
  ggtitle("") +
  shade_confidence_interval(endpoints = myth_ci_percentile_ch8, fill = NULL,
                            color = "black") +
  shade_confidence_interval(endpoints = myth_ci_se_ch8, fill = NULL,
                            color = "grey70")
save_fig_ch8(p_boot_myth_ci, "bootstrap-distribution-mythbusters-CI-1.png",
             width = 6, height = 4)

# ============================================================
# Chapter 9: Hypothesis Testing
# ============================================================

## Output subdirectory for Ch9 images --------------------------
ch9_dir <- file.path(images_dir, "09-hypothesis-testing")
dir.create(ch9_dir, showWarnings = FALSE, recursive = TRUE)

save_fig_ch9 <- function(plot, filename, width = 6, height = 4) {
  path <- file.path(ch9_dir, filename)
  ggsave(path, plot = plot, width = width, height = height, dpi = 150)
  message("Saved: 09-hypothesis-testing/", filename)
}

## Data setup --------------------------------------------------
# almonds_sample_100 is in the moderndive package
hypo_test_ch9 <- almonds_sample_100 |>
  summarize(x_bar = mean(weight),
            s     = sd(weight),
            n     = length(weight),
            t     = (x_bar - 3.6) / (s / sqrt(n)))

t_value_ch9 <- hypo_test_ch9$t[[1]]

spotify_metal_deephouse <- spotify_by_genre |>
  filter(track_genre %in% c("metal", "deep-house")) |>
  select(track_id, track_genre, artists, track_name, popularity, popular_or_not)

## 9.1 t-distribution tails for almonds hypothesis test -------
p_t_curve_hypo <- ggplot(data.frame(x = c(-4, 4)), aes(x)) +
  stat_function(fun = dt, args = list(df = 99)) +
  geom_area(stat = "function", fun = dt, args = list(df = 99),
            fill = "pink", xlim = c(-4, -abs(t_value_ch9))) +
  geom_area(stat = "function", fun = dt, args = list(df = 99),
            fill = "white", xlim = c(-abs(t_value_ch9), abs(t_value_ch9))) +
  geom_area(stat = "function", fun = dt, args = list(df = 99),
            fill = "pink", xlim = c(abs(t_value_ch9), 4)) +
  labs(x = "t", y = "") +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = c(round(-abs(t_value_ch9), 2),
                                round(abs(t_value_ch9), 2)))
save_fig_ch9(p_t_curve_hypo, "t-curve-hypo-1.png", width = 3, height = 4)

## 9.2 Barplot: genre vs popularity (metal vs deep-house) ------
p_spotify_bar <- ggplot(spotify_metal_deephouse,
                        aes(x = track_genre, fill = popular_or_not)) +
  geom_bar() +
  labs(x = "Genre of track")
save_fig_ch9(p_spotify_bar, "spotify-genre-barplot-1.png",
             width = 6, height = 4)

## 9.3 Side-by-side barplots: original vs shuffled -------------
# spotify_52_original and spotify_52_shuffled are datasets from moderndive
height1 <- spotify_52_original |>
  group_by(track_genre, popular_or_not) |>
  summarize(n = n(), .groups = "drop") |>
  pull(n) |>
  max()
height2 <- spotify_52_shuffled |>
  group_by(track_genre, popular_or_not) |>
  summarize(n = n(), .groups = "drop") |>
  pull(n) |>
  max()
height_ch9 <- max(height1, height2)

plot_orig <- ggplot(spotify_52_original,
                    aes(x = track_genre, fill = popular_or_not)) +
  geom_bar() +
  labs(x = "Genre of track", title = "Original") +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(0, height_ch9))
plot_shuf <- ggplot(spotify_52_shuffled,
                    aes(x = track_genre, fill = popular_or_not)) +
  geom_bar() +
  labs(x = "Genre of track", y = "", title = "Shuffled") +
  coord_cartesian(ylim = c(0, height_ch9))
save_fig_ch9(plot_orig + plot_shuf,
             "spotify-genre-barplot-permuted-1.png",
             width = 8, height = 4)

## 9.4 & 9.5 Null distribution + p-value (Spotify) ------------
set.seed(2024)
null_distribution_ch9 <- spotify_metal_deephouse |>
  specify(formula = popular_or_not ~ track_genre, success = "popular") |>
  hypothesize(null = "independence") |>
  generate(reps = 1000, type = "permute") |>
  calculate(stat = "diff in props", order = c("metal", "deep-house"))

obs_diff_prop_ch9 <- spotify_metal_deephouse |>
  specify(formula = popular_or_not ~ track_genre, success = "popular") |>
  calculate(stat = "diff in props", order = c("metal", "deep-house"))

p_null_dist <- visualize(null_distribution_ch9, bins = 25)
save_fig_ch9(p_null_dist, "null-distribution-infer-1.png",
             width = 6, height = 4)

p_null_dist_pval <- visualize(null_distribution_ch9, bins = 25) +
  shade_p_value(obs_stat = obs_diff_prop_ch9, direction = "right")
save_fig_ch9(p_null_dist_pval, "null-distribution-infer-2-1.png",
             width = 6, height = 4)

## 9.6 Bootstrap distribution with percentile CI (Spotify) ----
set.seed(16)
bootstrap_distribution_ch9 <- spotify_metal_deephouse |>
  specify(formula = popular_or_not ~ track_genre, success = "popular") |>
  generate(reps = 1000, type = "bootstrap") |>
  calculate(stat = "diff in props", order = c("metal", "deep-house"))

percentile_ci_ch9 <- bootstrap_distribution_ch9 |>
  get_confidence_interval(level = 0.90, type = "percentile")

p_boot_ci <- visualize(bootstrap_distribution_ch9) +
  shade_confidence_interval(endpoints = percentile_ci_ch9)
save_fig_ch9(p_boot_ci,
             "bootstrap-distribution-two-prop-percentile-1.png",
             width = 6, height = 4)

## 9.7 Boxplot: IMDb rating by genre (movies_sample) -----------
p_action_romance <- ggplot(data = movies_sample,
                           aes(x = genre, y = rating)) +
  geom_boxplot() +
  labs(y = "IMDb rating")
save_fig_ch9(p_action_romance, "action-romance-boxplot-1.png",
             width = 6, height = 4)

## 9.8 Movies null distribution with p-value ------------------
set.seed(76)
null_distribution_movies_ch9 <- movies_sample |>
  specify(formula = rating ~ genre) |>
  hypothesize(null = "independence") |>
  generate(reps = 1000, type = "permute") |>
  calculate(stat = "diff in means", order = c("Action", "Romance"))

obs_diff_means_ch9 <- movies_sample |>
  specify(formula = rating ~ genre) |>
  calculate(stat = "diff in means", order = c("Action", "Romance"))

p_null_movies <- visualize(null_distribution_movies_ch9, bins = 10) +
  shade_p_value(obs_stat = obs_diff_means_ch9, direction = "both")
save_fig_ch9(p_null_movies, "null-distribution-movies-2-1.png",
             width = 6, height = 4)

## 9.9 Boxplot: air time by carrier (HA vs AS) -----------------
flights_sample_ch9 <- flights |>
  filter(carrier %in% c("HA", "AS"))

p_ha_as <- ggplot(data = flights_sample_ch9,
                  mapping = aes(x = carrier, y = air_time)) +
  geom_boxplot() +
  labs(x = "Carrier", y = "Air Time")
save_fig_ch9(p_ha_as, "ha-as-flights-boxplot-1.png",
             width = 6, height = 4)

# ============================================================
# Chapter 10: Inference for Regression
# ============================================================

## Data setup ---------------------------------------------------
un_data_ch10 <- un_member_states_2024 |>
  select(country,
         life_exp = life_expectancy_2022,
         fert_rate = fertility_rate_2022) |>
  na.omit()

## 10.1 UN fertility vs life expectancy with regression line ----
p_regline_ch10 <- ggplot(un_data_ch10, aes(x = life_exp, y = fert_rate)) +
  geom_point() +
  labs(x = "Life Expectancy (x)",
       y = "Fertility Rate (y)",
       title = "Relationship between fertility rate and life expectancy") +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.5)
suppressMessages(save_fig(p_regline_ch10, "fig-regline-ch10.png"))

## 10.2 Old Faithful scatterplot --------------------------------
p_geyserplot1 <- ggplot(old_faithful_2024,
                        aes(x = duration, y = waiting)) +
  geom_point(alpha = 0.3) +
  labs(x = "duration", y = "waiting")
save_fig(p_geyserplot1, "fig-geyserplot1.png")

## 10.3 Spotify popularity by genre boxplot --------------------
set.seed(6)
spotify_for_anova <- spotify_by_genre |>
  select(artists, track_name, popularity, track_genre) |>
  filter(track_genre %in% c("country", "hip-hop", "rock"))

p_pop_by_genre <- ggplot(spotify_for_anova,
                          aes(x = track_genre, y = popularity)) +
  geom_boxplot() +
  labs(x = "Genre", y = "Popularity")
save_fig(p_pop_by_genre, "fig-pop-by-genre-plot.png")

## 10.4 t-distribution p-value figure --------------------------
n_of <- nrow(old_faithful_2024)
shade_fn <- function(t, a, b) {
  z <- dt(t, df = n_of - 2)
  z[abs(t) < b & -abs(t) > a] <- NA
  z
}
p_pvalue1 <- ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
  stat_function(fun = dt, args = list(df = n_of - 2)) +
  stat_function(fun = shade_fn, args = list(a = -2, b = 2),
                geom = "area", fill = "blue", alpha = 0.2) +
  scale_x_continuous(name = "t", breaks = seq(-4, 4, 2)) +
  scale_y_continuous(labels = NULL) +
  theme(axis.title.y = element_blank(), axis.ticks.y = element_blank())
save_fig(p_pvalue1, "fig-pvalue1.png")

## 10.5 Annotated residual for one eruption ---------------------
model_ch10 <- lm(waiting ~ duration, data = old_faithful_2024)
reg_pts_ch10 <- get_regression_points(model_ch10)

# The Rmd drills down on the observation where duration=211s and waiting=178min.
# If that exact row is absent (different data vintage), fall back to row 1 so
# the figure still renders with a valid annotated residual.
of_index <- which(old_faithful_2024$duration == 211 &
                    old_faithful_2024$waiting == 178)
if (length(of_index) == 0) of_index <- 1L
of_pt   <- reg_pts_ch10 |> slice(of_index)
x_of    <- of_pt$duration
y_of    <- of_pt$waiting
yhat_of <- of_pt$waiting_hat

p_residual_example <- ggplot(old_faithful_2024, aes(x = duration, y = waiting)) +
  geom_point() +
  labs(x = "duration", y = "waiting",
       title = "Relationship of duration and waiting times") +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.5) +
  annotate("point", x = x_of, y = y_of,    col = "red", size = 4) +
  annotate("point", x = x_of, y = yhat_of, col = "red", shape = 15, size = 4) +
  annotate("segment",
           x = x_of, xend = x_of, y = y_of, yend = yhat_of,
           color = "blue",
           arrow = arrow(type = "closed", length = unit(0.02, "npc")))
suppressMessages(save_fig(p_residual_example, "fig-residual-example.png"))

## 10.6 Side-by-side scatter + residual plot --------------------
g_scatter <- ggplot(old_faithful_2024, aes(x = duration, y = waiting)) +
  geom_point(alpha = 0.6) +
  labs(x = "duration", y = "waiting") +
  geom_smooth(method = "lm", color = "blue", se = FALSE, linewidth = 0.5)
g_resid <- reg_pts_ch10 |>
  ggplot(aes(x = waiting_hat, y = residual)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, color = "blue")
suppressMessages(
  save_fig(g_scatter + g_resid, "fig-scatt-and-residual.png", width = 8, height = 4)
)

## 10.7 Non-linear residual example ----------------------------
set.seed(76)
x_range <- range(old_faithful_2024$duration)
data_nonlin <- old_faithful_2024 |>
  mutate(
    x = duration,
    y = 150 + (((x / 2 - x_range[1]) * (x / 2 - x_range[2])) /
                 (x_range[2] - x_range[1])) * (-1 / 2) +
      rnorm(n(), 0, 1.5)
  )
nonlin_model <- lm(y ~ x, data = data_nonlin)
nonlin_pts   <- get_regression_points(nonlin_model)

g_nl1 <- ggplot(data_nonlin, aes(x = x, y = y)) +
  geom_point(alpha = 0.6) +
  labs(x = "duration", y = "waiting") +
  geom_smooth(method = "lm", color = "blue", alpha = 0.3,
              se = FALSE, linewidth = 0.5)
g_nl2 <- ggplot(nonlin_pts, aes(x = y_hat, y = residual)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, color = "blue")
suppressMessages(
  save_fig(g_nl1 + g_nl2, "fig-non-linear.png", width = 8, height = 4)
)

## 10.8 Time-series residual plot ------------------------------
# old_faithful_2024 has a date column
p_timeplot <- old_faithful_2024 |>
  mutate(residual = resid(model_ch10)) |>
  ggplot(aes(y = residual, x = date)) +
  geom_point()
save_fig(p_timeplot, "fig-time-plot.png", width = 7, height = 4)

## 10.9 Histogram + QQ plot of residuals ----------------------
s_ch10 <- sigma(model_ch10)
g_hist_resid <- ggplot(reg_pts_ch10, aes(x = residual)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = 10, color = "white") +
  stat_function(fun = dnorm,
                args = list(mean = 0, sd = s_ch10),
                col = "blue") +
  xlim(-50, 50) +
  labs(x = "residual")
g_qq_resid <- ggplot(reg_pts_ch10, aes(sample = residual)) +
  geom_qq() +
  geom_qq_line(col = "blue", linewidth = 0.5)
p_model1hist <- gridExtra::grid.arrange(g_hist_resid, g_qq_resid, ncol = 2)
ggsave(file.path(images_dir, "fig-model1residualshist.png"),
       plot = p_model1hist, width = 8, height = 4, dpi = 150)
message("Saved: fig-model1residualshist.png")

## 10.10 Not-normal residuals example --------------------------
set.seed(3)
# Create a right-skewed distribution by squaring normal variates (/40 for scale,
# -10 for centering) to contrast with normal residuals in the histogram + QQ pair.
skewed_resids <- reg_pts_ch10 |>
  mutate(`Not normal` = rnorm(n = n(), mean = 0, sd = s_ch10)^2 / 40 -
           mean(rnorm(n = n(), 0, sd = s_ch10)) - 10)
g_nn1 <- ggplot(skewed_resids, aes(x = `Not normal`)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = 10, color = "white") +
  stat_function(fun = dnorm, args = list(mean = 0, sd = s_ch10),
                col = "blue") +
  xlim(-50, 50) +
  labs(x = "residual")
g_nn2 <- ggplot(skewed_resids, aes(sample = `Not normal`)) +
  geom_qq() +
  geom_qq_line(col = "blue", linewidth = 0.5)
p_not_normal <- gridExtra::grid.arrange(g_nn1, g_nn2, ncol = 2)
ggsave(file.path(images_dir, "fig-not-normal-residuals.png"),
       plot = p_not_normal, width = 8, height = 4, dpi = 150)
message("Saved: fig-not-normal-residuals.png")

## 10.11 Residual plot (duration vs residual) ------------------
p_resid_plot_ch10 <- ggplot(reg_pts_ch10,
                              aes(x = duration, y = residual)) +
  geom_point(alpha = 0.6) +
  labs(x = "duration", y = "residual") +
  geom_hline(yintercept = 0)
save_fig(p_resid_plot_ch10, "fig-residual-plot-ch10.png")

## 10.12 Equal-variance / heteroscedastic residuals example ----
p_eq_var <- old_faithful_2024 |>
  mutate(eps = (rnorm(n(), 0, 0.075 * duration^2)) * 0.4) |>
  ggplot(aes(x = duration, y = eps)) +
  geom_point() +
  labs(x = "duration", y = "residual") +
  geom_hline(yintercept = 0, col = "blue", linewidth = 0.5)
save_fig(p_eq_var, "fig-equal-variance-residuals.png")

## 10.13 Bootstrap distribution of slope ----------------------
set.seed(76)
bootstrap_distn_slope <- old_faithful_2024 |>
  specify(formula = waiting ~ duration) |>
  generate(reps = 1000, type = "bootstrap") |>
  calculate(stat = "slope")
p_boot_slope <- visualize(bootstrap_distn_slope)
save_fig(p_boot_slope, "fig-bootstrap-distribution-slope.png")

## 10.14 Null distribution of slope ----------------------------
set.seed(76)
null_distn_slope <- old_faithful_2024 |>
  specify(waiting ~ duration) |>
  hypothesize(null = "independence") |>
  generate(reps = 1000, type = "permute") |>
  calculate(stat = "slope")
p_null_slope <- visualize(null_distn_slope)
save_fig(p_null_slope, "fig-null-distribution-slope.png")

## 10.15 Null distribution with p-value shaded -----------------
b1_ch10 <- coef(model_ch10)[["duration"]]
p_pvalue_slope <- visualize(null_distn_slope) +
  shade_p_value(obs_stat = b1_ch10, direction = "both")
save_fig(p_pvalue_slope, "fig-p-value-slope.png")

## 10.16 Coffee scatter matrix (GGally) -----------------------
coffee_data <- coffee_quality |>
  select(aroma, flavor, moisture_percentage,
         continent_of_origin, total_cup_points) |>
  mutate(continent_of_origin = as.factor(continent_of_origin))
p_coffee_matrix <- suppressMessages(
  GGally::ggpairs(coffee_data)
)
save_fig(p_coffee_matrix, "fig-coffee-scatter-matrix.png",
         width = 9, height = 7)

## 10.17 MLR bootstrap CI (coffee) ----------------------------
set.seed(76)
boot_distribution_mlr <- coffee_quality |>
  specify(total_cup_points ~ continent_of_origin + aroma +
            flavor + moisture_percentage) |>
  generate(reps = 1000, type = "bootstrap") |>
  fit()

observed_fit_mlr <- coffee_quality |>
  specify(total_cup_points ~ continent_of_origin + aroma +
            flavor + moisture_percentage) |>
  fit()

confidence_intervals_mlr <- boot_distribution_mlr |>
  get_confidence_interval(
    level = 0.95,
    type  = "percentile",
    point_estimate = observed_fit_mlr
  )
p_ci_slopes <- visualize(boot_distribution_mlr) +
  shade_confidence_interval(endpoints = confidence_intervals_mlr)
save_fig(p_ci_slopes, "fig-ci-slopes-multiple.png",
         width = 9, height = 7)

## 10.18 Coffee residual diagnostics (grid.arrange) -----------
mlr_model <- lm(
  total_cup_points ~ continent_of_origin + aroma + flavor + moisture_percentage,
  data = coffee_data
)
fit_and_res_mult <- get_regression_points(mlr_model)
g_cof1 <- fit_and_res_mult |>
  ggplot(aes(x = total_cup_points_hat, y = residual)) +
  geom_point() +
  labs(x = "fitted values (total cup points)", y = "residual") +
  geom_hline(yintercept = 0, col = "blue")
g_cof2 <- ggplot(fit_and_res_mult, aes(sample = residual)) +
  geom_qq() +
  geom_qq_line(col = "blue", linewidth = 0.5)
p_coffee_diag <- gridExtra::grid.arrange(g_cof1, g_cof2, ncol = 2)
ggsave(file.path(images_dir, "fig-grid-arrange-plot-check.png"),
       plot = p_coffee_diag, width = 8, height = 4, dpi = 150)
message("Saved: fig-grid-arrange-plot-check.png")

# ============================================================
# Chapter 11: Tell Your Story with Data
# ============================================================

## Data setup --------------------------------------------------
house_prices <- house_prices |>
  mutate(
    log10_price = log10(price),
    log10_size  = log10(sqft_living)
  )

## 11.1 EDA: price, size, condition histograms -----------------
hp1 <- ggplot(house_prices, aes(x = price)) +
  geom_histogram(color = "white") +
  labs(x = "price (USD)", title = "House price") +
  theme(plot.margin = margin(t = 10, r = 10, b = 20, l = 10))
hp2 <- ggplot(house_prices, aes(x = sqft_living)) +
  geom_histogram(color = "white") +
  labs(x = "living space (square feet)", title = "House size") +
  theme(plot.margin = margin(t = 10, r = 10, b = 20, l = 10))
hp3 <- ggplot(house_prices, aes(x = condition)) +
  geom_bar() +
  labs(x = "condition", title = "House condition") +
  theme(plot.margin = margin(t = 10, r = 10, b = 20, l = 10))
suppressMessages(
  save_fig(hp1 + hp2 + hp3 + plot_layout(ncol = 2),
           "fig-house-prices-viz.png", width = 8, height = 6)
)

## 11.2 Before/after log10 transform for price -----------------
hp_log1 <- ggplot(house_prices, aes(x = price)) +
  geom_histogram(color = "white") +
  labs(x = "price (USD)", title = "House price: Before")
hp_log2 <- ggplot(house_prices, aes(x = log10_price)) +
  geom_histogram(color = "white") +
  labs(x = "log10 price (USD)", title = "House price: After")
suppressMessages(
  save_fig(hp_log1 + hp_log2, "fig-log10-price-viz.png", width = 8, height = 4)
)

## 11.3 Before/after log10 transform for size ------------------
hp_sz1 <- ggplot(house_prices, aes(x = sqft_living)) +
  geom_histogram(color = "white") +
  labs(x = "living space (square feet)", title = "House size: Before")
hp_sz2 <- ggplot(house_prices, aes(x = log10_size)) +
  geom_histogram(color = "white") +
  labs(x = "log10 living space (square feet)", title = "House size: After")
suppressMessages(
  save_fig(hp_sz1 + hp_sz2, "fig-log10-size-viz.png", width = 8, height = 4)
)

## 11.4 Interaction and parallel slopes models side-by-side ----
p_hp_interaction <- ggplot(
  house_prices,
  aes(x = log10_size, y = log10_price, col = condition)
) +
  geom_point(alpha = 0.05) +
  geom_smooth(method = "lm", se = FALSE) +
  guides(color = "none") +
  labs(y = "log10 price", x = "log10 size",
       title = "House prices in Seattle")
p_hp_parallel <- ggplot(
  house_prices,
  aes(x = log10_size, y = log10_price, col = condition)
) +
  geom_point(alpha = 0.05) +
  geom_parallel_slopes(se = FALSE) +
  labs(y = NULL, x = "log10 size")
suppressMessages(
  save_fig(p_hp_interaction + p_hp_parallel, "fig-house-price-parallel-slopes-1.png",
           width = 10, height = 5)
)

## 11.5 Interaction model faceted ------------------------------
p_hp_int2 <- ggplot(
  house_prices,
  aes(x = log10_size, y = log10_price, col = condition)
) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(y = "log10 price", x = "log10 size",
       title = "House prices in Seattle") +
  facet_wrap(~condition)
suppressMessages(
  save_fig(p_hp_int2, "fig-house-price-interaction-2.png", width = 9, height = 6)
)

## 11.6 Interaction model with prediction ----------------------
price_interaction <- lm(log10_price ~ log10_size * condition,
                         data = house_prices)
new_house_log_size <- log10(1900)
# Predict for a 1,900 sq-ft house in condition 5 (best condition),
# matching the example in the Rmd (condition = factor(5)).
# Build newdata matching the class/levels of condition in the training data.
EXAMPLE_CONDITION <- 5L   # condition value used in the book's prediction example
if (is.factor(house_prices$condition)) {
  new_cond <- factor(EXAMPLE_CONDITION, levels = levels(house_prices$condition))
} else {
  # integer or numeric condition
  new_cond <- as(EXAMPLE_CONDITION, class(house_prices$condition))
}
new_house_df   <- data.frame(log10_size = new_house_log_size, condition = new_cond)
new_house_pred <- predict(price_interaction, newdata = new_house_df)

p_hp_int3 <- ggplot(
  house_prices,
  aes(x = log10_size, y = log10_price, col = condition)
) +
  geom_point(alpha = 0.05) +
  labs(y = "log10 price", x = "log10 size",
       title = "House prices in Seattle") +
  geom_smooth(method = "lm", se = FALSE) +
  geom_vline(xintercept = new_house_log_size,
             linetype = "dashed", linewidth = 1) +
  annotate("point", x = new_house_log_size, y = new_house_pred,
           col = "black", size = 3)
suppressMessages(
  save_fig(p_hp_int3, "fig-house-price-interaction-3.png", width = 7, height = 5)
)

## 11.7 US births 1999 line plot -------------------------------
US_births_1999 <- US_births_1994_2003 |>
  filter(year == 1999)
p_us_births <- ggplot(US_births_1999, aes(x = date, y = births)) +
  geom_line() +
  labs(x = "Date",
       y = "Number of births",
       title = "US Births in 1999")
save_fig(p_us_births, "fig-us-births.png", width = 7, height = 4)

message("\nAll figures generated successfully in: ", images_dir)
