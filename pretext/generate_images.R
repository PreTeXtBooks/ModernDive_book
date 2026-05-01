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

## 5.1 Scatterplot: fertility vs life expectancy ----------------
p_numxplot1 <- ggplot(un_data_ch5,
                      aes(x = life_exp, y = fert_rate)) +
  geom_point(alpha = 0.1) +
  labs(x = "Life Expectancy", y = "Fertility Rate")
save_fig(p_numxplot1, "fig-numxplot1.png")

## 5.2 Scatterplot with regression line -------------------------
p_numxplot3 <- ggplot(un_data_ch5, aes(x = life_exp, y = fert_rate)) +
  geom_point(alpha = 0.1) +
  labs(x = "Life Expectancy",
       y = "Fertility Rate",
       title = "Relationship of life expectancy and fertility rate") +
  geom_smooth(method = "lm", se = FALSE)
suppressMessages(save_fig(p_numxplot3, "fig-numxplot3.png"))

## 5.3 Scatterplot with annotated residual (Bosnia) -------------
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

## 5.4 Life expectancy histogram --------------------------------
p_lifeexp2022hist <- ggplot(gapminder2022, aes(x = life_exp)) +
  geom_histogram(binwidth = 5, color = "white") +
  labs(x = "Life expectancy", y = "Number of countries",
       title = "Histogram of distribution of worldwide life expectancies")
suppressMessages(save_fig(p_lifeexp2022hist, "fig-lifeexp2022hist.png"))

## 5.5 Faceted histogram by continent ---------------------------
p_catxplot0b <- ggplot(gapminder2022, aes(x = life_exp)) +
  geom_histogram(binwidth = 5, color = "white") +
  labs(x = "Life expectancy", y = "Number of countries",
       title = "Histogram of distribution of worldwide life expectancies") +
  facet_wrap(~continent, nrow = 2)
suppressMessages(save_fig(p_catxplot0b, "fig-catxplot0b.png", width = 8, height = 5))

## 5.6 Boxplot by continent -------------------------------------
p_catxplot1 <- ggplot(gapminder2022, aes(x = continent, y = life_exp)) +
  geom_boxplot() +
  labs(x = "Continent", y = "Life expectancy",
       title = "Life expectancy by continent")
save_fig(p_catxplot1, "fig-catxplot1.png", width = 7, height = 4)

## 5.7 Best-fitting line: 4-panel residuals figure --------------
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

## 5.8 Three lines example --------------------------------------
example_data <- tibble(x = c(0, 0.5, 1), y = c(2, 1, 3))
p_three_lines <- ggplot(example_data, aes(x = x, y = y)) +
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
  geom_hline(yintercept = 2.5, col = "red", linetype = "dotted", linewidth = 1) +
  geom_abline(intercept = 2, slope = -1, col = "forestgreen",
              linetype = "dashed", linewidth = 1) +
  geom_point(size = 4)
suppressMessages(save_fig(p_three_lines, "fig-three-lines.png", width = 5, height = 4))

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

## 11.4 Parallel slopes model ----------------------------------
p_hp_parallel <- ggplot(
  house_prices,
  aes(x = log10_size, y = log10_price, col = condition)
) +
  geom_point(alpha = 0.05) +
  geom_parallel_slopes(se = FALSE) +
  labs(y = "log10 price", x = "log10 size",
       title = "House prices in Seattle")
save_fig(p_hp_parallel, "fig-house-price-parallel-slopes.png",
         width = 7, height = 5)

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
