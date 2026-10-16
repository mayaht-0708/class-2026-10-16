library(ggplot2)

weather <- read.csv("ft_lauderdale_hourly_weather.csv")

temperature_column <- grep("^temperature_2m", names(weather), value = TRUE)
humidity_column <- grep("^relative_humidity_2m", names(weather), value = TRUE)
precipitation_column <- grep("^precipitation", names(weather), value = TRUE)

if (length(temperature_column) != 1 ||
    length(humidity_column) != 1 ||
    length(precipitation_column) != 1) {
  stop("Could not find the temperature, humidity, and precipitation columns in ft_lauderdale_hourly_weather.csv.")
}

weather$time <- as.POSIXct(
  weather$time,
  format = "%Y-%m-%dT%H:%M",
  tz = "America/New_York"
)

if (all(is.na(weather$time))) {
  stop("The weather data does not contain usable times.")
}

weather$date <- as.Date(weather$time)
weather$month <- as.Date(format(weather$date, "%Y-%m-01"))

monthly_mean <- aggregate(
  weather[c(temperature_column, humidity_column)],
  by = list(month = weather$month),
  FUN = mean,
  na.rm = TRUE
)
monthly_precipitation <- aggregate(
  weather[[precipitation_column]],
  by = list(month = weather$month),
  FUN = sum,
  na.rm = TRUE
)
monthly_weather <- data.frame(
  month = monthly_mean$month,
  temperature = monthly_mean[[temperature_column]] * 9 / 5 + 32,
  humidity = monthly_mean[[humidity_column]],
  precipitation = monthly_precipitation$x
)

measurements <- c(
  "Temperature (°F)",
  "Relative humidity (%)",
  "Precipitation (mm/month)"
)
weather_long <- data.frame(
  month = rep(monthly_weather$month, times = length(measurements)),
  measurement = factor(rep(measurements, each = nrow(monthly_weather)), levels = measurements),
  value = c(
    monthly_weather$temperature,
    monthly_weather$humidity,
    monthly_weather$precipitation
  )
)

weather_plot <- ggplot(
  weather_long,
  aes(x = month, y = value, fill = measurement)
) +
  geom_col(width = 24, alpha = 0.9, na.rm = TRUE) +
  facet_wrap(vars(measurement), ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c("#a78bfa", "#f472b6", "#43aa8b"), guide = "none") +
  scale_x_date(
    breaks = monthly_weather$month,
    date_labels = "%b\n%Y",
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  labs(
    title = "Fort Lauderdale Weather by Month",
    subtitle = "Monthly average temperature and humidity, plus monthly total precipitation • September 2025–September 2026",
    x = NULL,
    y = NULL,
    caption = "FORT LAUDERDALE, FL  •  MONTHLY SUMMARIES FROM HOURLY DATA  •  SOURCE: OPEN-METEO"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#0b1020", color = NA),
    panel.background = element_rect(fill = "#0b1020", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#26324a"),
    panel.grid.major.x = element_line(color = "#26324a", linetype = "dotted"),
    panel.spacing.y = grid::unit(0.8, "lines"),
    strip.background = element_rect(fill = "#263b5d", color = "#38bdf8", linewidth = 0.7),
    strip.text = element_text(
      face = "bold",
      color = "#f8fafc",
      margin = margin(7, 8, 7, 8)
    ),
    text = element_text(color = "#e2e8f0"),
    axis.text = element_text(color = "#aab6cc"),
    axis.ticks = element_line(color = "#475569"),
    plot.title = element_text(size = 25, face = "bold", color = "#f8fafc", margin = margin(b = 8)),
    plot.subtitle = element_text(size = 12, color = "#94a3b8", margin = margin(b = 18)),
    plot.caption = element_text(size = 9, color = "#64748b", margin = margin(t = 14)),
    plot.margin = margin(22, 24, 20, 24)
  )

ggsave(
  "ft-lauderdale-weather.png",
  plot = weather_plot,
  width = 11,
  height = 9,
  dpi = 300,
  bg = "#0b1020"
)
