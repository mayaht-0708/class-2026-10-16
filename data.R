start_date <- "2025-09-01"
end_date <- "2026-09-30"

weather_url <- paste0(
  "https://archive-api.open-meteo.com/v1/archive?",
  "latitude=26.1224&longitude=-80.1373",
  "&hourly=temperature_2m,relative_humidity_2m,precipitation,wind_speed_10m",
  "&start_date=", start_date,
  "&end_date=", end_date,
  "&timezone=America%2FNew_York&format=csv"
)

weather <- read.csv(weather_url, skip = 2, comment.char = "#")

write.csv(weather, "ft_lauderdale_hourly_weather.csv", row.names = FALSE)
