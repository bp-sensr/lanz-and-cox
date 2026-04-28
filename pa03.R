pa03 <- wt_audio_scanner("/Volumes/abmi_general/BCParks/ARU/TRIANGLE/2017/PA03A", file_type = "wav")

lat <- 50.864369
lon <- -129.081898
tz  <- "America/Vancouver"

pa03_aug <- pa03 %>%
  filter(between(julian, 213, 243)) %>%
  mutate(
    datetime = as.POSIXct(recording_date_time, tz = tz),
    date = as.Date(datetime)
  )

sun_times <- pa03_aug %>%
  distinct(date) %>%
  mutate(lat = lat, lon = lon) %>%
  getSunlightTimes(
    data = .,
    keep = c("sunrise", "sunset"),
    tz = tz
  ) %>%
  mutate(
    sunrise = force_tz(sunrise, tz),
    sunset  = force_tz(sunset, tz)
  ) %>%
  select(date, sunrise, sunset)

pa03_aug %>%
  left_join(sun_times, by = "date") %>%
  mutate(
    time_to_sunrise = as.numeric(difftime(sunrise, datetime, units = "mins")),
    time_to_sunset  = as.numeric(difftime(sunset,  datetime, units = "mins")),
    time_to_boundary = if_else(
      abs(time_to_sunrise) < abs(time_to_sunset),
      time_to_sunrise,
      time_to_sunset
    )
  ) |>
  filter(between(time_to_sunrise,40,120)) %>%
  map(.x = .$file_path, .f = ~file.copy(.x, to = "/users/alexandremacphail/desktop/triangle"))
