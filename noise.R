bcsi_recs_2 <- wt_audio_scanner("/Volumes/abmi_general/BCParks/ARU/SCOTT/2024", file_type = "wav")


# Download recording report from the City project
ki <- wt_download_report(1750, 'ARU', 'recording')

# Select an individual location
selected_loc <- 'EDALD02DUN1|EDALD03CAN1|EDALD03HSL1|EDALD03KLK1|EDALD03RAP1|EDALD03TUC1'

# Download the recordings and scan the files
wt_download_media(ki |> filter(grepl(selected_loc, location)), output = "/users/alexandremacphail/aptest", type = "recording")
j <- wt_audio_scanner("/users/alexandremacphail/aptest", file_type = "flac", extra_cols = F)

# Convert flac to wav and scan again
j |> select(file_path) %>% map(.x = .$file_path, .f = ~seewave::wav2flac(.x, reverse = T, overwrite = T))
j <- wt_audio_scanner("/users/alexandremacphail/aptest", file_type = "wav", extra_cols = F)

# Get index and LDFCs
wt_run_ap(j |> filter(grepl(selected_loc,location)), output_dir = paste0("/users/alexandremacphail/aptest/bunch"), path_to_ap = "/users/alexandremacphail/APNnew/AnalysisPrograms")

# Move the files to ldfcs folder physically

# Wrangle the data
zz <- wt_glean_ap(j, input_dir = "/users/alexandremacphail/aptest/ldfcs", purpose = "biotic")

# Get species data
proj <- wt_download_report(1750, 'ARU', 'main')

# Filter species data. Take only the first minute and the distinct number of species detected
spp <- proj |>
  filter(grepl('SST1|A02CAR1|KER1|KER2|TEH1|RUP1|MCO1|MRD1|MUT2|RIT|HAW2|C02RVA1|WCS1|WWR1|LYM|BAR|RIA',location), detection_time < 60) |>
  group_by(location, recording_date_time) |>
  summarise(n = n_distinct(species_code)) |>
  ungroup()

# Get noises data
leq_dat <- decibel_meter_data |>
  filter(grepl('SST1|A02CAR1|KER1|TEH1|KER2|RUP1|MCO1|MRD1|MUT2|RIT|HAW2|C02RVA1|WCS1|WWR1|LYM|BAR|RIA',location)) |>
  mutate(time_10 = floor_date(Time, "1 minute")) |>
  group_by(location, time_10) |>
  summarise(leq = mean(LEQ_dB_A)) |>
  ungroup() |>
  mutate(time_10 = lubridate::force_tz(time_10, "UTC"))

# Extract wrangled indices, filter to NDSI and only the first minute
zz1 <- zz[[1]] |>
  filter(index_variable == "Ndsi", ResultMinute == 0) |>
  mutate(recording_date_time = lubridate::force_tz(recording_date_time, "UTC")) |>
  dplyr::select(location, recording_date_time, index_value)

# Join everything
join_all <- spp |>
  left_join(leq_dat, by = c("location" = "location", "recording_date_time" = "time_10")) |>
  left_join(zz1, by = c("location" = "location", "recording_date_time" = "recording_date_time"))

dat <- join_all |>
  filter(!is.na(leq), !is.na(index_value), !is.na(n)) |>
  mutate(across(c(leq, index_value), scale))

# Scaling factor for plotting richness alongside index_value
scale_fac <- max(dat$index_value, na.rm = TRUE) / max(dat$n, na.rm = TRUE)

# Long format for ggplot
dat_long <- dat |>
  mutate(richness_scaled = n * scale_fac) |>
  pivot_longer(
    cols = c(index_value, richness_scaled),
    names_to = "metric",
    values_to = "value"
  )

# Plot observed values
ggplot(dat_long, aes(x = leq, y = value, colour = metric)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(
    name = "NDSI",
    sec.axis = sec_axis(~ . / scale_fac, name = "Species richness (n)")
  ) +
  scale_colour_manual(
    values = c("index_value" = "black", "richness_scaled" = "red"),
    labels = c("index_value" = "NDSI", "richness_scaled" = "Species richness")
  ) +
  xlab("Noise (dB)") +
  theme_bw()

# Fit GLMM
m <- lme4::glmer(n ~ leq * index_value + (1 | location),
                 data = dat, family = poisson)

# Create new data for predictions
newdat <- expand.grid(
  leq = seq(min(dat$leq), max(dat$leq), length.out = 200),
  index_value = c(-0.5, 0, 0.5),   # representative low / mid / high
  location = NA
)

newdat$pred <- predict(
  m,
  newdat,
  re.form = NA,      # fixed effects only
  type = "response"  # back-transformed from log link
)

# Categorize index for plotting
newdat <- newdat |>
  mutate(index_cat = case_when(
    index_value < -0.3 ~ "Negative",
    index_value >  0.3 ~ "Positive",
    TRUE ~ "Neutral (~0)"
  ))

ggplot(newdat, aes(x = leq, y = pred, colour = index_cat)) +
  geom_line(linewidth = 1.2) +
  scale_colour_manual(values = c(
    "Negative" = "blue",
    "Neutral (~0)" = "gray40",
    "Positive" = "red"
  )) +
  labs(
    x = "Noise (dB)",
    y = "Predicted Species Richness",
    colour = "Index Category"
  ) +
  theme_bw()

