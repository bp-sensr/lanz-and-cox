bcsi_recs

bcsi_tasks <- wt_get_sync("project_aru_tasks", project = 4021)
targets <- c(dawn = 5, dusk = 2, night = 2)

task_counts <- bcsi_tasks |>
  mutate(year = year(recording_date_time),
         hour = hour(recording_date_time),
         typ = case_when(hour %in% c(4:7) ~ "dawn",
                         hour %in% c(19:22) ~ "dusk",
                         hour %in% c(0:3,23) ~ "night",
                         TRUE ~ NA_character_)) |>
  group_by(location, year, typ) |>
  tally() |>
  ungroup() |>
  mutate(
    target = targets[typ],
    task_missing = pmax(target - n, 0)
  ) |>
  group_by(location, year) |>
  mutate(typ_c = n_distinct(typ), .after = typ) |>
  ungroup() |>
  group_by(location) |>
  mutate(year_c = n_distinct(year), .after = year) |>
  filter(!task_missing == 0)

set.seed(123)

bcsi_recs |>
  mutate(
    year   = year(recording_date_time),
    hour   = hour(recording_date_time),
    julian = yday(recording_date_time),
    typ = case_when(
      hour %in% 4:7        ~ "dawn",
      hour %in% 19:22      ~ "dusk",
      hour %in% c(0:3, 23) ~ "night",
      TRUE ~ NA_character_
    )
  ) |>
  anti_join(
    bcsi_tasks |>
      select(location, recording_date_time),
    by = c("location", "recording_date_time")
  ) |>
  left_join(
    task_counts |> select(location:typ, task_missing),
    by = c("location", "year", "typ")
  ) |>
  filter(
    julian %in% 120:190,
    !is.na(task_missing),
    task_missing > 0
  ) |>
  group_by(location, year, typ) |>
  mutate(rn = row_number(runif(n()))) |>
  filter(rn <= first(task_missing)) |>
  ungroup() |>
  select(-rn) |>
  arrange(location) |>
  mutate(sample_rate = 24000) |>
  rename(length_seconds = recording_duration) |>
  wt_make_aru_tasks(task_method = "1SPT", task_length = 180) |>
  mutate(task_is_complete = "f") |>
  write_excel_csv("/users/alexandremacphail/desktop/lanzpick.csv")

