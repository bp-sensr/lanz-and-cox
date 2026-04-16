audit_local <- function(task_id) {
  result <- request("http://127.0.0.1:8000") |>
    req_url_path_append("bis/get-aru-full-audit") |>
    req_url_query(ar_task_id = task_id) |>
    req_method("GET") |>
    req_perform() |>
    resp_body_json(simplifyVector = TRUE)

  if (length(result) == 0 || !all(c("ar_source_species_id", "ar_target_species_id") %in% names(result))) return(tibble())

  species <- wt_get_species() |> select(species_code, species_id, species_common_name)

  as_tibble(result) |>
    left_join(species, by = c("ar_source_species_id" = "species_id")) |>
    rename(source_species_code = species_code, source_species_common_name = species_common_name) |>
    left_join(species, by = c("ar_target_species_id" = "species_id")) |>
    rename(target_species_code = species_code, target_species_common_name = species_common_name) |>
    select(-ar_unique_name)
}

wt_get_sync("project_aru_tasks", project = 4346) |>
  slice(1:3) |>
  pull(internal_task_id) |>
  audit_local() |>
  select(ar_tag_id, ar_date, ar_action, source_species_code, target_species_code) |>
  distinct()

