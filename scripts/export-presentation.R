# 発表の業績をCSVファイルとして出力するスクリプト

# YAMLファイルの読み込み
files <- list.files("data/presentation", pattern = "\\.ya?ml$", full.names = TRUE)
data <- purrr::map(files, yaml::read_yaml)

extract_presenters <- function(yaml) {
  purrr::map_chr(yaml$presenters, "name") |>
    paste(collapse = "，")
}

extract_title <- function(yaml) {
  title <- yaml$title
  if (!is.null(yaml$url)) {
    paste0(title, "，", yaml$url)
  } else {
    title
  }
}

extract_event <- function(yaml) {
  paste(yaml$event_name, yaml$orgnizer, yaml$location, sep = "，")
}

extract_date <- function(yaml) {
  date <- lubridate::ym(yaml$presentation_date)
  paste0(lubridate::year(date), "年", lubridate::month(date), "月")
}

calc_contribution <- function(yaml, my_name, contributer_weight = 80, supervisor_weight = 20) {
  my_type <- yaml$presenters |>
    purrr::keep(~ .x$name %in% my_name) |>
    purrr::map_chr("type")

  n_contrib <- sum(purrr::map_chr(yaml$presenters, "type") == "contributor")
  n_super   <- sum(purrr::map_chr(yaml$presenters, "type") == "supervisor")

  contribution_percent <- ifelse(my_type == "contributor",
                                 contributer_weight / n_contrib,
                                 supervisor_weight / n_super)

  paste0(contribution_percent, "%")
}

# 出力したい情報
presentations <- dplyr::tibble(
  presenters   = purrr::map_chr(data, extract_presenters),
  title        = purrr::map_chr(data, extract_title),
  event        = purrr::map_chr(data, extract_event),
  date         = purrr::map_chr(data, extract_date),
  contribution = purrr::map_chr(data, calc_contribution, c("Keisuke ANDO", "安藤 圭祐"))
)

# CSVファイルとして保存
readr::write_csv(presentations, "output/presentations.csv")
