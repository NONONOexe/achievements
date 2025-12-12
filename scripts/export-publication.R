#' CSV形式にで論文情報をエクスポートするためのスクリプト

calc_my_contribution <- function(authors) {
  my_idx  <- which(purrr::map_chr(authors, "name") %in% my_name)
  my_type <- authors[[my_idx]]$type

  n_contrib <- sum(purrr::map_chr(authors, "type") == "contributor")
  n_super   <- sum(purrr::map_chr(authors, "type") == "supervisor")

  ifelse(my_type == "contributor", 80 / n_contrib, 20 / n_super)
}

format_title <- function(title, doi, url) {
  id <- ifelse(!is.null(doi), doi, ifelse(!is.null(url), url, NA_character_))
  paste0(title, ifelse(!is.na(id), paste0("，", id), ""))
}

format_publication_info <- function(venue, volume, issue, pages) {
  volume_str   <- ifelse(!is.null(volume), paste0("vol. ", volume), "")
  issue_str    <- ifelse(!is.null(issue), paste0("no. ", issue), "")
  pages_prefix <- ifelse(grepl("-", pages), "pp. ", "p. ")
  pages_str    <- paste0(pages_prefix, pages)

  parts <- c(venue, volume_str, issue_str, pages_str)
  parts <- parts[parts != ""]

  paste0(parts, collapse = "，")
}

format_publication_date <- function(date_str) {
  date <- lubridate::ym(date_str)
  paste0(lubridate::year(date), "年", lubridate::month(date), "月")
}

my_name <- c("安藤 圭祐", "Keisuke ANDO")

files <- list.files("data", pattern = "\\.ya?ml$", full.names = TRUE)
raw <- purrr::map(files, yaml::read_yaml)

publications <- dplyr::tibble(
  title            = purrr::map_chr(raw, "title"),
  authors_list     = purrr::map(raw, "authors"),
  doi              = purrr::map(raw, "doi"),
  url              = purrr::map(raw, "url"),
  venue            = purrr::map_chr(raw, "venue"),
  volume           = purrr::map(raw, "volume"),
  issue            = purrr::map(raw, "issue"),
  pages            = purrr::map(raw, "pages"),
  publication_date = purrr::map_chr(raw, "publication_date"),
  publication_type = purrr::map_chr(raw, "publication_type"),
)

publications <- publications |>
  dplyr::filter(publication_type %in% c("domestic_conference",
                                 "poster",
                                 "technical_report")) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    authors_str          = paste(purrr::map_chr(authors_list, "name"), collapse = "，"),
    title_str            = format_title(title, doi, url),
    my_contribution_str  = paste0(calc_my_contribution(authors_list), "%"),
    publication_info_str = format_publication_info(venue, volume, issue, pages),
    publication_date_str = format_publication_date(publication_date)
  ) |>
  dplyr::ungroup()

csv_data <- publications |>
  dplyr::transmute(
    `著者`          = authors_str,
    `タイトル・DOI` = title_str,
    `掲載情報`      = publication_info_str,
    `発行年月`      = publication_date_str,
    `寄与率`        = my_contribution_str,
  )

readr::write_csv(csv_data, "output/publications.csv")
