#' YAMLファイルにそのスキーマの種類を属性として追加するためのスクリプト
#' 同様にして，スキーマに属性を追加したい場合には，このスクリプトを修正することで対応できます．

# YAMLファイルの読み込み
yaml_files <- tibble::tibble(file = list.files("data", pattern = "\\.ya?ml$", full.names = TRUE),
                             data = purrr::map(file, yaml::read_yaml))

# ==== publicationスキーマの修正 ====

# publication_typeキーを持つYAMLファイルのみ抽出
publication_yamls <- yaml_files |>
  dplyr::filter(purrr::map_lgl(data, ~ "publication_type" %in% names(.x)))

# スキーマ情報を追加してoutput-yamlディレクトリに保存
publication_yamls <- publication_yamls |>
  dplyr::mutate(data_with_schema = purrr::map(data,
                                              ~ c(list(meta = list(schema = "publication")), .x)),
                output_file = fs::path("output", basename(file)))

# YAMLファイルの書き出し
purrr::walk2(publication_yamls$data_with_schema, publication_yamls$output_file, yaml::write_yaml)


# ==== presentationスキーマの修正 ====

# presentation_typeキーを持つYAMLファイルのみ抽出
publication_yamls <- yaml_files |>
  dplyr::filter(purrr::map_lgl(data, ~ "presentation_type" %in% names(.x)))

# スキーマ情報を追加してoutput-yamlディレクトリに保存
publication_yamls <- publication_yamls |>
  dplyr::mutate(data_with_schema = purrr::map(data,
                                              ~ c(list(meta = list(schema = "presentation")), .x)),
                output_file = fs::path("output", basename(file)))

# YAMLファイルの書き出し
purrr::walk2(publication_yamls$data_with_schema, publication_yamls$output_file, yaml::write_yaml)
