tepix <- local({
    url <- "http://cdn.tsetmc.com/api/Index/GetIndexB2History/32097828799138957"
    response <- xml2::read_html(url) |> xml2::xml_text()
    response <- jsonlite::fromJSON(response)[[1]]
    df <- response |> dplyr::select(2, 3) |> purrr::set_names(c("date", "value"))
    df <- df |>
        dplyr::mutate(date = as.character(date) |> as.Date(format = "%Y%m%d")) |>
        tibble::as_tibble()

    df <- df |> dplyr::mutate(date = shide::as_jdate(date))
    df
})

usethis::use_data(tepix, overwrite = TRUE)
