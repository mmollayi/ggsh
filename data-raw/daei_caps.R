library(tidyverse)

req <- httr2::request("https://www.rsssf.org/miscellaneous/daei-intlg.html")
x <- httr2::req_perform(req)
x <- httr2::resp_body_html(x) |>
    xml2::xml_child(2) |>
    xml2::xml_child(7) |>
    xml2::xml_text()

df <- x |>
    str_remove("^[:space:]*") |>
    str_extract("(.|\n)*?(?=\n\n)") |>
    str_split_1("\n") |>
    str_sub_all(c(1, 6, 13, 26, 42, 57, 66, 87), c(5, 9, 25, 41, 56, 65, 86, 98))|>
    map(str_trim)

df <- df[-1] |>
    do.call(what = "rbind") |>
    `colnames<-`(c("caps", "goals", "date", "venue", "opponent", "score", "competition", "persian_date")) |>
    as_tibble()

df <- df |>
    mutate(goals = str_extract(goals, "^\\d+")) |>
    mutate(date = lubridate::dmy(date))

df <- df |>
    mutate(competition = ifelse(competition == "", "Friendlies", competition))

df$notes <- NA_character_
i1 <- str_which(df$score, "\\[1\\]")
df$notes[i1] <- "Iran lost 3-4 on penalty kicks"
i2 <- str_which(df$score, "\\[2\\]")
df$notes[i2] <- "Iran won 4-3 on penalty kicks"
i3 <- str_which(df$score, "\\[3\\]")
df$notes[i3] <- "Iran lost 7-8 on penalty kicks"
i4 <- str_which(df$score, "\\[4\\]")
df$notes[i4] <- "Iran won 4-3 on penalty kicks"
i5 <- str_which(df$score, "\\[5\\]")
df$notes[i5] <- "match abandoned in 61' at 1-0 (North Korea walked off) and awarded 3-0
    to Iran; Daei had scored the only goal from a penalty but as the score
    at abandonment is irrelevant to the award of the match, his goal is not
    included in his record"
i6 <- str_which(df$score, "\\[6\\]")
df$notes[i6] <- "Iran lost 3-4 on penalty kicks"

df <- df |>
    mutate(score = str_remove(score, "\\[\\d\\]")) |>
    mutate(goals = as.integer(goals)) |>
    replace_na(list(goals = 0L))

df <- df |>
    mutate(persian_date = na_if(persian_date, "")) |>
    mutate(persian_date = stringi::stri_join("13", persian_date)) |>
    mutate(persian_date = str_replace_all(persian_date, " ", "0")) |>
    mutate(persian_date = shide::jdate(persian_date)) |>
    mutate(persian_date = if_else(is.na(persian_date), shide::as_jdate(date), persian_date))

daei_caps <- df |>
    select(persian_date, competition, opponent, venue, score, goals, notes) |>
    rename(date = persian_date)

usethis::use_data(daei_caps, overwrite = TRUE, internal = FALSE)
