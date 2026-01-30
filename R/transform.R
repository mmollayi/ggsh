transform_jdate <- function() {
    scales::new_transform(
        "jdate",
        transform = function(x) {
            if (!inherits(x, "jdate")) {
                cli::cli_abort(
                    "{.fun transform_jdate} works with objects of class {.cls jdate} only"
                )
            }

            as.numeric(x)
        },
        inverse = function(x) {
            shide::jdate(x)
        },
        breaks = scales::breaks_pretty(),
        domain = jdate(c("-1095-01-01", "2326-12-29"))
    )
}

transform_jdatetime <- function(tz = NULL) {
    force(tz)
    to_time <- function(x) {
        jdatetime(x, tzone = tz)
    }
    from_time <- function(x) {
        if (!inherits(x, "jdatetime")) {
            cli::cli_abort(
                "{.fun transform_jdatetime} works with objects of class {.cls jdatetime} only"
            )
        }

        if (is.null(tz)) {
            tz <<- sh_tzone(x)
        }

        as.numeric(x)
    }
    scales::new_transform(
        "jdatetime",
        transform = "from_time",
        inverse = "to_time",
        breaks = scales::breaks_pretty(),
        domain = to_time(c(-Inf, Inf))
    )
}
