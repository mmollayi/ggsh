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
        domain = jdate_make(c(-1095, 2326), c(1, 12), c(1, 29))
    )
}

transform_jdatetime <- function(tz = NULL) {
    force(tz)
    scales::new_transform(
        "jdatetime",
        transform = function(x) {
            if (!inherits(x, "jdatetime")) {
                cli::cli_abort(
                    "{.fun transform_jdatetime} works with objects of class {.cls jdatetime} only"
                )
            }

            if (is.null(tz)) {
                tz <<- sh_tzone(x)
            }

            as.numeric(x)
        },
        inverse = function(x) {
            jdatetime(x, tzone = tz)
        },
        breaks = scales::breaks_pretty(),
        domain = jdatetime_make(c(-1095, 2326), c(1, 12), c(1, 29), tzone = "UTC")
    )
}
