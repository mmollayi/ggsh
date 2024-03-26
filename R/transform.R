transformt_jdate <- function() {
    scales::new_transform(
        "jdate",
        transform = function(x) {
            if (!inherits(x, "jdate")) {
                cli::cli_abort("{.fun transform_date} works with objects of class {.cls jdate} only")
            }

            as.numeric(x)
        },
        inverse = function(x) {
            shide::jdate(x)
        },
        breaks = jdate_breaks(),
        domain = jdate(c("-1095-01-01", "2326-12-29"))
    )
}

jdate_breaks <- function(n = 5) {
    force(n)
    function(x) {
        shide::as_jdate(unname(scales::breaks_pretty(n)(as.Date(x))))
    }
}
