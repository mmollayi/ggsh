#' Transformation for Jalali dates and date-times
#'
#' Create transformation objects for `jdate` and `jdatetime` values for use
#' with `ggsh` scales such as `scale_x_jdate()` and `scale_x_jdatetime()`.
#'
#' @param tz Optional time zone used when transforming `jdatetime` values.
#' If `NULL`, the first non-NULL time zone found in the data is used.
#' @return A transformation object created with [scales::new_transform()].
#' @seealso
#' [scales::new_transform()], [scales::transform_date()], [scales::transform_time()]
#' @examples
#' dates <- jdate(c("1400-01-01", "1401-01-01", "1402-01-01"))
#' # Create a transformation object
#' trans <- transform_jdate()
#' # Transform Jalali dates to numeric values
#' trans$transform(dates)
#' # Recover the original dates
#' trans$inverse(trans$transform(dates))
#' # Generate and format axis breaks
#' trans$format(trans$breaks(range(dates)))
#'
#' # Use a specific time zone when converting back from numeric values
#' trans <- transform_jdatetime(tz = "UTC")
#' times <- jdatetime("1403-01-01 09:00:00", tz = "Asia/Tehran") + 0:1
#' trans$inverse(trans$transform(times))
#' @export
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

#' @rdname transform_jdate
#' @export
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
