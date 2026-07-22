#' Label Jalali dates and date-times
#'
#' Create labelling functions that format `jdate` and `jdatetime` values.
#' These functions are primarily intended for use with the `labels` argument of
#' `ggsh` scales such as `scale_x_jdate()` and `scale_x_jdatetime()`.
#'
#' @param format A date or date-time format string.
#' @param tz Optional time zone for `jdatetime` objects.
#' @return A labelling function that takes a vector of `jdate` or `jdatetime`
#'    values and returns a character vector of the same length.
#' @seealso [scales::label_date()]
#' @examples
#' x <- seq(jdatetime("1405-04-15", format = "%F"), by = "1 month", length.out = 2)
#' demo_jdatetime(x)
#' demo_jdatetime(x, labels = label_jdatetime("%m/%d"))
#' demo_jdatetime(x, labels = label_jdatetime("%e %B"))
#' @export
label_jdate <- function(format = NULL, ...) {
    force_all(format)
    function(x) {
        format(x, format = format, ...)
    }
}

#' @rdname label_jdate
#' @export
label_jdatetime <- function(format = NULL, tz = NULL, ...) {
    force_all(format, tz)
    function(x) {
        format(x, format = format, tz = tz, ...)
    }
}
