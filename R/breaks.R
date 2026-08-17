#' Equally spaced breaks for Jalali date and date-time scales
#'
#' An equivalent of [scales::breaks_width()] that returns a break-generating
#' function for use with `ggsh` scales. The returned function generates equally
#' spaced breaks for a given `jdate` or `jdatetime` range.
#'
#' @param width Interval between consecutive breaks. For details on
#'    supported interval specifications, see "Interval specification".
#' @param offset Optional offset or vector of offsets used to shift
#'    breaks away from the default interval boundaries. When multiple
#'    offsets are supplied, they are applied sequentially in the
#'    order given. For details on supported interval specifications,
#'    see "Interval specification".
#' @details
#' Given a `jdate` or `jdatetime` range, the returned function:
#'
#' 1. Extends the range, if necessary, so that it is covered by complete intervals.
#'
#' 2. Generates equally spaced break positions at the specified interval.
#'
#' 3. Applies `offset`, if supplied, to shift the resulting break positions.
#' @return
#' A break-generating function that takes a range of `jdate` or `jdatetime`
#' values and returns a vector of break positions of the same type.
#'
#' @seealso [scales::breaks_width()]
#' @section Interval specification:
#' Intervals are specified as strings of the form `"{n} {unit}"`, where "n" is
#' a positive integer and "unit" is a valid date or time unit.
#' Examples include "2 days",  "15 min" and "15 minute".
#' @examples
#' three_months <- jdatetime(c("1405-06-01", "1405-09-01"), format = "%F")
#' demo_jdatetime(three_months)
#' demo_jdatetime(
#'     three_months,
#'     breaks = breaks_width_ggsh("1 month"),
#'     labels = label_jdatetime("%m/%d")
#' )
#' # Or equivalently:
#' demo_jdatetime(
#'     three_months,
#'     date_breaks = "1 month",
#'     labels = label_jdatetime("%m/%d")
#' )
#'
#' # Shift monthly breaks by 14 days
#' demo_jdatetime(
#'     three_months,
#'     breaks = breaks_width_ggsh("1 month", offset = "14 days"),
#'     labels = label_jdatetime("%m/%d")
#' )
#' @export
breaks_width_ggsh <- function(width, offset = 0) {
    force_all(width, offset)

    function(x) {
        x <- fullseq(x, width)
        for (i in seq_along(offset)) {
            x <- offset_by(x, offset[i])
        }
        x
    }
}

offset_by <- function(x, size) {
    UseMethod("offset_by")
}

#' @export
offset_by.jdate <- function(x, size) {
    fun <- function(x) seq(x, length.out = 2, by = size)[2]
    out <- lapply(x, fun)
    do.call(c, out)
}

#' @export
offset_by.jdatetime <- function(x, size) {
    fun <- function(x) seq(x, length.out = 2, by = size)[2]
    out <- lapply(x, fun)
    do.call(c, out)
}

#' Generate interval-aligned Jalali date and date-time sequences
#'
#' @description
#' Methods for [scales::fullseq()] supporting `jdate` and `jdatetime` vectors.
#'
#' These methods generate complete sequences of regularly spaced Jalali date or
#' date-time values covering a specified range. The resulting sequence is aligned
#' to interval boundaries determined by size, and may extend beyond the
#' lower and upper limits of `range` so that the entire range is covered by
#' complete intervals.
#'
#' @param range A `jdate` or `jdatetime` vector of length two
#'   specifying a range.
#' @param size Interval used to generate the sequence and determine alignment
#'    boundaries. For details on supported interval specifications,
#'    see "Interval specification".
#' @inheritParams rlang::args_dots_empty
#' @return An object of the same class as `x` (`jdate` or `jdatetime`).
#' @inheritSection breaks_width_ggsh Interval specification
#' @examples
#' fullseq(jdate(c("1405-05-11", "1405-06-15")), "1 month")
#' fullseq(jdatetime(c("1405-05-11 10:12:00", "1405-05-11 11:45:00")), "1 hour")
#' @method fullseq jdate
#' @export
fullseq.jdate <- function(range, size, ...) {
    check_dots_empty()
    seq(sh_floor(range[1], size), sh_ceiling(range[2], size), by = size)
}

#' @rdname fullseq.jdate
#' @method fullseq jdatetime
#' @export
fullseq.jdatetime <- function(range, size, ...) {
    check_dots_empty()
    seq(sh_floor(range[1], size), sh_ceiling(range[2], size), by = size)
}
