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
#' date-time values covering the range of x. The resulting sequence is aligned
#' to interval boundaries determined by size, and may extend beyond the
#' minimum and maximum values of x so that the entire range is covered by
#' complete intervals.
#'
#' @return An object of the same class as `x` (`jdate` or `jdatetime`).
#' @examples
#' fullseq(jdate(c("1405-05-11", "1405-06-15")), "1 month")
#' fullseq(jdatetime(c("1405-05-11 10:12:00", "1405-05-11 11:45:00")), "1 hour")
#' @method fullseq jdate
#' @export
fullseq.jdate <- function(range, size, ...) {
    seq(sh_floor(range[1], size), sh_ceiling(range[2], size), by = size)
}

#' @rdname fullseq.jdate
#' @method fullseq jdatetime
#' @export
fullseq.jdatetime <- function(range, size, ...) {
    seq(sh_floor(range[1], size), sh_ceiling(range[2], size), by = size)
}
