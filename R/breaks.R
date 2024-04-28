breaks_width2 <- function(width, offset = 0) {
    force_all(width, offset)

    function(x) {
        x <- fullseq(x, width)
        for (i in offset) {
            x <- offset_by(x, i)
        }
        x
    }
}

offset_by <- function(x, size) {
    UseMethod("offset_by")
}

offset_by.jdate <- function(x, size) {
    fun <- function(x) seq(x, length.out = 2, by = size)[2]
    out <- lapply(x, fun)
    do.call(c, out)
}

#' @method fullseq jdate
#' @export
fullseq.jdate <- function(range, size, ...) {
    seq(sh_floor(range[1], size), sh_ceiling(range[2], size), by = size)
}
