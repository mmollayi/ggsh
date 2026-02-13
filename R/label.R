label_jdate <- function(format = "%Y-%m-%d") {
    function(x) {
        format(x, format = format)
    }
}
