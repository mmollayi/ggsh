label_jdate <- function(format = NULL, labels = NULL) {
    force_all(format, labels)
    function(x) {
        format(x, format = format, labels = labels)
    }
}

label_jdatetime <- function(format = NULL, tz = NULL, labels = NULL) {
    force_all(format, tz, labels)
    function(x) {
        format(x, format = format, tz = tz, labels = labels)
    }
}
