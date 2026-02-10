force_all <- function(...) list(...)

# brought over from scales package
demo_ggplot <- function(x, scale_name, ...) {
    call <- substitute(list(...))
    call[[1]] <- as.name(scale_name)
    cat(paste0(deparse(call), "\n", collapse = ""))

    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        cli::cli_inform("Skipping; {.pkg ggplot2} not installed")
        return(invisible())
    }

    scale <- getExportedValue("ggplot2", scale_name)
    df <- data.frame(x = x, stringsAsFactors = FALSE)
    ggplot2::ggplot(df, ggplot2::aes(x, 1)) +
        ggplot2::geom_blank() +
        scale(NULL, ...) +
        ggplot2::scale_y_continuous(NULL, breaks = NULL) +
        ggplot2::theme(aspect.ratio = 1 / 5)
}

#' @inherit scales::demo_continuous
#' @keywords internal
#' @export
demo_jdatetime <- function(x, ...) {
    demo_ggplot(x, "scale_x_jdatetime", ...)
}
