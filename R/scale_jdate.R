#' @export
scale_type.jdate <- function(x) c("jdate", "continuous")

x_aes <- c("x", "xmin", "xmax", "xend", "xintercept")
y_aes <- c("y", "ymin", "ymax", "yend", "yintercept")

#' Position scales for Jalali date/time data
#'
#' @inheritParams ggplot2::scale_x_date
#' @examples
#' library(ggplot2)
#'
#' last_month <- shide::jdate_now() - 0:29
#' set.seed(1)
#' df <- data.frame(
#'   date = last_month,
#'   price = runif(30)
#' )
#' base <- ggplot(df, aes(date, price)) +
#'   geom_line()
#'
#' base + scale_x_jdate(date_labels = "%m-%d")
#' base + scale_x_jdate(date_breaks = "1 week", date_minor_breaks = "1 day", expand = c(0, 0))
#'
#' # Set limits
#' base + scale_x_jdate(limits = c(shide::jdate_now() - 7, NA))
#' @export
scale_x_jdate <- function(name = waiver(),
                          breaks = waiver(),
                          date_breaks = waiver(),
                          labels = waiver(),
                          date_labels = waiver(),
                          minor_breaks = waiver(),
                          date_minor_breaks = waiver(),
                          limits = NULL,
                          expand = waiver(),
                          oob = censor,
                          guide = waiver(),
                          position = "bottom",
                          sec.axis = waiver()) {

    sc <- jdatetime_scale(
        x_aes,
        "date",
        name = name,
        palette = identity,
        breaks = breaks,
        date_breaks = date_breaks,
        labels = labels,
        date_labels = date_labels,
        minor_breaks = minor_breaks,
        date_minor_breaks = date_minor_breaks,
        guide = guide,
        limits = limits,
        expand = expand,
        oob = oob,
        position = position
    )

    ggplot2:::set_sec_axis(sec.axis, sc)
}

#' @rdname scale_x_jdate
#' @export
scale_y_jdate <- function(name = waiver(),
                          breaks = waiver(),
                          date_breaks = waiver(),
                          labels = waiver(),
                          date_labels = waiver(),
                          minor_breaks = waiver(),
                          date_minor_breaks = waiver(),
                          limits = NULL,
                          expand = waiver(),
                          oob = censor,
                          guide = waiver(),
                          position = "bottom",
                          sec.axis = waiver()) {

    sc <- jdatetime_scale(
        y_aes,
        "date",
        name = name,
        palette = identity,
        breaks = breaks,
        date_breaks = date_breaks,
        labels = labels,
        date_labels = date_labels,
        minor_breaks = minor_breaks,
        date_minor_breaks = date_minor_breaks,
        guide = guide,
        limits = limits,
        expand = expand,
        oob = oob,
        position = position
    )

    ggplot2:::set_sec_axis(sec.axis, sc)
}

jdatetime_scale <- function(aesthetics, transform,
                            palette, breaks = pretty_breaks(), minor_breaks = waiver(),
                            labels = waiver(), date_breaks = waiver(),
                            date_labels = waiver(),
                            date_minor_breaks = waiver(), timezone = NULL,
                            guide = "legend", call = caller_call(), ...) {
    call <- call %||% rlang::current_call()

    if (is.character(breaks)) breaks <- breaks_width_ggsh(breaks)
    if (is.character(minor_breaks)) minor_breaks <- breaks_width_ggsh(minor_breaks)

    if (!is.waive(date_breaks)) {
        breaks <- breaks_width_ggsh(date_breaks)
    }
    if (!is.waive(date_minor_breaks)) {
        minor_breaks <- breaks_width_ggsh(date_minor_breaks)
    }
    if (!is.waive(date_labels)) {
        labels <- function(self, x) {
            label_jdate(date_labels)(x)
        }
    }

    # x/y position aesthetics should use ScaleContinuousDate or
    if (all(aesthetics %in% c(x_aes, y_aes))) {
        scale_class <- switch(
            transform,
            date = ScaleContinuousJdate
        )
    } else {
        scale_class <- ScaleContinuous
    }

    transform <- switch(
        transform,
        date = transform_jdate()
    )

    sc <- continuous_scale(
        aesthetics,
        palette = palette,
        breaks = breaks,
        minor_breaks = minor_breaks,
        labels = labels,
        guide = guide,
        transform = transform,
        call = call,
        ...,
        super = scale_class
    )
    sc$timezone <- timezone
    sc
}

ScaleContinuousJdate <- ggproto("ScaleContinuousJdate", ScaleContinuous,
    secondary.axis = waiver(),
    map = function(self, x, limits = self$get_limits()) {
        self$oob(x, limits)
    },
    get_breaks = function(self, limits = self$get_limits()) {
        breaks <- ggproto_parent(ScaleContinuous, self)$get_breaks(limits)
        if (is.null(breaks)) {
            return(NULL)
        }
        breaks <- floor(breaks)
    },
    break_info = function(self, range = NULL) {
        breaks <- ggproto_parent(ScaleContinuous, self)$break_info(range)
        if (!(is.waive(self$secondary.axis) || self$secondary.axis$empty())) {
            self$secondary.axis$init(self)
            breaks <- c(breaks, self$secondary.axis$break_info(breaks$range, self))
        }
        breaks
    },
    sec_name = function(self) {
        if (is.waive(self$secondary.axis)) {
            waiver()
        } else {
            self$secondary.axis$name
        }
    },
    make_sec_title = function(self, title) {
        if (!is.waive(self$secondary.axis)) {
            self$secondary.axis$make_title(title)
        } else {
            ggproto_parent(ScaleContinuous, self)$make_sec_title(title)
        }
    }
)
