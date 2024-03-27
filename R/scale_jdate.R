scale_type.jdate <- function(x) c("jdate", "continuous")

label_jdate <- function(format = "%Y-%m-%d") {
    function(x) {
        format(x, format = format)
    }
}

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
        c("x", "xmin", "xmax", "xend"),
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

    if (!is.waive(date_breaks)) {
        breaks <- breaks_width(date_breaks)
    }
    if (!is.waive(date_minor_breaks)) {
        minor_breaks <- breaks_width(date_minor_breaks)
    }
    if (!is.waive(date_labels)) {
        labels <- function(self, x) {
            #tz <- self$timezone %||% "UTC"
            label_jdate(date_labels)(x)
        }
    }

    # x/y position aesthetics should use ScaleContinuousDate or
    # ScaleContinuousDatetime; others use ScaleContinuous
    if (all(aesthetics %in% c("x", "xmin", "xmax", "xend", "y", "ymin", "ymax", "yend"))) {
        scale_class <- switch(
            transform,
            date = ScaleContinuousJdate
            #time = ScaleContinuousDatetime
        )
    } else {
        scale_class <- ScaleContinuous
    }

    transform <- switch(
        transform,
        date = transform_jdate()
        #time = transform_time(timezone)
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

ScaleContinuousJdate <- ggproto(
    "ScaleContinuousJdate",
    ScaleContinuous,
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
