# nocov start
force_all <- function(...) list(...)

# Standalone types check brought over from rlang
check_string <- function(
        x,
        ...,
        allow_empty = TRUE,
        allow_na = FALSE,
        allow_null = FALSE,
        arg = caller_arg(x),
        call = caller_env()
) {
    if (!missing(x)) {
        is_string <- .rlang_check_is_string(
            x,
            allow_empty = allow_empty,
            allow_na = allow_na,
            allow_null = allow_null
        )
        if (is_string) {
            return(invisible(NULL))
        }
    }

    stop_input_type(
        x,
        "a single string",
        ...,
        allow_na = allow_na,
        allow_null = allow_null,
        arg = arg,
        call = call
    )
}

.rlang_check_is_string <- function(x, allow_empty, allow_na, allow_null) {
    if (is_string(x)) {
        if (allow_empty || !is_string(x, "")) {
            return(TRUE)
        }
    }

    if (allow_null && is_null(x)) {
        return(TRUE)
    }

    if (allow_na && (identical(x, NA) || identical(x, na_chr))) {
        return(TRUE)
    }

    FALSE
}

# nocov end
