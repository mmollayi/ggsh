x <- "1404-11-19"
df <- data.frame(
    time1 = jdatetime(x, "%Y-%m-%d", tzone = "") + 0:6 * 3600,
    time2 = jdatetime(x, "%Y-%m-%d", tzone = "UTC") + 0:6 * 3600,
    time3 = jdatetime(x, "%Y-%m-%d", tzone = "Asia/Tehran") + (0:6 + 13) * 3600,
    y = 1
)

test_that("inherits timezone from data", {
    # Local time
    p <- ggplot(df, aes(y = y)) +
        geom_point(aes(time1)) +
        scale_x_jdatetime()
    sc <- get_panel_scales(p)$x
    expect_true(identical(sc$timezone, ""))
    expect_equal(sc$get_labels()[1], "00:00")

    # UTC
    p <- ggplot(df, aes(y = y)) +
        geom_point(aes(time2)) +
        scale_x_jdatetime()
    sc <- get_panel_scales(p)$x
    expect_equal(sc$timezone, "UTC")
    expect_equal(sc$get_labels()[1], "00:00")
})

test_that("first timezone wins", {
    p <- ggplot(df, aes(y = y)) +
        geom_point(aes(time3)) +
        geom_point(aes(time2), colour = "red") +
        scale_x_jdatetime()
    sc <- get_panel_scales(p)$x
    expect_equal(sc$timezone, "Asia/Tehran")
})

test_that("timezone is not cached across calls", {
    scale_x <- scale_x_jdatetime(date_breaks = "hour", date_labels = "%H:%M")

    p1 <- ggplot(df, aes(y = y)) + geom_point(aes(time2)) + scale_x
    p2 <- ggplot(df, aes(y = y)) + geom_point(aes(time3)) + scale_x

    expect_equal(get_panel_scales(p1)$x$timezone, "UTC")
    expect_equal(get_panel_scales(p2)$x$timezone, "Asia/Tehran")
})

test_that("time scale date breaks and labels work", {
    skip_if_not_installed("hms")

    d <- c(base_time(), base_time() + 5 * 24 * 3600) - base_time()

    sc <- scale_x_time(date_breaks = "1 day", date_labels = "%d")
    sc$train(d)

    breaks <- sc$get_breaks()
    expect_length(breaks, 6)
    labels <- sc$get_labels(breaks)
    expect_equal(labels, paste0("0", 1:6))
})
