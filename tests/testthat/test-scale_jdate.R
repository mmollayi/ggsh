x <- "1404-11-19"
df <- data.frame(
    time1 = jdatetime(x, "%Y-%m-%d", tzone = "") + 0:6 * 3600,
    time2 = jdatetime(x, "%Y-%m-%d", tzone = "UTC") + 0:6 * 3600,
    time3 = jdatetime(x, "%Y-%m-%d", tzone = "Asia/Tehran") + (0:6 + 13) * 3600,
    y = 1
)

test_that("jdate(time) scales coerce data types", {
    d <- jdate(x)
    dt <- as_jdatetime(d)

    sc <- scale_x_jdatetime()
    df <- sc$transform_df(data.frame(x = d))
    expect_equal(df$x, as.numeric(dt))

    sc <- scale_x_jdate()
    df <- sc$transform_df(data.frame(x = dt))
    expect_equal(df$x, as.numeric(d))
})

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

test_that("jdate scale breaks and labels work", {
    d <- jdate(x) + c(0, 6)

    sc <- scale_x_jdate(date_breaks = "1 day", date_labels = "%d")
    sc$train(d)
    breaks <- sc$get_breaks()
    expect_equal(length(breaks), length(fullseq(d, "days")))
    labels <- sc$get_labels(breaks)
    expect_equal(labels, as.character(19:26))
})

test_that("jdatetime scale breaks and labels work", {
    dt <- jdatetime(x, "%Y-%m-%d", tzone = "Asia/Tehran") + c(0, 6)

    sc <- scale_x_jdatetime(date_breaks = "1 second", date_labels = "%S")
    sc$train(dt)
    breaks <- sc$get_breaks()
    expect_equal(length(breaks), length(fullseq(dt, "seconds")))
    labels <- sc$get_labels(breaks)
    expect_equal(labels, paste0("0", as.character(0:6)))
})

test_that("jdate(time) scales throw errors on numeric input", {
    sc <- scale_x_jdatetime()
    expect_error(sc$transform_df(data.frame(x = 1)))

    sc <- scale_x_jdate()
    expect_error(sc$transform_df(data.frame(x = 1)))
})

# Visual tests ------------------------------------------------------------

test_that("jdate scale draws correctly", {
    # Adapted from ggplot2's scale-date visual tests.
    # The same RNG seed and sampling strategy are used so that the
    # resulting plot closely matches the ggplot2 reference plots,
    # allowing visual inspection to focus on Jalali axis behavior.
    set.seed(321)
    df <- data.frame(
        dx = seq(jdate("1403-12-30"), length.out = 100, by = "1 day")[sample(
            100,
            50
        )],
        price = runif(50)
    )
    df <- df[order(df$dx), ]

    dt <- ggplot(df, aes(dx, price)) + geom_line()
    expect_doppelganger("dates along x, default breaks", dt)
    expect_doppelganger(
        "scale_x_jdate(labels = label_jdate(\"%m/%d\"))",
        dt + scale_x_jdate(labels = label_jdate("%m/%d"))
    )
    expect_doppelganger(
        "scale_x_jdate(labels = label_jdate(\"%J\"))",
        dt + scale_x_jdate(labels = label_jdate("%J"))
    )
})
