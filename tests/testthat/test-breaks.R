test_that("breaks_pretty() dispatches to pretty.jdate() for jdate inputs", {
    x <- jdate_make(1404, 11, 23:24)
    expect_s3_class(scales::breaks_pretty(n = 2)(x), "jdate")
})

test_that("breaks_pretty() dispatches to pretty.jdatetime() for jdatetime inputs", {
    x <- jdatetime_make(1404, 11, 23:24, tzone = "Asia/Tehran")
    expect_s3_class(scales::breaks_pretty(n = 2)(x), "jdatetime")
})

test_that("breaks_width_ggsh() can take numeric offset", {
    x <- jdatetime_make(1404, 11, 23:24, tzone = "Asia/Tehran")
    expect_equal(
        breaks_width_ggsh("1 day", offset = 1)(x),
        jdatetime_make(1404, 11, 23:24, second = 1, tzone = "Asia/Tehran")
    )
})

test_that("breaks_width_ggsh() can take date offset", {
    x <- jdate_make(1404, 11, 23:24)
    expect_equal(
        breaks_width_ggsh("1 year", offset = "3 months")(x),
        jdate(c("1404-04-01", "1405-04-01"))
    )
})

test_that("breaks_width_ggsh() can take time offset", {
    x <- jdatetime_make(1404, 11, 23:24, tzone = "Asia/Tehran")
    expect_equal(
        breaks_width_ggsh("1 day", offset = "1 hour")(x),
        jdatetime_make(1404, 11, 23:24, hour = 1, tzone = "Asia/Tehran")
    )
})

test_that("breaks_width_ggsh() can take difftime offset", {
    x <- jdatetime_make(1404, 11, 23:24, tzone = "Asia/Tehran")
    expect_equal(
        breaks_width_ggsh("1 day", offset = as.difftime(1, units = "hours"))(x),
        jdatetime_make(1404, 11, 23:24, hour = 1, tzone = "Asia/Tehran")
    )
})

test_that("breaks_width_ggsh() can take vector offset", {
    x <- jdatetime_make(1404, 11, 23:24, tzone = "Asia/Tehran")
    expect_equal(
        breaks_width_ggsh("1 month", offset = c("1 day", "1 hour"))(x),
        jdatetime_make(1404, 11:12, 2, hour = 1, tzone = "Asia/Tehran")
    )
})
