test_that("breaks_pretty() dispatches to pretty.jdate() for jdate inputs", {
    x <- jdate_make(1404, 11, 23:24)
    expect_s3_class(scales::breaks_pretty(n = 2)(x), "jdate")
})

test_that("breaks_pretty() dispatches to pretty.jdatetime() for jdatetime inputs", {
    x <- jdatetime_make(1404, 11, 23:24, tzone = "Asia/Tehran")
    expect_s3_class(scales::breaks_pretty(n = 2)(x), "jdatetime")
})
