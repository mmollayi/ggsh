test_that("fullseq() dispatches to fullseq.jdate() for jdate inputs", {
    x <- jdate_make(1404, day = 23:24)
    expect_s3_class(scales::fullseq(x, "days"), "jdate")
})

test_that("fullseq() dispatches to fullseq.jdatetime() for jdatetime inputs", {
    x <- jdatetime_make(1404, second = 1:2, tzone = "Asia/Tehran")
    expect_s3_class(scales::fullseq(x, "secs"), "jdatetime")
})

test_that("fullseq works for jdate as expected", {
    x <- shide::jdate("1403-01-01") + 1:30
    expect_equal(fullseq(x, "1 month"), shide::jdate(c("1403-01-01", "1403-02-01")))
})

test_that("fullseq works for jdatetime as expected", {
    tz <- "Asia/Tehran"
    dt <- shide::jdatetime(c("1400-01-01 08:29:58", "1400-01-01 08:30:10"), tz)

    expect_equal(
        fullseq(dt, "1 hour"),
        shide::jdatetime_make(1400, 1, 1, 8:9, tzone = tz)
    )

    expect_error(fullseq(dt, ".5 secs"))
})
