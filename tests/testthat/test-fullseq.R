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
