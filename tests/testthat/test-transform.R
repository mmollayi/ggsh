dt <- jdatetime("1404-11-19 15:06:12", tzone = "Asia/Tehran")
d <- as_jdate(dt)

test_that("transform_jdatetime is inversible", {
    trans <- transform_jdatetime()
    expect_identical(dt, trans$inverse(trans$transform(dt)))
})

test_that("transform_jdate is inversible", {
    trans <- transform_jdate()
    expect_identical(d, trans$inverse(trans$transform(d)))
})

test_that("tz arugment overrules default time zone", {
    trans <- transform_jdatetime(tz = "UTC")
    x <- trans$inverse(trans$transform(dt))

    expect_equal(attr(x, "tzone"), "UTC")
})
