dt <- jdatetime("1404-11-19 15:06:12", tzone = "Asia/Tehran")
d <- as_jdate(dt)

test_that("transform_jdatetime learns timezones", {
    trans <- transform_jdatetime()
    trans$transform(dt)

    expect_equal(get("tz", environment(trans$transform)), "Asia/Tehran")
})

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

    expect_equal(sh_tzone(x), "UTC")
})

test_that("transform_jdate can invert domain", {
    testthat::skip("Blocked: shide does not yet support Inf/-Inf for jdate")
    trans <- transform_jdate()
    expect_equal(trans$transform(trans$domain), c(-Inf, Inf))
})


test_that("transform_jdatetime can invert domain", {
    testthat::skip("Blocked: shide does not yet support Inf/-Inf for jdatetime")
    trans <- transform_jdatetime()
    expect_equal(trans$transform(trans$domain), c(-Inf, Inf))
})
