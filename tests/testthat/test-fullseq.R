test_that("fullseq works for jdate as expected", {
    x <- shide::jdate("1403-01-01") + 1:30
    expect_equal(fullseq(x, "1 month"), shide::jdate(c("1403-01-01", "1403-02-01")))
})
