test_that("label_jdate() works as expected for jdate input", {
    x <- jdate("1404-11-24")

    expect_equal(label_jdate()(x), "1404-11-24")
    expect_equal(label_jdate(format = "%J")(x), "1404/11/24")
    expect_equal(label_jdate()(jdate(NA_real_)), NA_character_)
})
