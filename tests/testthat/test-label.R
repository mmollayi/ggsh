test_that("date_format works correctly", {
    d <- jdate("1404-11-24")

    expect_equal(label_jdate()(d), "1404-11-24")
    expect_equal(label_jdate(format = "%J")(d), "1404/11/24")
    expect_equal(label_jdate()(jdate(NA_real_)), NA_character_)
})
