test_that("label_jdate() works as expected for jdate input", {
    x <- jdate("1404-11-24")

    expect_equal(label_jdate()(x), "1404-11-24")
    expect_equal(label_jdate(format = "%J")(x), "1404/11/24")
    expect_equal(label_jdate()(jdate(NA_real_)), NA_character_)
})

test_that("label_jdatetime() works as expected for jdatetime input", {
    x <- jdatetime("1404-11-24 13:26:00", tzone = "Asia/Tehran")

    expect_equal(label_jdatetime()(x), "1404-11-24 13:26:00")
    expect_equal(label_jdatetime(format = "%T")(x), "13:26:00")
    expect_equal(label_jdatetime()(jdatetime(NA_real_)), NA_character_)
    expect_equal(label_jdatetime(tz = "UTC")(x), "1404-11-24 09:56:00")
})

test_that("label functions can take shide_label object", {
    lbls <- sh_labels(month = letters[1:12], weekday = letters[1:7], am_pm = c("a", "p"))
    d <- jdate("1405-01-01")
    dt <- as_jdatetime(d, tzone = "Asia/Tehran")

    expect_equal(label_jdate(format = "%B%A", labels = lbls)(d), "aa")
    expect_equal(label_jdatetime(format = "%B%A%p",labels = lbls)(dt), "aaa")
})
