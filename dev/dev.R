tepix |>
    # dplyr::filter(date > shide::jalali("1402-03-01")) |>
    ggplot(aes(date, value)) + geom_line() + scale_x_jdate(limits = c(shide::jdate_now() - 40, NA))


last_month <- shide::jdate_now() - 0:29
set.seed(1)
df <- data.frame(
    date = last_month,
    price = runif(30)
)
base <- ggplot(df, aes(date, price)) +
    geom_line()

# The date scale will attempt to pick sensible defaults for
# major and minor tick marks. Override with date_breaks, date_labels
# date_minor_breaks arguments.
base + scale_x_jdate(date_labels = "%m-%d")
base + scale_x_jdate(date_breaks = "1 week")
base + scale_x_jdate(date_minor_breaks = "1 day")

# Set limits
base + scale_x_jdate(limits = c(shide::jdate_now() - 7, NA))
