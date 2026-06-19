t <- get_monthly_wool_data() %>%
  mutate(date = lubridate::date(date))

wp <- build_color_palette(unique(t$group),meat_colours[1:3])

plot_generic_ts(t,x = "date",y_line = "value",group = "group",y_line_label = "wool price!",palette_line = wp,x_breaks = "18 months",forecast = TRUE,forecast_start = "2021-01-01",forecast_end = "2025-01-01" )


plot_generic_ts(t,x = "date",y_line = "value",group = "group",y_line_label = "wool price!",palette_line = wp,x_breaks = "18 months",forecast = TRUE,forecast_start = 2021,forecast_end = 2025 )

inherits(t[["date"]], c("Date", "POSIXct", "POSIXlt"))


plot_generic_ts(t,x = "date",
                y_line = "value",
                group = "group",
                y_line_label = "wool price!",
                palette_line = wp,
                x_breaks = "18 months",
                x_freq = "monthly",
                forecast = TRUE,
                forecast_start = as.Date("2017-12-01"),
                forecast_end = as.Date("2025-06-01" ))
