
bon_sk_tabs <- function(
    Bon_sk_year,
    forecast_year,
    forecastdate
) {

  highlight_row <- which(Bon_sk_year$CountDate == forecastdate)

  ### 10-year average

  #### Prediction

  pred10yr_tab <- Bon_sk_year |>
    dplyr::filter(dplyr::between(CountDate,
                                 as.Date(paste0(forecast_year, "-06-01")),
                                 forecastdate)) |>
    dplyr::ungroup() |>
    dplyr::arrange(CountDate) |>
    dplyr::mutate(cum_cnt = cumsum(Sockeye)) |>
    dplyr::arrange(dplyr::desc(CountDate)) |>
    dplyr::mutate(Date = format(CountDate, "%d-%b")) |>
    dplyr::select(Date, Sockeye, cum_cnt,
                  pred_Ave_10yr_10_days_early:pred_Ave_10yr_1_days_early,
                  pred_Ave_10yr,
                  pred_Ave_10yr_1_days_late:pred_Ave_10yr_10_days_late) |>
    `colnames<-`(c("Date", "Daily counts", "Cumulative count",
                   paste(10:1, "Days Early"), "10yr Average", paste(1:10, "Days Late"))) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric),
                                \(x) format(round(x), scientific = FALSE, big.mark = ","))) |>
    knitr::kable(format = "html") |>
    kableExtra::kable_styling(full_width = TRUE) |>
    kableExtra::column_spec(14, background = "lightgrey") |>
    kableExtra::row_spec(1, background = "lightgrey") |>
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover")) |>
    kableExtra::scroll_box(height = "400px", width = "100%") |>
    htmltools::browsable()

  #### Proportion complete

  prop10yr_tab <- Bon_sk_year |>
    dplyr::filter(CountDate >= as.Date(paste0(forecast_year, "-06-01"))) |>
    dplyr::ungroup() |>
    dplyr::mutate(Date = format(CountDate, "%d-%b")) |>
    dplyr::select(Date,
                  Ave_10yr_10_days_early:Ave_10yr_1_days_early,
                  Ave_10yr,
                  Ave_10yr_1_days_late:Ave_10yr_10_days_late) |>
    `colnames<-`(c("Date", paste(10:1, "Days Early"), "10yr Average", paste(1:10, "Days Late"))) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) round(x, 2))) |>
    knitr::kable(format = "html") |>
    kableExtra::kable_styling(full_width = TRUE) |>
    kableExtra::column_spec(12, background = "lightgrey") |>
    kableExtra::row_spec(highlight_row, background = "lightgrey") |>
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover")) |>
    kableExtra::scroll_box(height = "400px", width = "100%") |>
    htmltools::browsable()


  ### 5-year average

  #### Prediction

  pred5yr_tab <- Bon_sk_year |>
    dplyr::filter(dplyr::between(CountDate,
                                 as.Date(paste0(forecast_year, "-06-01")),
                                 forecastdate)) |>
    dplyr::ungroup() |>
    dplyr::arrange(CountDate) |>
    dplyr::mutate(cum_cnt = cumsum(Sockeye)) |>
    dplyr::arrange(dplyr::desc(CountDate)) |>
    dplyr::mutate(Date = format(CountDate, "%d-%b")) |>
    dplyr::select(Date, Sockeye, cum_cnt,
                  pred_Ave_5yr_10_days_early:pred_Ave_5yr_1_days_early,
                  pred_Ave_5yr,
                  pred_Ave_5yr_1_days_late:pred_Ave_5yr_10_days_late) |>
    `colnames<-`(c("Date", "Daily counts", "Cumulative count",
                   paste(10:1, "Days Early"), "5yr Average", paste(1:10, "Days Late"))) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric),
                                \(x) format(round(x), scientific = FALSE, big.mark = ","))) |>
    knitr::kable(format = "html") |>
    kableExtra::kable_styling(full_width = TRUE) |>
    kableExtra::column_spec(14, background = "lightgrey") |>
    kableExtra::row_spec(1, background = "lightgrey") |>
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover")) |>
    kableExtra::scroll_box(height = "400px", width = "100%") |>
    htmltools::browsable()

  #### Proportion complete

  prop5yr_tab <- Bon_sk_year |>
    dplyr::filter(CountDate >= as.Date(paste0(forecast_year, "-06-01"))) |>
    dplyr::ungroup() |>
    dplyr::mutate(Date = format(CountDate, "%d-%b")) |>
    dplyr::select(Date,
                  Ave_5yr_10_days_early:Ave_5yr_1_days_early,
                  Ave_5yr,
                  Ave_5yr_1_days_late:Ave_5yr_10_days_late) |>
    `colnames<-`(c("Date", paste(10:1, "Days Early"), "5yr Average", paste(1:10, "Days Late"))) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) round(x, 2))) |>
    knitr::kable(format = "html") |>
    kableExtra::kable_styling(full_width = TRUE) |>
    kableExtra::column_spec(12, background = "lightgrey") |>
    kableExtra::row_spec(highlight_row, background = "lightgrey") |>
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover")) |>
    kableExtra::scroll_box(height = "400px", width = "100%") |>
    htmltools::browsable()


  cat("##### Average timing info {.tabset}", "\n\n")

  cat("###### 10-year average", "\n\n")
  cat("####### Prediction", "\n\n")
  print(pred10yr_tab)
  cat("\n\n")
  cat("####### Proportion complete", "\n\n")
  print(prop10yr_tab)
  cat("\n\n")

  cat("###### 5-year average", "\n\n")
  cat("####### Prediction", "\n\n")
  print(pred5yr_tab)
  cat("\n\n")
  cat("####### Proportion complete", "\n\n")
  print(prop5yr_tab)
  cat("\n\n")

}
