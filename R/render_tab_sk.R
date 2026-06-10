
render_tab_sk <- function(pred_date, counts, river_env, write_local = write_local) {

  for_year <- lubridate::year(pred_date)

  Bon_sk <- Bon_sk_fun(pred_date, counts)

  # June-July window for plots and tables; full-year cumulative totals are
  # retained inside Bon_sk for proportion/timing calculations.
  Bon_sk_year <- Bon_sk |>
    dplyr::filter(year == for_year) |>
    dplyr::filter(dplyr::between(month, 6, 7)) |>
    dplyr::mutate(Sockeye = ifelse(CountDate > pred_date, NA_real_, Sockeye))

  inital_blurb_fun_sk(Bon_sk |> dplyr::filter(CountDate == pred_date))

  cat("\n\n")

  summary_plot_tabs(river_env, Bon_sk, pred_date,
                    species_col   = "Sockeye",
                    species_label = "Sockeye salmon",
                    start_month   = 6,
                    end_month     = 7,
                    ref_years     = c(2015))

  if ((lubridate::month(pred_date) < 6) |
      (lubridate::month(pred_date) == 6 & lubridate::mday(pred_date) < 15)) {
    cat("\n\n")
    cat("**Model predictions for the sockeye season will start on June 15th**")
    cat("\n\n")
  } else {
    mod_wrapper_fun_sk(pred_date, counts, river_env, #ocean_cov,
                       Bon_sk_year,
                       write_local = write_local)
  }


  bon_sk_tabs(Bon_sk_year, for_year, pred_date)

}
