
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

  current_yr_count <- Bon_sk |>
    dplyr::filter(year == for_year, CountDate <= pred_date) |>
    dplyr::pull(Sockeye) |>
    sum(na.rm = TRUE)

  if (current_yr_count >= 100) {
    summary_plot_tabs(river_env, Bon_sk, pred_date,
                      species_col   = "Sockeye",
                      species_label = "Sockeye salmon",
                      start_month   = 6,
                      end_month     = 7,
                      ref_years     = c(2015))
  } else {
    cat("**Summary plots will appear once cumulative sockeye counts reach 100 fish.**")
    cat("\n\n")
  }

  before_june15 <- (lubridate::month(pred_date) < 6) |
    (lubridate::month(pred_date) == 6 & lubridate::mday(pred_date) < 14)

  if (!before_june15 & current_yr_count > 1000) {
    mod_wrapper_fun_sk(pred_date, counts, river_env, #ocean_cov,
                       Bon_sk_year,
                       write_local = write_local)
  } else {
    cat("\n\n")
    if (before_june15 & current_yr_count <= 1000) {
      cat("**Model predictions for the sockeye season will start on June 15th once cumulative counts exceed 1,000 fish.**")
    } else if (before_june15) {
      cat("**Model predictions for the sockeye season will start on June 15th.**")
    } else {
      cat("**Model predictions will begin once cumulative sockeye counts exceed 1,000 fish.**")
    }
    cat("\n\n")
  }


  bon_sk_tabs(Bon_sk_year, for_year, pred_date)

}
