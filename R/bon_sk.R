
#' Calculates Sockeye predictions based on 5 and 10 year average run timing.
#'
#' Like \code{Bon_ch_fun} but operates on the full calendar year (no season
#' sub-setting) and uses the \code{Sockeye} column from the dam-count data.
#'
#' @param pred_date Date. The last date of counts on which predictions are based.
#'   Defaults to \code{NULL} (uses today).
#' @param dat Data frame of daily Bonneville counts as returned by
#'   \code{bon_dat_fun()}.
#'
#' @return A tibble analogous to the output of \code{Bon_ch_fun}, with
#'   cumulative counts, proportions, rolling average proportions, lag/lead
#'   timing adjustments, and error metrics based on Sockeye counts.
#' @export
#'
#' @examples
Bon_sk_fun <- function(pred_date = NULL,
                       dat) {

  # Use all rows — Sockeye run spans the full year, so no season sub-setting.
  season_dat <- dat |>
    dplyr::ungroup()

  dat2 <- season_dat |>
    dplyr::right_join(
      season_dat |>
        dplyr::select(year, month, mday) |>
        tidyr::expand(year, tidyr::nesting(month, mday))
    ) |>
    dplyr::mutate(
      CountDate = as.Date(ifelse(
        is.na(CountDate),
        as.Date(paste(year, month, mday, sep = "-")),
        CountDate
      ))
    ) |>
    dplyr::mutate(Sockeye = tidyr::replace_na(.data$Sockeye, 0)) |>
    dplyr::group_by(.data$year) |>
    dplyr::arrange(.data$year, .data$month, .data$mday) |>
    dplyr::mutate(
      total = cumsum(.data$Sockeye),
      total = ifelse(.data$CountDate > pred_date, NA, total),
      prop  = .data$total / sum(.data$Sockeye),
      prop  = pmax(.0004, pmin(.9995, prop))
    ) |>
    dplyr::group_by(.data$month, .data$mday) |>
    dplyr::mutate(
      Ave_5yr  = dplyr::lag(zoo::rollapply(.data$prop,         width = 5,  FUN = \(x) mean(x, na.rm = TRUE), align = "right", fill = NA_real_), 1),
      Ave_10yr = dplyr::lag(zoo::rollapply(.data$prop,         width = 10, FUN = \(x) mean(x, na.rm = TRUE), align = "right", fill = NA_real_), 1),
      across(c(Ave_5yr, Ave_10yr), \(x) pmax(.0004, pmin(.9995, x))),
      Ave_10yr_daily_cnt = dplyr::lag(zoo::rollapply(.data$Sockeye, width = 10, FUN = \(x) mean(x, na.rm = TRUE), align = "right", fill = NA_real_), 1)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(year >= lubridate::year(pred_date) - 30)

  dat3 <- dat2 |>
    dplyr::group_by(year) |>
    dplyr::mutate(
      # lags
      !!!purrr::set_names(
        purrr::list_flatten(
          purrr::map(c("Ave_5yr", "Ave_10yr"), function(col) {
            purrr::map(1:10, function(k) {
              dplyr::expr(dplyr::lag(!!dplyr::sym(col), !!k))
            })
          })
        ),
        purrr::flatten_chr(purrr::map(c("Ave_5yr", "Ave_10yr"), ~ paste0(.x, "_", 1:10, "_days_late")))
      ),
      # leads
      !!!purrr::set_names(
        purrr::list_flatten(
          purrr::map(c("Ave_5yr", "Ave_10yr"), function(col) {
            purrr::map(1:10, function(k) {
              dplyr::expr(dplyr::lead(!!dplyr::sym(col), !!k))
            })
          })
        ),
        purrr::flatten_chr(purrr::map(c("Ave_5yr", "Ave_10yr"), ~ paste0(.x, "_", 1:10, "_days_early")))
      )
    ) |>
    dplyr::ungroup()

  dat3 |>
    dplyr::mutate(dplyr::across(Ave_5yr:dplyr::last_col(), \(x) { .data$total / x }, .names = "pred_{.col}")) |>
    dplyr::group_by(year) |>
    dplyr::mutate(
      obs_tot          = tail(.data$total, 1),
      er_5             = obs_tot - pred_Ave_5yr,
      er_10            = obs_tot - pred_Ave_10yr,
      log_er_5         = log(obs_tot / pred_Ave_5yr),
      log_er_10        = log(obs_tot / pred_Ave_10yr),
      logit_prop       = qlogis(prop),
      logit_prop5yr_er  = logit_prop - qlogis(Ave_5yr),
      logit_prop10yr_er = logit_prop - qlogis(Ave_10yr),
      APE_5            = abs(er_5)  / obs_tot,
      APE_10           = abs(er_10) / obs_tot
    ) |>
    dplyr::arrange(.data$year, .data$month, .data$mday) |>
    dplyr::group_by(month, mday) |>
    dplyr::mutate(
      MAPE_5yr  = dplyr::lag(zoo::rollmean(.data$APE_5,  k = 15, align = "right", fill = NA_real_), 1),
      MAPE_10yr = dplyr::lag(zoo::rollmean(.data$APE_10, k = 15, align = "right", fill = NA_real_), 1),
      logit_prop_sd_5yr  = dplyr::lag(zoo::rollapply(.data$logit_prop5yr_er,  width = 15, FUN = \(x) sqrt(mean(x^2)), align = "right", fill = NA_real_), 1),
      ,
      logit_prop_sd_10yr = dplyr::lag(zoo::rollapply(.data$logit_prop10yr_er, width = 15, FUN = \(x) sqrt(mean(x^2)), align = "right", fill = NA_real_), 1),
    ) |>
    dplyr::arrange(desc(CountDate))
}
