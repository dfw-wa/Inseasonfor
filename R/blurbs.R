
inital_blurb_fun<-function(Bon_ch_day,season_dates,morph){




  forecastdate<-Bon_ch_day$CountDate
  forecast_season<-Bon_ch_day$season
  forecast_year<-Bon_ch_day$year

  daily_10_yr_intervals<-
    (Bon_ch_day$total/
       plogis(qnorm(rev(c(.025,.25,.5,.75,.975)),qlogis(Bon_ch_day$Ave_10yr),Bon_ch_day$logit_prop_sd_10yr))) |>
    round() |>
    format(scientific=FALSE, big.mark=",")

    cat(sprintf(
      "The cumulative %s season %s adult Chinook passage at Bonneville Dam through %s is **%s**. The 10-year (%s) average proportion of the count that has occurred at Bonneville Dam through %s is **%s%%**. Based on the cumulative counts to date and this proportion, the expected total %s (%s) season dam count would be %s (95%% prediction interval = %s and 50%% prediction interval = %s) adult Chinook.",
      forecast_season,
      morph,
      format(forecastdate, "%B %d, %Y"),
      format(round(Bon_ch_day$total), scientific = FALSE, big.mark = ","),
      paste0((forecast_year - 10), "--", (forecast_year - 1)),
      format(forecastdate, "%B %d"),
      sprintf("%.2f",Bon_ch_day$Ave_10yr * 100),
      forecast_season,
      chk_season_print(forecast_season, season_dates),
      format(round(Bon_ch_day$pred_Ave_10yr), scientific = FALSE, big.mark = ","),
      paste(daily_10_yr_intervals[c(1, 5)], collapse = " -- "),
      paste(daily_10_yr_intervals[c(2, 4)], collapse = " -- ")
    ))

}


#' Generate the introductory blurb for the Sockeye in-season forecast.
#'
#' Analogous to \code{inital_blurb_fun} but for Sockeye: no sub-season
#' framing, no morph argument.
#'
#' @param Bon_sk_day Single-row tibble from \code{Bon_sk_fun()} filtered to
#'   \code{CountDate == pred_date}.
#'
#' @return Invisibly NULL; called for its side-effect of \code{cat()}-ing HTML.
#' @export
#'
#' @examples
inital_blurb_fun_sk <- function(Bon_sk_day) {

  forecastdate  <- Bon_sk_day$CountDate
  forecast_year <- Bon_sk_day$year

  daily_10_yr_intervals <-
    (Bon_sk_day$total /
       plogis(qnorm(rev(c(.025, .25, .5, .75, .975)),
                    qlogis(Bon_sk_day$Ave_10yr),
                    Bon_sk_day$logit_prop_sd_10yr))) |>
    round() |>
    format(scientific = FALSE, big.mark = ",")

  cat(sprintf(
    "The cumulative Sockeye passage at Bonneville Dam through %s is **%s**. The 10-year (%s) average proportion of the annual count that has occurred at Bonneville Dam through %s is **%s%%**. Based on the cumulative counts to date and this proportion, the expected total annual dam count would be %s (95%% prediction interval = %s and 50%% prediction interval = %s) Sockeye.",
    format(forecastdate, "%B %d, %Y"),
    format(round(Bon_sk_day$total), scientific = FALSE, big.mark = ","),
    paste0((forecast_year - 10), "--", (forecast_year - 1)),
    format(forecastdate, "%B %d"),
    sprintf("%.2f", Bon_sk_day$Ave_10yr * 100),
    format(round(Bon_sk_day$pred_Ave_10yr), scientific = FALSE, big.mark = ","),
    paste(daily_10_yr_intervals[c(1, 5)], collapse = " -- "),
    paste(daily_10_yr_intervals[c(2, 4)], collapse = " -- ")
  ))

}

