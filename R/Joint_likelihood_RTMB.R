




#' joint likelihhod data constructor
#'
#' @param dat
#' @param preseason_forecast
#' @param preseason_forecast_log_sd
#'
#' @return
#'
#' @examples
make_joint_likelihood_dat<-function(dat,
                                    preseason_forecast,
                                    preseason_forecast_log_sd){

## Data
list(
logCFlow= dat |> tidyr::fill(cfs_mean_ema) |> dplyr::pull(cfs_mean_ema) |> log() |> scale() |> c(),
InseasonCount  =dat$cum_cnt,
final_bon_log =log(head(dat$tot_adult,-1)),
log_pre_season_forecast = log(preseason_forecast),
preseason_log_sd = preseason_forecast_log_sd
)

}

#' make hoint likelihood parameters
#'
#' @param dat
#'
#' @return
#'
#' @examples
make_joint_like_params_fun<-function(mod_data){

#params
  list(
mu = qlogis(mean(head(mod_data$InseasonCount,-1)/(exp(mod_data$final_bon_log)))),
year_eff   = rep(0.05,length(mod_data$InseasonCount)),
phi        = .5,
tau_proc_err = -2,
B1  = -.2,
log_pred_sd = -.5
)

}





#' fit the joint likelihood model
#'
#' @param dat
#' @param forecast
#' @param forecast_log_sd
#'
#' @return
#' @export
#'
#' @examples
#'
fit_joint_likelihood<-function(dat,forecast,forecast_log_sd){

  RTMB_data<-make_joint_likelihood_dat(dat |> dplyr::filter(year>=2005),forecast,forecast_log_sd)

  RTMB_params<-make_joint_like_params_fun(RTMB_data)

  RTMB_NLL<-Inseasonfor(RTMB_data)

  mod_obj<- RTMB::MakeADFun(RTMB_NLL,RTMB_params,random=c("year_eff"))

  opt <- nlminb(mod_obj$par, mod_obj$fn, mod_obj$gr)

  sdr <- RTMB::sdreport(mod_obj)

  adrep_est<-as.list(sdr, "Est", report=TRUE)
  adrep_sd<-as.list(sdr, "Std", report=TRUE)

  cnt<-RTMB_data$InseasonCount |> tail(1)

  pred<-adrep_est$logitp |> tail(1)
  pred_sd<-adrep_sd$logitp |> tail(1)

  tibble::as_tibble(setNames(as.list(cnt/plogis(qnorm(c(.975,.75,.5,.25,.025),pred,pred_sd))), c("Lo 95","Lo 50","predicted_abundance","Hi 50","Hi 95"))) |>
    dplyr::bind_cols(tibble::tibble(
      logit_p= adrep_est$logitp |> tail(1)|> c(),
                                                                  logit_p_sd= adrep_sd$logitp |> tail(1) |> c(),
                                                                  coef_cfs_mean_ema= adrep_est$B1 |> tail(1)|> c(),
                                                                  coef_cfs_mean_ema_sd= adrep_sd$B1 |> tail(1)|> c()
    )) |>
    dplyr::mutate(mod_type="Joint_Lik",.before=dplyr::everything())



}


#' Retrospective MAPE for joint likelihood model
#'
#' @param dat Full dataset (same as what fit_joint_likelihood2 takes)
#' @param forecast_season Season string
#' @param n_retro Number of retrospective years (default 15)
#'
#' @return List with MAPE and per-year prediction table
#' @export
retro_mape_joint_lik_sk <- function(dat, forecast_log_sd, n_retro = 15) {

  forecast_year <- max(dat$year)
  retro_years <- (forecast_year - n_retro):(forecast_year )

  results <- purrr::map_dfr(retro_years, function(yr) {
    # Subset: pretend yr is the forecast year (drop future data)
    dat_retro <- dat |>
      dplyr::mutate(
        tot_adult = ifelse(year == yr, NA, tot_adult),
        log_tot_adult = ifelse(year == yr, NA, log_tot_adult)
      ) |>
      dplyr::filter(year <= yr)

    tryCatch({

      pred <- fit_joint_likelihood(dat_retro, forecast=sockeye_age3 |> dplyr::filter(year==yr) |> dplyr::pull(forecast),forecast_log_sd)
      pred |> dplyr::mutate(
        retro_year = yr,
        observed = dat |>
          dplyr::filter(year == yr) |>
          dplyr::pull(tot_adult)
      )
    }, error = function(e) {
      tibble::tibble(retro_year = yr, error = conditionMessage(e))
    })
  })

  mape <- results |>
    head(-1) |>
    dplyr::filter(!is.na(observed), !is.na(predicted_abundance)) |>
    dplyr::summarise(
      log_rmse=sqrt(mean(log(predicted_abundance/observed)^2)),
      RMSE = sqrt(mean((predicted_abundance - observed)^2)),
      MAPE = mean(abs(predicted_abundance - observed) / observed) * 100
    ) |>
    dplyr::select(MAPE,RMSE,log_rmse)

  cur_pred <- tail(results,1) |> cbind(mape) |>
    dplyr::mutate(`Hi 50`=exp(qnorm(.75,log(predicted_abundance),log_rmse)),
                  `Hi 95`=exp(qnorm(.975,log(predicted_abundance),log_rmse)),
                  `Lo 50`=exp(qnorm(.25,log(predicted_abundance),log_rmse)),
                  `Lo 95`=exp(qnorm(.025,log(predicted_abundance),log_rmse))) |> dplyr::select(-c(retro_year,observed))

  list(MAPE = mape, retro_predictions = results, cur_pred=cur_pred)
}



#' likelihood function for joint likelihood model in RTMB
#'
#' @param parms
#'
#' @return
#' @export
#'
#' @examples
Inseasonfor <- function(data_list) {

  function(parms){

  RTMB::getAll(parms,data_list, warn=FALSE)

  ## Derived quantities

  # proportion complete
  logitp <- mu + year_eff + B1 * logCFlow
  p <- 1/(1+exp(-logitp))


  ## Negative log-likelihood
  nll <- 0

  ## AR(1) year effect on proportion complere
  phi2 <- 2 / (1 + exp(-phi)) - 1
  tau_proc_err2 <- exp(tau_proc_err)
  nll <- nll - RTMB::dautoreg(year_eff,mu=0, phi=phi2, scale=tau_proc_err2,log=TRUE)

  ## Observation model
  old_pred<-log((InseasonCount[1:(length(InseasonCount)-1)]/
                   (p[1:(length(InseasonCount)-1)])))
  current_pred<-log(InseasonCount[(length(InseasonCount))]/(p[(length(InseasonCount))]))

  #### previous years' total vs predictions
  pred_sd<- exp(log_pred_sd)
  nll <- nll - sum(RTMB::dnorm(final_bon_log, old_pred,pred_sd, log = TRUE))
  #### current year's preseason forecast vs prediction
  nll <- nll - RTMB::dnorm(log_pre_season_forecast, current_pred, preseason_log_sd, log = TRUE)


  ## Reporting

  RTMB::REPORT(p)
  RTMB::REPORT(current_pred)
  RTMB::ADREPORT(logitp)
  RTMB::ADREPORT(current_pred)
  RTMB::ADREPORT(B1)

  return(nll)
  }
}
