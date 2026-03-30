




#' joint likelihhod data constructor2
#'
#' @param dat
#' @param preseason_forecast
#' @param preseason_forecast_log_sd
#'
#' @return
#'
#' @examples
make_joint_likelihood_dat2<-function(dat,forecast_season){



  dat2<-dat |> dplyr::mutate(Delta_logjCK=c(NA,diff(log_lag_jack)),
                             prop=cum_cnt/tot_adult
                             ) |>
    dplyr::left_join(agecomp |> #this is from a file in the data-raw folder that needs to be updated each year using run reconstruction outputs of age proportions
                       dplyr::filter(Season==forecast_season) |>
                       dplyr::select(year=Year,one_ocean_prop=one_ocean_prop )) |>
    dplyr::mutate(one_ocean_prop =ifelse(is.na(one_ocean_prop ),mean(one_ocean_prop ,na.rm=T),one_ocean_prop ),
                  Age4=one_ocean_prop *tot_adult, #age 4 is carryover notation from when this was developed for spring Chinook. For falls, this represents the age 3's.
                  Age5_6=tot_adult-Age4,
                  lag_age4=dplyr::lag(Age4)) |>
  dplyr::filter(year>=
                  ifelse(forecast_season%in%c("Tule","Bright"),2001,1990))


## Data
list(
  logjCK=scale(dat2$log_lag_jack)[,1],
  Delta_logjCK=scale(dat2$Delta_logjCK)[,1],
  lag_logCK4=scale(log(dat2$lag_age4)),
  logCK4=log(head(dat2$Age4,-1)),
  logCK5_6=log(head(dat2$Age5_6,-1)),

  logCFlow=dat2 |> tidyr::fill(cfs_mean_ema) |> dplyr::pull(cfs_mean_ema) |> log() |> scale() |> c(),
  InseasonCount=dat2$cum_cnt,#%>% tail(1),
  obslogitp=qlogis(pmax(pmin(dat2$prop,.99),.01)) %>% head(-1),
  final_bon_log=log(head(dat2$tot_adult,-1))

)

}

#' make joint likelihood parameters
#'
#' @param dat
#'
#' @return
#'
#' @examples
make_joint_like_params_fun2<-function(mod_data){

n_year<-length(mod_data$logjCK)
#params
  list(
mu = mean(mod_data$obslogitp),
beta=rep(.1,3),
alpha=rep(10,n_year),
tau_alpha=0,
alpha2=10,
year_eff=rep(.1,n_year),
phi=0,
tau_proc_err=0,
B1=0,
# tau_logCK4=0,
# tau_logCK5_6=0,
# log_4_forecast=12,
# log_56_forecast=8,
# log_sigma_inseas=0,
resid_err=0
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
fit_joint_likelihood2<-function(dat,forecast_season){

  RTMB_data<-make_joint_likelihood_dat2(dat,forecast_season)

  RTMB_params<-make_joint_like_params_fun2(RTMB_data)

  RTMB_NLL<-Inseasonfor2(RTMB_data)

  mod_obj<- RTMB::MakeADFun(RTMB_NLL,RTMB_params,random=c("alpha","year_eff"),silent=TRUE)#,"log_4_forecast","log_56_forecast"))

  opt <- nlminb(mod_obj$par, mod_obj$fn, mod_obj$gr,trace=0)

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
    dplyr::mutate(mod_type="Joint_Lik",
                  .before=dplyr::everything())


}


#' Retrospective MAPE for joint likelihood model
#'
#' @param dat Full dataset (same as what fit_joint_likelihood2 takes)
#' @param forecast_season Season string
#' @param n_retro Number of retrospective years (default 15)
#'
#' @return List with MAPE and per-year prediction table
#' @export
retro_mape_joint_lik2 <- function(dat, forecast_season, n_retro = 15) {

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
      pred <- fit_joint_likelihood2(dat_retro, forecast_season)
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
Inseasonfor2 <- function(data_list) {

  function(parms){

  RTMB::getAll(parms,data_list, warn=FALSE)

  ## Derived quantities

    ### preseason process model
    #### Age 4 sib regression
    logCK4_mu <- alpha + beta[1]*logjCK +
      beta[2]*Delta_logjCK

    #### Age 5-6 sib regression
    logCK5_6_mu = alpha2 + beta[3]*lag_logCK4;



  # proportion complete
  logitp <- mu + year_eff + B1 * logCFlow
  p <- 1/(1+exp(-logitp))



  ## Negative log-likelihood
  nll <- 0

  ## alpha random walk

  ### Age4 sib regression intercept (random walk)

  sd_alpha=exp(tau_alpha);

                  for(i in 2:length(alpha)){
                    nll <- nll-  RTMB::dnorm(alpha[i]-alpha[i-1],0 ,
                                 sd_alpha,log=TRUE)
                  }
    nll <- nll - (RTMB::dexp(sd_alpha,1,log=TRUE))#+(tau_alpha))

  ## AR(1) year effect on proportion complete
  phi2 <- 2 / (1 + exp(-phi)) - 1
  tau_proc_err2 <- exp(tau_proc_err)
  nll <- nll - RTMB::dautoreg(year_eff,mu=0, phi=phi2, scale=tau_proc_err2,log=TRUE)

  nll <- nll - (RTMB::dexp(tau_proc_err2,1,log=TRUE))

  pred<-InseasonCount/p

  pre_season_forecast<-exp(logCK4_mu)+exp(logCK5_6_mu)

  nll <- nll - sum(RTMB::dnorm(log(pred),
                               log(pre_season_forecast) ,
                               exp(resid_err),log=TRUE))

  nll <- nll - sum(RTMB::dnorm(head(log(pred),-1),
                               final_bon_log ,
                               .001,log=TRUE))


  ## Observation model
  nll <- nll - (RTMB::dexp(exp(resid_err),1,log=TRUE))#+(resid_err))


  #### preseason foreacst model
   # sd_logCK4 = exp(tau_logCK4)
   # sd_logCK5_6 = exp(tau_logCK5_6)

   ####### past years
   # nll <- nll - sum(RTMB::dnorm(logCK4,head(logCK4_mu,-1) ,sd_logCK4,log=TRUE))
   #
   # nll <- nll - sum(RTMB::dnorm(logCK5_6,head(logCK5_6_mu,-1) ,sd_logCK5_6,log=TRUE))

   ####### current year
#  pre_season_forecast<-exp(tail(logCK4_mu,1))+exp(tail(logCK5_6_mu,1))
#  pre_season_pred<-(tail(InseasonCount,1)/tail(p,1))
# #
#  nll <- nll - RTMB::dnorm(log(pre_season_forecast), log(pre_season_pred), exp(resid_err), log = TRUE)
# #
# old_pred<-(head(InseasonCount,-1)/head(p,-1))
# old_obs<-final_bon_log
#
# nll <- nll - sum(RTMB::dnorm(log(old_pred), final_bon_log,  exp(resid_err), log = TRUE))
#
# RTMB::REPORT(old_pred)

   #### current year's preseason forecast vs prediction
#
#    nll <- nll - RTMB::dnorm(log_4_forecast,tail(logCK4_mu,1) ,exp(tau_logCK4),log=TRUE)
#
#    nll <- nll - RTMB::dnorm(log_56_forecast,tail(logCK5_6_mu,1) ,exp(tau_logCK5_6),log=TRUE)
#
#    #### props
#
#    forecast<-exp(log_4_forecast)+exp(log_56_forecast)
#
#    logitp_for<-RTMB::qlogis(InseasonCount/forecast)
#
#    nll <- nll - sum(RTMB::dnorm(head(logitp,-1),obslogitp ,exp(resid_err),log=TRUE))
#
#    nll <- nll - sum(RTMB::dnorm(tail(logitp,1),logitp_for ,exp(resid_err),log=TRUE))
#
#    nll <- nll - (RTMB::dexp(exp(resid_err),.25,log=TRUE)+(resid_err))
#
#


  ## Reporting

  # RTMB::REPORT(p)
  # RTMB::REPORT(current_pred)
  RTMB::ADREPORT(logitp)
  # RTMB::ADREPORT(current_pred)
  # RTMB::ADREPORT(B1)


  # RTMB::REPORT(forecast);
  # RTMB::ADREPORT(logitp);
  RTMB::ADREPORT(pre_season_forecast);
  # RTMB::ADREPORT(log(forecast));
  RTMB::ADREPORT(B1);
  # RTMB::ADREPORT(phi2);
  RTMB::ADREPORT(p);
  # RTMB::ADREPORT(pre_season_pred);
  # RTMB::REPORT(props);



  return(nll)
  }
}
