#' fit three models and save results to csv.
#'
#' @param pred_date
#' @param dat
#' @param forecast
#' @param forecast_log_sd
#' @param joint_like_data_file
#'
#' @return
#' @export
#'
#' @examples
mod_results_sk<-function(pred_date,
                      Count_dat = Bon_cnts,
                      River_dat = flow_temp_dat,
                      # Ocean_dat = ocean_cov,
                      Bon_sk_year = Bon_sk_year,
                      write_local=FALSE,
                      forecast_log_sd=forecast_log_sd){
  # if (is.null(mod_result_file)) {
  #   mod_result_file <- get_default_model_result_path()
  # }
# browser()
file_path <- here::here("inst", "data-cache", "forecast_results.csv")

  if (file.exists(file_path)) {
    local_data <-
      readr::read_csv(file_path)

    local_data2<- local_data |>
      dplyr::mutate(year=lubridate::year(date)) |>
      dplyr::filter(year==lubridate::year(pred_date),
                    ecotype=="Sk")



    if(nrow(local_data2)==0){
      sdate<-  as.Date(paste0(lubridate::year(pred_date),("-06-15")))

    }else{
          sdate <- max(local_data2$date)+1
    }


    #
  } else {
    local_data<-NULL
    local_data2<-data.frame(ecotype=character(0))

    sdate<-  as.Date(paste0(lubridate::year(pred_date),"-06-15"))

  }





  forecast_year <- lubridate::year(pred_date)

  if(sdate<=pred_date){
    new_dat<-data.frame()
    for (i in seq.Date(from=sdate,to=pred_date,by=1)){

print(i)
      forecast_year<-lubridate::year(as.Date(i))
      forecast_month<-lubridate::month(as.Date(i))
      forecast_mday<-lubridate::mday(as.Date(i))

      fish_river_ocean_i<-cnts_for_mod_fun_sk(as.Date(i),Bon_cnts=Count_dat) |>
        dplyr::left_join(River_dat |>
                           dplyr::filter(month==forecast_month,
                                         md==forecast_mday) |>
                           dplyr::select(year=Year,cfs_mean_ema,temp_mean_ema) |> dplyr::group_by(year) |> dplyr::summarize(across(c(cfs_mean_ema, temp_mean_ema),\(x)mean(x,na.rm=TRUE))) ,
        ) |>
        dplyr::left_join(
          sockeye_age3 #add age 3 predictor
        ) |>
        # dplyr::left_join(
          # Ocean_dat
        # ) |>
        dplyr::mutate(
          pink_ind=dplyr::case_when(
            year<2000 ~ 0,
            year %% 2 == 0 ~1,
            TRUE~ -1),
          cnt_by_flow= cfs_mean_ema*log_cum_cnt,
          cnt_by_temp= temp_mean_ema *log_cum_cnt,
        ) |>   dplyr::filter(year<=forecast_year)


      #don't try modeling the counts on the last day of season when we know what they are! or late in the fall season when runs are pretty much complete
      if( as.Date(i) < as.Date(paste0(lubridate::year(pred_date),"-07-15"))){

        # browser()
      #ARIMA
      ARIMA_for<-do_salmonForecasting_fun_sk(fish_river_ocean_i,cov_vec=c("log_cum_cnt","cnt_by_flow","pink_ind"))
      #
      #   #DLM
        DLM_for<-do_sibregresr_fun_sk(fish_river_ocean_i,cov_vec=c("lag_log_Age3_tot" ,"log_cum_cnt","cnt_by_flow","pink_ind"))
      #Joint_like
      joint_likelihood_fit1<-retro_mape_joint_lik_sk(dat=fish_river_ocean_i,forecast_log_sd = forecast_log_sd,
                              n_retro = 15)$cur_pred
      #

        # joint_likelihood_fit2<-fit_joint_likelihood2(fish_river_ocean_i ,ifelse(morph=="",forecast_season,morph))




      #combined
      comb_for<-   dplyr::bind_rows(
        DLM_for,
      ARIMA_for,
      # joint_likelihood_fit1,
      joint_likelihood_fit1
      ) |>
        dplyr::mutate(
          date=as.Date(i),
          dplyr::across(dplyr::where(is.numeric),\(x)round(x,3)),
          ecotype="Sk"
        )

      new_dat<-
        dplyr::bind_rows(new_dat,
                         comb_for
                    )
}

  }


    dat<-dplyr::bind_rows(local_data,new_dat)

if(write_local){
  readr::write_csv(dat, here::here("inst","data-cache","forecast_results.csv"))
}else{
  readr::write_csv(dat, here::here("inst","data-cache","forecast_results.csv"))
}



    return(
      dplyr::bind_rows(local_data2,new_dat) |>
      # add 10 year timing to model resutls
        dplyr::bind_rows(
          Bon_sk_year |>
            dplyr::ungroup()|> dplyr::filter(dplyr::between(CountDate,                                            as.Date(paste0(forecast_year,"-06-15")),
                                                            pred_date)) |>
            dplyr::mutate(`Lo 95`=total/plogis(qnorm(.975,qlogis(Ave_10yr),logit_prop_sd_10yr)),
                          `Lo 50`=total/plogis(qnorm(.75,qlogis(Ave_10yr),logit_prop_sd_10yr)),
                          `Hi 50`=total/plogis(qnorm(.25,qlogis(Ave_10yr),logit_prop_sd_10yr)),
                          `Hi 95`=total/plogis(qnorm(.025,qlogis(Ave_10yr),logit_prop_sd_10yr)),
                          mod_type="10-year\nave. timing",
                          MAPE_10yr=MAPE_10yr*100) |>
            dplyr::select(mod_type,predicted_abundance=pred_Ave_10yr,`Lo 95`:`Hi 95`,MAPE=MAPE_10yr,date=CountDate)

           )
    )
}else{
  return(

    local_data2|>
      # add 10 year timing to model resutls
      dplyr::bind_rows(
        Bon_sk_year |>
          dplyr::ungroup()|> dplyr::filter(dplyr::between(CountDate,                                            as.Date(paste0(forecast_year,"-06-15")),
                                                          pred_date)) |>
          dplyr::mutate(`Lo 95`=total/plogis(qnorm(.975,qlogis(Ave_10yr),logit_prop_sd_10yr)),
                        `Lo 50`=total/plogis(qnorm(.75,qlogis(Ave_10yr),logit_prop_sd_10yr)),
                        `Hi 50`=total/plogis(qnorm(.25,qlogis(Ave_10yr),logit_prop_sd_10yr)),
                        `Hi 95`=total/plogis(qnorm(.025,qlogis(Ave_10yr),logit_prop_sd_10yr)),
                        mod_type="10-year\nave. timing",
                        MAPE_10yr=MAPE_10yr*100) |>
          dplyr::select(mod_type,predicted_abundance=pred_Ave_10yr,`Lo 95`:`Hi 95`,MAPE=MAPE_10yr,date=CountDate)

      )

         )
}

  }















#' make forecast using sibregresr
#'
#' @param fish_river_ocean data
#'
#' @return
#' @export
#'
#' @examples
do_sibregresr_fun_sk<-function(data,cov_vec=c("lag_log_Age3_tot" ,"log_cum_cnt","cnt_by_flow","pink_ind")){#,"temp_mean_ema","lag2_Spr_NPGO","lag2_Spr_PDO"

  ## data for sibregresr package
  sib_reg_dat<-data |> dplyr::mutate(Stock=paste("Bon","Chk",sep="_")) |> dplyr::select(Stock,ReturnYear=year ,Age4=tot_adult) |>
    dplyr::filter(ReturnYear<max(ReturnYear))


  sib_reg_cov<-data |> dplyr::select(ReturnYear=year,all_of(cov_vec))



  pen_dlm_forecast_cov<-sibregresr::forecast_fun(
    df = sib_reg_dat,
    include = c("r2d2DLM"),
    transformation = log,
    inverse_transformation = exp,
    scale_x = TRUE,
    scale_y = TRUE,
    perf_yrs = 15,
    wt_yrs = 1,
    covariates = sib_reg_cov ,
    include_youngest = TRUE,
    form =formula(paste(c("y ~ 1" , cov_vec),collapse=" + "))
  )

  # test<-pen_dlm_forecast_cov$ fits |>
  #   dplyr::filter(purrr::map_lgl(error, ~!is.null(.x))) |>
  #   dplyr::mutate(error=purrr::map_chr(error,as.character))
  # test$error[[1]]
  # sibregresr::make_table(pen_dlm_forecast_cov$forecasts,"r2d2Dlm")


  forecast<-pen_dlm_forecast_cov$forecasts |> dplyr::filter(Age=="4",ReturnYear==max(ReturnYear),model_name=="r2d2DLM") |>
    dplyr::ungroup() |>
    dplyr::mutate(`Lo 95`=exp(qnorm(.025,log(Pred),log_sd)),
                  `Lo 50`=exp(qnorm(.25,log(Pred),log_sd)),
                  `Hi 50`=exp(qnorm(.75,log(Pred),log_sd)),
                  `Hi 95`=exp(qnorm(.975,log(Pred),log_sd))) |>
    dplyr::select(model=model_name,
                  predicted_abundance=Pred,
                  MAPE,RMSE,
                  `Lo 95`:`Hi 95`)







  info<-pen_dlm_forecast_cov$fits |> dplyr::filter(Age==4,n_years==-1)



  coefs<-c(unlist(tail(info$MLE[[1]]$result$obj$report()$coefs,1))) |> `names<-`(c("intercept",cov_vec[]))


  covs<-c(1,unlist(tail(info$xy_dat[[1]],1)[,c(cov_vec)]))|> `names<-`(c("intercept",cov_vec[]))

  mean_effects<-coefs*covs |> `names<-`(c("intercept",cov_vec[]))






  forecast|>
    dplyr::bind_cols(tibble::as_tibble_row(coefs)|>
                       dplyr::rename_with(~ paste0("coef_", .x))) |>
    dplyr::bind_cols(tibble::as_tibble_row(covs)|>
                       dplyr::rename_with(~ paste0("covar_", .x))) |>
    dplyr::bind_cols(tibble::as_tibble_row(mean_effects)|>
                       dplyr::rename_with(~ paste0("effect_", .x))) |>
    dplyr::mutate(mod_type="DLM",.before=dplyr::everything())

}




#' ARIMA (salmonForecasting) model forecast
#'
#' @param data
#' @param cov_vec
#'
#' @return
#' @export
#'
#' @examples
do_salmonForecasting_fun_sk<-function(data,cov_vec=c("log_cum_cnt","cnt_by_flow","pink_ind")){


  salmonForecasting_dat<-data |> dplyr::mutate(species="Bon_Spr",period=1) |> dplyr::select(species,period,year,abundance=tot_adult,log_lag_jack=lag_log_Age3_tot ,log_cum_cnt,cfs_mean_ema:dplyr::last_col()) |>
    tidyr::fill(c("log_lag_jack",cov_vec[])) |>
    dplyr::mutate(
      dplyr::across(c("log_lag_jack",cov_vec[]),\(x)c(scale(x)))
    )





      ARIMA_forecast<-SalmonForecasting::do_forecast(salmonForecasting_dat,
                                                 covariates =c("log_lag_jack",cov_vec[]),max_vars=2,n_cores=3,do_stacking = FALSE,TY_ensemble=15,write_model_summaries=FALSE,include_mod = TRUE)




  best_weighting<-ARIMA_forecast$ens$forecast_skill |> dplyr::filter(grepl("w",model)) |> dplyr::filter(MAPE==min(MAPE)) |> dplyr::pull(model)

  #best pred
  pred<-ARIMA_forecast$ens$ensembles |> dplyr::ungroup() |>  dplyr::filter(year==max(year),
                                                                           model== best_weighting) |>
    dplyr::left_join(ARIMA_forecast$ens$forecast_skill)


  #average coefficients
  coef_mat<-ARIMA_forecast$rp$top_mods |>
    dplyr::mutate(dplyr::across(-mod,unlist)) |>
    dplyr::filter(year==max(year)) |>
    dplyr::pull(mod) |>
    lapply(\(x)x[[1]]) |>
    dplyr::bind_rows()

  model_weights<-ARIMA_forecast$ens$final_model_weights |> dplyr::pull(substr(best_weighting,1,(nchar(best_weighting)-2)))


  ave_coefs<-colSums(coef_mat *
                       model_weights ,na.rm=T)

  ave_coefs_non_int_or_ARMA<-ave_coefs[!names(ave_coefs)%in%c("intercept",paste0("ar",1:10),paste0("ma",1:10),paste0("sar",1:10),paste0("sma",1:10),"drift")]


  covars<-salmonForecasting_dat[salmonForecasting_dat$year==max(salmonForecasting_dat$year),c("log_lag_jack",cov_vec[])]


  covar_effects<-ave_coefs_non_int_or_ARMA*covars[names(ave_coefs_non_int_or_ARMA)]




  pred |> dplyr::bind_cols(tibble::as_tibble_row(ave_coefs)|>
                             dplyr::rename_with(~ paste0("coef_", .x))) |>
    dplyr::bind_cols(tibble::as_tibble(covars)|>
                       dplyr::rename_with(~ paste0("covar_", .x))) |>
    dplyr::bind_cols(tibble::as_tibble(covar_effects)|>
                       dplyr::rename_with(~ paste0("effect_", .x))) |>
    dplyr::mutate(mod_type="ARIMA",.before=dplyr::everything())

}
