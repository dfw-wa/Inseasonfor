


pred_tabs_fig<-function(pred_date,model_results,season_dates){

  # covar effects tab

  covar_effect_tab<-model_results |> dplyr::filter(date==pred_date,mod_type%in%c("ARIMA","DLM")) |> dplyr::select(Model=mod_type,coef_intercept, coef_log_lag_jack:coef_cnt_by_flow,covar_log_lag_jack:covar_cnt_by_flow,effect_log_lag_jack:effect_cnt_by_flow) |>
    gt::gt() |>
    gt::tab_spanner_delim(delim = "_",limit=1,split="first") |>
    gt::fmt_number(
      columns = dplyr::where(is.numeric),
      decimals = 2
    ) |> gt::tab_footnote(footnote="The response and the covariates are z-scored before fitting the DLM, but the response is not standardized for the ARIMA model",locations=gt::cells_column_labels(columns = Model)) |>  gt::tab_options(table.width = "100%") |>  gt::cols_label_with(fn=function(x) gsub("_", " ", x)) |>
    gt::tab_caption("Fitted coefficients, covariate values, and effects (coefficient * covariate) for the DLM and ARIMA models. Covariates are the log of the jack returns in the previous years, the log of the cumulative adult counts to date, and an interaction between the cumulative counts and a moving average of river discharge.")

  # flow effects tab

  flow_effect_tab<-model_results |> dplyr::filter(date==pred_date,mod_type%in%c("Joint_Lik")) |> dplyr::select(`Coefficient`=coef_cfs_mean_ema) |>
    dplyr::mutate(
      `Covariate` =(flow_temp_dat |>
                      dplyr::filter(month==forecast_month,md==forecast_md) |> tidyr::fill(cfs_mean_ema) |> dplyr::pull(cfs_mean_ema) |> log() |> scale() |> c() |> tail(1)),
      Effect=`Covariate`*`Coefficient`

    ) |>
    gt::gt() |>
    gt::fmt_number(
      columns = dplyr::where(is.numeric),
      decimals = 2
    )  |>
    gt::tab_caption("Fitted coefficient, covariate value, and effect (coefficient * covariate) for the log transformed and z-scaled moving average of river discharge in the linear predictor of the proportion of the run that is complete within the joint-likelihood model.") |> gt::tab_options(table.width = "100%")


  # prediction table

  Pred_tab<-
    model_results |> dplyr::filter(date==pred_date) |> dplyr::select(Model=mod_type,Pred =predicted_abundance,`Lo 95`:`Hi 95`,MAPE) |>
    # dplyr::mutate(dplyr::across(dplyr::where(is.numeric),\(x)format(round(x),scientific=FALSE, big.mark=","))) |>
    gt::gt() |>
    gt::fmt_percent("MAPE",decimals=0,scale_values =FALSE) |>
    gt::fmt_integer(Pred:`Hi 95`) |>
    gt::sub_missing() |>
    # gt::tab_footnote(footnote = "I have not yet implemented a retrospective assessment of performance for the joint-likelihood model",
    #                  locations = gt::cells_body(columns=MAPE,rows = 3)) |>
    gt::tab_footnote(footnote = "Prediction intervals for different model types are calculated using different methods, which complicates comparison somewhat. See Methods Description for more detail.",
                     locations = gt::cells_column_labels(columns=`Lo 95`:`Hi 95`))|>
    gt::tab_caption(paste0("Predictions of the ",forecast_season, " season (", chk_season_print("spring",season_dates),") total adult Chinook salmon count with prediction intervals (50% and 95%) and the mean absolute percent error (MAPE) of predictions made with counts through ",pred_date  |>  format("%B %d"), " in a 15-year retrospective assesment."))


  if(!pred_date %in%
     as.Date(paste0(lubridate::year(pred_date),c("-06-15","-07-31")))){
    # Pred_tab<-Pred_tab |> gt::tab_footnote(footnote = "I have not yet implemented a retrospective assessment of performance for the joint-likelihood model",
    #                                        locations = gt::cells_body(columns=MAPE,rows = 3))
  }

  # prediction

  cb_palette <- c("#000000",
                  "#E69F00",
                  "#56B4E9",
                  "#009E73")

  pred_plot<-model_results |> ggplot2::ggplot(ggplot2::aes(x=date,y=predicted_abundance,color=mod_type))+ggplot2::geom_line()+ggplot2::geom_point(size=2.5)+ggplot2::ylab("Predicted total dam count")+ ggplot2::scale_y_continuous(labels = scales::unit_format(suffix="k",scale = 1e-3))+ggplot2::scale_color_manual(values = cb_palette) +ggplot2::theme_gray()+ggplot2::theme(axis.title.x = ggplot2::element_blank(),text = ggplot2::element_text(size=18),legend.position = "top",legend.title = ggplot2::element_blank())


  ## Prediction interval

  ymax<-model_results |> dplyr::filter(date==pred_date) |> dplyr::pull(predicted_abundance) |> max() |> (\(x) x * 2)()

  ymin<-model_results |> dplyr::filter(date==pred_date) |> dplyr::pull(predicted_abundance) |> max() |> (\(x) x * .4)()



  pred_int_plot<-model_results |> ggplot2::ggplot(ggplot2::aes(x=date,y=predicted_abundance,color=mod_type))+
    ggplot2::geom_ribbon(ggplot2::aes(ymin=`Lo 95`,ymax=`Hi 95`),alpha=.65,fill="grey40",color=NA)+
    ggplot2::geom_ribbon(ggplot2::aes(ymin=`Lo 50`,ymax=`Hi 50`),alpha=.85,fill="grey20",color=NA)+
    ggplot2::geom_line()+
    ggplot2::geom_point(size=2.5)+ggplot2::facet_wrap(~mod_type)+ ggplot2::scale_y_continuous(labels = scales::unit_format(suffix="k",scale = 1e-3))+ ggplot2::scale_x_date(
      # date_breaks = "5 days",
      date_labels = "%b %d"
    )+ggplot2::scale_color_manual(values = cb_palette) +ggplot2::theme_grey()+ggplot2::theme(axis.title.x = ggplot2::element_blank(),text = ggplot2::element_text(size=18),legend.position = "none")+ggplot2::ylab("Predicted total dam count")+
    ggplot2::coord_cartesian(ylim = c(ymin, ymax), clip = "on")


  list(
    covar_effect_tab =  covar_effect_tab,
    flow_effect_tab =  flow_effect_tab,
    Pred_tab =  Pred_tab,
    pred_plot = pred_plot,
    pred_int_plot = pred_int_plot
  )
}



mod_wrapper_fun<-function(pred_date,Bon_cnts,flow_temp_dat,#ocean_cov,
                          Bon_ch_year,season_dates,season_end_date,
                          write_local,
                          morph){


    captured_output2 <- capture.output({ # to capture printed statements and prevent them from going in the rendered output
  model_results<-   mod_results(pred_date  = pred_date,
                             Count_dat = Bon_cnts,
                             River_dat = flow_temp_dat,
                             # Ocean_dat = ocean_cov,
                             Bon_ch_year = Bon_ch_year,
                             write_local=write_local,
                             morph=morph)
})

  mod_figs_tabs<-pred_tabs_fig(pred_date=pred_date,model_results,season_dates=season_dates)


  cat("\n\n")

  cat("##### Run size predictions {.tabset}","\n\n")

  cat("###### Prediction table","\n\n")
  print(mod_figs_tabs$Pred_tab)
  cat("\n\n")


  cat("###### Prediction plot","\n\n")
  print(mod_figs_tabs$pred_plot)
  cat("\n\n")
  cat(paste("Daily predictions of total counts at Bonneville from different models."), "\n")

  cat("\n\n")

  cat("###### Prediction interval plot","\n\n")
  print(mod_figs_tabs$pred_int_plot)
  cat("\n\n")
  cat(paste("Daily predictions and prediction intervals (95% and 50%).  Prediction intervals for different model types are calculated using different methods, so may not be directly comparable. See Methods Description for more details."), "\n")
  cat("\n\n")

  if(pred_date<season_end_date&
     (!((morph=="Tule"&(as.Date(pred_date)>as.Date(paste0(lubridate::year(pred_date),"-09-25"))))|
          (morph=="Bright"&(as.Date(pred_date)>as.Date(paste0(lubridate::year(pred_date),"-10-15"))))))){
  cat("###### Covariate effects","\n\n")
  print(mod_figs_tabs$covar_effect_tab)
  cat("\n\n")
  print(mod_figs_tabs$flow_effect_tab)
  cat("\n\n")
  }

  cat("\n\n")
  cat("###### Methods description","\n\n")

  cat("

####### DLM

This is a penalized dynamic linear model fit with the [`Sibregresr` package](https://dfw-wa.github.io/sibregresr/articles/Overview.html). The coefficients are the:

1)  log-transformed total count of jacks at Bonneville Dam in the *previous* year,
2)  log-transformed cumulative adult count at Bonneville Dam in the *current* year,
3)  and interaction between an exponential moving average of river discharge and the cumulative adult count.

The prediction interval is based on the standard deviation (in log-space) of forecasts from a 15-year retrospective. In other words, the root mean square error of the forecast error in log space from a 15-year retrospective is used as the standard deviation in a lognormal prediction interval.

####### ARIMA

This is an ensemble of ARIMA models fit with the [`SalmonForecasting` package](https://github.com/wdfw-fp/salmonforecast) (see README at the linked page for details). The covariates are the same as those described above for the DLM. Prediction intervals are the weighted average of the prediction intervals provided by the `forecast::forecast()` function in `R`.

####### Joint likelihood

This is a version of a model that has been used for this purpose of in-season forecasting for several years. The proportion of the run that is complete on a given day is modeled with autoregressive (order 1) errors and an effect of the river discharge covariate. The pre-season forecast is generated using an age-specific sibling regression. I have not yet implemented a retrospective assessment of performance because, in this version, the pre-season forecasts and prediction uncertainty need to be provided, and I have not pulled in the past 15 years' forecasts.

####### 10 year average run timing

This method takes the 10-year average of the proportion of the run that was complete on a given day and uses it to expand the cumulative counts to date. Performance and prediction intervals are based on a 15-year retrospective as described above for the DLM but assuming a normal prediction distribution for the proportion complete in logit space.

      ")

  cat("\n\n")

  }

