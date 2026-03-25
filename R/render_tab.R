

render_tab<-function(pred_date,counts,river_env,do_plots,seas_dats,seas_end,write_local=write_local,morph){

  if(morph!=""){
    counts2 <-counts  |>
      dplyr::rename(AdultChinook=paste(morph,"Adult",sep="_"),
                    JackChinook=paste(morph,"Jack",sep="_"))
if(morph=="Tule"){
  counts2 <-counts2  |>
    dplyr::mutate(JackChinook=JackChinook+Tule_Stubby )
}

  }else{
    counts2 <-counts
  }

  for_year<-lubridate::year(pred_date)
  Bon_ch<-Bon_ch_fun(pred_date,counts2)
  Bon_ch_year<-Bon_ch |> dplyr::filter(year==for_year)|>
    dplyr::filter(month>=2) |>
    dplyr::mutate(
    AdultChinook=ifelse(CountDate>pred_date,NA_real_,AdultChinook),
)

  inital_blurb_fun(Bon_ch|> dplyr::filter(CountDate==pred_date),
                   season_dates=season_dates,
                   morph=morph)


  cat("\n\n")


    summary_plot_tabs(river_env,Bon_ch,pred_date)


    if((lubridate::month(pred_date)<4)|
       ((lubridate::month(pred_date)==4)&
        (lubridate::mday(pred_date)<5))){
      cat("\n\n")

      cat("**Model predictions for spring season wiill start on April 5th**")
      cat("\n\n")

    }else{
      if(((lubridate::month(pred_date)==8)&
          (lubridate::mday(pred_date)<15))){
        cat("\n\n")

        cat("**Model predictions for fall season will start on August 15th**")
            cat("\n\n")



      }else{


    mod_wrapper_fun(pred_date,counts2,river_env,#ocean_cov,
                    Bon_ch_year,season_dates=seas_dats,
                    season_end_date=seas_end,
                    write_local=write_local,
                    morph)



      }
    }


    bon_ch_tabs(
      Bon_ch_year,
      for_year,
      pred_date
    )


}
