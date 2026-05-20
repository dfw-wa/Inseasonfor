
add_10_yr_env_fun<-function(dat,cur_yr){

##add 10-year average to current year flow and temp data for plotting
avg_10yr <- dat |>
  dplyr::filter(Year < cur_yr & Year >= cur_yr - 10) |>
  dplyr::group_by(month,md) |>
  dplyr::summarise(
    `Flow (kcfs)`= mean(cfs_USACoE[Year < cur_yr & Year >= cur_yr - 10], na.rm = TRUE),
    `River temp. (F)`= mean(temp_f_USACE[Year < cur_yr & Year >= cur_yr - 10], na.rm = TRUE),
    .groups="drop") |>
  dplyr::mutate(type="10yr ave.")

current_yr<- dat |>
  dplyr::filter(Year == cur_yr ) |>
  dplyr::select(
    `Flow (kcfs)`=cfs_USACoE,
    `River temp. (F)`= temp_f_USACE,
    flw_date,month,md) |>
  dplyr::mutate(type=as.character(cur_yr))


  current_yr |>
  dplyr::bind_rows(avg_10yr)

}


current_year_cnts_plot<-function(env_dat, Bon_ch_year, cur_yr,
                                  species_col = "AdultChinook",
                                  species_label = "adult Chinook salmon",
                                  ref_years = NULL){

  flow_temp_dat2<-add_10_yr_env_fun(env_dat,cur_yr)

  # Named color map so order of levels doesn't matter
  color_vals <- c(
    setNames("#56B4E9", as.character(cur_yr)),
    "10yr ave." = "#E69F00"
  )

  plot_dat <-
    Bon_ch_year |>
    dplyr::rename(!!as.character(cur_yr) := !!dplyr::sym(species_col)) |>
    dplyr::select(
      Date = CountDate,
      !!as.character(cur_yr),
      `10yr ave.` = Ave_10yr_daily_cnt,
      month,
      md = mday
    )|>
    tidyr::pivot_longer(c(!!as.character(cur_yr),`10yr ave.`), names_to = "type",values_to = "Adult count") |>
    dplyr::left_join(
      flow_temp_dat2 |>
    dplyr::mutate(`Flow (kcfs)`=`Flow (kcfs)`/1000)
    ) |>
      tidyr::pivot_longer(cols=c(`Adult count`, `Flow (kcfs)`, `River temp. (F)`),names_to="Param",values_to="Value")

  # Bind reference year flow/temp rows after the final pivot (no count data for ref years).
  # inner_join on month/md ensures ref year is filtered to the same date range as plot_dat.
  if (!is.null(ref_years)) {
    ref_palette <- c("#D55E00", "#CC79A7", "#009E73")[seq_along(ref_years)]
    color_vals  <- c(color_vals, setNames(ref_palette, as.character(ref_years)))

    valid_month_md <- dplyr::distinct(plot_dat, month, md)

    ref_rows <- env_dat |>
      dplyr::filter(Year %in% ref_years) |>
      dplyr::select(Year, month, md,
                    `Flow (kcfs)`       = cfs_USACoE,
                    `River temp. (F)`   = temp_f_USACE) |>
      dplyr::inner_join(valid_month_md, by = c("month", "md")) |>
      dplyr::mutate(
        `Flow (kcfs)` = `Flow (kcfs)` / 1000,
        Date = as.Date(paste(cur_yr, month, md, sep = "-")),
        type = as.character(Year)
      ) |>
      dplyr::select(Date, month, md, type, `Flow (kcfs)`, `River temp. (F)`) |>
      tidyr::pivot_longer(cols = c(`Flow (kcfs)`, `River temp. (F)`),
                          names_to = "Param", values_to = "Value")

    plot_dat <- dplyr::bind_rows(plot_dat, ref_rows)
  }

  p <- plot_dat |>
    dplyr::arrange(type) |>
    ggplot2::ggplot(ggplot2::aes(x=Date,y=Value,col=type))+ggplot2::geom_line()+ ggplot2::geom_point(size=2.5
      ) + ggplot2::facet_wrap(~Param,ncol=1,scales="free_y") +
    ggplot2::scale_color_manual(values=color_vals) +
    ggplot2::labs(y="",
                  col=NULL,
                  shape=NULL) +
    ggplot2::theme(legend.key=ggplot2::element_blank())+ggplot2::theme_grey()+ggplot2::theme(axis.title.x = ggplot2::element_blank(),text = ggplot2::element_text(size=18))

  print(p)
  cat("\n\n")
  cat(paste0("Daily ", species_label, " counts, flow, and temperature measurements taken at Bonneville Dam."), "\n\n")

}


percent_complete<-function(Bon_ch, f_yr, forecastdate,
                           start_month = 3, end_month = NULL){
  p<-
    Bon_ch |> dplyr::filter(dplyr::between(year,f_yr-15,f_yr-1)) |> dplyr::mutate(date=(as.Date(paste(f_yr,month,mday,sep="-")))) |>
    dplyr::filter(if (is.null(end_month)) month >= start_month else dplyr::between(month, start_month, end_month)) |>
    ggplot2::ggplot(ggplot2::aes(x=date,y=prop))+ggplot2::geom_vline(ggplot2::aes(xintercept = forecastdate),col="firebrick",lty=2,lwd=1)+ggplot2::geom_boxplot(ggplot2::aes(group = date))+ ggplot2::scale_x_date(
      date_breaks = "1 month",
      date_labels = "%b"
    )+ggplot2::ylab("Percent passage complete")+ ggplot2::scale_y_continuous(labels = scales::unit_format(suffix="%",scale = 100))+ggplot2::theme_grey()+ggplot2::theme(axis.title.x = ggplot2::element_blank(),text = ggplot2::element_text(size=18))


  print(p)

  cat("\n\n")

  cat(sprintf("<strong>Percent of the run complete by date in %s--%s</strong>. Lower and upper hinges correspond to the first and third quartiles (the 25th and 75th percentiles).The upper whisker extends from the hinge to the largest value no further than 1.5 * IQR from the hinge (where IQR is the inter-quartile range, or distance between the first and third quartiles). The lower whisker extends from the hinge to the smallest value at most 1.5 * IQR of the hinge. Data beyond the end of the whiskers are called &quot;outlying&quot; points and are plotted individually.",f_yr-15,f_yr-1), "\n\n")
}



prediction_error<-function(Bon_ch_year, line_date,
                           start_month = 3, end_month = NULL){
  p<-
    Bon_ch_year |>
    dplyr::filter(if (is.null(end_month)) month >= start_month else dplyr::between(month, start_month, end_month)) |> ggplot2::ggplot(ggplot2::aes(y = (MAPE_10yr),x=CountDate))+ggplot2::geom_col(fill="grey10")+ggplot2::geom_vline(ggplot2::aes(xintercept = line_date),col="firebrick",lty=2,lwd=1,alpha=.5)+ggplot2::ylab("Mean absolute percent error (MAPE)") + ggplot2::scale_y_continuous(labels = scales::unit_format(suffix="%",scale = 100))+ggplot2::theme_grey()+ggplot2::theme(axis.title.x = ggplot2::element_blank(),text = ggplot2::element_text(size=18))

  print(p)

cat("\n\n")

cat(paste("Mean absolute percent error  (over 15 year retrospectiv) of predictions based on cumulative counts and 10-year average run timing, by day of year."), "\n\n")
}



summary_plot_tabs<-function(flow_temp_dat1, Bon_ch, forecastdate,
                            species_col = "AdultChinook",
                            species_label = "adult Chinook salmon",
                            start_month = 3, end_month = NULL,
                            ref_years = NULL){

    for_year<-lubridate::year(forecastdate)

  Bon_ch_year<-Bon_ch |> dplyr::filter(year==for_year)|>
    dplyr::filter(if (is.null(end_month)) month >= start_month else dplyr::between(month, start_month, end_month)) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(species_col), ~ifelse(CountDate > forecastdate, NA_real_, .x)))


cat("##### {.tabset}","\n\n")

cat("###### Current year","\n\n")
current_year_cnts_plot(flow_temp_dat1, Bon_ch_year, for_year,
                       species_col = species_col, species_label = species_label,
                       ref_years = ref_years)
cat("\n\n")

cat("###### Percent complete","\n\n")
(percent_complete(Bon_ch, for_year, forecastdate,
                  start_month = start_month, end_month = end_month))
cat("\n\n")

cat("###### Prediction error","\n\n")
(prediction_error(Bon_ch_year, forecastdate,
                  start_month = start_month, end_month = end_month))

cat("##### {-}","\n\n")

}


