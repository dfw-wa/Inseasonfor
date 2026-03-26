


#' chk_season_print
#'
#' @param seasn
#' @param season_dates
#'
#' @return
#'
#' @examples
chk_season_print<-function(seasn,season_dates){
 seas_row<- season_dates |> dplyr::filter(season==seasn)

 paste0(seas_row$start_m," ",seas_row$start_md,"--",seas_row$end_m," ",seas_row$end_md)
}

#' Title
#'
#' @param day
#'
#' @return
#' @export
#'
#' @examples
chk_season<-function(day){

  month=lubridate::month(day)
  mday=lubridate::mday(day)
  ifelse(
    dplyr::between(month,8,10),"fall",
    ifelse(
      (month==7|(month==6 & mday>15)),"summer",
      ifelse(
        month>=3,"spring",
        "Winter?")
    )
  )
}



#' bon_dat_fun
#'
#' Reads window count data from fish passage center. Taken from Ben's fpcDamCounts package
#'
#' @param pred_date  the last date of counts for which predictions are to be based
#' @param count_file the file where fish counts are stored
#' @param url the fishpassage center url where the data are pulled from.
#'
#' @return tibble with daily counts, proportions of runs complete, and forecasts
#' @export
#'
#' @examples
#'
#' bon_dat_fun()
#'
bon_dat_fun <- function(pred_date = NULL,
                        sdate = NULL,
                        past_bon_cnts = NULL,
                        url = "https://www.fpc.org/adults/R_coeadultcount_runsum") {

  if (is.null(sdate) & !is.null(past_bon_cnts)) {
    sdate <- max(past_bon_cnts$CountDate) + 1
  }

  if (is.null(pred_date)) {
    edate <- Sys.Date() + 2
  } else {
    edate <- pred_date
  }

  full_url <- glue::glue("{url}_salmon_getresults.php?dam=BON&sdate={sdate}&edate={edate}")
  gh_log("notice", "Checking URL: ", full_url)

  # --- HTTP status check with httr2 (swap to httr if preferred) ---
  resp <- httr2::request(full_url) |> httr2::req_perform()
  status <- httr2::resp_status(resp)
  if (status != 200) {
    gh_log("error", "FPC returned HTTP status ", status, " for URL: ", full_url)
    stop("FPC returned HTTP status ", status)
  }

  # --- Read data ---
  new_dat <- tryCatch(
    {
      dat <- readr::read_csv(
        full_url,
        col_types = readr::cols(CountDate = readr::col_date(format = "%m/%d/%Y"))
      )

      if (nrow(dat) == 0) {
        gh_log("error", "Read succeeded but returned 0 rows: ", full_url)
        stop("Read succeeded but returned 0 rows")
      }

      gh_log("notice", "Successfully read ", nrow(dat), " rows from FPC")

      dat |>
        dplyr::select(CountDate, AdultChinook, JackChinook) |>
        dplyr::mutate(
          year = lubridate::year(.data$CountDate),
          yday = lubridate::yday(.data$CountDate),
          month = lubridate::month(.data$CountDate),
          mday = lubridate::mday(.data$CountDate),
          season = dplyr::case_when(
            .data$month >= 8 ~ "fall",
            (.data$month == 7 | (.data$month == 6 & .data$mday > 15)) ~ "summer",
            .data$month >= 3 ~ "spring",
            TRUE ~ "Winter?"
          )
        )
    },
    error = function(e) {
      gh_log("error", "Failed to parse CSV from FPC: ", conditionMessage(e))
      stop("Failed to parse CSV from FPC: ", conditionMessage(e))
    }
  )

  dplyr::bind_rows(past_bon_cnts, new_dat)
}








#' Function to calculate exponential moving avg on a time series (x)
#'
#' @param x
#' @param ratio
#'
#' @return
#'
#' @examples
ema <- function(x, ratio){
  c(stats::filter(x * ratio, 1 - ratio, "recursive", init=x[1]))
}




# # mean daily flow (cfs) at The Dalles from USGS- Begins 1/1/1939
# get_usgs_flow_data <- function(forecastdate){
#   # Download the data
#   dataRetrieval::readNWISdv(
#     # Columbia River at The Dalles, Site 14105700
#     siteNumbers = "14105700",
#     startDate = "1939-1-1",
#     endDate = forecastdate,
#     #"00060 is flow, 00010 is temp
#     parameterCd = c("00060", "00010"),
#     statCd = "00003"
#   ) %>%
#     # pull relevant columns, rename
#     dplyr::select(date=Date,
#                   # Dollar-sign is regex for ends with. Don't want the _cd columns
#                   cfs = dplyr::matches("00060_00003$")
#
#     ) %>%
#     # create the other columns
#     dplyr::mutate(Year=year(date)) %>%
#     dplyr::transmute(
#       Year = Year,
#       flw_date=date,
#       cfs = cfs
#     )
#
# }





#' get flow data from CRB DART
#'
#' @param forecastdate
#' @param start_year
#'
#' @return
#' @export
#'
#' @examples
get_CBR_DART_flow_temp_data<-function(forecastdate,start_year=1960){
  readr::read_csv(paste0("https://www.cbr.washington.edu/dart/cs/php/rpt/mg.php?sc=1&mgconfig=river&outputFormat=csvSingle",paste0(paste0("&year%5B%5D=",lubridate::year(forecastdate):start_year),collapse = ""),"&loc%5B%5D=BON&data%5B%5D=Outflow&data%5B%5D=Temp+%28Scroll+Case%29&startdate=1%2F1&enddate=12%2F1&avgyear=0&consolidate=1&grid=1&y1min=0&y1max=&y2min=&y2max=&size=large")) %>%
    dplyr::select(-c(unit,datatype )) |>
    tidyr::pivot_wider(names_from = parameter,values_from = value) |>
    dplyr::mutate(flw_date=lubridate::ymd(paste0( year,`mm-dd`,sep="-"))) %>%
    #make date column names match
    dplyr::mutate(cfs_CBR=(round(outflow ,0)*1000),#convert from kcfs to cfs and round to nearest throusandto match usgs data
                  temp_f_CBR=round((tempscr*.55556)+32),
                  Year=as.numeric(year)) %>%
    dplyr::select(Year,flw_date,cfs_CBR,temp_f_CBR)#select columns of interest
}




#' ean daily flow (kcfs) at Bonneville from US Army Corps of Engineers
#'
#' @param forecastdate
#' @param startdate
#' @param dam
#'
#' @return
#' @export
#'
#' @examples
get_usace_flow_temp_data <- function(forecastdate,startdate,dam="BON") {

  # Format dates as "MM/DD/YYYY 07:00", then URL encode
  start_str <- URLencode(paste0(format(startdate, "%m/%d/%Y"), " 07:00"))
  end_str <- URLencode(paste0(format(forecastdate+2, "%m/%d/%Y"), " 07:00"))

  url <- paste0(
    "https://www.nwd-wc.usace.army.mil/dd/common/web_service/webexec/ecsv?id=",
    dam,
    ".Flow-Out.Ave.~1Day.1Day.CBT-REV%3Aunits%3Dkcfs%7CBON-ScrollCase.Temp-Water.Inst.~1Day.0.CBT-RAW%3Aunits%3DF&headers=true&filename=&timezone=PST",
    "&lookback=6220w2d10h50m&lookforward=-11h50m",
    "&startdate=", start_str,
    "&enddate=", end_str
  )


  readr::read_csv(
    file=url,

    # Rename columns, skip 1st row so the old colnames aren't put into the df
    col_names = c("date", "kcfs","temp"),
    col_types = readr::cols(date = readr::col_datetime("%d-%b-%Y %H:%M"),
                            kcfs = readr::col_double(),
                            temp= readr::col_double()),
    skip = 1 ) %>%

    dplyr::transmute(Date = as.Date(date),
                     Year = lubridate::year(date),
                     day_nu = lubridate::yday(date),
                     `Flow (kcfs)`=kcfs,
                     `River temp. (F)`=temp) |>
    dplyr::group_by(Date) |>
    dplyr::summarise(dplyr::across(c( `Flow (kcfs)`,`River temp. (F)`),\(x)mean(x,na.rm=T))) |>
    dplyr::ungroup()


}

#----------------------------------------------------------
#' get_flow_data
#'
#' @param forecastdate
#' @param flow_file
#'
#' @return
#' @export
#'
#' @examples
get_flow_data<-function(forecastdate=NULL,
                        sdate=NULL,
                        flow_file=NULL){

  # Use default location if not provided
  # if (is.null(flow_file)) {
  #   flow_file <- get_default_flow_path()
  # }
  #
  #
  # if (file.exists(flow_file)) {
  #   local_data <-
  #     readr::read_csv(flow_file)
  #
  #   sdate <- max(local_data$flw_date)+1
  #   #
  # } else {
  #   message("Local file not found at: ", flow_file)
  #   local_data<-NULL

  if(is.null(sdate)&!is.null(flow_file)){
    sdate<-max(flow_file$flw_date)+1
  }
  # }

  if(is.null(forecastdate)){
    edate<-Sys.Date()-1
  } else{
    edate<-forecastdate
  }

  # if(edate>=sdate){
    #obtain the data from multile web sources
    ##usgs
    # usgs_dat<- get_usgs_flow_data(forecastdate)
    ##CBR
    # CBR_DAT_Dat<- try( get_CBR_DART_flow_temp_data(edate,start_year = lubridate::year(sdate))|>
    #                      dplyr::filter(flw_date>=sdate))
    #

    ##USACoE
    USACoE_dat<-get_usace_flow_temp_data(edate,startdate=as.Date(sdate),dam="BON") %>%
      dplyr::mutate(flw_date=  as.Date(Date)) %>% #make date column names match
      dplyr::mutate(cfs_USACoE=(round(`Flow (kcfs)`,0)),
                    temp_f_USACE=`River temp. (F)`) %>% #convert from kcfs to cfs and round to nearest throusandto match usgs data
      dplyr::select(flw_date,cfs_USACoE,temp_f_USACE)#select columns of interest

    #merge them where there is a column for each sources
    # if(is.null(dim(CBR_DAT_Dat))) {
    #   all_dat<-USACoE_dat %>%
    #     dplyr::mutate(cfs_mean=cfs_USACoE,temp_mean=temp_f_USACE)
    # }else{
      all_dat<-USACoE_dat #%>% dplyr::full_join(CBR_DAT_Dat,by="flw_date") %>%
    #     dplyr::mutate(cfs_mean=dplyr::select(.,c(cfs_USACoE,cfs_CBR)) %>% rowMeans(na.rm=T),
    #                   temp_mean=dplyr::select(.,c(temp_f_USACE,temp_f_CBR)) %>% rowMeans(na.rm=T)
    #     )
    # }

return(flow_file |>
       dplyr::bind_rows(all_dat|>
         dplyr::filter(flw_date<lubridate::today())
         ))

  #   dat<-dplyr::bind_rows(local_data, all_dat |> dplyr::filter(flw_date >max(local_data$flw_date)) ) |> dplyr::filter(flw_date<lubridate::today())
  #   readr::write_csv(dat,flow_file)
  #   return(dat|> flow_ema_fun())
  #
  # }
  # else{
  #   return(local_data |> flow_ema_fun())
  # }
}



#' flow_ema_fun
#'
#' @param dat
#' @param start_month
#'
#' @return
#'
#' @examples
flow_ema_fun<-function(dat,start_month=2){
  dat %>%
    dplyr::mutate(Year=lubridate::year(flw_date),
                  month=lubridate::month(flw_date),
                  md=lubridate::mday(flw_date)) %>%
    dplyr::filter(month>=start_month) |>
    dplyr::group_by(Year) %>% dplyr::arrange(flw_date) %>%

    # calculate exponential moving avg of flow using ema fn above
    dplyr::mutate(cfs_mean_ema=ema(cfs_USACoE, ratio=0.1),
                  temp_mean_ema=ema(temp_f_USACE, ratio=0.1)) %>%
    dplyr::ungroup()%>% dplyr::mutate(
      month=lubridate::month(flw_date),
      monthday=lubridate::mday(flw_date),
      flw_date=as.Date(flw_date)
    )

}

get_flow_day<-function(dat,day){
  dat%>% dplyr::mutate(
    month=lubridate::month(flw_date),
    monthday=lubridate::mday(flw_date)
  ) |>
    dplyr::filter(month ==lubridate::month(day) & monthday == lubridate::mday(day))  %>%


    dplyr::mutate(logCflow_mean_ema = (log(cfs_mean_ema) - mean(log(cfs_mean_ema), na.rm=T))/sd(log(cfs_mean_ema), na.rm=T))
}



#' make some ocean covariates
#'
#' @return
#' @export
#'
#' @examples
ocean_cov_fun<-function(pred_year,ocean_cov_file=NULL){

  # Use default location if not provided
  # if (is.null(ocean_cov_file)) {
  #   ocean_cov_file <- get_default_ocean_path()
  # }
  #
  # if(file.exists(ocean_cov_file)){
  #   local_dat<-readr::read_csv(ocean_cov_file)
  # }else{
  #   message("Local file not found at: ", ocean_cov_file)
  #   local_dat<-data.frame(Year=-1)
  # }
  #
  # if((max(local_dat$Year)<pred_year)){
    dat<- readr::read_table("https://www.o3d.org/npgo/data/NPGO.txt", skip = 29, col_names = FALSE, comment = "#") |>
      dplyr::filter(!is.na(X2)) |>
      dplyr::rename(Year = X1, Month = X2, NPGO = X3) |>
      dplyr::mutate(dplyr::across(c(Year, Month), \(x) as.integer(as.numeric(x))),
                    NPGO=round(NPGO,4))|>

      dplyr::left_join(
        readr::read_table("https://psl.noaa.gov/pdo/data/pdo.timeseries.ersstv5.csv", skip = 1, col_names = FALSE, comment = "#") |>
          dplyr::rename(Date = X1, PDO = X2) |>
          dplyr::filter(!PDO < -99) |>
          dplyr::mutate(
            Date = as.Date(Date),
            Month = lubridate::month(Date),
            Year = as.integer(lubridate::year(Date),
                              PDO=round(PDO,4))
          ) |>
          dplyr::select(-Date)
      ) |>

      dplyr::mutate(
        Year = ifelse(Month < 3, Year - 1, Year),
        Season = dplyr::case_when(
          Month %in% 3:5 ~ "Spr",
          Month %in% 6:8 ~ "Sum",
          Month %in% 9:11 ~ "Fal",
          Month %in% c(12, 1, 2) ~ "Win"
        )
      ) |>

      dplyr::group_by(Season, Year) |>
      dplyr::summarise(dplyr::across(c(NPGO, PDO), mean), .groups = "drop") |>
      tidyr::pivot_longer(cols = c(NPGO, PDO)) |>
      tidyr::pivot_wider(values_from = value, names_from = c(Season, name))


    # readr::write_csv(dat,ocean_cov_file)

    return(dat)
  # }else{
  #   return(local_dat)
  # }

}


#' cnts_for_mod_fun
#'
#' @param forecastdate
#' @param Bon_cnts
#'
#' @return
#' @export
#'
#' @examples
cnts_for_mod_fun<-function(forecastdate,Bon_cnts){

  forecast_year<-lubridate::year(forecastdate)
  forecast_month<-lubridate::month(forecastdate)
  forecast_mday<-lubridate::mday(forecastdate)
  forecast_season<-chk_season(forecastdate)
  #
  Bon_cnts |>
    dplyr::filter(season ==forecast_season) |>
    dplyr::arrange(.data$year,.data$month,.data$mday) |>
    dplyr::group_by(.data$year) |>
    dplyr::mutate(
      cum_cnt=cumsum(.data$AdultChinook),
      tot_adult=tail(.data$cum_cnt,1),
      tot_jack=sum(.data$JackChinook)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(month==forecast_month,mday==forecast_mday) |>
    dplyr::mutate(lag_jack=dplyr::lag(tot_jack,1)
    ) |>
    dplyr::select(year,cum_cnt,tot_adult,tot_jack,lag_jack) |>
    dplyr::mutate(tot_adult=ifelse(year==forecast_year,NA,tot_adult),
  log_cum_cnt=log(cum_cnt),log_tot_adult=log(tot_adult),log_lag_jack=log(lag_jack))
}


retry_get_data <- function(expr, max_tries = 4, name = "data", wait_sec = 120) {
  num_tries <- 0
  result <- NULL

  while (num_tries < max_tries) {
    num_tries <- num_tries + 1
    gh_log("notice", sprintf("Attempt %d to get %s ...", num_tries, name))

    attempt <- try(eval(expr), silent = TRUE)

    if (inherits(attempt, "try-error")) {
      err_msg <- conditionMessage(attr(attempt, "condition"))
      gh_log("warning", " -> Failed with error: ", err_msg)
    } else if (is.data.frame(attempt) && nrow(attempt) > 0) {
      gh_log("notice", sprintf(" -> Success on attempt %d", num_tries))
      result <- attempt
      break
    } else {
      gh_log("warning", " -> Returned empty result.")
    }

    if (num_tries < max_tries) {
      gh_log("notice", sprintf("Waiting %d seconds before retry...", wait_sec))
      Sys.sleep(wait_sec)
    }
  }

  if (is.null(result)) {
    gh_log("error", sprintf("Failed to get %s after %d attempts.", name, max_tries))
    stop(sprintf("Failed to get %s after %d attempts.", name, max_tries))
  }

  result
}


gh_log <- function(type = c("notice", "warning", "error"), ...) {
  type <- match.arg(type)
  txt <- paste0("::", type, "::", paste(..., collapse = ""))
  cat(txt, "\n", file = stderr())  # goes to stderr -> Actions logs
}
