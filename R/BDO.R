

BDO_dat<-function(use_DWG=TRUE){


  if(use_DWG){
    BDO_raw<-readr::read_csv( "https://data.wa.gov/resource/w52y-hhyj.csv?$limit=50000") |>
      dplyr::mutate(count_date =as.Date(count_date))
  }else{
    #     bdo_con <- DBI::dbConnect(
    #   RPostgres::Postgres(),
    #   dbname = Sys.getenv("dbname"),
    #   host = Sys.getenv("host"),
    #   port = as.integer(Sys.getenv("port")),
    #   user = Sys.getenv("user"),
    #   password = Sys.getenv("password")
    # )
    # BDO_raw<-DBI::dbGetQuery(bdo_con, 'SELECT * FROM "bdo"."raw_data"')
  }







  bdo_dat <- BDO_raw |>
    dplyr::filter(!is.na(fish_count)) |>
    dplyr::group_by(count_date,ladder,stock,stage) |>
    dplyr::summarise(fish_count=sum(fish_count)) |>
    dplyr::ungroup() |>
    tidyr::complete(count_date,
                    tidyr::nesting(stock, stage), fill = list(fish_count = 0)) |>
    dplyr::mutate(fish_count=ifelse(fish_count=="NaN",NA_integer_,as.integer(fish_count)),
                  fish_count=ifelse(fish_count<0,0,fish_count),
                  is_jack=stage=="Jack",is_jack) |>
    dplyr::group_by(count_date,ladder,is_jack) |>
    dplyr::mutate(stage_prop=proportions(fish_count)) |>
    dplyr::ungroup() |>
    dplyr::rename(CountDate=count_date,Ladder=ladder) |>
    dplyr::select(-fish_count,-is_jack) |>
    tidyr::pivot_wider(names_from = c(stock,stage),values_from = stage_prop)

  bdo_dat
}

#' Get laddersplit count report from FPC (from Ben's fpcDamCounts)
#'
#' @param pred_date
#' @param url
#'
#' @return
#' @export
#'
#' @examples
fpc_laddersplit<-function (pred_date=NULL,
                           sdate=as.Date(paste0(lubridate::year(pred_date),"-08-01")),
                           url = "https://www.fpc.org/adults/R_adultcoequeries_laddersplitreport_results_get"
)
{


  if(is.null(pred_date)){
    edate<-Sys.Date()+2
  } else{
    edate<-pred_date
  }

  cap_out <- capture.output({
    out<-  suppressMessages( suppressWarnings(readr::read_csv(glue::glue("{url}salmon.php?sdate={sdate}&edate={edate}"),
                                                              col_types = readr::cols(CountDate = readr::col_date(format = "%m/%d/%Y"))))) |>
      dplyr::filter(!is.na(CountDate)) |>
      dplyr::filter(Location =="Bonneville") |>
      dplyr::select(CountDate,Ladder,AdultChinook,JackChinook) |>
      #add one day that is missing (Sept 5 2017, presumably due to eagle creek fire) and assume that count was same as the previous day.
      tidyr::complete(CountDate = seq(min(CountDate), max(CountDate), by = "day"),Ladder)|>
      dplyr::group_by(Ladder) |>
      dplyr::arrange(CountDate) |>
      tidyr::fill(c(AdultChinook,JackChinook), .direction = "down")
  })

  out
}



tule_bright_split<-function(pred_date){

  BDO_dat_obs<-BDO_dat()


  ladder_cnts<-
    ladder_cnts_historic |>
    dplyr::bind_rows(
      fpc_laddersplit(pred_date))

    #   get_adult_ladder_DART(start_year = as.integer(format(Sys.Date(), "%Y")), end_year = as.integer(format(Sys.Date(), "%Y")), proj = "BON") |>
    # pivot_DART_ladder_counts() |>
    #   dplyr::mutate(
    #   CountDate=lubridate::ymd(CountDate)
      # )

      # )



  ladder_cnts |>
    dplyr::mutate(
      # CountDate=lubridate::ymd(CountDate),
      year=lubridate::year(.data$CountDate),
      yday=lubridate::yday(.data$CountDate),
      month=lubridate::month(.data$CountDate),
      mday=lubridate::mday(.data$CountDate)
    ) |>
    dplyr::filter(dplyr::between(month,8,11)) |>
    dplyr::left_join(BDO_dat_obs) |>
    dplyr::group_by(year,Ladder) |>
    dplyr::arrange(CountDate) |>
    dplyr::mutate(
      dplyr::across(c(AdultChinook,JackChinook),\(x)tidyr::replace_na(x,0)),
      #interpolate missing days
      dplyr::across(Bright_Adult :Tule_Stubby ,\(x)zoo::na.approx(x,na.rm=F,rule=2)),
      #multiply proportions by total counts
      dplyr::across(c(Bright_Adult,Tule_Adult,Tule_Stubby) ,\(x)x*AdultChinook),
      dplyr::across(c(Bright_Jack,Tule_Jack) ,\(x)x*JackChinook)
    ) |>
    dplyr::group_by(CountDate,year,month,mday) |>
    dplyr::summarise(dplyr::across(Bright_Adult :Tule_Stubby ,\(x)(sum(x,na.rm=TRUE)))) |>
    dplyr::mutate(season="fall")


}





get_adult_ladder_DART <- function(start_year = 1999,
                                  end_year = as.integer(base::format(Sys.Date(), "%Y")),
                                  proj = "BON",
                                  start_mmdd = "8/1",
                                  end_mmdd = "11/30") {

  years <- start_year:end_year

  data_list <- base::lapply(years, function(yr) {
    base_url <- base::paste0(
      "https://www.cbr.washington.edu/dart/cs/php/rpt/adult_ladder.php?",
      "sc=1",
      "&year=", yr,
      "&proj=", proj,
      "&startdate=", utils::URLencode(start_mmdd),
      "&enddate=", utils::URLencode(end_mmdd)
    )

    # base::message("Fetching HTML for year ", yr)

    try_result <- base::tryCatch({

      html <- xml2::read_html(base_url)

      csv_link <- rvest::html_element(html, "a[href$='.csv']")
      href <- rvest::html_attr(csv_link, "href")

      if (base::is.na(href)) {
        base::warning("No CSV link found for year ", yr)
        return(NULL)
      }

      full_csv_url <- base::paste0("https://www.cbr.washington.edu", href)

      # base::message(" → Found CSV URL: ", full_csv_url)

      dat <- readr::read_csv(full_csv_url, show_col_types = FALSE)

      # Only keep rows where the first column is a valid date
      date_col <- dat[[1]]
      date_rows <- lubridate::ymd(date_col, quiet = TRUE)
      dat <- dat[!is.na(date_rows), ]

      dat$Year <- yr
      dat

    }, error = function(e) {
      base::warning("Error fetching year ", yr, ": ", e$message)
      NULL
    })

    try_result
  })

  dplyr::bind_rows(data_list)



}

pivot_DART_ladder_counts <- function(ladder_data) {
  # Create a named vector to map column names to ladder side
  ladder_map <- c(
    "Chinook Left Count" = "Bradford Island",
    "Chinook Right Count" = "Washington Shore",
    "Jack Chinook Left Count" = "Bradford Island",
    "Jack Chinook Right Count" = "Washington Shore"
  )

  # Reshape adult counts
  adult_long <-
    ladder_data|>
    dplyr::select(CountDate=Date,
                  `Bradford Island`=`Chinook Left Count`,
                  `Washington Shore`=`Chinook Right Count`) |>
    tidyr::pivot_longer(c( `Bradford Island`,`Washington Shore`),
                        names_to = "Ladder", values_to = "AdultChinook")


  # Reshape jack counts
  jack_long <-
    ladder_data|>
    dplyr::select(CountDate=Date,
                  `Bradford Island`=`Jack Chinook Left Count`,
                  `Washington Shore`=`Jack Chinook Right Count`) |>
    tidyr::pivot_longer(c( `Bradford Island`,`Washington Shore`),
                        names_to = "Ladder", values_to = "JackChinook")

  # Join adult and jack counts by all other columns + Ladder
  merged <- dplyr::left_join(adult_long, jack_long)

  merged
}




