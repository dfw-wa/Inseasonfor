## code to prepare `ladder_cnts_historic` dataset

cur_date<-lubridate::today()-5
cur_year<-lubridate::year(cur_date)

ladder_cnts<-fpc_laddersplit(cur_date,
                             sdate=as.Date("1999-08-01"))

ladder_cnts_historic<-ladder_cnts |>
  dplyr::filter(lubridate::year(CountDate)<cur_year,
                dplyr::between(lubridate::month(CountDate),8,11))

usethis::use_data(ladder_cnts_historic, overwrite = TRUE)


## code to prepare `past_bon_cnts` dataset

past_bon_cnts<-bon_dat_fun(pred_date=NULL,
                      sdate="1980-03-01",
            url = "https://www.fpc.org/adults/R_coeadultcount_runsum")|>
  dplyr::filter(month>=3,
                year<cur_year)

usethis::use_data(past_bon_cnts, overwrite = TRUE)

## code to prepare `past_flow_dat` dataset


past_flow_dat<-get_flow_data(sdate="1980-03-01") |>
  dplyr::filter(
    lubridate::year(flw_date)<cur_year)


usethis::use_data(past_flow_dat, overwrite = TRUE)
