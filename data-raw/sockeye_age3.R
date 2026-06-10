## code to prepare `sockeye_age3` dataset goes here
sockeye_age3<-read.csv(here::here("data-raw","sockeye_age3_25.csv"))
usethis::use_data(sockeye_age3, overwrite = TRUE)
