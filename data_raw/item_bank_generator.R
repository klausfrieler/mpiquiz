library(tidyverse)


read_metal_item_bank <- function(){
  metal_item_bank <- readxl::read_xlsx("data_raw/item_banks/metal_item_bank.xlsx")
  metal_item_bank[metal_item_bank$role != "black_metal",]$role <- "target"
  metal_item_bank[metal_item_bank$role == "black_metal",]$role <- "foil"
  #SRS_item_bank[SRS_item_bank$correct == "none",]$correct <- "item5"
  usethis::use_data(metal_item_bank, overwrite = TRUE)
  metal_item_bank
}

read_classical_item_bank <- function(){
  classical_item_bank <- readxl::read_xlsx("data_raw/item_banks/classical_item_bank.xlsx")
  #SRS_item_bank[SRS_item_bank$correct == "none",]$correct <- "item5"
  usethis::use_data(classical_item_bank, overwrite = TRUE)
  classical_item_bank
}

read_jazz_item_bank <- function(){
  jazz_item_bank <- readxl::read_xlsx("data_raw/item_banks/jazz_item_bank.xlsx")
  #SRS_item_bank[SRS_item_bank$correct == "none",]$correct <- "item5"
  usethis::use_data(jazz_item_bank, overwrite = TRUE)
  jazz_item_bank
}

read_hiphop_item_bank <- function(){
  hiphop_item_bank <- readxl::read_xlsx("data_raw/item_banks/hiphop_item_bank.xlsx")
  #SRS_item_bank[SRS_item_bank$correct == "none",]$correct <- "item5"
  usethis::use_data(hiphop_item_bank, overwrite = TRUE)
  hiphop_item_bank
}

metal_item_bank <- read_metal_item_bank()
classical_item_bank <- read_classical_item_bank()
jazz_item_bank <- read_jazz_item_bank()
hiphop_item_bank <- read_hiphop_item_bank()
MGQ_item_banks <- list("metal" = metal_item_bank,
                       "classical" = classical_item_bank,
                       "jazz" = jazz_item_bank,
                       "hiphop" = hiphop_item_bank)
usethis::use_data(MGQ_item_banks, overwrite = TRUE)
