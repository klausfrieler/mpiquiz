library(tidyverse)


read_metal_item_bank <- function(){
  metal_item_bank <- readxl::read_xlsx("data_raw/item_banks/metal_item_bank.xlsx")
  metal_item_bank[metal_item_bank$role != "black_metal",]$role <- "target"
  metal_item_bank[metal_item_bank$role == "black_metal",]$role <- "foil"
  #SRS_item_bank[SRS_item_bank$correct == "none",]$correct <- "item5"
  usethis::use_data(metal_item_bank, overwrite = TRUE)
  metal_item_bank
}

metal_item_bank <- read_metal_item_bank()
MGQ_item_banks <- list("metal" = metal_item_bank)
usethis::use_data(MGQ_item_banks, overwrite = TRUE)
