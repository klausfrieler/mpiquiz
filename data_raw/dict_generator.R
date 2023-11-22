library(tidyverse)

mpiquiz_dict_raw <-
  map_dfr(list.files("./data_raw/dicts/", full.names = TRUE), function(filepath) {
    dict <- readxl::read_xlsx(filepath) %>%
      filter(nchar(de) != 0, nchar(en) != 0) %>%
      mutate(de_f = de)
    acronym <- substr(basename(filepath), 1, 3)
    #messagef("Reading %s_dict", acronym)
    # if(toupper(acronym) %in%  c("MGQ")){
    #   dict <- dict %>% mutate(key = sprintf("%s_%s", acronym, key))
    # }
    return(dict)
  })

mpiquiz_dict <- psychTestR::i18n_dict$new(mpiquiz_dict_raw)

usethis::use_data(mpiquiz_dict, overwrite = TRUE)

