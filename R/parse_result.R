#' parse_mgc_result
#'
#' This function parses a entry in a typical psychTestR battery result list and returns it as a neat data frame
#' Will check for all quizzes at once.
#' @param entry (list) One entry of a psychTestR battery result list
#' @export
#'
parse_mgc_result <- function(entry){
  quiz_results <- names(entry)[stringr::str_detect(names(entry), "^MGQ_")]
  if(length(quiz_results) == 0){
    return(NULL)
  }
  res <- purrr::map_dfr(quiz_results, function(x) {
    tmp <- entry[[x]] %>% as.data.frame() %>% select(-c(items, q0, raw)) %>% distinct()
    raw <- map_dfr(stringr::str_split(entry[[x]]$raw, ","), function(r){
      names <- trimws(str_split_fixed(r, ":", 2)[,1])
      str_split_fixed(r, ":", 2)[,2] %>%
        as.numeric() %>%
        t() %>%
        as.data.frame() %>%
        as_tibble() %>%
        set_names(names)
    })
    tmp %>% bind_cols(raw) %>% mutate(quiz = x, num_items = num_targets + num_foils)

  }) %>%
    select(quiz, num_items, num_targets, num_foils, num_correct, perc_correct, f1 = points, everything())
  #browser()
  return(res)
}
