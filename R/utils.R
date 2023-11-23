messagef <- function(...) message(sprintf(...))
printf <- function(...) print(sprintf(...))

is.scalar.character <- function(x) {
  is.character(x) && is.scalar(x)
}

is.scalar.logical <- function(x) {
  is.logical(x) && is.scalar(x)
}

is.scalar <- function(x) {
  identical(length(x), 1L)
}

tagify <- function(x) {
  stopifnot(is.character(x) || is(x, "shiny.tag"))
  if (is.character(x)) {
    stopifnot(is.scalar(x))
    shiny::p(x)
  } else x
}

tagify_with_line_breaks <- function(x, sep = "//", style = NULL){
  stopifnot(is.character(x) || is(x, "shiny.tag"))
  if(!is.character(x)){
    return(x)
  }
  elems <- strsplit(x, sep)[[1]]
  ret <- shiny::p()
  for(i in seq_along(elems)){
    if(i != length(elems)) {
      ret <- shiny::tagAppendChildren(ret, shiny::span(elems[[i]]), shiny::br())
    }
    else{
      ret <- shiny::tagAppendChildren(ret, shiny::span(elems[[i]]))

    }
  }
  shiny::tagAppendAttributes(ret, style = style)
}


join_dicts <- function(dict1, dict2 = NULL, keys1 = NULL, keys2 = NULL){
  if(is.null(dict2 )){
    if(!is.null(keys1)){
      dict1 <- dict1 %>% filter(key %in% keys1)
    }
    return(dict1)
  }

  dict1 <- dict1 %>% as.data.frame()
  if(!is.null(keys1)){
    dict1 <- dict1 %>% filter(key %in% keys1)
  }
  for(i in seq_along(dict2)){
    tmp <- dict2[[i]] %>% as.data.frame()
    if(!is.null(keys2)){
      tmp <- tmp %>% filter(key %in% keys2)
    }
    common_names <- intersect(names(dict1), names(tmp))
    if(length(common_names) == 1){
      stop("Incompatible dictionaries")
    }
    dict1 <- dplyr::bind_rows(dict1[,common_names], tmp[,common_names]) %>% dplyr::distinct(key, .keep_all = T)
  }
  dict1 %>% psychTestR::i18n_dict$new()
}
