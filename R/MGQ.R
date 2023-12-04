get_MGQ_item_sequence <- function(num_items = NULL, item_bank = NULL, equal_probability = T, seed = NULL){
  #browser()
  if(!is.null(seed)){
    set.seed(seed)
  }
  max_num_targets <- nrow(item_bank %>% filter(role != "foil"))
  max_num_foils   <- nrow(item_bank %>% filter(role == "foil"))
  max_items <- nrow(item_bank)
  num_items <- max(4, min(num_items, max_items))
  if(equal_probability){
    if(num_items > 2 * min(max_num_targets, max_num_foils)) {
      messagef("NUmber of items requested is too large, Setting to %d", 2 * min(max_num_targets, max_num_foils))
      num_targets <- num_foils <- min(max_num_targets, max_num_foils)
      num_items <-  2*num_targets
    }
    else{
      num_targets <- round(num_items/2)
      num_foils <- round(num_items/2)
    }
    if(num_items %% 2) num_targets <- num_targets + 1
    messagef("Found %d/%d targets/foils %.2f) for %d items",
             num_targets,
             num_foils,
             num_targets/num_items,
             num_items)
    targets <- item_bank %>% filter(role != "foil") %>% sample_n(num_targets)
    foils <- item_bank %>% filter(role == "foil") %>% sample_n(num_foils)
    items <- bind_rows(targets, foils) %>% sample_n(nrow(.))
    return(list(items = items, num_targets = num_targets, num_foils = num_foils))
  }

  num_targets <- 0
  p <- mean(item_bank$role == "target")
  sd <- sqrt(p * (1 - p)/max_items)
  lower <- p - 2 * sd
  upper <- p + 2 * sd
  while(num_targets/num_items < lower || num_targets/num_items > upper ){
    items <- item_bank %>% sample_n(num_items)
    num_targets <- nrow(items %>% filter(role != "foil"))
    num_foils <- nrow(items %>% filter(role == "foil"))
    messagef("Found %d/%d targets/foils %.2f) for %d items", num_targets, num_foils, num_targets/num_items, num_items)
  }
  return(list(items = items, num_targets = num_targets, num_foils = num_foils))
}

MGQ_item_page <- function(num_items = 10,
                          item_bank = NULL,
                          type = NULL,
                          dict = mpiquiz::mpiquiz_dict,
                          timeout = 180,
                          on_complete = NULL){
  #browser()
  item_data <- get_MGQ_item_sequence(num_items, item_bank)
  items <- item_data[["items"]]
  num_targets <- item_data[["num_targets"]]
  num_foils <- item_data[["num_foils"]]

  labels <- items %>% pull(name)
  choices <- sprintf("%s:%s", items %>% pull(role), labels)
  timer_script <- sprintf("var myTimer;can_advance = true;if(myTimer)window.clearTimeout(myTimer);myTimer = window.setTimeout(function(){if(can_advance){Shiny.onInputChange('next_page', performance.now());console.log('TIMEOUT')}}, %d);console.log('Set timer: ' + %d + 's');", timeout * 1000, timeout)
  psychTestR::join(
    psychTestR::code_block(function(state, ...){
      psychTestR::save_result(state, label = "items",
                              value = paste(sprintf("%s:%s", items$role, items$name),
                                            collapse = ", "))
      psychTestR::save_result(state, label = "num_targets", value = num_targets)
      psychTestR::save_result(state, label = "num_foils", value = num_foils)
    }),
    psychTestR::checkbox_page(
      label = "q0",
      prompt = shiny::div(
        shiny::tags$script(timer_script),
        shiny::p(psychTestR::i18n(sprintf("PROMPT_%s", toupper(type)), sub = list(time_out = as.character(timeout))))),
      choices = choices,
      labels = labels,
      trigger_button_text = psychTestR::i18n("CONTINUE"),
      save_answer = T,
      on_complete = on_complete
    )

  )
  #, dict = dict)
}

mean_f1 <- function(num_targets, num_foils, TP, FP, FN, TN){
  f1 <- 2 * TP/(2 * TP + FN + FP)
  f1_inv <- 2 * TN/(2*TN + FP + FN)
  base_line <- .5 * max(2*num_targets/(2*num_targets + num_foils), 2*num_foils/(2*num_foils  + num_targets))
  points <- .5 * (f1 + f1_inv)
  points_scaled <- max(0, (points - base_line)/(1 - base_line))
  #messagef("Points: %f, scaled: %f, base_line: %f", points, points_scaled, base_line)
  points_scaled
}

simulate_rater <- function(num_targets, num_foils, num_selected = 1, size = 100, p_target = NULL, p_foil = NULL){
  num_items <- num_targets +  num_foils
  base_set <- c(rep(1, num_targets), rep(0, num_foils))
  items <- sample(base_set, num_items)
  empty <- rep(0, num_items)

  purrr::map_dfr(1:size, function(i){
    #browser()
    ratings <- empty
    if(is.null(p_target)){
      idz <- sample(1:num_items, size = num_selected)
      ratings[idz] <- 1
    }
    else{
      idz_target <- sample(which(items == 1), round(sum(items) * p_target))
      idz_foil <- setdiff(which(items == 0), sample(which(items == 0), round(sum(1 - items) * p_foil)))
      idz <- union(idz_target, idz_foil)
      ratings[idz] <- 1

    }

    TP <- sum(items == ratings & items == 1)
    FP <- sum(items != ratings & items == 1)
    TN <- sum(items == ratings & items == 0)
    FN <- sum(items != ratings & items == 0)
    points <- mean_f1(num_targets, num_foils, TP, FP, FN, TN)
    tibble(num_items,
           num_targets,
           num_foils,
           N = num_selected,
           TP = TP,
           FP = FP,
           TN = FN,
           FN = FN,
           points = points,
           iter = i)
  })
}
MGQ_scoring <- function(label){
  psychTestR::code_block(function(state, ...) {
    results <- psychTestR::get_results(state = state, complete = FALSE) %>% as.list()
    results <- results[[label]]
    if(is.null(results)){
      warning("MGQ_scoring: Found invalid results")
      return()
    }
    res <- results$q0
    num_targets = results$num_targets
    num_foils <- results$num_foils
    num_items <- num_targets + num_foils
    res <- purrr::map_chr(stringr::str_split(res, ":"), ~{.x[1]})
    correct <- sum(res != "foil" & res != "")
    incorrect <- sum(res == "foil")
    #points <- correct - 2*incorrect
    correct <- c(rep(TRUE, correct), rep(FALSE, num_targets - correct))
    TP <- sum(correct)
    FN <- num_targets - TP
    FP <- incorrect
    TN <- results$num_foils - FP
    f1 <- 2 * TP/(2 * TP + FN + FP)
    f1_inv <- 2 * TN/(2*TN + FP + FN)
    base_line <- .5 * max(2*num_targets/(2*num_targets + num_foils), 2*num_foils/(2*num_foils  + num_targets))
    #p_e <- num_targets/num_items
    #base_line <- (1 - p_e)/p_e
    #r <- (mean(correct) - p_e)/p_e
    points <- .5 * (f1 + f1_inv)
    points <- max(0, (points - base_line)/(1 - base_line))
    #browser()
    message(sprintf("TP:%d, FP:%d, TN:%d, FN:%d, f1: %.2f, f1_inv: %.2f", TP, FP, TN, FN, f1, f1_inv))
    psychTestR::save_result(state, label = "perc_correct", value = mean(correct, na.rm = T))
    psychTestR::save_result(state, label = "num_items", value = num_items)
    psychTestR::save_result(state, label = "num_selected", value = length(res[nzchar(res)]))
    psychTestR::save_result(state, label = "num_correct", value = sum(correct, na.rm = T))
    psychTestR::save_result(state, label = "points", value = round(points *100, 0))
    psychTestR::save_result(state, label = "raw", value = sprintf("TP:%d, FP:%d, TN:%d, FN:%d", TP, FP, TN, FN))
    psychTestR::save_result(state, label = "FP", value = FP)
    psychTestR::save_result(state, label = "FN", value = FN)
  })

}

MGQ_welcome_page <- function(dict = mpiquiz::mpiquiz_dict, type = "metal", timeout = 10){
  instructions_id <- sprintf("INSTRUCTIONS_%s", toupper(type))
  psychTestR::new_timeline(
    psychTestR::one_button_page(
      body = shiny::div(
        shiny::h4(psychTestR::i18n(sprintf("WELCOME_%s", toupper(type)))),
        shiny::div(psychTestR::i18n(instructions_id, sub = list(time_out = timeout)),
                   style = "margin-left:0%;width:50%;text-align:justify;margin-bottom:30px")
      ),
      button_text = psychTestR::i18n("CONTINUE")
    ), dict = dict)
}

MGQ_clear_page <- function(dict = mpiquiz::mpiquiz_dict){
  psychTestR::new_timeline(
    psychTestR::one_button_page(
      body = shiny::div(
        shiny::h4(psychTestR::i18n("FINISHED")),
        shiny::tags$script("can_advance = false;if(myTimer){window.clearTimeout(myTimer);console.log('MGQ: Cleared timeout')};")
      ),
      button_text = psychTestR::i18n("CONTINUE")
    ), dict = dict)
}

MGQ_final_page <- function(dict = mpiquiz::mpiquiz_dict){
  psychTestR::new_timeline(
    psychTestR::final_page(
      body = shiny::div(
        shiny::h4(psychTestR::i18n("THANK_YOU")),
        shiny::div(psychTestR::i18n("CLOSE_BROWSER"),
                   style = "margin-left:0%;display:block"),
        shiny::tags$script("can_advance = false;if(myTimer){window.clearTimeout(myTimer);console.log('MGQ: Cleared timeout')};")
      )
    ), dict = dict)
}

MGQ_feedback_with_score <- function(dict = mpiquiz::mpiquiz_dict, label){
  feedback_macro <- "FEEDBACK_SINGLE_PAGE"
  psychTestR::new_timeline(
    psychTestR::reactive_page(function(state,...){
      results <- psychTestR::get_results(state = state, complete = TRUE, add_session_info = F) %>% as.list()

      res <- results[[label]]
      text <- shiny::div(
        shiny::tags$script("can_advance = false;if(myTimer)window.clearTimeout(myTimer);console.log('MGQ: Cleared timeout');"),
        shiny::p(psychTestR::i18n(feedback_macro,
                                  sub = list(num_correct = res$num_correct,
                                             num_items = res$num_targets,
                                             FP = res$FP,
                                             FN = res$FN,
                                             points = res$points,
                                             perc_correct = round(100 * res$perc_correct, 1)))))
      psychTestR::one_button_page(body = text,
                                  button_text = psychTestR::i18n("CONTINUE"))
    }),
    dict = dict)
}

#' MGQ
#'
#' This function defines a MGQ  module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the MSM in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For demoing the MSM, consider using \code{\link{MGQ_demo}()}.
#' For a standalone implementation of the MGQ,
#' consider using \code{\link{MGQ_standalone}()}.
#' @param num_items (Integer scalar) Number of items in the test. Default NULL pulls all items.
#' @param type (Character scalar) Type of quiz to implement. Currently supported: classical, metal, jazz, hiphop
#' @param with_welcome (Logical scalar) Whether to show a welcome page.
#' @param with_finish (Logical scalar) Whether to show a finished page.
#' @param with_feedback (Logical scalar) Whether to include feedback to the participants.
#' @param label (Character scalar) Label to give the MGQ results in the output file.
#' @param dict The psychTestR dictionary used for internationalisation.
#' @param timeout (Double scalar) The time to answer (in seconds)
#' @param ... Further arguments to be passed to \code{MGQ_main_test()}.
#' @export
#'
MGQ <- function(num_items = NULL,
                type = "metal",
                with_welcome = TRUE,
                with_finish = FALSE,
                with_feedback = FALSE,
                label = "MGQ",
                dict = mpiquiz::mpiquiz_dict,
                timeout = 180,
                ...){
  if(!(type %in% names(mpiquiz::MGQ_item_banks))){
    stop(sprintf("Type must be one of %s", paste(names(mpiquiz::MGQ_item_banks), collapse =", ")))
  }
  item_bank <- mpiquiz::MGQ_item_banks[[type]]
  label <- sprintf("%s_%s", label, type)
  main <- psychTestR::new_timeline(
    MGQ_main_test(num_items = num_items, item_bank = item_bank, type = type, timeout = timeout, label = label),
    dict = dict)

  psychTestR::join(
    psychTestR::begin_module(label),
    if (with_welcome) MGQ_welcome_page(timeout = timeout, type = type),
    main,
    if(with_feedback) MGQ_feedback_with_score(dict = dict, label = label),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    # psychTestR::code_block(function(state, ...){
    #   results <- psychTestR::get_results(state, complete = F)
    #   browser()
    # }),
    if(with_finish) MGQ_final_page(),
    if(!with_finish && !with_feedback) MGQ_clear_page(),

    psychTestR::end_module())

}

MGQ_main_test <- function(num_items = NULL, item_bank = NULL, type = "metal", timeout = 180, label = "MGQ"){
  elts <- psychTestR::join(
    MGQ_item_page(num_items = num_items, item_bank = item_bank, type = type, dict = dict, timeout = timeout),
    MGQ_scoring(label)
  )
  elts
}
#' Demo MGQ
#'
#' This function launches a demo for the MGQ
#'
#' @param num_items (Integer scalar) Number of items in the test. Default NULL pulls all items.
#' @param type (Character scalar) Type of quiz to implement. Currently supported: classical, metal, jazz, hiphop
#' @param timeout (Double scalar) The time to answer (in seconds)
#' @param title (Character scalar) The title
#' @param admin_password (Scalar character) Password for accessing the admin panel.
#' Defaults to \code{"demo"}.
#' @param researcher_email (Scalar character)
#' If not \code{NULL}, this researcher's email address is displayed
#' at the bottom of the screen so that online participants can ask for help.
#' Defaults to \email{longgold@gold.uc.ak},
#' the email address of this package's developer.
#' @param dict The psychTestR dictionary used for internationalisation.
#' @param language The language you want to run your demo in.
#' Possible languages include English (\code{"en"}) and German (informal: \code{"de"}, formal: \code{"de_f"}).
#' The first language is selected by default
#' @param ... Further arguments to be passed to \code{\link{MGQ}()}.
#' @export
#'
MGQ_demo <- function(num_items = 3L,
                     type = "metal",
                     timeout = 180,
                     title = "MGQ Demo",
                     dict = mpiquiz::mpiquiz_dict,
                     admin_password = "demo",
                     researcher_email = "klaus.frieler@ae.mpg.de",
                     language = c("en", "de", "de_f")){
  elts <- psychTestR::join(
    MGQ_welcome_page(dict = dict, type = type, timeout = timeout),
    MGQ(num_items = num_items, type = type, with_welcome = F, with_feedback = T,  with_finish =  F, timeout = timeout),
    MGQ_final_page(dict = dict)
  )

  psychTestR::make_test(
    elts,
    opt = psychTestR::test_options(title = title,
                                   admin_password = "MGQdemo",
                                   logo = "https://s3-eu-west-1.amazonaws.com/media.dots.org/img/mpiae_logo.png",
                                   logo_width = "200px",
                                   logo_height = "auto",
                                   problems_info = "",
                                   researcher_email = "klaus.frieler@ae.mpg.de",
                                   languages = c("de", "en", "de_f"),
                                   demo = TRUE))
}

#' Standalone MGQ
#'
#' This function launches a standalone testing session for the MGQ
#' This can be used for data collection, either in the laboratory or online.
#' @param title (Scalar character) Title to display during testing.
#' @param num_items (Scalar integer) Number of items to be adminstered. Default NULL pulls all items.
#' @param type (Character scalar) Type of quiz to implement. Currently supported: classical, metal, jazz, hiphop
#' @param timeout (Double scalar) The time to answer (in seconds)
#' @param with_id (Logical scalar) Whether to show a ID page.
#' @param with_welcome (Logical scalar) Whether to show a welcome page.
#' @param with_feedback (Logical scalar) Whether to include feedback to the participants.
#' @param admin_password (Scalar character) Password for accessing the admin panel.
#' @param researcher_email (Scalar character)
#' If not \code{NULL}, this researcher's email address is displayed
#' at the bottom of the screen so that online participants can ask for help.
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include English (\code{"EN"}),
#' and German (informal: \code{"DE"}, formal: \code{"DE_f"}).
#' The first language is selected by default
#' @param dict The psychTestR dictionary used for internationalisation.
#' @param validate_id (Character scalar or closure) Function for validating IDs or string "auto" for default validation
#' which means ID should consist only of  alphanumeric characters.
#' @param ... Further arguments to be passed to \code{\link{MGQ}()}.
#' @export
#'
MGQ_standalone  <- function(title = NULL,
                            num_items = NULL,
                            type = "metal",
                            timeout = 180,
                            with_id = FALSE,
                            with_welcome = TRUE,
                            with_feedback = TRUE,
                            admin_password = "conifer",
                            researcher_email = "klaus.frieler@ae.mpg.de",
                            languages = c("en", "de", "de_f"),
                            dict = mpiquiz::mpiquiz_dict,
                            validate_id = "auto",
                            ...) {
  elts <- psychTestR::join(
    if(with_id)
      psychTestR::new_timeline(
        psychTestR::get_p_id(prompt = psychTestR::i18n("ENTER_ID"),
                             button_text = psychTestR::i18n("CONTINUE"),
                             validate = validate_id),
        dict = dict),
    MGQ(
      num_items = num_items,
      type = type,
      with_welcome =  with_welcome,
      with_finish = FALSE,
      with_feedback = with_feedback,
      dict = dict,
      timeout = timeout,
      ...),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    MGQ_final_page(dict = dict)
  )
  if(is.null(title)){
    #extract title as named vector from dictionary
    title <- purrr::map_chr(languages, ~{dict$translate(sprintf("TESTNAME_%s", toupper(type)), .x)})
    names(title) <- languages

  }
  psychTestR::make_test(
    elts,
    opt = psychTestR::test_options(title = title,
                                   admin_password = admin_password,
                                   researcher_email = researcher_email,
                                   demo = FALSE,
                                   languages = tolower(languages)))
}
