#' Add a wordcloud to the slide
#'
#' Adds a wordcloud to the current slide. It will return the corresponding
#' `iframe` tag which will show the wordcloud results.
#' This function must be called once per wordcloud.
#'
#' @param question A character -string- representing the wordcloud question.
#' @param max_words A numeric indicating max words allowed per user. The minimum
#'   value will be 1.
#' @param width A character with a valid html `width` value for the iframe.
#' @param height A character with a valid html `height` value for the iframe.
#' @param ... Additional parameters to be passed to wordcloud::wordcloud
#'   function.
#'
#' @return An html `iframe` tag which will show the wordcloud results.
#'
#' @export
#'
wordcloud <- function(question, max_words = 2,
                      width = "100%", height = "500px", ...) {
  max_words <- as.integer(max(1, max_words))
  act_objs <- elems$objects
  curr_id <- max(c(0, as.numeric(unlist(lapply(act_objs, function(act_obj) {
    if (is(act_obj, "Wordcloud")) {
      strsplit(act_obj@id, "_")[[1]][[2]]
    }
  })))))
  new_id <- paste0("wordcloud_", curr_id + 1)
  new_wordcloud <- Wordcloud(
    id = new_id,
    question = as.character(question),
    max_words = max_words,
    dots = list(...)
  )
  elems$objects <- append(act_objs, new_wordcloud)
  paste0(
    '<p align="center"><button id="',
    new_id,
    '_btn" onClick=\'document.getElementById("',
    new_id,
    '_frame").src = "',
    app_info$params$url,
    "?viewer=",
    app_info$params$key,
    "&object=",
    new_id,
    '";document.getElementById("',
    new_id,
    '_btn").style.display = "none";\'>Load wordcloud</button></p><iframe id="',
    new_id,
    '_frame" width="100%" height="500px" frameborder="0" scrolling="no"></iframe>'
  )
}

Wordcloud <- setClass(
  "Wordcloud",
  slots = c(
    id = "character",
    question = "character",
    max_words = "integer",
    dots = "list"
  )
)

#### Functions to create the `interactingan` Shiny server app

add_wordclouds_vars <- function(file, wordclouds) {
  if (length(wordclouds) == 0) {
    return()
  }

  cat('library("wordcloud")\n\n', file = file, append = TRUE)
  cat(paste(
    "# words that each user wrote",
    paste0(
      lapply(wordclouds, function(x) x@id), "_ans", " <- reactiveVal(list())",
      collapse = "\n"
    ),
    "",
    "",
    sep = "\n"
  ), file = file, append = TRUE)
}

add_wordclouds_ui <- function(file, wordclouds) {
  lapply(wordclouds, add_wordcloud_ui, file)
}

add_wordcloud_ui <- function(wordcloud, file) {
  cat(paste(
    "  # wordcloud input",
    "  conditionalPanel(",
    paste0(
      '    "(output.is_viewer==false) && (output.act_object==\'',
      wordcloud@id,
      "') && (output.done_",
      wordcloud@id,
      '==false)",'
    ),
    paste0('    h3("', wordcloud@question, '"),'),
    paste(
      "    textAreaInput(",
      paste0('      "', wordcloud@id, '_words",'),
      paste0(
        '      label = "Words (up to ',
        wordcloud@max_words,
        ' words)",'
      ),
      '      placeholder = "Type your words (separated by comma) ..."',
      "    ),",
      paste0('    actionButton("', wordcloud@id, '_send", "Send"),'),
      sep = "\n"
    ),
    '    align = "center"',
    "  ),",
    "",
    "  # wordcloud results plot",
    "  conditionalPanel(",
    paste0(
      '    "((output.is_viewer==true) || (output.done_',
      wordcloud@id,
      "==true)) && (output.act_object=='",
      wordcloud@id,
      '\')",'
    ),
    paste0('    wellPanel(plotOutput("', wordcloud@id, '"))'),
    "  ),",
    "",
    "",
    sep = "\n"
  ), file = file, append = TRUE)
}

add_wordcloud_admin_panel <- function(wordcloud) {
  paste(
    paste0("    if (length(", wordcloud@id, "_ans()) > 0) {"),
    "      res <- paste0(",
    "        res,",
    "        paste(",
    paste0('          "', wordcloud@question, '",'),
    paste0(
      "          paste(names(",
      wordcloud@id,
      "_ans()), ",
      wordcloud@id,
      '_ans(), collapse = "\\n"),'
    ),
    '          sep = "\\n"',
    "        ),",
    '        "\\n\\n"',
    "      )",
    "    }",
    sep = "\n"
  )
}

add_wordclouds_server <- function(file, wordclouds) {
  invisible(lapply(wordclouds, add_wordcloud_server, file))
}

add_wordcloud_server <- function(wordcloud, file) {
  cat(paste(
    "  # check if the current user has submited to this wordcloud",
    paste0("  output$done_", wordcloud@id, " <- reactive({"),
    paste0("    curr_user()$id %in% names(", wordcloud@id, "_ans())"),
    "  })",
    paste0(
      '  outputOptions(output, "done_',
      wordcloud@id,
      '", suspendWhenHidden = FALSE)'
    ),
    "",
    "",
    sep = "\n", collapse = ""
  ), file = file, append = TRUE)

  cat(paste(
    "  # for each answer, save the voters id and words",
    paste0("  observeEvent(input$", wordcloud@id, "_send, {"),
    paste0("    words <- input$", wordcloud@id, "_words"),
    '    words <- trimws(strsplit(words, ",")[[1]])',
    "    if (length(words) == 0) {",
    '      showNotification("Please enter your words", type = "error")',
    "      return()",
    "    }",
    paste0(
      "    words <- words[seq_len(min(length(words), ",
      wordcloud@max_words,
      "))]"
    ),
    paste0("    act_ans <- ", wordcloud@id, "_ans()"),
    "    act_ans[[curr_user()$id]] <- words",
    paste0("    ", wordcloud@id, "_ans(act_ans)"),
    "  })",
    "",
    sep = "\n", collapse = ""
  ), file = file, append = TRUE)

  # default parameters for wordcloud plot
  wc_params <- list(
    min.freq = "1", random.order = "FALSE", colors = 'brewer.pal(8, "Dark2")'
  )
  wc_params[names(wordcloud@dots)] <- wordcloud@dots
  cat(paste(
    "  # create the wordcloud answers plot",
    paste0("  output$", wordcloud@id, " <- renderPlot({"),
    paste0("    act_ans <- table(unlist(", wordcloud@id, "_ans()))"),
    "    if (length(act_ans) == 0) {",
    "      return()",
    "    }",
    "    set.seed(8818) # so everyone gets the same cloud",
    "    wordcloud(",
    "      names(act_ans),",
    "      as.vector(act_ans),",
    paste("     ", names(wc_params), "=", wc_params, collapse = ",\n"),
    "    )",
    "  })",
    "",
    "",
    sep = "\n"
  ), file = file, append = TRUE)
}
