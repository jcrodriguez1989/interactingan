#' Add a question to the slide
#'
#' Adds a question to the current slide. It will return the corresponding
#' `iframe` tag which will show the questions done.
#' This function must be called once per question.
#'
#' @param question A character -string- representing the question.
#' @param allow_anonymous A logical indicating if audience can ask questions as
#'   an anonymous user.
#' @param allow_multiple A logical indicating if audience can ask more than one
#'   question.
#' @param max_chars A numeric indicating max characters allowed per question.
#'   The minimum value will be 1.
#' @param width A character with a valid html `width` value for the iframe.
#' @param height A character with a valid html `height` value for the iframe.
#'
#' @return An html `iframe` tag which will show the questions asked.
#'
#' @export
#'
question <- function(question, allow_anonymous = TRUE, allow_multiple = TRUE,
                     max_chars = 160, width = "100%", height = "500px") {
  max_chars <- as.integer(max(1, max_chars))
  act_objs <- elems$objects
  curr_id <- max(c(0, as.numeric(unlist(lapply(act_objs, function(act_obj) {
    if (is(act_obj, "Question")) {
      strsplit(act_obj@id, "_")[[1]][[2]]
    }
  })))))
  new_id <- paste0("question_", curr_id + 1)
  new_question <- Question(
    id = new_id,
    allow_anonymous = allow_anonymous,
    allow_multiple = allow_multiple,
    question = as.character(question),
    max_chars = max_chars
  )
  elems$objects <- append(act_objs, new_question)
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
    '_btn").style.display = "none";\'>Load question</button></p><iframe id="',
    new_id,
    '_frame" width="100%" height="500px" frameborder="0" scrolling="yes"></iframe>'
  )
}

Question <- setClass(
  "Question",
  slots = c(
    id = "character",
    question = "character",
    allow_anonymous = "logical",
    allow_multiple = "logical",
    max_chars = "integer"
  )
)

#### Functions to create the `interactingan` Shiny server app

add_questions_vars <- function(file, questions) {
  if (length(questions) == 0) {
    return()
  }

  cat(paste(
    "# each user questions",
    paste0(
      lapply(questions, function(x) x@id), "_ans", " <- reactiveVal()"
    ),
    "",
    "",
    sep = "\n"
  ), file = file, append = TRUE)
}

add_questions_ui <- function(file, questions) {
  lapply(questions, add_question_ui, file)
}

add_question_ui <- function(question, file) {
  cat(paste(
    "  # question input",
    "  conditionalPanel(",
    paste0(
      '    "(output.is_viewer==false) && (output.act_object==\'',
      question@id,
      "')",
      if (!question@allow_multiple) {
        paste0(" && (output.done_", question@id, "==false)")
      },
      '",'
    ),
    paste0('    h3("', question@question, '"),'),
    if (question@allow_anonymous) {
      paste0('    checkboxInput("', question@id, '_anonymous", "Anonymous"),')
    },
    "    textAreaInput(",
    paste0('      "', question@id, '_question",'),
    '      "",',
    paste0(
      '      placeholder = "Type your question (max of ',
      question@max_chars,
      ' characters)"'
    ),
    "    ),",
    paste0('    actionButton("', question@id, '_send", "Send"),'),
    '    align = "center"',
    "  ),",
    "",
    "  # questions viewer pane",
    "  conditionalPanel(",
    paste0(
      '    "(output.is_viewer==true) && (output.act_object==\'',
      question@id,
      '\')",'
    ),
    paste0(
      '    uiOutput("',
      question@id,
      '_viewer")'
    ),
    "  ),",
    "",
    "",
    sep = "\n"
  ), file = file, append = TRUE)
}

add_questions_server <- function(file, questions) {
  invisible(lapply(questions, add_question_server, file))
}

add_question_server <- function(question, file) {
  if (!question@allow_multiple) {
    cat(paste(
      "  # check if the current user has submited to this question",
      paste0("  output$done_", question@id, " <- reactive({"),
      paste0("    curr_user()$id %in% ", question@id, "_ans()$user"),
      "  })",
      paste0(
        '  outputOptions(output, "done_',
        question@id,
        '", suspendWhenHidden = FALSE)'
      ),
      "",
      "",
      sep = "\n", collapse = ""
    ), file = file, append = TRUE)
  }

  cat(paste(
    "  # for each question, save the user info",
    paste0("  observeEvent(input$", question@id, "_send, {"),
    paste0("    question <- trimws(input$", question@id, "_question)"),
    '    if (question == "") {',
    '      showNotification("Please enter your question", type = "error")',
    "      return()",
    "    }",
    paste0("    if (nchar(question) > ", question@max_chars, ") {"),
    "      showNotification(",
    "        paste0(",
    '          "Too many characters: ",',
    "          nchar(question),",
    '          " (max of ",',
    paste0("          ", question@max_chars, ","),
    '          ")"',
    "        ),",
    '        type = "error"',
    "      )",
    "      return()",
    "    }",
    "    question <- data.frame(",
    "      user = curr_user()$id,",
    if (question@allow_anonymous) {
      paste0(
        "      name = ifelse(input$",
        question@id,
        '_anonymous, "Anonymous", curr_user()$name),'
      )
    } else {
      "      name = curr_user()$name,"
    },
    if (question@allow_anonymous) {
      paste0(
        "      avatar = ifelse(input$",
        question@id,
        '_anonymous, "", curr_user()$avatar),'
      )
    } else {
      "      avatar = curr_user()$avatar,"
    },
    # '      time = format(Sys.time(), "%I:%M %p"),',
    '      time = format(Sys.time(), "%H:%M"),',
    "      question = question,",
    "      stringsAsFactors = FALSE",
    "    )",
    paste0(
      "    if (is.null(",
      question@id,
      "_ans()) || !any(apply(",
      question@id,
      "_ans(), 1, function(x) all(x == question)))) {"
    ),
    '      # solves when "send" is hit very fast',
    paste0(
      "      ",
      question@id,
      "_ans(rbind(",
      question@id,
      "_ans(), question))"
    ),
    '      showNotification("Question sent", type = "message")',
    paste0(
      '      updateTextAreaInput(session, "',
      question@id,
      '_question", value = "")'
    ),
    "    }",
    "  })",
    sep = "\n", collapse = ""
  ), file = file, append = TRUE)

  cat(paste(
    "  # show the questions, nicely printed in the pane",
    paste0("  output$", question@id, "_viewer <- renderUI({"),
    paste0("    act_qs <- ", question@id, "_ans()"),
    "    if (is.null(act_qs) || nrow(act_qs) == 0) {",
    "      return(wellPanel())",
    "    }",
    # '    wellPanel(style = "overflow-y:scroll; max-height: 300px",',
    "    wellPanel(",
    "      apply(act_qs, 1, function(x) wellPanel(",
    "        HTML(paste0(",
    "          '<img src=\"',",
    "          avatars_url, x[\"avatar\"], '.png\" ',",
    "          'title=\"', ifelse(x[\"avatar\"] == \"\", \"\", x[\"user\"]),",
    "          '\" height=\"40\" width=\"40\">'",
    "        )),",
    '        h4(x["name"]),',
    "        x[\"time\"],",
    '        HTML(gsub(" ", "&nbsp;", gsub("\n", br(), paste0("\n", x["question"]))))',
    "      )),",
    "    )",
    "  })",
    "",
    "",
    sep = "\n"
  ), file = file, append = TRUE)
}
