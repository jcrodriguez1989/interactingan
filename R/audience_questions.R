#' Allow audience questions
#'
#' Allows audience questions. It will return the corresponding `iframe` tag
#' which will show a questions viewer pane to the current slide.
#' This function can be called several times, it will return the viewer pane
#' for each time it is called.
#'
#' @param allow_anonymous A logical indicating if audience can ask questions as
#'   an anonymous user.
#' @param max_chars A numeric indicating max characters allowed per question.
#'   The minimum value will be 1.
#' @param width A character with a valid html `width` value for the iframe.
#' @param height A character with a valid html `height` value for the iframe.
#'
#' @return An html `iframe` tag which will show the questions done.
#'
#' @export
#'
audience_questions <- function(allow_anonymous = TRUE, max_chars = 160,
                               width = "100%", height = "500px") {
  max_chars <- as.integer(max(1, max_chars))
  elems$audience_questions <- AudQs(
    enabled = TRUE,
    allow_anonymous = allow_anonymous,
    max_chars = max_chars
  )
  new_id <- "aud_qs"
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
    '_btn").style.display = "none";\'>Load audience questions</button></p><iframe id="',
    new_id,
    '_frame" width="100%" height="500px" frameborder="0" scrolling="yes"></iframe>'
  )
}

AudQs <- setClass(
  "AudQs",
  slots = c(
    enabled = "logical",
    allow_anonymous = "logical",
    max_chars = "integer"
  )
)

#### Functions to create the `interactingan` Shiny server app

add_aud_qs_vars <- function(file, aud_qs) {
  if (!aud_qs@enabled) {
    return()
  }

  cat(paste(
    "# audience questions",
    "aud_qs <- reactiveVal()",
    "",
    "",
    sep = "\n"
  ), file = file, append = TRUE)
}

add_aud_qs_ui <- function(file, aud_qs) {
  if (!aud_qs@enabled) {
    return()
  }

  cat(paste(
    "  # button for audience to ask questions",
    "  conditionalPanel(",
    '    "(output.is_viewer==false)",',
    "    h2(actionLink(",
    '      "aud_qs",',
    '      label = "",',
    '      icon = icon("question-circle"),',
    '      title = "Ask the speaker"',
    "    )),",
    '    align = "center"',
    "  ),",
    "",
    "  # questions viewer pane",
    "  conditionalPanel(",
    '    "(output.is_viewer==true) && (output.act_object==\'aud_qs\')",',
    '    uiOutput("aud_qs_viewer")',
    "  ),",
    "",
    "",
    sep = "\n"
  ), file = file, append = TRUE)
}

add_aud_qs_server <- function(file, aud_qs) {
  if (!aud_qs@enabled) {
    return()
  }

  allow_anon <- elems$audience_questions@allow_anonymous
  max_chars <- elems$audience_questions@max_chars
  cat(paste(
    paste0(
      "  # audience question form (max of ",
      max_chars,
      " chars per question)"
    ),
    paste0("  max_aud_q_chars <- ", max_chars),
    "  observeEvent(input$aud_qs, {",
    "    showModal(modalDialog(",
    '      title = "Ask the speaker",',
    "      HTML(paste0(",
    "        '<img src=\"',",
    "        avatars_url,",
    "        curr_user()$avatar,",
    "        '.png\" height=\"40\" width=\"40\">'",
    "      )),",
    "      curr_user()$name,",
    if (allow_anon) '      checkboxInput("q_anonymous", "Anonymous"),',
    "      textAreaInput(",
    '        "q_question",',
    '        "",',
    "        placeholder = paste0(",
    '          "Type your question (max of ",',
    "          max_aud_q_chars,",
    '          " characters)"',
    "        )",
    "      ),",
    "      footer = fluidRow(",
    '        actionButton("send_q", label = "Send"),',
    '        modalButton("Dismiss"),',
    '        align = "center"',
    "      ),",
    "      easyClose = TRUE,",
    '      size = "s",',
    '      align = "center"',
    "    ))",
    "  })",
    "",
    sep = "\n"
  ), file = file, append = TRUE)

  cat(paste(
    "  observeEvent(input$send_q, {",
    "    question <- trimws(input$q_question)",
    '    if (question == "") {',
    '      showNotification("Please enter your question", type = "error")',
    "      return()",
    "    }",
    "    if (nchar(question) > max_aud_q_chars) {",
    "      showNotification(",
    "        paste0(",
    '          "Too many characters: ",',
    "          nchar(question),",
    '          " (max of ",',
    "          max_aud_q_chars,",
    '          ")"',
    "        ),",
    '        type = "error"',
    "      )",
    "      return()",
    "    }",
    "    question <- data.frame(",
    "      user = curr_user()$id,",
    if (allow_anon) {
      '      name = ifelse(input$q_anonymous, "Anonymous", curr_user()$name),'
    } else {
      "      name = curr_user()$name,"
    },
    if (allow_anon) {
      '      avatar = ifelse(input$q_anonymous, "", curr_user()$avatar),'
    } else {
      "      avatar = curr_user()$avatar,"
    },
    # '      time = format(Sys.time(), "%I:%M %p"),',
    '      time = format(Sys.time(), "%H:%M"),',
    "      question = question,",
    "      stringsAsFactors = FALSE",
    "    )",
    "    if (is.null(aud_qs()) || !any(apply(aud_qs(), 1, function(x) all(x == question)))) {",
    '      # solves when "send" is hit very fast',
    "      aud_qs(rbind(aud_qs(), question))",
    "      removeModal()",
    '      showNotification("Question sent", type = "message")',
    "    }",
    "  })",
    "",
    sep = "\n"
  ), file = file, append = TRUE)

  cat(paste(
    "  # show the questions, nicely printed in the pane",
    "  output$aud_qs_viewer <- renderUI({",
    "    aud_qs <- aud_qs()",
    "    if (is.null(aud_qs) || nrow(aud_qs) == 0) {",
    "      return(wellPanel())",
    "    }",
    # '    wellPanel(style = "overflow-y:scroll; max-height: 300px",',
    "    wellPanel(",
    "      apply(aud_qs, 1, function(x) {",
    "        wellPanel(",
    "          HTML(paste0(",
    "            '<img src=\"',",
    "            avatars_url, x[\"avatar\"], '.png\" ',",
    "            'title=\"', ifelse(x[\"avatar\"] == \"\", \"\", x[\"user\"]),",
    "            '\" height=\"40\" width=\"40\">'",
    "          )),",
    '          h4(x["name"]),',
    "          x[\"time\"],",
    '          HTML(gsub(" ", "&nbsp;", gsub("\\n", br(), paste0("\\n", x["question"]))))',
    "        )",
    "      })",
    "    )",
    "  })",
    "",
    "",
    sep = "\n"
  ), file = file, append = TRUE)
}
