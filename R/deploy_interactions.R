#' Deploy all interactions to the server
#'
#' Deploys all created interaction objects to the, `interactingan` Shiny server,
#' previously configured by the `set_app` function.
#'
#' @export
#'
deploy_interactions <- function() {
  # create a random folder for the app to deploy
  app_dir <- paste0(tempdir(), "/app")
  out_file <- paste0(app_dir, "/app.R")

  dir.create(app_dir, showWarnings = FALSE)
  unlink(out_file)

  create_shiny_file(out_file)

  if (app_info$params$deployed) {
    dots <- app_info$params$dots
    fun_call <- as.call(append(
      rsconnect::deployApp,
      c(
        appDir = app_dir,
        appName = app_info$params$app_name,
        launch.browser = FALSE,
        forceUpdate = TRUE,
        dots
      )
    ))
    output <- capture.output({
      conn_success <- eval(fun_call, list2env(dots))
    })

    if (!conn_success ||
      !any(grepl("Application successfully deployed to ", output))) {
      stop("Could not set app. Make sure `rsconnect` is well configured.")
    }
  }

  invisible(out_file)
}

create_shiny_file <- function(out_file) {
  # Shiny app header (includes)
  add_app_header(out_file)

  # Global vars
  polls <- elems$polls
  if (elems$audience_questions) {
    add_aud_qs_vars(out_file)
  }
  add_polls_vars(out_file, polls)

  # UI
  add_ui_header(out_file)
  add_obj_selector_ui(out_file, elems)
  if (elems$audience_questions) {
    add_aud_qs_ui(out_file)
  }
  add_polls_ui(out_file, polls)
  add_ui_footer(out_file)

  # Server
  add_server_header(out_file)
  if (elems$audience_questions) {
    add_aud_qs_server(out_file)
  }
  add_polls_server(out_file, polls)
  add_server_footer(out_file)

  # shinyApp call
  add_app_footer(out_file)

  invisible(out_file)
}

add_app_header <- function(file) {
  cat(paste(
    'library("ggplot2")',
    'library("shiny")',
    "",
    "# get RStudio hex stickers urls from GitHub (to use as profile pics)",
    'avatars <- readLines("https://github.com/rstudio/hex-stickers/tree/master/PNG")',
    "avatars <- avatars[",
    '  grep(\'" href="/rstudio/hex-stickers/blob/master/PNG/.*\\\\.png\', avatars)',
    "]",
    "avatars <- sort(sub(",
    '  "\\\\.png.*", "",',
    '  sub(".*/rstudio/hex-stickers/blob/master/PNG/", "", avatars)',
    "))",
    "avatars_url <- ",
    '  "https://raw.githubusercontent.com/rstudio/hex-stickers/master/PNG/"',
    "",
    "# currently selected object by the viewer user",
    'act_object <- reactiveVal("none")',
    "# connected users",
    "users <- reactiveVal()",
    "",
    "",
    sep = "\n"
  ), file = file, append = TRUE)
}

add_aud_qs_vars <- function(file) {
  cat(paste(
    "# audience questions",
    "aud_qs <- reactiveVal()",
    "",
    "",
    sep = "\n"
  ), file = file, append = TRUE)
}

add_polls_vars <- function(file, polls) {
  invisible(lapply(polls, add_poll_vars, file))
}

add_poll_vars <- function(poll, file) {
  cat(paste(
    "# users that voted for each answer",
    paste0(poll@id, "_ans", " <- reactiveVal(list("),
    paste0(
      "  opt_",
      poll@options,
      c(rep(" = NULL,", length(poll@options) - 1), " = NULL"),
      collapse = "\n"
    ),
    "))",
    "",
    "",
    sep = "\n"
  ), file = file, append = TRUE)
}

add_ui_header <- function(file) {
  cat(paste(
    "ui <- fluidPage(",
    paste0('  title = "interactingan: ', app_info$params$app_name, '",'),
    "",
    sep = "\n"
  ), file = file, append = TRUE)
}

add_obj_selector_ui <- function(out_file, elems) {
  polls <- elems$polls

  objs <- c(
    '      "Select object" = "empty"',
    lapply(seq_along(polls), function(i) {
      paste0('      "', polls[[i]]@question, '" = "', polls[[i]]@id, '"')
    })
  )

  if (elems$audience_questions) {
    objs <- c(
      objs, paste0('      "Audience Questions" = "aud_qs"')
    )
  }

  cat(paste(
    "  # if it is the viewer user, then show the interactive object selector",
    "  conditionalPanel(",
    '    "(output.is_viewer==true)",',
    '    selectInput(inputId = "act_obj", label = "", choices = c(',
    paste(objs, collapse = ",\n"),
    "    )),",
    '    align = "center"',
    "  ),",
    "",
    "",
    sep = "\n"
  ), file = out_file, append = TRUE)
}

add_aud_qs_ui <- function(file) {
  cat(paste(
    "  # button for audience to ask questions",
    "  conditionalPanel(",
    '    "(output.is_viewer==false)",',
    '    h2(actionLink("aud_qs", label = "", icon = icon("question-circle"))),',
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

add_polls_ui <- function(file, polls) {
  lapply(polls, add_poll_ui, file)
  cat("  NULL
", file = file, append = TRUE)
}

add_poll_ui <- function(poll, file) {
  cat(paste(
    "  # poll voting buttons",
    "  conditionalPanel(",
    paste0('    "(output.is_viewer==false) && (output.act_object==\'', poll@id, "') && (output.done_", poll@id, '==false)",'),
    paste0('    h3("', poll@question, '"),'),
    paste0(
      '    fluidRow(actionButton(inputId = "', poll@id, "_opt_",
      poll@options,
      '", label = "',
      poll@options,
      '")),',
      collapse = "\n"
    ),
    '    align = "center"',
    "  ),",
    "",
    "  # poll results plot",
    "  conditionalPanel(",
    paste0(
      '    "(output.is_viewer==true) && (output.act_object==\'',
      poll@id,
      '\')",'
    ),
    paste0('    plotOutput("', poll@id, '")'),
    "  ),",
    "",
    "",
    sep = "\n"
  ), file = file, append = TRUE)
}

add_ui_footer <- function(file) {
  cat(paste(
    ")",
    "",
    "",
    sep = "\n"
  ), file = file, append = TRUE)
}

add_server_header <- function(file) {
  key <- app_info$params$key
  cat(paste(
    "server <- function(input, output, session) {",
    "  # set user info, with random name and avatar",
    "  rand_prof <- sample(avatars, 1)",
    "  curr_user <-",
    "    data.frame(",
    "      avatar = rand_prof,",
    "      name = rand_prof,",
    "      id = paste0(",
    '        session$request$REMOTE_ADDR, ":", session$request$REMOTE_PORT',
    "      ),",
    "      stringsAsFactors = FALSE",
    "    )",
    "",
    "  # if it is an audience user, then let it select avatar and name",
    "  observeEvent(getQueryString(), {",
    paste0(
      '    if (!is.null(getQueryString()$viewer) && getQueryString()$viewer == "',
      key,
      '") {'
    ),
    "      return()",
    "    }",
    "",
    "    # if the user previously logged in, then assign previous profile",
    "    if (curr_user$id %in% users()$id) {",
    "      users <- users()",
    "      curr_user <- users[users$id == curr_user$id,, drop = FALSE]",
    "      return()",
    "    }",
    "",
    "    showModal(modalDialog(",
    '      title = "My profile",',
    "      fluidRow(",
    "        column(6, selectInput(",
    '          "profile_avatar", "",',
    "          choices = avatars, selected = rand_prof",
    "        )),",
    '        column(6, htmlOutput("profile_avatar_show"))',
    "      ),",
    '      textInput("profile_name", "Your name", rand_prof),',
    "      footer = fluidRow(",
    '        actionButton("profile_submit", label = "Save"),',
    '        align = "center"',
    "      ),",
    "      easyClose = FALSE,",
    '      size = "s"',
    "    ))",
    "    output$profile_avatar_show <- renderText(paste0(",
    "      '<img src=\"',",
    "      avatars_url, input$profile_avatar, '.png\" height=\"120\" width=\"120\">'",
    "    ))",
    "",
    "    observeEvent(input$profile_submit, {",
    "      user_name <- trimws(input$profile_name)",
    "      if (user_name %in% users()$name) {",
    '        showNotification("User name already taken", type = "error")',
    "        return()",
    "      }",
    "      curr_user$name <- user_name",
    "      curr_user$avatar <- input$profile_avatar",
    "      users(rbind(users(), curr_user))",
    "      removeModal()",
    "    })",
    "  })",
    "",
    "  # is_viewer checks if it is the slides viewer user",
    "  output$is_viewer <- reactive({",
    paste0(
      '    !is.null(getQueryString()$viewer) && getQueryString()$viewer == "',
      key,
      '"'
    ),
    "  })",
    '  outputOptions(output, "is_viewer", suspendWhenHidden = FALSE)',
    "",
    "  # show the selected object",
    "  observeEvent({",
    "    getQueryString()",
    "    input$act_obj",
    "  }, {",
    paste0(
      '    if (!is.null(getQueryString()$viewer) && getQueryString()$viewer == "',
      key,
      '") {'
    ),
    "      act_object(input$act_obj)",
    "    }",
    "  })",
    "  output$act_object <- reactive({",
    "    act_object()",
    "  })",
    '  outputOptions(output, "act_object", suspendWhenHidden = FALSE)',
    "",
    "",
    sep = "\n"
  ), file = file, append = TRUE)
}

add_aud_qs_server <- function(file) {
  cat(paste(
    "  # audience question form (max of 160 chars per question)",
    "  max_aud_q_chars <- 160",
    "  observeEvent(input$aud_qs, {",
    "    showModal(modalDialog(",
    '      title = "Question",',
    "      HTML(paste0(",
    "        '<img src=\"',",
    "        avatars_url,",
    "        curr_user$avatar,",
    "        '.png\" height=\"40\" width=\"40\">'",
    "      )),",
    "      curr_user$name,",
    '      checkboxInput("q_anonymous", "Anonymous"),',
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
    "      user = curr_user$id,",
    '      name = ifelse(input$q_anonymous, "Anonymous", curr_user$name),',
    '      avatar = ifelse(input$q_anonymous, "", curr_user$avatar),',
    # '      time = format(Sys.time(), "%I:%M %p"),',
    '      time = format(Sys.time(), "%H:%M"),',
    "      question = question,",
    "      stringsAsFactors = FALSE",
    "    )",
    "    aud_qs(rbind(aud_qs(), question))",
    "    removeModal()",
    '    showNotification("Question sent", type = "message")',
    "  })",
    "",
    "  # show the questions, nicely printed in the pane",
    "  output$aud_qs_viewer <- renderUI({",
    "    aud_qs <- aud_qs()",
    "    if (is.null(aud_qs) || nrow(aud_qs) == 0) {",
    "      return(wellPanel())",
    "    }",
    # '    wellPanel(style = "overflow-y:scroll; max-height: 300px",',
    "    wellPanel(",
    "      apply(aud_qs, 1, function(x) wellPanel(",
    "        HTML(paste0(",
    "          '<img src=\"',",
    "          avatars_url, x[\"avatar\"], '.png\" ',",
    "          'title=\"', ifelse(x[\"avatar\"] == \"\", \"\", x[\"user\"]),",
    "          '\" height=\"40\" width=\"40\">'",
    "        )),",
    '        h4(x["name"]),',
    "        x[\"time\"],",
    '        x[\"question\"]',
    "      )),",
    "    )",
    "  })",
    "",
    "",
    sep = "\n"
  ), file = file, append = TRUE)
}

add_polls_server <- function(file, polls) {
  invisible(lapply(polls, add_poll_server, file))
}

add_poll_server <- function(poll, file) {
  cat(paste(
    "  # check if the current user has already voted this poll",
    paste0("  output$done_", poll@id, " <- reactive({"),
    paste0("    curr_user$id %in% unlist(", poll@id, "_ans())"),
    "  })",
    paste0('  outputOptions(output, "done_', poll@id, '", suspendWhenHidden = FALSE)'),
    "",
    "",
    sep = "\n", collapse = ""
  ), file = file, append = TRUE)

  cat(paste(
    "  # for each answer, save the voters ids",
    paste0("  observeEvent(input$", poll@id, "_opt_", poll@options, ", {"),
    paste0("    act_ans <- ", poll@id, "_ans()"),
    paste0(
      "    act_ans$opt_",
      poll@options,
      " <- unique(c(act_ans$opt_",
      poll@options,
      ", curr_user$id))"
    ),
    paste0("    ", poll@id, "_ans(act_ans)"),
    "  })",
    "",
    sep = "\n", collapse = ""
  ), file = file, append = TRUE)

  cat(paste(
    "  # create the poll answers plot",
    paste0("  output$", poll@id, " <- renderPlot({"),
    paste0("    act_ans <- ", poll@id, "_ans()"),
    "    act_ans <- data.frame(",
    '      Option = gsub("opt_", "", names(act_ans)),', 
    "      N = unlist(lapply(act_ans, length))",
    "    )",
    "    act_ans$Votes <- 100 * act_ans$N / max(1, sum(act_ans$N))",
    "    ggplot(act_ans) +",
    "      geom_col(aes(x = Option, y = Votes, fill = Option)) +",
    "      geom_text(aes(x = Option, y = Votes, label = N), size = 12) +",
    "      theme(",
    '        legend.position = "none",',
    "        text = element_text(size = 30)",
    "      ) +",
    "      scale_y_continuous(breaks = seq(0, 100, by = 25), limits = c(0, 100))",
    "  })",
    "",
    "",
    sep = "\n"
  ), file = file, append = TRUE)
}

add_server_footer <- function(file) {
  cat(paste(
    "}",
    "",
    "",
    sep = "\n"
  ), file = file, append = TRUE)
}

add_app_footer <- function(file) {
  cat(paste(
    "shinyApp(ui = ui, server = server)",
    "",
    sep = "\n"
  ), file = file, append = TRUE)
}
