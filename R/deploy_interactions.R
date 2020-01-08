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
    # output <- capture.output({
    #   conn_success <- rsconnect::deployApp(
    #     appDir = app_dir,
    #     appName = app_info$params$app_name,
    #     launch.browser = FALSE,
    #     forceUpdate = TRUE,
    #     app_info$params$dots
    #   )
    # })

    if (!conn_success ||
      !any(grepl("Application successfully deployed to ", output))) {
      stop("Could not set app. Make sure `rsconnect` is well configured.")
    }
  }

  invisible(out_file)
}

create_shiny_file <- function(out_file) {
  add_app_header(out_file)

  polls <- elems$polls
  add_polls_vars(out_file, polls)

  add_ui_header(out_file)
  add_obj_selector_ui(out_file, polls)
  add_polls_ui(out_file, polls)
  add_ui_footer(out_file)

  add_server_header(out_file)
  add_polls_server(out_file, polls)
  add_server_footer(out_file)

  add_app_footer(out_file)

  invisible(out_file)
}

add_app_header <- function(file) {
  cat(paste(
    'library("ggplot2")',
    'library("shiny")',
    "",
    'act_object <- reactiveVal("none")',
    "used_objects <- reactiveVal()",
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
    paste0(poll@id, "_ans", " <- reactiveVal(list("),
    paste0(
      "  opt_",
      poll@options,
      c(rep(" = 0,", length(poll@options) - 1), " = 0"),
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
    "",
    sep = "\n"
  ), file = file, append = TRUE)
}

add_obj_selector_ui <- function(out_file, polls) {
  cat(paste(
    "  conditionalPanel(",
    '    "(output.is_viewer==true)",',
    '    selectInput(inputId = "act_obj", label = "", choices = c(',
    paste(
      lapply(seq_along(polls), function(i) {
        paste0('      "', polls[[i]]@question, '" = "', polls[[i]]@id, '"')
      }),
      collapse = ",\n"
    ),
    "    ))",
    "  ),",
    "",
    "",
    sep = "\n"
  ), file = out_file, append = TRUE)
}

add_polls_ui <- function(file, polls) {
  lapply(polls, add_poll_ui, file)
  cat("  NULL
", file = file, append = TRUE)
}

add_poll_ui <- function(poll, file) {
  cat(paste(
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
  cat(paste(
    "server <- function(input, output, session) {",
    "  curr_user <-",
    '    paste0(session$request$REMOTE_ADDR, ":", session$request$REMOTE_PORT)',
    "",
    "  output$is_viewer <- reactive({",
    '    !is.null(getQueryString()$viewer) && getQueryString()$viewer == "TRUE"',
    "  })",
    '  outputOptions(output, "is_viewer", suspendWhenHidden = FALSE)',
    "",
    "  observeEvent({",
    "    getQueryString()",
    "    input$act_obj",
    "  }, {",
    '    if (!is.null(getQueryString()$viewer) && getQueryString()$viewer == "TRUE") {',
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

add_polls_server <- function(file, polls) {
  invisible(lapply(polls, add_poll_server, file))
}

add_poll_server <- function(poll, file) {
  cat(paste(
    paste0("  output$done_", poll@id, " <- reactive({"),
    paste0("    curr_user %in% used_objects()$", poll@id),
    "  })",
    paste0('  outputOptions(output, "done_', poll@id, '", suspendWhenHidden = FALSE)'),
    "",
    "",
    sep = "\n", collapse = ""
  ), file = file, append = TRUE)

  cat(paste(
    paste0("  observeEvent(input$", poll@id, "_opt_", poll@options, ", {"),
    paste0("    act_ans <- ", poll@id, "_ans()"),
    paste0("    act_ans$opt_", poll@options, " <- act_ans$opt_", poll@options, " + 1"),
    paste0("    ", poll@id, "_ans(act_ans)"),
    "    new_used <- used_objects()",
    paste0("    new_used$", poll@id, " <- c(new_used$", poll@id, ", curr_user)"),
    "    used_objects(new_used)",
    "  })",
    "",
    sep = "\n", collapse = ""
  ), file = file, append = TRUE)

  cat(paste(
    paste0("  output$", poll@id, " <- renderPlot({"),
    paste0("    act_ans <- ", poll@id, "_ans()"),
    "    act_ans <- data.frame(",
    '      Option = gsub("opt_", "", names(act_ans)), N = unlist(act_ans)',
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
