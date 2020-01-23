#' Add a poll to the slide
#'
#' Adds a poll to the current slide. It will return the corresponding `iframe`
#' tag which will show the poll results.
#' This function must be called once per poll.
#'
#' @param question A character -string- representing the poll question.
#' @param options A character vector with the possible poll answers.
#' @param multiple_opts A logical indicating if multiple options can be
#'   selected.
#' @param width A character with a valid html `width` value for the iframe.
#' @param height A character with a valid html `height` value for the iframe.
#'
#' @return An html `iframe` tag which will show the poll results.
#'
#' @export
#'
poll <- function(question, options, multiple_opts = FALSE, width = "100%",
                 height = "500px") {
  act_objs <- elems$objects
  curr_id <- max(c(0, as.numeric(unlist(lapply(act_objs, function(act_obj) {
    if (is(act_obj, "Poll")) {
      strsplit(act_obj@id, "_")[[1]][[2]]
    }
  })))))
  new_id <- paste0("poll_", curr_id + 1)
  new_poll <- Poll(
    id = new_id,
    question = question,
    options = as.character(options),
    multiple = multiple_opts
  )
  elems$objects <- append(act_objs, new_poll)
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
    '_btn").style.display = "none";\'>Load poll</button></p><iframe id="',
    new_id,
    '_frame" width="100%" height="500px" frameborder="0" scrolling="no"></iframe>'
  )
}

Poll <- setClass(
  "Poll",
  slots = c(
    id = "character",
    question = "character",
    options = "character",
    multiple = "logical"
  )
)

#### Functions to create the `interactingan` Shiny server app

add_polls_vars <- function(file, polls) {
  invisible(lapply(polls, add_poll_vars, file))
}

add_poll_vars <- function(poll, file) {
  cat(paste(
    "# users that voted for each answer",
    paste0(poll@id, "_ans", " <- reactiveVal(list("),
    paste0(
      '  "',
      poll@options,
      c(rep('" = NULL,', length(poll@options) - 1), '" = NULL'),
      collapse = "\n"
    ),
    "))",
    "",
    "",
    sep = "\n"
  ), file = file, append = TRUE)
}

add_polls_ui <- function(file, polls) {
  lapply(polls, add_poll_ui, file)
}

add_poll_ui <- function(poll, file) {
  cat(paste(
    "  # poll voting buttons",
    "  conditionalPanel(",
    paste0(
      '    "(output.is_viewer==false) && (output.act_object==\'',
      poll@id,
      "') && (output.done_",
      poll@id,
      '==false)",'
    ),
    paste0('    h3("', poll@question, '"),'),
    "    selectInput(",
    paste0('      inputId = "', poll@id, '_sel",'),
    '      label = "",',
    paste0(
      '      choices = c("',
      paste0(poll@options, collapse = '", "'),
      '"),'
    ),
    paste0("      multiple = ", poll@multiple, ","),
    "      selectize = FALSE,",
    paste0("      size = ", length(poll@options)),
    "    ),",
    paste0('    actionButton("', poll@id, '_send", label = "Send"),'),
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

add_polls_server <- function(file, polls) {
  invisible(lapply(polls, add_poll_server, file))
}

add_poll_server <- function(poll, file) {
  cat(paste(
    "  # check if the current user has already voted this poll",
    paste0("  output$done_", poll@id, " <- reactive({"),
    paste0("    curr_user()$id %in% unlist(", poll@id, "_ans())"),
    "  })",
    paste0(
      '  outputOptions(output, "done_',
      poll@id,
      '", suspendWhenHidden = FALSE)'
    ),
    "",
    "",
    sep = "\n", collapse = ""
  ), file = file, append = TRUE)
  
  cat(paste(
    "  # for each answer, save the voters ids",
    paste0("  observeEvent(input$", poll@id, "_send, {"),
    paste0("    act_sels <- input$", poll@id, "_sel"),
    paste0("    act_ans <- ", poll@id, "_ans()"),
    "    for (act_sel in act_sels) {",
    "      act_ans[[act_sel]] <- unique(c(act_ans[[act_sel]], curr_user()$id))",
    "    }",
    paste0("    ", poll@id, "_ans(act_ans)"),
    "  })",
    "",
    sep = "\n", collapse = ""
  ), file = file, append = TRUE)
  
  cat(paste(
    "  # create the poll answers plot",
    paste0("  output$", poll@id, " <- renderPlot({"),
    paste0("    act_ans <- ", poll@id, "_ans()"),
    "    opts <- names(act_ans)",
    "    act_ans <- data.frame(",
    "      Option = factor(opts, levels = opts),",
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
