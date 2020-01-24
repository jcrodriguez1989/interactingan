#' Add a rating to the slide
#'
#' Adds a rating to the current slide. It will return the corresponding `iframe`
#' tag which will show the rating results.
#' This function must be called once per rating.
#'
#' @param question A character -string- representing the rating question.
#' @param max_value A numeric indicating the max value of the rate. The minimum
#'   value will be 2.
#' @param width A character with a valid html `width` value for the iframe.
#' @param height A character with a valid html `height` value for the iframe.
#'
#' @return An html `iframe` tag which will show the rating results.
#'
#' @export
#'
rating <- function(question, max_value = 5, width = "100%", height = "500px") {
  max_value <- as.integer(max(2, max_value))
  act_objs <- elems$objects
  curr_id <- max(c(0, as.numeric(unlist(lapply(act_objs, function(act_obj) {
    if (is(act_obj, "Rating")) {
      strsplit(act_obj@id, "_")[[1]][[2]]
    }
  })))))
  new_id <- paste0("rating_", curr_id + 1)
  new_rating <- Rating(
    id = new_id,
    question = question,
    max_value = max_value
  )
  elems$objects <- append(act_objs, new_rating)
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
    '_btn").style.display = "none";\'>Load rating</button></p><iframe id="',
    new_id,
    '_frame" width="100%" height="500px" frameborder="0" scrolling="no"></iframe>'
  )
}

Rating <- setClass(
  "Rating",
  slots = c(
    id = "character",
    question = "character",
    max_value = "integer"
  )
)

#### Functions to create the `interactingan` Shiny server app

add_ratings_vars <- function(file, ratings) {
  if (length(ratings) == 0) {
    return()
  }

  cat(paste(
    "# rating that each user gave",
    paste0(
      lapply(ratings, function(x) x@id), "_ans", " <- reactiveVal(list())"
    ),
    "",
    "",
    sep = "\n"
  ), file = file, append = TRUE)
}

add_ratings_ui <- function(file, ratings) {
  lapply(ratings, add_rating_ui, file)
}

add_rating_ui <- function(rating, file) {
  cat(paste(
    "  # rating slider",
    "  conditionalPanel(",
    paste0(
      '    "(output.is_viewer==false) && (output.act_object==\'',
      rating@id,
      "') && (output.done_",
      rating@id,
      '==false)",'
    ),
    paste0('    h3("', rating@question, '"),'),
    "    sliderInput(",
    paste0('      inputId = "', rating@id, '_sel",'),
    '      label = "",',
    "      step = 1,",
    "      min = 1,",
    paste0("      max = ", rating@max_value, ","),
    paste0("      value = ", as.integer((rating@max_value + 1) / 2)),
    "    ),",
    paste0('    actionButton("', rating@id, '_send", label = "Send"),'),
    '    align = "center"',
    "  ),",
    "",
    "  # rating results plot",
    "  conditionalPanel(",
    paste0(
      '    "((output.is_viewer==true) || (output.done_',
      rating@id,
      "==true)) && (output.act_object=='",
      rating@id,
      '\')",'
    ),
    paste0('    plotOutput("', rating@id, '")'),
    "  ),",
    "",
    "",
    sep = "\n"
  ), file = file, append = TRUE)
}

add_ratings_server <- function(file, ratings) {
  invisible(lapply(ratings, add_rating_server, file))
}

add_rating_server <- function(rating, file) {
  cat(paste(
    "  # check if the current user has submited to this rating",
    paste0("  output$done_", rating@id, " <- reactive({"),
    paste0("    curr_user()$id %in% names(", rating@id, "_ans())"),
    "  })",
    paste0(
      '  outputOptions(output, "done_',
      rating@id,
      '", suspendWhenHidden = FALSE)'
    ),
    "",
    "",
    sep = "\n", collapse = ""
  ), file = file, append = TRUE)

  cat(paste(
    "  # for each answer, save the voters ids and rating",
    paste0("  observeEvent(input$", rating@id, "_send, {"),
    paste0("    rating <- input$", rating@id, "_sel"),
    paste0("    act_ans <- ", rating@id, "_ans()"),
    "    act_ans[[curr_user()$id]] <- rating",
    paste0("    ", rating@id, "_ans(act_ans)"),
    "  })",
    "",
    sep = "\n", collapse = ""
  ), file = file, append = TRUE)

  cat(paste(
    "  # create the rating answers plot",
    paste0("  output$", rating@id, " <- renderPlot({"),
    paste0("    act_ans <- unlist(", rating@id, "_ans())"),
    paste0("    opts <- as.character(seq_len(", rating@max_value, "))"),
    "    ratings <- table(act_ans)",
    "    ratings[opts[!opts %in% names(ratings)]] <- 0",
    "    ratings <- ratings[order(as.numeric(names(ratings)))]",
    "    score <- 0",
    "    if (!is.null(act_ans)) {",
    "      score <- round(mean(act_ans), 2)",
    "    }",
    "    act_ans <- data.frame(",
    "      Rating = factor(opts, levels = opts),",
    "      N = as.vector(ratings)",
    "    )",
    "    act_ans$Votes <- 100 * act_ans$N / max(1, sum(act_ans$N))",
    "    ggplot(act_ans) +",
    "      geom_col(aes(x = Rating, y = Votes, fill = Rating)) +",
    "      geom_text(aes(x = Rating, y = Votes, label = N), size = 12) +",
    '      ggtitle(paste0("Score: ", score)) +',
    "      theme(",
    '        legend.position = "none",',
    "        plot.title = element_text(hjust = 0.5),",
    "        axis.title.x = element_blank(),",
    "        axis.title.y = element_blank(),",
    "        text = element_text(size = 30)",
    "      ) +",
    "      scale_y_continuous(breaks = seq(0, 100, by = 25), limits = c(0, 100))",
    "  })",
    "",
    "",
    sep = "\n"
  ), file = file, append = TRUE)
}
