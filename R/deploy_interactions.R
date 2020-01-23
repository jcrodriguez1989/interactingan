#' Deploy all interactions to the server
#'
#' Deploys all created interaction objects to the, `interactingan` Shiny server,
#' previously configured by the `set_app` function.
#'
#' @param theme A character -string- with the name of the theme to use for the
#'   Shiny app. Must be "default" or any valid name for
#'   `shinythemes::shinytheme(theme)`.
#' @param hide_selector A logical indicating if the object selector should be
#'   shown on the top of each object.
#'
#' @importFrom utils capture.output
#'
#' @export
#'
deploy_interactions <- function(theme = "spacelab", hide_selector = TRUE) {
  # create a random folder for the app to deploy
  app_dir <- paste0(app_info$params$out_dir, "/app")
  out_file <- paste0(app_dir, "/app.R")

  dir.create(app_dir, showWarnings = FALSE)
  unlink(out_file)

  create_shiny_file(out_file, theme, hide_selector)

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
      stop(paste0(
        "Could not set app. Make sure `rsconnect` is well configured:\n",
        "https://docs.rstudio.com/shinyapps.io/getting-started.html#configure-rsconnect"
      ))
    }
  }

  invisible(out_file)
}

#' @importFrom methods is
create_shiny_file <- function(out_file, theme, hide_selector) {
  # Shiny app header (includes)
  add_app_header(out_file)

  objects <- elems$objects
  polls <- objects[unlist(lapply(objects, is, "Poll"))]
  wordclouds <- objects[unlist(lapply(objects, is, "Wordcloud"))]
  ratings <- objects[unlist(lapply(objects, is, "Rating"))]

  # Global vars
  add_aud_qs_vars(out_file, elems$audience_questions)
  add_polls_vars(out_file, polls)
  add_wordclouds_vars(out_file, wordclouds)
  add_ratings_vars(out_file, ratings)

  # UI
  add_ui_header(out_file, theme)
  add_obj_selector_ui(out_file, elems, hide_selector)
  add_aud_qs_ui(out_file, elems$audience_questions)
  add_polls_ui(out_file, polls)
  add_wordclouds_ui(out_file, wordclouds)
  add_ratings_ui(out_file, ratings)
  add_ui_footer(out_file)

  # Server
  add_server_header(out_file)
  add_aud_qs_server(out_file, elems$audience_questions)
  add_polls_server(out_file, polls)
  add_wordclouds_server(out_file, wordclouds)
  add_ratings_server(out_file, ratings)
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
    "# connected users",
    "users <- reactiveVal()",
    "# currently selected object by the viewer user",
    'act_object <- reactiveVal("none")',
    "",
    "",
    sep = "\n"
  ), file = file, append = TRUE)
}

add_ui_header <- function(file, theme) {
  cat(paste(
    "ui <- fluidPage(",
    paste0('  title = "interactingan: ', app_info$params$app_name, '",'),
    ifelse(
      theme != "default",
      paste0('  theme = shinythemes::shinytheme("', theme, '"),'),
      ""
    ),
    "",
    "",
    sep = "\n"
  ), file = file, append = TRUE)
}

add_obj_selector_ui <- function(out_file, elems, hide_selector) {
  objects <- elems$objects

  objs <- c(
    '      "Select object" = "empty"',
    lapply(seq_along(objects), function(i) {
      paste0('      "', objects[[i]]@question, '" = "', objects[[i]]@id, '"')
    })
  )

  if (elems$audience_questions@enabled) {
    objs <- c(
      objs, paste0('      "Audience Questions" = "aud_qs"')
    )
  }

  cat(paste(
    "  # if it is the viewer user, then show the interactive object selector",
    "  conditionalPanel(",
    paste0('    "(output.is_viewer==true) && ', tolower(!hide_selector), '",'),
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
    "    reactiveVal(data.frame(",
    "      avatar = rand_prof,",
    "      name = rand_prof,",
    "      id = paste0(",
    '        session$request$REMOTE_ADDR, ":", session$request$REMOTE_PORT',
    "      ),",
    "      stringsAsFactors = FALSE",
    "    ))",
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
    "    if (curr_user()$id %in% users()$id) {",
    "      users <- users()",
    "      curr_user(users[users$id == curr_user()$id,, drop = FALSE])",
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
    "      user <- curr_user()",
    "      user$name <- user_name",
    "      user$avatar <- input$profile_avatar",
    "      users(rbind(users(), user))",
    "      curr_user(user)",
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
    "  # allow object selector through url",
    "  observeEvent(getQueryString(), {",
    "    if (",
    "      !is.null(getQueryString()$viewer) &&",
    paste0('      getQueryString()$viewer == "', key, '" &&'),
    "      !is.null(getQueryString()$object)",
    "    ) {",
    '      updateSelectInput(session, "act_obj", selected = getQueryString()$object)',
    "    }",
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
