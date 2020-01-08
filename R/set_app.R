#' Set up the interactions server
#'
#' `interactingan` package creates a Shiny app to allow the interactions, this
#' app must be hosted on any Shiny server, by default it will try to use
#' [shinyapps.io](shinyapps.io).
#' Calling this function will reset all existing interaction objects.
#'
#' @param app_name A character -string- with the name of the Shiny app to
#'   create. Must be unique in your Shiny server, or it will overwrite it.
#' @param url A character -string- (optional) to provide a valid existing
#'   `interactingan` server instance that was previously configured.
#' @param ... Additional parameters passed to `rsconnect::deployApp` function.
#'
#' @return A QR code image that will point to the server url.
#'
#' @import qrcode
#' @importFrom rsconnect deployApp
#'
#' @export
#'
set_app <- function(app_name = "presentation", url = NULL, ...) {
  elems$polls <- list()
  res_params <- list(
    app_name = app_name,
    url = url,
    dots = list(...)
  )

  if (!is.null(url)) {
    res_params$url <- url
    res_params$deployed <- FALSE
    app_info$params <- res_params
    if (require("qrcode")) {
      return(qrcode_gen(url))
    }
    return(invisible(url))
  }

  output <- capture.output({
    conn_success <- rsconnect::deployApp(
      appDir = empty_app(),
      appName = app_name,
      launch.browser = FALSE,
      forceUpdate = TRUE,
      ...
    )
  })

  if (!conn_success ||
    !any(grepl("Application successfully deployed to ", output))) {
    stop("Could not set app. Make sure `rsconnect` is well configured.")
  }

  conn_url <- output[grepl("Application successfully deployed to ", output)]
  res_params$deployed <- TRUE
  res_params$url <- gsub("Application successfully deployed to ", "", conn_url)
  app_info$params <- res_params
  if (require("qrcode")) {
    return(qrcode_gen(res_params$url))
  }
  invisible(res_params$url)
}

# creates an empty Shiny app (to start the server)
empty_app <- function() {
  # create a random folder for the empty app to deploy
  app_dir <- paste0(tempdir(), "/empty_app")
  out_file <- paste0(app_dir, "/app.R")
  dir.create(app_dir, showWarnings = FALSE)

  cat(paste(
    'library("shiny")',
    "",
    "ui <- fluidPage()",
    "server <- function(input, output, session) {}",
    "",
    "shinyApp(ui = ui, server = server)",
    "",
    "",
    sep = "\n"
  ), file = out_file, append = FALSE)
  app_dir
}
