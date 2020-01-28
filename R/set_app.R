#' Set up the interactions server
#'
#' `interactingan` package creates a Shiny app to allow the interactions, this
#' app must be hosted on any Shiny server, by default it will try to use
#' [shinyapps.io](shinyapps.io).
#' Calling this function will reset all existing interaction objects.
#' At the moment, just one interactive object per slide is allowed.
#'
#' To enter the `interactingan` server admin panel, in a browser, enter to the
#' URL: "SERVER_URL/?viewer=`key`&admin", for example
#' https://my_username.shinyapps.io/presentation/?viewer=IACC&admin
#'
#' @param app_name A character -string- with the name of the Shiny app to
#'   create. Must be unique in your Shiny server, or it will overwrite it.
#' @param key A character -string- with a personal key to access interactions
#'   viewer.
#' @param url A character -string- (optional) to provide a valid existing
#'   `interactingan` server instance that was previously configured. For
#'   instance, if the `interactingan` server app is going to be deployed in an
#'   own Shiny server.
#' @param out_dir A character -string- (optional) indicating the path in which
#'   to save the `interactingan` server app file. It is useful if the app is
#'   going to be deployed to an own Shiny server.
#' @param ... Additional parameters passed to `rsconnect::deployApp` function.
#'
#' @return A QR code image that will point to the server url.
#'
#' @importFrom rsconnect deployApp
#'
#' @export
#'
set_app <- function(app_name = "presentation", key = "IACC", url = NULL,
                    out_dir = tempdir(), ...) {
  elems$initialize()
  res_params <- list(
    app_name = app_name,
    key = key,
    url = url,
    out_dir = out_dir,
    dots = list(...)
  )

  if (!is.null(url)) {
    res_params$url <- url
    res_params$deployed <- FALSE
    app_info$params <- res_params
    return(qr_gen(url))
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
    stop(paste0(
      "Could not set app. Make sure `rsconnect` is well configured:\n",
      "https://docs.rstudio.com/shinyapps.io/getting-started.html#configure-rsconnect"
    ))
  }

  conn_url <- output[grepl("Application successfully deployed to ", output)]
  res_params$deployed <- TRUE
  res_params$url <- gsub("Application successfully deployed to ", "", conn_url)
  app_info$params <- res_params
  qr_gen(res_params$url)
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

#' @import ggplot2
#' @importFrom qrcode qrcode_gen
#'
qr_gen <- function(text) {
  if (!require("qrcode")) {
    return(invisible(text))
  }

  qr_matrix <- qrcode_gen(text, dataOutput = TRUE, plotQRcode = FALSE)
  qr_matrix <- as.data.frame.table(qr_matrix)
  qr_matrix[1:2] <- lapply(qr_matrix[1:2], as.numeric)
  qr_matrix <- qr_matrix[qr_matrix$Freq == 1, ]

  Var1 <- Var2 <- NULL # to avoid R CMD check warnings
  ggp <- ggplot(qr_matrix, aes(Var1, Var2))
  ggp + geom_tile() + theme_void() + theme(aspect.ratio = 1)
}
