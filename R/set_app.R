#' @importFrom qrcode qrcode_gen
#' @importFrom rsconnect deployApp
#'
#' @export
#'
set_app <- function(app_name = "presentation", url = NULL, ...) {
  elems$polls <- list()
  res_params <- list(
    app_name = app_name,
    url = url
    # dots = as.list(as.environment(...))
  )

  # qrCodeSpec # if not imported ~> bug in qrcode?
  if (!is.null(url)) {
    res_params$url <- url
    app_info$params <- res_params
    # return(qrcode_gen(url))
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
  res_params$url <- gsub("Application successfully deployed to ", "", conn_url)
  app_info$params <- res_params
  # invisible(qrcode_gen(res_params$url))
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
