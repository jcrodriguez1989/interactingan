#' Allow audience questions
#'
#' Allows audience questions. It will return the corresponding `iframe` tag 
#' which will show a questions viewer pane to the current slide.
#'
#' @param width A character with a valid html `width` value for the iframe.
#' @param height A character with a valid html `height` value for the iframe.
#'
#' @return An html `iframe` tag which will show the questions done.
#'
#' @export
#'
audience_questions <- function(width = "100%", height = "500px") {
  elems$audience_questions <- TRUE
  paste0(
    '<iframe width="', width, '" height="', height, '" src="',
    app_info$params$url,
    "?viewer=",
    app_info$params$key,
    '" frameborder="0" scrolling="no"></iframe>'
  )
}
