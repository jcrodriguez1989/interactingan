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
#' @param width A character with a valid html `width` value for the iframe.
#' @param height A character with a valid html `height` value for the iframe.
#'
#' @return An html `iframe` tag which will show the questions done.
#'
#' @export
#'
audience_questions <- function(allow_anonymous = TRUE, max_chars = 160,
                               width = "100%", height = "500px") {
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
    '_frame" width="100%" height="500px" frameborder="0" scrolling="no"></iframe>'
  )
}
