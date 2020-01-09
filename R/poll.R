#' Add a poll to the slide
#'
#' Adds a poll to the current slide. It will return the corresponding `iframe`
#' tag which will show the poll results.
#'
#' @param question A character -string- representing the poll question.
#' @param options A character vector with the possible poll answers.
#' @param width A character with a valid html `width` value for the iframe.
#' @param height A character with a valid html `height` value for the iframe.
#'
#' @return An html `iframe` tag which will show the poll results.
#'
#' @export
#'
poll <- function(question, options, width = "100%", height = "500px") {
  act_polls <- elems$polls
  curr_id <- max(c(0, as.numeric(unlist(lapply(act_polls, function(act_poll) {
    strsplit(act_poll@id, "_")[[1]][[2]]
  })))))
  new_id <- paste0("poll_", curr_id + 1)
  new_poll <- Poll(
    id = new_id,
    question = question,
    options = as.character(options)
  )
  elems$polls <- append(act_polls, new_poll)
  paste0(
    '<iframe width="', width, '" height="', height, '" src="',
    app_info$params$url,
    "?viewer=TRUE",
    '" frameborder="0" scrolling="no"></iframe>'
  )
}

Poll <- setClass("Poll",
  slots = c(
    id = "character",
    question = "character",
    options = "character"
  )
)
