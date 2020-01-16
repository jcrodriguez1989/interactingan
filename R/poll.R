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

Poll <- setClass("Poll",
  slots = c(
    id = "character",
    question = "character",
    options = "character",
    multiple = "logical"
  )
)
