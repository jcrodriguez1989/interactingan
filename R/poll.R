#' @export
#'
poll <- function(question, options, width = "80%", height = "80%") {
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
    "?viewer=TRUE&object=", new_id,
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
