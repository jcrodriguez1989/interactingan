#' Add a wordcloud to the slide
#'
#' Adds a wordcloud to the current slide. It will return the corresponding
#' `iframe` tag which will show the wordcloud results.
#' This function must be called once per wordcloud.
#'
#' @param question A character -string- representing the wordcloud question.
#' @param max_words A numeric indicating max words allowed per user.
#' @param width A character with a valid html `width` value for the iframe.
#' @param height A character with a valid html `height` value for the iframe.
#' @param ... Additional parameters to be passed to wordcloud::wordcloud
#'   function.
#'
#' @return An html `iframe` tag which will show the wordcloud results.
#'
#' @export
#'
wordcloud <- function(question, max_words = 2,
                      width = "100%", height = "500px", ...) {
  act_objs <- elems$objects
  curr_id <- max(c(0, as.numeric(unlist(lapply(act_objs, function(act_obj) {
    if (is(act_obj, "Wordcloud")) {
      strsplit(act_obj@id, "_")[[1]][[2]]
    }
  })))))
  new_id <- paste0("wordcloud_", curr_id + 1)
  new_wordcloud <- Wordcloud(
    id = new_id,
    question = question,
    max_words = max(1, max_words),
    dots = list(...)
  )
  elems$objects <- append(act_objs, new_wordcloud)
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
    '_btn").style.display = "none";\'>Load wordcloud</button></p><iframe id="',
    new_id,
    '_frame" width="100%" height="500px" frameborder="0" scrolling="no"></iframe>'
  )
}

Wordcloud <- setClass("Wordcloud",
  slots = c(
    id = "character",
    question = "character",
    max_words = "numeric",
    dots = "list"
  )
)
