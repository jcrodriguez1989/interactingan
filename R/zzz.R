AudQs <- setClass(
  "AudQs",
  slots = c(
    enabled = "logical",
    allow_anonymous = "logical",
    max_chars = "numeric"
  )
)

# class and global object that contains all the interactive elements of the
# presentation
#' @importFrom methods new
Elems <- setRefClass(
  "Elems",
  fields = list(
    objects = "list",
    audience_questions = "AudQs"
  ),
  methods = list(
    initialize = function() {
      .self$objects <- list()
      .self$audience_questions <- AudQs(
        enabled = FALSE, allow_anonymous = TRUE, max_chars = 160
      )
      .self
    }
  )
)
elems <- Elems$new()

# class and global object that contains the information related to the app
# server
#' @importFrom methods new
AppInfo <- setRefClass(
  "AppInfo",
  fields = list(
    params = "list"
  )
)
app_info <- AppInfo$new()
