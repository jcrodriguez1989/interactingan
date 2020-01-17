AudQs <- setClass(
  "AudQs",
  slots = c(
    enabled = "logical",
    allow_anonymous = "logical"
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
      .self$audience_questions <- AudQs(enabled = FALSE, allow_anonymous = TRUE)
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
