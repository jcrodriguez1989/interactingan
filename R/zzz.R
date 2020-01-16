# class and global object that contains all the interactive elements of the
# presentation
#' @importFrom methods new
Elems <- setRefClass(
  "Elems",
  fields = list(
    objects = "list",
    audience_questions = "logical"
  ),
  methods = list(
    initialize = function() {
      .self$audience_questions <- FALSE
      .self
    })
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
