# class and global object that contains all the interactive elements of the
# presentation
Elems <- setRefClass(
  "Elems",
  fields = list(
    polls = "list"
  )
)
elems <- Elems$new()

# class and global object that contains the information related to the app
# server
AppInfo <- setRefClass(
  "AppInfo",
  fields = list(
    params = "list"
  )
)
app_info <- AppInfo$new()
