#' checks to see if a .gitignore needs to be create or edited based on the template file
#'
#' @param template  the template .gitignore file.  Default is part of the package data.
#' @returns nothing, called for its side effect.  Gives messages out if  file is created or edited
#' @export

gitignore_init <- function(template = system.file("extdata/gitignore_template", package = "cctu")) {
  # check for .gitignore file
  if (!file.exists(".gitignore")) {
    file.copy(
      from = template,
      to = ".gitignore"
    )
    message(".gitignore file created")
  } else {
    patterns <- readLines(template)
    file <- readLines(".gitignore")
    file_length <- length(file)
    for (pattern in patterns) {
      present <- grepl(pattern, file, fixed = TRUE) |> any()
      if (!present) file <- c(file, pattern)
    }
    if (file_length < length(file)) {
      writeLines(file, ".gitignore")
      message(".gitignore edited")
    }
  }
}

#' Checks for problem files to avoid saving in github
#'
#' @param folder The folder to run checks within. Needs to be a git repository. Defaults to current working directory
#' @param extensions A character vector giving the file extensions to search for
#'
#' @returns invisibly returns a list of the un-tracked and tracked problem files. Will print on terminal a report as the main objective.
#' @export
#'

gitcheck <- function(folder = ".",
                     extensions = c("csv","xls", "xlsx", "png", "eps", "jpg", "pdf", "doc", "docx", "zip")) {
  wd <- setwd(folder)
  on.exit( setwd(wd))
  untracked <- system( "git ls-files --others --exclude-standard", intern = TRUE)
  tracked <- system( "git ls-files --exclude-standard", intern = TRUE)

  untrack_alert <- lapply(extensions, \(x){
    grep(paste0(".*\\.", x, "$"), untracked, value = TRUE)
  }) |> unlist()
  track_alert <- lapply(extensions, \(x){
    grep(paste0(".*\\.", x, "$"), tracked, value = TRUE)
  }) |> unlist()
  if (length(untrack_alert)) {
    message(
      "These files may get tracked accidentally but are not yet a problem\n\n",
      paste(untrack_alert, collapse = "\n")
    )
  }
  if (length(track_alert)) {
    message(
      "These files are being tracked.\nConsider moving and taking further steps.\nStop future tracking and remove from git history\n\n",
      paste(track_alert, collapse = "\n")
    )
  }
  if (length(untrack_alert) + length(track_alert) == 0) {
    message("No problem files found\n")
  }
  invisible(list(untracked = untrack_alert, tracked = track_alert))
}


# to test needs a temporary git repo... add and comit some files, run the gitinit on it...

