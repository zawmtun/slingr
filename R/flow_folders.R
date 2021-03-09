#' Workflow function: Create project folders
#'
#' Create project folders with a minimal structure. See Details.
#'
#' In a typical new workflow setup, create a RStudio project folder first. Then,
#' run this function in R console using the default parameter. \cr
#' \cr
#' Minimal folder structure: \cr
#' code - R script files (.R) \cr
#' data_raw - Raw data from external source \cr
#' data_derived - Data files generated in the analysis process \cr
#' figs - Figures and plots \cr
#' docs - Report files (.Rmd and their derivatives or .docx) \cr
#'
#' @param project_path Project folder path.
#'
#' @return
#' @export
#'
#' @examples
#' temp_folder <- tempdir()
#' flow_folders(temp_folder)
#' list.dirs(temp_folder)
flow_folders <- function(project_path = ".") {
  folders <- c(
    "code",
    "data_raw",
    "data_derived",
    "figs",
    "docs"
  )

  folders_text <- paste("-", folders)
  exist <- dir.exists(file.path(project_path, folders))

  # Create folders
  invisible(
    lapply(
      folders[!exist],
      function(folder, p) dir.create(file.path(p, folder)),
      p = project_path
    )
  )

  # Feedback message
  if (all(exist)) {
    cat(
      "Folders already exists:\n",
      paste(folders_text, collapse = "\n"),
      sep = ""
    )
  } else if (!all(exist) & any(exist)) {
    cat(
      "These folders already exist:\n",
      paste(folders_text[exist], collapse = "\n"),
      "\nThe following folders have been created:\n",
      paste(folders_text[!exist], collapse = "\n"),
      sep = ""
    )
  } else if (!all(exist) & !any(exist)) {
    cat(
      "The following folders have been created:\n",
      paste(folders_text, collapse = "\n"),
      sep = ""
    )
  }
}
