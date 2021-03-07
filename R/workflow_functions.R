#' Create project folders
#'
#' This function creates project folders with a minimal structure.

create_folders <- function(project_path = ".") {
  folders <- c(
    "code",
    "data_raw",
    "data_derived",
    "figs",
    "docs"
  )

  exist <- sapply(folders, dir.exists)
  exist_folders <- names(exist)[exist]
  not_exist_folders <- names(exist)[!exist]

  exist_folders_text <- paste("- ", exist_folders)
  not_exist_folders_text <- paste("- ", not_exist_folders)

  if (all(exist)) {
    cat(
      "Folders already exists:\n",
      paste(exist_folders_text, collapse = "\n"),
      sep = ""
    )
  } else if (any(exist)) {
    invisible(
      lapply(
        not_exist_folders,
        function(folder, p) dir.create(path = file.path(p, folder)),
        p = project_path
      )
    )

    cat(
      "These folders already exist:\n",
      paste(exist_folders_text, collapse = "\n"),
      "\nThe following folders have been created:\n",
      paste(not_exist_folders_text, collapse = "\n"),
      sep = ""
    )
  } else if (!all(exist)) {
    invisible(
      lapply(
        not_exist_folders,
        function(folder, p) dir.create(path = file.path(p, folder)),
        p = project_path
      )
    )

    cat(
      "The following folders have been created:\n",
      paste(not_exist_folders_text, collapse = "\n"),
      sep = ""
    )
  }
}
