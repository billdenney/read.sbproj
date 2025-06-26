#' Read a Matlab .sbproj file
#'
#' @param file The filename to read (usually ending in .sbproj)
#' @param ... Passed to `read_sbproj_view()`
#' @returns An "sbproj" S3 object
#' @export
read_sbproj <- function(file, ...) {
  zip_filenames <- zip::zip_list(file)
  xml_file <- get_file_by_extension(zip_filenames$filename, "xml")
  proj_file <- get_file_by_extension(zip_filenames$filename, "proj")
  mat_files <- get_file_by_extension(zip_filenames$filename, "mat")
  view_file <- get_file_by_extension(zip_filenames$filename, "view")
  unknown_file <- setdiff(zip_filenames$filename, c(xml_file, proj_file, mat_files, view_file))
  if (length(unknown_file) > 0) {
    warning("The following unknown files were found within ")
  }
  assert_one_file(xml_file, filetype = "xml", zipfile = file)
  assert_one_file(proj_file, filetype = "proj", zipfile = file)
  assert_one_file(view_file, filetype = "view", zipfile = file)
  unzip_dir <- file.path(tempdir(), "read_sbproj_tmp")
  on.exit(unlink(unzip_dir, recursive = TRUE))
  utils::unzip(zipfile = file, files = c(view_file, mat_files), overwrite = TRUE, junkpaths = TRUE, exdir = unzip_dir)
  ret <-
    list(
      mat =
        lapply(
          X =
            stats::setNames(
              file.path(unzip_dir, mat_files),
              nm = basename(mat_files)
            ),
          FUN = R.matlab::readMat
        ),
      view = read_sbproj_view(file = file.path(unzip_dir, view_file), ...)
    )
  class(ret) <- "sbproj"
  ret
}

get_file_by_extension <- function(filenames, extension) {
  filenames[endsWith(tolower(filenames), paste0(".", extension))]
}

assert_one_file <- function(filelist, filetype, zipfile) {
  if (length(filelist) != 1) {
    stop("Exactly one ", filetype, " file was expected in ", zipfile, ". ", length(filelist), " files found")
  }
  filelist
}

#' Read the .view file within an .sbproj file
#'
#' @param file The filename to read
#' @param ... Passed to `as_sbproj_view()`
#' @returns An "sbproj_view" object
#' @export
read_sbproj_view <- function(file, ...) {
  xml_parsed <- xml2::read_xml(file)
  as_sbproj_view(xml_parsed, ...)
}
