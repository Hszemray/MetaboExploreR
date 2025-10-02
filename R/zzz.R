#Ignore column names used in dplyr and tibble pipes

#' @importFrom utils globalVariables
utils::globalVariables(
  c(
    ".",
    ".data",
    "AcquiredTime",
    "Area",
    "failed_samples",
    "FileName",
    "FullPeptideName",
    "MoleculeName",
    "molecule_name",
    "Note",
    "PC1",
    "PC2",
    "Precursor",
    "Precursor Name",
    "Name",
    "SIL",
    "SIL_name",
    "V1",
    "V2",
    "batch",
    "concentration_factor",
    "corrected_mean",
    "dataBatch",
    "dataSource",
    "facet_label",
    "invalid_wiff_files",
    "lipid",
    "lipid_class",
    "matches",
    "metabolite_code",
    "name",
    "original_mean",
    "p1",
    "p2",
    "p3",
    "plateID",
    "precursor_name",
    "sample.flag",
    "sample_data_source",
    "sample_ID",
    "sample_matrix",
    "sample_name",
    "sample_plate_id",
    "sample_plate_order",
    "sample_run_index",
    "sample_timestamp",
    "sample_type",
    "sample_type_factor",
    "sample_type_factor_rev",
    "source_prefix",
    "template_version",
    "value"
  )
)

if (require("MetaboExploreR", quietly = TRUE)) {
  .onAttach <- function(libname, pkgname) {
    user <- NULL

    if (requireNamespace("rstudioapi", quietly = TRUE) &&
        rstudioapi::isAvailable()) {
      user <- rstudioapi::showPrompt("Login", "Enter your username:", "")
    }


    if (is.null(user) || identical(user, "")) {
      if (requireNamespace("tcltk", quietly = TRUE)) {
        tt <- tcltk::tktoplevel()
        tcltk::tkwm.title(tt, "Enter Username")

        user_var <- tcltk::tclVar("")
        entry <- tcltk::tkentry(tt, textvariable = user_var)
        tcltk::tkpack(tcltk::tklabel(tt, text = "Username:"), entry)

        done <- tcltk::tclVar(0)
        onOK <- function()
          tcltk::tclvalue(done) <- 1
        ok_button <- tcltk::tkbutton(tt, text = "OK", command = onOK)
        tcltk::tkpack(ok_button)

        tcltk::tkwait.variable(done)
        user <- tcltk::tclvalue(user_var)
        tcltk::tkdestroy(tt)
      }
    }

    if (identical(user, "ANPC")) {
      package_name <- "MetaboExploreR"
      message("Welcome, ANPC! Opening workflow R Markdown...")

      rmd_file <- system.file("rmd", "workflow.Rmd", package = package_name)

      if (rmd_file != "") {
        if (requireNamespace("rstudioapi", quietly = TRUE) &&
            rstudioapi::isAvailable()) {
          rstudioapi::navigateToFile(rmd_file)
        } else {
          out_html <- tempfile(fileext = ".html")
          rmarkdown::render(rmd_file, output_file = out_html, quiet = TRUE)
          browseURL(out_html)
        }
      }
    }
  }
}
