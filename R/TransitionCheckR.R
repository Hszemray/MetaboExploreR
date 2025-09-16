#' transition_checkR
#'
#' This function checks Q1 and Q3 transitions to ensure all transitions are unique.
#' @param transition_df df of transitions (mrm_template) for SkylineR or qcCheckR
#' @return A message if all transitions are unique or a list of non unique transitions to be fixed
#' @export
#' @examples
#' \dontrun{
#' transition_checkR(transition_df)
#' }
transition_checkR <- function(transition_df) {
  df <- transition_df
  df$combo <- paste(df$`Precursor Mz`, df$`Product Mz`, sep = "_")

  combo_counts <- table(df$combo)

  non_unique_combos <- names(combo_counts[combo_counts > 1])

  non_unique_df <- df[df$combo %in% non_unique_combos, ]

  non_unique_metabolites <- unique(non_unique_df$`Precursor Name`)

  summary_df <- non_unique_df[,1:6]

  if (length(non_unique_metabolites) == 0) {
    message("Congratulations all mrm transitions are unique")
  } else {
    message("Please correct the following transition clashes:")
    summary_df <- summary_df[order(summary_df$`Precursor Mz`), ]
    return(summary_df)
  }
}

#' compare_mrm_template_with_guide
#'
#' This function checks if all internal standards from the note column in the transition list has a match in concentration guide
#' @param mrm_template df of transitions (mrm_template) for SkylineR or qcCheckR
#' @param concentration_guide df of concentrations for SIL internal standards
#' @return A message if all transitions are unique or a list of non unique transitions to be fixed
#' @export
#' @examples
#' \dontrun{
#' compare_mrm_template_with_guide(mrm_template, concentration_guide)
#' }
compare_mrm_template_with_guide <- function(mrm_template, concentration_guide) {
  if (!("Note" %in% names(mrm_template))) {
    stop("Missing required column:'Note' in mrm_template")
  }

  if (!("SIL_name" %in% names(concentration_guide))) {
    stop("Missing required column:'SIL_name' in concentration_guide")
  }

  note_values <- mrm_template$Note
  note_values <- note_values[!is.na(note_values)]

  sil_values <- concentration_guide$SIL_name

  unmatched_notes <- note_values[!note_values %in% sil_values]

  if (length(unmatched_notes) == 0) {
    message("Congrats! All metabolite targets have a match in conc.")
  } else {
    message("Please fix the following metabolite targets with no matches")
    return(unmatched_notes)
  }
}
