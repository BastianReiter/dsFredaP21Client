
#' ds.P21.AssessComorbidity
#'
#' `r lifecycle::badge("experimental")` \cr\cr
#' Using functionality in package \code{comorbidity}, map ICD codes to morbidity categories and calculate a comorbidity score.
#'
#' Linked to server-side ASSIGN method \code{P21.AssessComorbidityDS()}
#'
#' @param DiagnosisData \code{string} - Name of the server-side \code{data.frame} holding diagnostic code data and a grouping identifier feature
#' @param DiagnosticCodeFeature \code{string} - Name of the feature containing diagnostic codes
#' @param IDFeature \code{string} - Name of the feature containing patient- or case-specific identifier (or any other feature grouping the diagnostic codes)
#' @param IgnoredCategories Optional \code{character vector} - Containing morbidity categories that should be ignored in score calculation. See documentation on \code{comorbidity::comorbidity()} for reference.
#' @param ReturnScoreValueOnly \code{logical} - Whether the assigned \code{data.frame} should only contain the feature with comorbidity score values and not the category-specific features
#' @param ScoreFeatureName \code{string} - The name of the feature that holds the comorbidity score values in the assigned \code{data.frame}
#' @param Arg.map \code{string} - The value for argument 'map' in \code{comorbidity::comorbidity()}
#' @param Arg.assign0 \code{logical} - The value for argument 'assign0' in \code{comorbidity::comorbidity()}
#' @param Arg.weights \code{string} - The value for argument 'weights' in \code{comorbidity::score()}
#' @param OutputName \code{string} - Assigned name of the server-side returned \code{data.frame}
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#'
#' @return A \code{list} of messages about object assignment for monitoring purposes
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ds.P21.AssessComorbidity <- function(DiagnosisData,
                                     DiagnosticCodeFeature,
                                     IDFeature,
                                     IgnoredCategories = NULL,
                                     ReturnScoreValueOnly = FALSE,
                                     ScoreFeatureName = "ComorbidityScore",
                                     Arg.map = "charlson_icd10_quan",
                                     Arg.assign0 = TRUE,
                                     Arg.weights = "quan",
                                     OutputName = "P21.Comorbidity",
                                     DSConnections = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- Argument Validation ---
  assert_that(is.string(DiagnosisData),
              is.string(DiagnosticCodeFeature),
              is.string(IDFeature),
              is.flag(ReturnScoreValueOnly),
              is.string(ScoreFeatureName),
              is.string(Arg.map),
              is.flag(Arg.assign0),
              is.string(Arg.weights),
              is.string(OutputName))
  if (!is.null(IgnoredCategories)) { assert_that(is.character(IgnoredCategories)) }

  # Special validation rules implemented with base::stopifnot() instead of assertthat::assert_that()
  if (str_starts(Arg.map, "charlson")) { stopifnot("To calculate Charlson Comorbidity Score, the value for argument 'Arg.weights' must be one of 'charlson' or 'quan'!" = (Arg.weights %in% c("charlson", "quan"))) }
  if (str_starts(Arg.map, "elixhauser")) { stopifnot("To calculate Elixhauser Comorbidity Score, the value for argument 'Arg.weights' must be one of 'vw' or 'swiss'!" = (Arg.weights %in% c("vw", "swiss"))) }

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

  # Execute server-side assign function
  DSI::datashield.assign(conns = DSConnections,
                         symbol = OutputName,
                         value = call("P21.AssessComorbidityDS",
                                      DiagnosisData.S = DiagnosisData,
                                      DiagnosticCodeFeature.S = DiagnosticCodeFeature,
                                      IDFeature.S = IDFeature,
                                      IgnoredCategories.S = IgnoredCategories,
                                      ReturnScoreValueOnly.S = ReturnScoreValueOnly,
                                      ScoreFeatureName.S = ScoreFeatureName,
                                      Arg.map.S = Arg.map,
                                      Arg.assign0.S = Arg.assign0,
                                      Arg.weights.S = Arg.weights))

  # Call helper function to check if object assignment succeeded
  AssignmentInfo <- ds.GetObjectStatus(OutputName,
                                       DSConnections = DSConnections)

#--- Print and invisibly return Messages ---------------------------------------

  # Print messages on console
  dsFredaClient::PrintMessages(AssignmentInfo)

  # Return Messages invisibly
  invisible(AssignmentInfo)
}
