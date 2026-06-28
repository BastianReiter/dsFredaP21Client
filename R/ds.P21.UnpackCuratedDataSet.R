
#' ds.P21.UnpackCuratedDataSet
#'
#' Make tables within Curated Data Set (\code{list} object) directly addressable in R server sessions
#'
#' Linked to server-side \code{ASSIGN} function \code{ExtractFromListDS()}
#'
#' @param CuratedDataSetName \code{string} - Name of Curated Data Set object (list) on server - Default: 'CuratedDataSet'
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#' @param DS.async \code{logical} - Value of argument 'async' in \code{DSI::datashield.assign()} / \code{DSI::datashield.aggregate()} - Default: \code{dsFredaClient::Set.DSSettings$DS.async}
#'
#' @return A \code{list} of messages about object assignment for monitoring purposes
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ds.P21.UnpackCuratedDataSet <- function(CuratedDataSetName = "P21.CuratedDataSet",
                                        DSConnections = NULL,
                                        DS.async = dsFredaClient::Set.DSSettings$DS.async)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- Argument Validation ---
  assert_that(is.string(CuratedDataSetName),
              is.flag(DS.async))

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- dsFredaClient::CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

  # Initiate output messaging objects
  Messages <- list()
  Messages$Assignment <- list()

  # Get curated P21 table names
  P21TableNames.CDS <- dsFredaP21Client::Meta.Tables$TableName.Curated

  for(i in 1:length(P21TableNames.CDS))
  {
      # Execute server-side assign function
      DSI::datashield.assign(conns = DSConnections,
                             symbol = paste0("P21.CDS.", P21TableNames.CDS[i]),      # E.g. 'CDS.Metastasis'
                             value = call("ExtractFromListDS",
                                          ListName.S = CuratedDataSetName,
                                          ObjectName.S = P21TableNames.CDS[i]),
                             async = DS.async)

      # Call helper function to check if object assignment succeeded
      Messages$Assignment <- c(Messages$Assignment,
                               ds.GetObjectStatus(ObjectName = paste0("P21.CDS.", P21TableNames.CDS[i]),
                                                  DSConnections = DSConnections,
                                                  DS.async = DS.async))
  }

  # Turn list into (named) vector
  Messages$Assignment <- purrr::list_c(Messages$Assignment)

  # Add topic element to start of vector
  Messages$Assignment <- c(Topic = "Object assignment on servers",
                           Messages$Assignment)


  # Print messages and return Messages object
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Print messages on console
  dsFredaClient::PrintMessages(Messages)

  # Return Messages invisibly
  invisible(Messages)
}
