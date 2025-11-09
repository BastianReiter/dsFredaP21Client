
#' P21.LoadRawDataSet
#'
#' `r lifecycle::badge("experimental")` \cr\cr
#' Load data from resources into R session on servers.
#'
#' @param ServerSpecifications \code{data.frame} - Same \code{data.frame} used for login. Used here only for acquisition of server-specific project names (in case they are differing) - Default: \code{NULL} for virtual project
#' @param ResourceNames.Mapping \code{character vector} - Usually the expected/required (generic) \code{resource} names. Can optionally be a named vector, with the names being expected names of \code{resources} and the vector values being the names of the assigned R \code{data.frames}.
#' @param ResourceNames.Dictionary Optional \code{list} of named \code{character vectors} - To enable server-specific mapping of deviating to required resource names. Names of list elements must match server names. For rules that should be applied on all servers, choose form \code{list(All = c('LookupName' = 'RequiredName'))}.
#' @param RunAssignmentChecks \code{logical} Indicating whether assignment checks should be performed or omitted for reduced execution time - Default: \code{TRUE}
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#'
#' @return A \code{list} of messages
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
P21.LoadRawDataSet <- function(ServerSpecifications = NULL,
                               ResourceNames.Mapping = setNames(dsFredaP21Client::Meta.Tables$TableName.Curated, nm = dsFredaP21Client::Meta.Tables$TableName.Raw),
                               ResourceNames.Dictionary = NULL,
                               RunAssignmentChecks = TRUE,
                               DSConnections = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # ServerSpecifications <- NULL
  # ResourceNames.Mapping <- setNames(dsFredaP21Client::Meta.Tables$TableName.Curated,
  #                                   nm = dsFredaP21Client::Meta.Tables$TableName.Raw)
  # ResourceNames.Dictionary <- list(ServerA = c(FALL = "Fall"),
  #                             ServerB = c(opsc = "OPS"))
  # RunAssignmentChecks <- TRUE
  # DSConnections <- CCPConnections

  # --- Argument Validation ---
  assert_that(is.character(ResourceNames.Mapping),
              is.flag(RunAssignmentChecks))
  if (!is.null(ServerSpecifications)) { assert_that(is.data.frame(ServerSpecifications)) }
  if (!is.null(ResourceNames.Dictionary)) { assert_that(is.list(ResourceNames.Dictionary)) }

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

  # Initiate output messaging objects
  Messages <- list()
  if (RunAssignmentChecks == TRUE) { Messages$Assignment <- c(Topic = "Object assignment on servers") }

  # Get server names
  ServerNames <- names(DSConnections)

  # If argument 'ResourceNames.Mapping' is an unnamed vector, set vector values as names
  if (is.null(names(ResourceNames.Mapping))) { names(ResourceNames.Mapping) <- ResourceNames.Mapping }

  # Get data on resource availability from separate function
  ResourceAvailability <- dsFredaClient::GetServerResourcesInfo(ServerSpecifications = ServerSpecifications,
                                                                ResourceNames.Required = names(ResourceNames.Mapping),
                                                                ResourceNames.Dictionary = ResourceNames.Dictionary,
                                                                DSConnections = DSConnections)

#-------------------------------------------------------------------------------
# Import resource data into R server sessions
#-------------------------------------------------------------------------------

  # Loop through all participating servers
  for (i in 1:length(ServerNames))
  {
      # Create data.frame containing mapping from resource names to R object names
      ResourcesToR <- ResourceAvailability$Resources.Required %>%
                          filter(Server == ServerNames[i],
                                 IsAvailable == TRUE,
                                 IsRequired == TRUE) %>%
                          mutate(RTableName = case_when(ResourceName.Generic %in% names(ResourceNames.Mapping) ~ paste0("P21.RDS.", ResourceNames.Mapping[ResourceName.Generic]),
                                                        .default = paste0("P21.RDS.", ResourceName.Generic)))

      # Loop through available and required resources and assign their content to data.frames in R session
      for (j in 1:nrow(ResourcesToR))
      {
          # We know that there is a resource (class 'resource') on the servers pointing to a local csv-file
          # Note: Currently, if unknown, there is no way to find out from client-side what exactly a specific resource points at (CSV, DB, Computational resource...)
          # First step is to create a 'ResourceClient' on the servers to handle the resource

          # Assign the resource client name to a symbol first
          ResourceClientName <- paste0("ResourceClient.", ResourcesToR$ResourceName.Generic[j])

          # Assign RessourceClient object on server
          DSI::datashield.assign.resource(conns = DSConnections[[i]],
                                          symbol = ResourceClientName,
                                          resource = ResourcesToR$ResourceName[j])

          # The first step requires a suitable 'ResourceResolver' to be registered on the servers. For csv-files this is already given by loading the 'resourcer' package on the servers.
          # Then we can actually load the data of the resource into the server R session by calling 'as.resource.data.frame' on it
          DSI::datashield.assign.expr(conns = DSConnections[[i]],
                                      symbol = ResourcesToR$RTableName[j],
                                      expr = bquote(as.resource.data.frame(x = .(as.name(ResourceClientName)),      # Everything in '.(...)' gets evaluated before complete expression is created and passed
                                                                           strict = 'TRUE')))

          # Add message about resource to R session mapping
          Messages$Assignment <- c(Messages$Assignment,
                                   Info = paste0("Server '", names(DSConnections)[i], "': Mapped resource '", ResourcesToR$ResourceName[j], "' to data.frame '", ResourcesToR$RTableName[j], "'."))
      }
  }


#-------------------------------------------------------------------------------
# Check if assignment on servers succeeded
#-------------------------------------------------------------------------------

  if (RunAssignmentChecks == TRUE)
  {
      BundledMessages <- list()

      # Loop through all tables to get info about assignment on servers
      for(tablename in ResourceNames.Mapping)
      {
          # Make sure assignment was successful on all servers
          ObjectStatus_Table <- ds.GetObjectStatus(ObjectName = paste0("P21.RDS.", tablename),
                                                   DSConnections = DSConnections)

          # Add info about table assignment to Messages
          BundledMessages <- c(BundledMessages,
                               ObjectStatus_Table["ObjectValidity"])   # Must select list element 'ObjectValidity' this way to keep naming of vector and thus class 'Success', 'Warning' and so forth
      }

      # Turn list into (named) vector and add it to Messages
      Messages$Assignment <- c(Messages$Assignment,
                               purrr::list_c(BundledMessages))
  }


#-------------------------------------------------------------------------------
# Assign list 'P21.RawDataSet' on all servers
#-------------------------------------------------------------------------------

  # Create list of vectors (one for each server) containing names of actually existing data.frames
  ExistingRDSTables <- paste0("P21.RDS.", ResourceNames.Mapping) %>%
                            map(function(tablename)
                                {
                                    if (!is.na(tablename))
                                    {
                                        unlist(ds.exists(x = tablename, datasources = DSConnections))
                                    } else {
                                        return(NULL)
                                    }
                                }) %>%
                            set_names(paste0("P21.RDS.", ResourceNames.Mapping)) %>%
                            list_transpose() %>%
                            map(\(TableNames) names(TableNames[TableNames == TRUE]))

  # For every server, consolidate all existing Raw Data Set tables in one list object called "P21.RawDataSet"
  ExistingRDSTables %>%
      purrr::iwalk(function(RDSTableNames, servername)
                   {
                      # Note: Tables within list 'P21.RawDataSet' are named WITHOUT prefix 'P21.RDS.'
                      dsFredaClient::ds.MakeList(ObjectNames = setNames(object = RDSTableNames,
                                                                        nm = str_remove(RDSTableNames, "P21.RDS.")),
                                                 OutputName = "P21.RawDataSet",
                                                 DSConnections = DSConnections[servername])
                   })

  if (RunAssignmentChecks == TRUE)
  {
      # Make sure assignment of RawDataSet was successful on all servers
      ObjectStatus_RawDataSet <- ds.GetObjectStatus(ObjectName = "P21.RawDataSet",
                                                    DSConnections = DSConnections)

      # Add info about RawDataSet assignment to Messages
      Messages$Assignment <- c(Messages$Assignment,
                               ObjectStatus_RawDataSet$ObjectValidity)
  }


#-------------------------------------------------------------------------------
# Print and invisibly return Messages object
#-------------------------------------------------------------------------------

  # Print messages on console
  PrintMessages(Messages)

  # Return Messages invisibly
  invisible(Messages)
}
