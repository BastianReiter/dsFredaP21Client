
#' P21.CheckServerRequirements
#'
#' `r lifecycle::badge("stable")` \cr\cr
#' Check if technical requirements are met on every participating server.
#'
#' @param ServerSpecifications \code{data.frame} - Same \code{data.frame} used for login. Used here only for acquisition of server-specific project names (in case they are differing). - Default: \code{NULL} for virtual project
#' @param RequiredPackages A \code{character vector} naming required packages
#' @param RequiredFunctions A named \code{character vector} containing names of required functions. Their type ('aggregate' or 'assign') is defined by the correspondent element names.
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#'
#' @return A \code{list} of \code{data.frames} containing gathered info and messages
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
P21.CheckServerRequirements <- function(ServerSpecifications = NULL,
                                        RequiredPackages = c("dsBase",
                                                             "dsFredaP21",
                                                             "dsTidyverse"),
                                        RequiredFunctions = c(assign = "P21.AugmentDataDS",
                                                              assign = "P21.CurateDataDS",
                                                              assign = "P21.DrawSampleDS",
                                                              assign = "P21.PrepareRawDataSetDS"),
                                        DSConnections = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # DSConnections <- CCPConnections
  # RequiredPackages = c("dsBase", "dsFredaP21")
  # RequiredFunctions = c(aggregate = "GetReportingObjectDS",
  #                       assign = "AugmentDataDS",
  #                       assign = "CurateDataDS",
  #                       assign = "ExtractFromListDS")

  # --- Argument Validation ---

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- dsFredaClient::CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

  # Initiate output messaging objects
  Messages <- list()
  Messages$PackageAvailability <- c(Topic = "Package availability")
  Messages$VersionOfdsFredaP21 <- c(Topic = "Version of dsFredaP21")
  Messages$FunctionAvailability <- c(Topic = "Function availability")
  Messages$TableAvailability <- c(Topic = "Opal DB table availability")

  # Get server names (sorted alphabetically)
  ServerNames <- sort(names(DSConnections))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Package availability on servers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Get info about installed server packages
  PackageAvailability <- as_tibble(DSI::datashield.pkg_status(conns = DSConnections)$package_status,
                                   rownames = "PackageName")

  # Check if defined set of packages is available on all servers
  RequiredPackageAvailability <- tibble(PackageName = RequiredPackages) %>%
                                      left_join(PackageAvailability, by = join_by(PackageName)) %>%
                                      rowwise() %>%
                                      mutate(across(all_of(ServerNames), ~ ifelse(is.na(.), FALSE, .)),      # Replace NA values with FALSE. NAs are introduced when a required package is not listed in 'PackageAvailability'.
                                             IsAvailableEverywhere = all(c_across(all_of(ServerNames)) == TRUE),
                                             NotAvailableAt = ifelse(IsAvailableEverywhere == FALSE,
                                                                     paste0(ServerNames[c_across(all_of(ServerNames)) == FALSE], collapse = ", "),
                                                                     NA)) %>%
                                      ungroup()

  # Compile output message concerning one package each and add it to Messages
  for (i in 1:nrow(RequiredPackageAvailability))
  {
      Row <- RequiredPackageAvailability[i, ]

      # Note: It's important to use 'dplyr::if_else()' instead of 'ifelse' here, otherwise the return won't be a named vector
      Message <- if_else(Row$IsAvailableEverywhere == TRUE,
                         MakeFunctionMessage(Text = paste0("Function '",
                                                           Row$PackageName,
                                                           "' is available on all servers!"),
                                             IsClassSuccess = TRUE),
                         MakeFunctionMessage(Text = paste0("Package '",
                                                           Row$PackageName,
                                                           "' is not available at ",
                                                           Row$NotAvailableAt),
                                             IsClassFailure = TRUE))

      Messages$PackageAvailability <- c(Messages$PackageAvailability,
                                        Message)
  }

  # Transform / Transpose data frame into more handy return object
  RequiredPackageAvailability <- RequiredPackageAvailability %>%
                                      select(-IsAvailableEverywhere,
                                             -NotAvailableAt) %>%
                                      pivot_longer(!PackageName,
                                                   names_to = "ServerName",
                                                   values_to = "IsAvailable") %>%
                                      pivot_wider(names_from = PackageName,
                                                  values_from = IsAvailable) %>%
                                      mutate(CheckPackageAvailability = case_when(if_all(-ServerName, ~ .x == TRUE) ~ "green",
                                                                                  TRUE ~ "red"))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Available version of dsFredaP21
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Get version number of dsFredaP21 on all servers and check for equality
  VersionOfdsFredaP21 <- as_tibble(DSI::datashield.pkg_status(conns = DSConnections)$version_status,
                                   rownames = "PackageName") %>%
                            filter(PackageName == "dsFredaP21") %>%
                            select(-PackageName)

  if (nrow(VersionOfdsFredaP21 > 0))
  {
      IsEqualEverywhere <- apply(VersionOfdsFredaP21, 1, function(Values) { all(Values == Values[1]) })
      MessageOverall <- NULL
      MessagesDetail <- NULL

      if (IsEqualEverywhere == TRUE)
      {
          MessageOverall <- MakeFunctionMessage(Text = paste0("Version of dsFredaP21 is equal on all servers (Ver. ", VersionOfdsFredaP21[1, 1], ")!"),
                                                IsClassSuccess = TRUE)
      }
      else
      {
          MessagesOverall <- MakeFunctionMessage(Text = paste0("Version of dsFredaP21 varies between servers!"),
                                                 IsClassWarning = TRUE)

          for (i in 1:ncol(VersionOfdsFredaP21))
          {
              MessagesDetail <- c(Messages$VersionOfdsFredaP21,
                                  MakeFunctionMessage(Text = paste0(names(VersionOfdsFredaP21)[i], ": Ver. ", VersionOfdsFredaP21[, i]),
                                                      IsClassInfo = TRUE))
          }
      }

      Messages$VersionOfdsFredaP21 <- c(Messages$VersionOfdsFredaP21,
                                        MessageOverall,
                                        MessagesDetail)
  }

  # Transform / Transpose data frame into more handy return object
  VersionOfdsFredaP21 <- VersionOfdsFredaP21 %>%
                            pivot_longer(everything(),
                                         names_to = "ServerName",
                                         values_to = "dsFredaP21Version")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FredaP21 disclosure settings
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # ---


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function availability on servers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Get coherent data frame of general function availability on all servers
  FunctionAvailability <- rbind(# Get data frame of available AGGREGATE functions
                                DSI::datashield.method_status(conns = DSConnections,
                                                              type = "aggregate"),
                                # Get data frame of available ASSIGN functions
                                DSI::datashield.method_status(conns = DSConnections,
                                                              type = "assign"))

  # Check if defined set of required functions is available on all servers
  RequiredFunctionAvailability <- data.frame(FunctionName = RequiredFunctions,
                                             FunctionType = names(RequiredFunctions)) %>%
                                      left_join(FunctionAvailability, by = join_by(FunctionName == name, FunctionType == type)) %>%
                                      rowwise() %>%
                                      mutate(across(all_of(ServerNames), ~ ifelse(is.na(.), FALSE, .)),      # Replace NA values with FALSE. NAs are introduced when a required function is not listed in 'FunctionAvailability'.
                                             IsAvailableEverywhere = all(c_across(all_of(ServerNames)) == TRUE),
                                             NotAvailableAt = ifelse(IsAvailableEverywhere == FALSE,
                                                                     paste0(ServerNames[c_across(all_of(ServerNames)) == FALSE], collapse = ", "),
                                                                     NA)) %>%
                                      ungroup()

  # Compile output message concerning one function each and add it to Messages
  for (i in 1:nrow(RequiredFunctionAvailability))
  {
      Row <- RequiredFunctionAvailability[i, ]

      # Note: It's important to use 'dplyr::if_else()' instead of 'ifelse' here, otherwise the return won't be a named vector
      Message <- if_else(Row$IsAvailableEverywhere == TRUE,
                         MakeFunctionMessage(Text = paste0("Function '",
                                                           Row$FunctionName,
                                                           "' is available on all servers!"),
                                             IsClassSuccess = TRUE),
                         MakeFunctionMessage(Text = paste0("Function '",
                                                           Row$FunctionName,
                                                           "' is not available at ",
                                                           Row$NotAvailableAt),
                                             IsClassWarning = TRUE))

      Messages$FunctionAvailability <- c(Messages$FunctionAvailability,
                                         Message)
  }

  # Transform / Transpose data frame into more handy return object
  RequiredFunctionAvailability <- RequiredFunctionAvailability %>%
                                      select(-FunctionType,
                                             -IsAvailableEverywhere,
                                             -NotAvailableAt) %>%
                                      pivot_longer(!FunctionName,
                                                   names_to = "ServerName",
                                                   values_to = "IsAvailable") %>%
                                      pivot_wider(names_from = FunctionName,
                                                  values_from = IsAvailable) %>%
                                      mutate(CheckFunctionAvailability = case_when(if_all(-ServerName, ~ .x == TRUE) ~ "green",
                                                                                   TRUE ~ "red"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Print messages and return list object
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Print messages on console
  PrintMessages(Messages)

  # Invisibly return list containing data frames of gathered info and messages
  invisible(list(PackageAvailability = RequiredPackageAvailability,
                 VersionOfdsFredaP21 = VersionOfdsFredaP21,
                 FunctionAvailability = RequiredFunctionAvailability,
                 Messages = Messages))
}
