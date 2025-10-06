
library(dplyr)
library(usethis)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta Data transported from dsFredaP21 package
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meta.Tables <- dsFredaP21::Meta.Tables
Meta.Features <- dsFredaP21::Meta.Features
Meta.Values <- dsFredaP21::Meta.Values
Set.DataHarmonization <- dsFredaP21::Set.DataHarmonization
Set.Dictionary <- dsFredaP21::Set.Dictionary
Set.FeatureObligations <- dsFredaP21::Set.FeatureObligations
Set.FeatureTracking <- dsFredaP21::Set.FeatureTracking
Set.FuzzyStringMatching <- dsFredaP21::Set.FuzzyStringMatching
Set.TransformativeExpressions <- dsFredaP21::Set.TransformativeExpressions


use_data(Meta.Tables, overwrite = TRUE)
use_data(Meta.Features, overwrite = TRUE)
use_data(Meta.Values, overwrite = TRUE)
use_data(Set.DataHarmonization, overwrite = TRUE)
use_data(Set.Dictionary, overwrite = TRUE)
use_data(Set.FeatureObligations, overwrite = TRUE)
use_data(Set.FeatureTracking, overwrite = TRUE)
use_data(Set.FuzzyStringMatching, overwrite = TRUE)
use_data(Set.TransformativeExpressions, overwrite = TRUE)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta Data: Define server requirements that are checked before running of CCPhos functions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Set.ServerRequirements <- list(#--- Data frame containing names of required packages ---
                                RequiredPackages = data.frame(PackageName = character()) %>%
                                                        add_row(PackageName = "dsBase") %>%
                                                        add_row(PackageName = "dsFredaP21"),
                                #--- Data frame containing names and types of required functions ---
                                RequiredFunctions = data.frame(FunctionName = character(),
                                                               FunctionType = character()) %>%
                                                        add_row(FunctionName = "GetReportingObjectDS", FunctionType = "aggregate") %>%
                                                        add_row(FunctionName = "AugmentDataDS", FunctionType = "assign") %>%
                                                        add_row(FunctionName = "CurateDataDS", FunctionType = "assign") %>%
                                                        add_row(FunctionName = "ExtractFromListDS", FunctionType = "assign"))

# Save data in .rda-file and make it part of package
use_data(Set.ServerRequirements, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Template data frame: Server specifications
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Initiate tibble that holds credentials of participating servers
ServerSpecifications <- tibble(ServerName = character(),
                               URL = character(),
                               ProjectName = character(),
                               Token = character())

# Add site "Sissy"
ServerSpecifications <- add_row(ServerSpecifications,
                                ServerName = "Sissi",
                                URL = "https://Sissi/",
                                ProjectName = "Project",
                                Token = "1234567890")

# Save data in .rda-file and make it part of package
use_data(ServerSpecifications, overwrite = TRUE)

