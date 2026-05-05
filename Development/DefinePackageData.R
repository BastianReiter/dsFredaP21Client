
library(dplyr)
library(usethis)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta Data transported from dsFredaP21 package
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meta.ADS <- dsFredaP21::Meta.ADS
Meta.Tables <- dsFredaP21::Meta.Tables
Meta.Features <- dsFredaP21::Meta.Features
Meta.Values <- dsFredaP21::Meta.Values
Set.CurationProcess <- dsFredaP21::Set.CurationProcess
Set.DataRemediation <- dsFredaP21::Set.DataRemediation
Set.Dictionary <- dsFredaP21::Set.Dictionary
Set.FeatureRequirements <- dsFredaP21::Set.FeatureRequirements
Set.FeatureTracking <- dsFredaP21::Set.FeatureTracking
Set.FuzzyStringMatching <- dsFredaP21::Set.FuzzyStringMatching
Set.PrimaryTableCleaning <- dsFredaP21::Set.PrimaryTableCleaning
Set.RecordSubsumption <- dsFredaP21::Set.RecordSubsumption
Set.SecondaryTableCleaning <- dsFredaP21::Set.SecondaryTableCleaning
Set.TransformativeExpressions <- dsFredaP21::Set.TransformativeExpressions


use_data(Meta.ADS, overwrite = TRUE)
use_data(Meta.Tables, overwrite = TRUE)
use_data(Meta.Features, overwrite = TRUE)
use_data(Meta.Values, overwrite = TRUE)
use_data(Set.CurationProcess, overwrite = TRUE)
use_data(Set.DataRemediation, overwrite = TRUE)
use_data(Set.DiagnosisAssociation, overwrite = TRUE)
use_data(Set.DiagnosisRedundancy, overwrite = TRUE)
use_data(Set.Dictionary, overwrite = TRUE)
use_data(Set.FeatureRequirements, overwrite = TRUE)
use_data(Set.FeatureTracking, overwrite = TRUE)
use_data(Set.FuzzyStringMatching, overwrite = TRUE)
use_data(Set.PrimaryTableCleaning, overwrite = TRUE)
use_data(Set.RecordSubsumption, overwrite = TRUE)
use_data(Set.SecondaryTableCleaning, overwrite = TRUE)
use_data(Set.TransformativeExpressions, overwrite = TRUE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta Data: Define server requirements that are checked before running of FredaP21 functions
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

