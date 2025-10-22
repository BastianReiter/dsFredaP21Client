
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - Virtual DataSHIELD infrastructure for testing purposes -
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Install newest base DataSHIELD packages
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# devtools::install_github(repo = "datashield/dsBase")
# devtools::install_github(repo = "datashield/dsBaseClient")

# Install own DataSHIELD packages
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# devtools::install_github(repo = "BastianReiter/dsFreda")
# devtools::install_github(repo = "BastianReiter/dsFredaClient")
# devtools::install_github(repo = "BastianReiter/dsCCPhos")
# devtools::install_github(repo = "BastianReiter/dsCCPhosClient")
# devtools::install_github(repo = "BastianReiter/CCPhosApp")

# Install additional Datashield-packages
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# install.packages("dsTidyverse")
# install.packages("dsTidyverseClient")
#
# devtools::install_github("tombisho/dsSynthetic", dependencies = TRUE)
# devtools::install_github("tombisho/dsSyntheticClient", dependencies = TRUE)
#
# devtools::install_github("neelsoumya/dsSurvival")
# devtools::install_github("neelsoumya/dsSurvivalClient")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load required packages
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(dplyr)
library(dsBaseClient)
library(dsFredaP21Client)
# library(dsFredaClient)
# library(dsTidyverseClient)
library(purrr)
library(resourcer)
library(stringr)
library(tibble)
library(tidyr)

# Print DataSHIELD errors right away
options(datashield.errors.print = TRUE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Establish Connections to virtual servers using dsCCPhosClient::ConnectToVirtualCCP()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#TestData <- readRDS("../dsCCPhos/Development/Data/RealData/CCPRealData_Frankfurt.rds")
TestData <- readRDS("../dsCCPhos/Development/Data/TestData/CCPTestData.rds")

# Definition of resources pointing to csv-files containing P21 data
Resource.FAB.csv <- resourcer::newResource(name = "Resource.FAB.csv",
                                           #url = "file://./Development/Test/DummyData.csv",
                                           url = "file://localhost/C:/Users/Basti/ARBEIT Lokal/dsFredaP21/Development/Data/RealData/FAB.csv",
                                           format = "csv")
Resource.Fall.csv <- resourcer::newResource(name = "Resource.Fall.csv",
                                           #url = "file://./Development/Test/DummyData.csv",
                                           url = "file://localhost/C:/Users/Basti/ARBEIT Lokal/dsFredaP21/Development/Data/RealData/Fall.csv",
                                           format = "csv")
Resource.ICD.csv <- resourcer::newResource(name = "Resource.ICD.csv",
                                           #url = "file://./Development/Test/DummyData.csv",
                                           url = "file://localhost/C:/Users/Basti/ARBEIT Lokal/dsFredaP21/Development/Data/RealData/ICD.csv",
                                           format = "csv")
Resource.OPS.csv <- resourcer::newResource(name = "Resource.OPS.csv",
                                           #url = "file://./Development/Test/DummyData.csv",
                                           url = "file://localhost/C:/Users/Basti/ARBEIT Lokal/dsFredaP21/Development/Data/RealData/OPS.csv",
                                           format = "csv")


CCPConnections <- dsCCPhosClient::ConnectToVirtualCCP(CCPTestData = TestData,
                                                      NumberOfServers = 3,
                                                      NumberOfPatientsPerServer = 2000,
                                                      AddedDsPackages = c("dsTidyverse",
                                                                          "dsFredaP21"),
                                                      Resources = list(FAB = Resource.FAB.csv,
                                                                       Fall = Resource.Fall.csv,
                                                                       ICD = Resource.ICD.csv,
                                                                       OPS = Resource.OPS.csv))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check server requirements using dsCCPhosClient::CheckServerRequirements()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dsCCPhosClient::CheckServerRequirements()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load P21 Raw Data Set (RDS) from resources to R sessions on servers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

P21.LoadRawDataSet(ServerSpecifications = NULL)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Prior to Data Curation harmonize feature names in RDS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ds.PrepareRawData(RawDataSetName = "P21.RawDataSet",
                  Module = "P21",
                  FeatureNameDictionary = list(Department = c(FAB = "Fachabteilung")),
                  CompleteCharacterConversion = TRUE,
                  RDSTableNames = dsFredaP21Client::Meta.Tables$TableName.Curated)


TestRDS <- DSLite::getDSLiteData(conns = CCPConnections,
                                 symbol = "P21.RawDataSet")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check RDS tables for existence and completeness
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

RDSTableCheck <- ds.GetDataSetCheck(DataSetName = "P21.RawDataSet",
                                    Module = "P21",
                                    Stage = "Raw")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Optionally: Draw random sample from Raw Data Set on servers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ds.P21.DrawSample(RawDataSetName = "P21.RawDataSet",
                  SampleSize = 5000,
                  SampleName = "P21.RawDataSet")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Transform Raw Data Set (RDS) into Curated Data Set (CDS)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Transform Raw Data Set (RDS) into Curated Data Set (CDS) (using default settings)
ds.P21.CurateData(RawDataSetName = "P21.RawDataSet",
                  Settings = NULL,
                  OutputName = "P21.CurationOutput")

CDSTableCheck <- ds.GetDataSetCheck(DataSetName = "P21.CuratedDataSet",
                                    Modul = "P21",
                                    Stage = "Curated")

# Get curation reports
CurationReport <- ds.GetCurationReport(Module = "P21")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Transform Curated Data Set (CDS) into Augmented Data Set (ADS)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Run ds.AugmentData
ds.P21.AugmentData(CuratedDataSetName = "P21.CuratedDataSet",
                   OutputName = "P21.AugmentationOutput")

ADSTableCheck <- ds.GetDataSetCheck(DataSetName = "AugmentedDataSet")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get overview of objects in server workspaces
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# - Using dsCCPhosClient::GetServerWorkspaceInfo() and dsCCPhosClient::ds.GetObjectMetaData()
#-------------------------------------------------------------------------------

# Collect comprehensive information about all workspace objects
ServerWorkspaceInfo <- GetServerWorkspaceInfo()

# Overview of all objects in server R sessions
View(ServerWorkspaceInfo$Overview)

# Detailed meta data of a particular object (also part of ServerWorkspaceInfo)
ObjectMetaData <- ds.GetObjectMetaData(ObjectName = "CDS_Patient")

# Explore Object meta data: Structural overview
View(ObjectMetaData$ServerA$Structure)

# Get type of feature 'PatientID'
ObjectMetaData$FirstEligible$DataTypes["PatientID"]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Process ADS tables
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# dsTidyverse
# ds.filter(df.name = "ADS_Patient",
#           tidy_expr = list(CountDiagnoses == 1),
#           newobj = "ADS_Patient_OneDiagnosis")
#
#
# ds.filter(df.name = "ADS_Patient",
#           tidy_expr = list(Gender == "Female"),
#           newobj = "ADS_Patient_OneDiagnosis")






Messages <- ds.JoinTables(TableNameA = "ADS_Patient_OneDiagnosis",
                          TableNameB = "ADS_Diagnosis",
                          ByStatement = "PatientID",
                          OutputName = "AnalysisDataSet")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data Exploration
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

CohortDescription <- ds.GetCohortDescription(DataSetName = "AugmentedDataSet",
                                             CCPDataSetType = "ADS")


# Transform data into display-friendly time series tables using auxiliary function 'DisplayTimeSeries()'
PatientCount_TimeSeries <- DisplayTimeSeries(TimeSeriesData = CohortDescription$CohortSize_OverTime,
                                             TimePointFeature = "DiagnosisYear",
                                             ValueFeature = "PatientCount",
                                             GroupingFeature = "Server",
                                             IncludeMissingTimePoints = TRUE)

Plot <- CohortDescription$CohortSize_OverTime %>%
            filter(Server != "All") %>%
            MakeColumnPlot(XFeature = DiagnosisYear,
                           YFeature = PatientCount,
                           GroupingFeature = Server)


Plot <- CohortDescription$GenderDistribution %>%
            filter(Server != "All") %>%
            MakeColumnPlot(XFeature = Gender,
                           YFeature = N,
                           GroupingFeature = Server)

Plot <- CohortDescription$AgeDistribution %>%
            filter(Server != "All") %>%
            MakeColumnPlot(XFeature = AgeGroup,
                           YFeature = N,
                           GroupingFeature = Server)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Perform exemplary analyses
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


Test <- ds.GetCrossTab(TableName = "ADS.Patient",
                       FeatureNames = c("Sex", "LastVitalStatus", "CountDiagnoses"),
                       RemoveNA = TRUE)

ds.MutateTable(TableName = "ADS_Diagnosis",
               MutateExpression = "UICCStageClassification = case_when(str_starts(TNM_T, '3') ~ 'III', .default = '<NA>')",
               OutputName = "TestUICC")


ds.names("TestUICC")


Test <- dsBaseClient::ds.corTest(x = "AnalysisDataSet$PatientAgeAtDiagnosis",
                                 y = "AnalysisDataSet$TimeDiagnosisToDeath")


Test <- dsBaseClient::ds.glm()





Test <- ds.GetTTEModel(TableName = "AnalysisDataSet",
                       TimeFeature = "TimeFollowUp",
                       EventFeature = "IsDocumentedDeceased",
                       ModelType = "survfit",
                       CovariateA = "UICCStageCategory",
                       #CovariateB = "UICCStageCategory",
                       MinFollowUpTime = 20)

library(ggplot2)
library(ggsurvfit)

Test$ServerC %>%
    ggsurvfit()


Test <- ds.GetFeatureInfo(TableName = "AnalysisDataSet",
                          FeatureName = "TNM_T")

Test <- ds.GetSampleStatistics(TableName = "AnalysisDataSet",
                               MetricFeatureName = "PatientAgeAtDiagnosis")

Test <- ds.GetFrequencyTable(TableName = "ADS_Diagnosis",
                             FeatureName = "TNM_T",
                             MaxNumberCategories = 20)

RelativeFrequencies <- Test$RelativeFrequencies %>%
                            mutate(across(-Server, ~ paste0("(", round(.x * 100, 0), "%)")))

TableData <- Test$AbsoluteFrequencies %>%
                  mutate(across(everything(), as.character)) %>%
                  bind_rows(RelativeFrequencies) %>%
                  group_by(Server) %>%
                      summarize(across(everything(), ~ paste0(.x, collapse = "  ")))


library(ggplot2)

PlotData <- Test$AbsoluteFrequencies %>%
                pivot_longer(cols = -Server,
                             names_to = "Value",
                             values_to = "AbsoluteFrequency") %>%
                filter(Server != "All")

Plot <- ggplot(data = as.data.frame(PlotData),
               mapping = aes(fill = Server,
                             x = Value,
                             y = AbsoluteFrequency)) +
            geom_bar(position = "stack",
                     stat = "identity")

Plot <- MakeColumnPlot(DataFrame = PlotData,
                       XFeature = Value,
                       YFeature = AbsoluteFrequency,
                       GroupingFeature = Server)



Test <- ExploreFeature(TableName = "AnalysisDataSet",
                       FeatureName = "TimeDiagnosisToDeath")




ds.GetObjectMetaData(ObjectName = "AugmentationOutput")


ds.mean(x = "ADS_Patients$PatientAgeAtDiagnosis",
        datasources = CCPConnections)


MetaData_ADS_Patients <- ds.GetObjectMetaData(ObjectName = "ADS_Patients")

View(MetaData_ADS_Patients$ServerA$ContentOverview)


SampleStatistics <- ds.GetSampleStatistics(TableName = "ADS_Patients",
                                           MetricFeatureName = "PatientAgeAtDiagnosis")


TestPlot <- MakeBoxPlot(SampleStatistics = SampleStatistics,
                        AxisTitle_y = "Patient age at diagnosis",
                        FillPalette = c("All" = CCPhosColors$MediumGrey,
                                        "ServerA" = CCPhosColors$Primary,
                                        "ServerB" = CCPhosColors$Secondary,
                                        "ServerC" = CCPhosColors$Tertiary))

TestPlot


# dsSurvivalClient::ds.Surv(time = "ADS_Patients$TimeFollowUp",
#                           event = "ADS_Patients$IsDocumentedDeceased",
#                           objectname = "TestSurv",
#                           datasources = CCPConnections)
#
# dsSurvivalClient::ds.survfit(formula = 'TestSurv',
#                              object = "TestSurvfit")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Log out from all servers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

DSI::datashield.logout(CCPConnections)






# Generate synthetic data using package dsSynthetic (which in turn makes use of packages synthpop and simstudy)

# library(dsSyntheticClient)
#
# SyntheticData <- ds.syn(data = "Diagnosis")
#
#                         method = "cart",
#                         m = 1,
#                         seed = 123)
#
# SyntheticData <- SyntheticData$ServerTotal$Warning
#
#
# SyntheticData



#OpalDB_A <- dsCCPhos::MakeTestDB(CCPTestData_A)
#res_CCPTestData_A <- resourcer::newResource(name = "CCPTest")
#resourcer::PostgresResourceConnector$new()

# res_CCPTestData_A <- resourcer::newResource(name = "CCPTestData",
#                                             url = "file://./Development/Data/RealData/CCPTestData_A.RData",
#                                             format = "list")

#Test <- resourcer::FileResourceGetter$new()
