
#===============================================================================


ServerWorkspace <- dsFredaClient::GetServerWorkspaceInfo()


GetObjectList <- c("CCP.ADS.Patient",
                   "CCP.ADS.Diagnosis",
                   "P21.CDS.Case",
                   "P21.Comorbidity")

for (objectname in GetObjectList)
{
    assign(objectname, DSLite::getDSLiteData(symbol = objectname,
                                             conns = CCPConnections)$ServerA)
}

#-------------------------------------------------------------------------------

P21PatientIDs <- P21.CDS.Case %>%
                    select(CaseID,
                           PatientID)

P21.Comorbidity <- P21.Comorbidity %>%
                        left_join(P21PatientIDs, by = join_by(CaseID))


PatIDMapping <- CCP.ADS.Patient$PatientID %>%
                    setNames(P21.Comorbidity$PatientID[sample(1:40000, size = 96)])


P21.Comorbidity.Fake <- P21.Comorbidity %>%
                              filter(PatientID %in% names(PatIDMapping)) %>%
                              mutate(PatientID = PatIDMapping[PatientID])

P21.CDS.Case.Fake <- P21.CDS.Case %>%
                          filter(PatientID %in% names(PatIDMapping)) %>%
                          mutate(PatientID = PatIDMapping[PatientID],
                                 AdmissionDate = as.Date(AdmissionDate - 2000),
                                 DischargeDate = as.Date(DischargeDate - 2000))


#-------------------------------------------------------------------------------


Tolerance.DischargeToDiagnosis <- 30
Tolerance.DiagnosisToAdmission <- 30

P21.CaseInfo <- P21.CDS.Case.Fake %>%
                    select(PatientID,
                           CaseID,
                           AdmissionDate,
                           DischargeDate)

MapCCPPatToP21Cases <- CCP.ADS.Patient %>%
                          select(PatientID) %>%
                          left_join(P21.CaseInfo, by = join_by(PatientID)) %>%
                          rename(P21CaseID = "CaseID")

Mapping <- CCP.ADS.Diagnosis %>%
              select(PatientID,
                     DiagnosisID,
                     DiagnosisDate) %>%
              left_join(MapCCPPatToP21Cases, by = join_by(PatientID), relationship = "many-to-many") %>%
              mutate(IsDiagnosisDateWithinCase = between(DiagnosisDate, AdmissionDate, DischargeDate),
                     IntervalDischargeToDiagnosis = as.numeric(DiagnosisDate - DischargeDate),
                     IntervalDiagnosisToAdmission = as.numeric(AdmissionDate - DiagnosisDate),
                     ReferenceCaseLikelihood = case_when(IsDiagnosisDateWithinCase == TRUE ~ 0,      # "Class A" candidate
                                                         between(IntervalDischargeToDiagnosis, 0, Tolerance.DischargeToDiagnosis) ~ IntervalDischargeToDiagnosis,      # "Class B" candidate - case in time-wise proximity BEFORE cancer diagnosis
                                                         between(IntervalDiagnosisToAdmission, 0, Tolerance.DiagnosisToAdmission) ~ IntervalDiagnosisToAdmission + Tolerance.DischargeToDiagnosis,      # "Class C" candidate - case in time-wise proximity AFTER cancer diagnosis. To make cases in this class less likely to be picked as reference case, add 'Tolerance.DischargeToDiagnosis'.
                                                         .default = NA)) %>%
              group_by(PatientID,
                       DiagnosisID) %>%
              arrange(desc(ReferenceCaseLikelihood), .by_group = TRUE) %>%
              summarize(HasReferenceP21Case = any(!is.na(ReferenceCaseLikelihood)),
                        ReferenceP21CaseID = ifelse(HasReferenceP21Case == TRUE, first(P21CaseID), NA))


Test <- Mapping %>%
            left_join(P21.Comorbidity.Fake, by = join_by(PatientID,
                                                         ReferenceP21CaseID == CaseID))


