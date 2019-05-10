CommittedRows <- mutate(CommittedRows, NofBeds_STD = ((NofBeds - mean(NofBeds)) / sd(NofBeds)))
NonCommittedRows <- mutate(NonCommittedRows, NofBeds_STD = ((NofBeds - mean(NofBeds)) / sd(NofBeds)))

CommittedRows <- mutate(CommittedRows, NofSurgicalOperations_STD = ((NofSurgicalOperations - mean(NofSurgicalOperations, na.rm = TRUE)) / sd(NofSurgicalOperations, na.rm = TRUE)))
NonCommittedRows <- mutate(NonCommittedRows, NofSurgicalOperations_STD = ((NofSurgicalOperations - mean(NofSurgicalOperations, na.rm = TRUE)) / sd(NofSurgicalOperations, na.rm = TRUE)))

CommittedRows <- mutate(CommittedRows, FullTimeEmploNonCommittedRowsees_STD = ((FullTimeEmploNonCommittedRowsees - mean(FullTimeEmploNonCommittedRowsees, na.rm = TRUE)) / sd(FullTimeEmploNonCommittedRowsees, na.rm = TRUE)))
NonCommittedRows <- mutate(NonCommittedRows, FullTimeEmploNonCommittedRowsees_STD = ((FullTimeEmploNonCommittedRowsees - mean(FullTimeEmploNonCommittedRowsees, na.rm = TRUE)) / sd(FullTimeEmploNonCommittedRowsees, na.rm = TRUE)))

groupTable <- data.frame(OwnershipType = OrgControlOverallNames[1], Code = row.names(measure.names)[1],)

groupTableHeaders <- c("Ownership Type", "Code", "C($\\mu$)", "C(sd)", "NC(mean)", "NC(sd)", "t", "p")

groupTable <- data.frame(OrgControlOverallNames[1], row.names(measure.names)[1], round(PSI4.diff.org1$estimate[1], 2), round(sd(Committed.OrgControl1$PSI_4_SURG_COMP_Score, na.rm = TRUE), 2), round(PSI4.diff.org1$estimate[2], 2), round(sd(NonCommitted.OrgControl1$PSI_4_SURG_COMP_Score, na.rm = TRUE), 2), round(PSI4.diff.org1$statistic, 2), as.character(round(PSI4.diff.org1$p.value, 10)))

rownames(groupTable) <- 1:nrow(groupTable)
colnames(groupTable) <- groupTableHeaders

pandoc.table(groupTable, style = "rmarkdown", split.cells = "inf", split.tables = "inf")
