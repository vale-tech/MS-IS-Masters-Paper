# Check for and install necessary packages
if (!('RODBC' %in% rownames(installed.packages()))) {
    install.packages('RODBC')
}
if (!('car' %in% rownames(installed.packages()))) {
    install.packages('car')
}
if (!('psych' %in% rownames(installed.packages()))) {
    install.packages('psych')
}
#if (!('sjPlot' %in% rownames(installed.packages()))) {
    #install.packages('sjPlot')
#}
if (!('summarytools' %in% rownames(installed.packages()))) {
    install.packages('summarytools')
}
if (!('dplyr' %in% rownames(installed.packages()))) {
    install.packages('dplyr')
}
if (!('pander' %in% rownames(installed.packages()))) {
    install.packages('pander')
}
if (!('ggplot2' %in% rownames(installed.packages()))) {
    install.packages('ggplot2')
}
if (!('BSDA' %in% rownames(installed.packages()))) {
    install.packages('BSDA')
}
if (!('nortest' %in% rownames(installed.packages()))) {
    install.packages('nortest')
}

# Load necessary libraries
library(RODBC)
library(car)
library(psych)
#library(sjPlot)
library(summarytools)
library(dplyr)
library(pander)
library(ggplot2)
library(BSDA)
library(nortest)

# Connect to data source and load data
cn <- odbcDriverConnect(connection = "Driver={SQL Server Native Client 11.0};server=192.168.1.12;database=DorenfestStage;Uid=sa;Pwd=Suprem3One!;")
hospitalData <- sqlFetch(cn, 'vHospitalDataSet', colnames = FALSE, rows_at_time = 1000, as.is = c(2, 3, 4, 5, 6, 7, 8, 27, 34, 35, 36))

# Misc. data cleanup and transformations
hospitalData$ISPlanCommittedToReduceMedicalErrors <- as.factor(hospitalData$ISPlanCommittedToReduceMedicalErrors)
hospitalData$Domain2_Score <- as.numeric(hospitalData$Domain2_Score)
hospitalData$Total_HAC_Score <- as.numeric(hospitalData$Total_HAC_Score)

# Select only desired columns
hospitalData.Demo <- hospitalData %>% select(ISPlanCommittedToReduceMedicalErrors, MedicareNumber, HospitalName, HospitalUniqueId, NofBeds, NofSurgicalOperations, FullTimeEmployees, OwnershipStatus, OrgControlOverall, OrgControlDetail, ParentName, ParentUniqueId)

hospitalData.Data <- hospitalData %>% select(HospitalUniqueId, AHRQ_PSI_90_Score, CLABSI_Score, CAUTI_Score, SSI_Score, Domain2_Score, Total_HAC_Score, PSI_12_POSTOP_PULMEMB_DVT_Score, PSI_14_POSTOP_DEHIS_Score, PSI_15_ACC_LAC_Score, PSI_4_SURG_COMP_Score, PSI_6_IAT_PTX_Score, PSI_90_SAFETY_Score)

measure.names <- data.frame(Code = c("PSI4", "PSI6", "PSI12", "PSI14", "PSI15", "PSI90", "D2", "HAC"), Measure = c("PSI_4_SURG_COMP_Score", "PSI_4_SURG_COMP_Score", "PSI_12_POSTOP_PULMEMB_DVT_Score", "PSI_14_POSTOP_DEHIS_Score", "PSI_15_ACC_LAC_Score", "PSI_90_SAFETY_Score", "Domain2_Score", "Total_HAC_Score"), row.names = 1)

# Filter data to only complete cases
# hospitalData.Data <- hospitalData.Data[complete.cases(hospitalData.Data),]

# Recombine data
hospitalData <- inner_join(hospitalData.Demo, hospitalData.Data, by = "HospitalUniqueId")

# Feature generation
# IS Plan
hospitalData <- hospitalData %>% mutate(PlanToReduceErrors = ifelse(.$ISPlanCommittedToReduceMedicalErrors == "1", "Committed", "Not Committed"))
hospitalData$PlanToReduceErrors <- as.factor(hospitalData$PlanToReduceErrors)
# Bed classification
hospitalData <- hospitalData %>% mutate(NofBedsCategory = ifelse(.$NofBeds <= 100, "0-100",
                                                          ifelse(.$NofBeds >= 101 & .$NofBeds <= 200, "101-200",
                                                          ifelse(.$NofBeds >= 201 & .$NofBeds <= 300, "201-300",
                                                          ifelse(.$NofBeds >= 301 & .$NofBeds <= 400, "301-400",
                                                          ifelse(.$NofBeds >= 401 & .$NofBeds <= 500, "401-500",
                                                          ifelse(.$NofBeds >= 501 & .$NofBeds <= 600, "501-600", "601+")))))))
hospitalData$NofBedsCategory <- as.factor(hospitalData$NofBedsCategory)
# OrgControlOverallNum

hospitalData <- mutate(hospitalData, OrgControlOverallNum = as.factor(as.numeric(OrgControlOverall)))

OrgControlOverallNames <- unique(hospitalData$OrgControlOverall)

# VIEW (DEBUG)
# view(dfSummary(hospitalData), method = "browser")

# Data Set Splitting
CommittedRows <- filter(hospitalData, hospitalData$ISPlanCommittedToReduceMedicalErrors == "1")
NonCommittedRows <- filter(hospitalData, hospitalData$ISPlanCommittedToReduceMedicalErrors == "0")

# Summaries
CommittedRows.summary <- describe(CommittedRows, skew = FALSE)
NonCommittedRows.summary <- describe(NonCommittedRows, skew = FALSE)

# All Hospitals
summaryTable <- descr(hospitalData$NofBeds, style = 'rmarkdown', transpose = TRUE)
summaryTable$stats[, 6:11] <- list(NULL)
attr(summaryTable$stats, "row.names") <- "Number of Beds"

r2 <- descr(hospitalData$NofSurgicalOperations, style = 'rmarkdown', transpose = TRUE)
r2$stats[, 6:11] <- list(NULL)
attr(r2$stats, "row.names") <- "Number of Surgical Procedures"

r3 <- descr(hospitalData$FullTimeEmployees, style = 'rmarkdown', transpose = TRUE)
r3$stats[, 6:11] <- list(NULL)
attr(r3$stats, "row.names") <- "Full Time Employees"

summaryTable <- rbind(summaryTable$stats, r2$stats)
summaryTable <- rbind(summaryTable, r3$stats)

# Committed Hospitals
summaryTable.C <- descr(CommittedRows$NofBeds, style = 'rmarkdown', transpose = TRUE)
summaryTable.C$stats[, 6:11] <- list(NULL)
attr(summaryTable.C$stats, "row.names") <- "Number of Beds"

r2 <- descr(CommittedRows$NofSurgicalOperations, style = 'rmarkdown', transpose = TRUE)
r2$stats[, 6:11] <- list(NULL)
attr(r2$stats, "row.names") <- "Number of Surgical Procedures"

r3 <- descr(CommittedRows$FullTimeEmployees, style = 'rmarkdown', transpose = TRUE)
r3$stats[, 6:11] <- list(NULL)
attr(r3$stats, "row.names") <- "Full Time Employees"

summaryTable.C <- rbind(summaryTable.C$stats, r2$stats)
summaryTable.C <- rbind(summaryTable.C, r3$stats)

# NonCommittedRows Hospitals
summaryTable.NC <- descr(NonCommittedRows$NofBeds, style = 'rmarkdown', transpose = TRUE)
summaryTable.NC$stats[, 6:11] <- list(NULL)
attr(summaryTable.NC$stats, "row.names") <- "Number of Beds"

r2 <- descr(NonCommittedRows$NofSurgicalOperations, style = 'rmarkdown', transpose = TRUE)
r2$stats[, 6:11] <- list(NULL)
attr(r2$stats, "row.names") <- "Number of Surgical Procedures"

r3 <- descr(NonCommittedRows$FullTimeEmployees, style = 'rmarkdown', transpose = TRUE)
r3$stats[, 6:11] <- list(NULL)
attr(r3$stats, "row.names") <- "Full Time Employees"

summaryTable.NC <- rbind(summaryTable.NC$stats, r2$stats)
summaryTable.NC <- rbind(summaryTable.NC, r3$stats)

C.OrgControlCount <- CommittedRows %>% group_by(OrgControlOverall) %>% summarise(n = n())
NC.OrgControlCount <- NonCommittedRows %>% group_by(OrgControlOverall) %>% summarise(n = n())

## histogram of demographics
#ggplot(CommittedRows, aes(x = NofBeds)) +
    #geom_histogram()
#ggplot(CommittedRows, aes(x = NofSurgicalOperations)) +
    #geom_histogram()
#ggplot(CommittedRows, aes(x = FullTimeEmployees)) +
    #geom_histogram()

# DEMOGRAPHIC DIFFERENCE IN MEAN TESTS
# NofBeds
NofBeds.Diff <- z.test(CommittedRows$NofBeds, NonCommittedRows$NofBeds, alternative = "two.sided", mu = 0, sigma.x = sd(CommittedRows$NofBeds), sigma.y = sd(NonCommittedRows$NofBeds))
# NofSurgicalOperations
C.NofSurgicalOperations.val <- na.omit(CommittedRows$NofSurgicalOperations)
NC.NofSurgicalOperations.val <- na.omit(NonCommittedRows$NofSurgicalOperations)
C.NofSurgicalOperations.sd <- sd(CommittedRows$NofSurgicalOperations, na.rm = TRUE)
NC.NofSurgicalOperations.sd <- sd(NonCommittedRows$NofSurgicalOperations, na.rm = TRUE)
NofSurgicalOperations.Diff <- z.test(C.NofSurgicalOperations.val, NC.NofSurgicalOperations.val, alternative = "two.sided", mu = 0, sigma.x = C.NofSurgicalOperations.sd, sigma.y = NC.NofSurgicalOperations.sd)
# FullTimeEmployees
C.FullTimeEmployees.val <- na.omit(CommittedRows$FullTimeEmployees)
NC.FullTimeEmployees.val <- na.omit(NonCommittedRows$FullTimeEmployees)
C.FullTimeEmployees.sd <- sd(CommittedRows$FullTimeEmployees, na.rm = TRUE)
NC.FullTimeEmployees.sd <- sd(NonCommittedRows$FullTimeEmployees, na.rm = TRUE)
FullTimeEmployees.Diff <- z.test(C.FullTimeEmployees.val, NC.FullTimeEmployees.val, alternative = "two.sided", mu = 0, sigma.x = C.FullTimeEmployees.sd, sigma.y = NC.FullTimeEmployees.sd)

# Response variable difference in mean tests
PSI4.diff <- t.test(CommittedRows$PSI_4_SURG_COMP_Score, NonCommittedRows$PSI_4_SURG_COMP_Score, alternative = "two.sided")
PSI6.diff <- t.test(CommittedRows$PSI_6_IAT_PTX_Score, NonCommittedRows$PSI_6_IAT_PTX_Score, alternative = "two.sided")
PSI12.diff <- t.test(CommittedRows$PSI_12_POSTOP_PULMEMB_DVT_Score, NonCommittedRows$PSI_12_POSTOP_PULMEMB_DVT_Score, alternative = "two.sided")
PSI14.diff <- t.test(CommittedRows$PSI_14_POSTOP_DEHIS_Score, NonCommittedRows$PSI_14_POSTOP_DEHIS_Score, alternative = "two.sided")
PSI15.diff <- t.test(CommittedRows$PSI_15_ACC_LAC_Score, NonCommittedRows$PSI_15_ACC_LAC_Score, alternative = "two.sided")
PSI90.diff <- t.test(CommittedRows$PSI_90_SAFETY_Score, NonCommittedRows$PSI_90_SAFETY_Score, alternative = "two.sided")
D2.diff <- t.test(CommittedRows$Domain2_Score, NonCommittedRows$Domain2_Score, alternative = "two.sided")
HAC.diff <- t.test(CommittedRows$Total_HAC_Score, NonCommittedRows$Total_HAC_Score, alternative = "two.sided")

# ANOVA between org control within each group
C.PSI4.aov <- aov(CommittedRows$PSI_4_SURG_COMP_Score ~ CommittedRows$OrgControlOverallNum)
summary(C.PSI4.aov)
C.PSI6.aov <- aov(CommittedRows$PSI_6_IAT_PTX_Score ~ CommittedRows$OrgControlOverallNum)
summary(C.PSI6.aov)
C.PSI12.aov <- aov(CommittedRows$PSI_12_POSTOP_PULMEMB_DVT_Score ~ CommittedRows$OrgControlOverallNum)
summary(C.PSI12.aov)
C.PSI14.aov <- aov(CommittedRows$PSI_14_POSTOP_DEHIS_Score ~ CommittedRows$OrgControlOverallNum)
summary(C.PSI14.aov)
C.PSI15.aov <- aov(CommittedRows$PSI_15_ACC_LAC_Score ~ CommittedRows$OrgControlOverallNum)
summary(C.PSI15.aov)
C.PSI90.aov <- aov(CommittedRows$PSI_90_SAFETY_Score ~ CommittedRows$OrgControlOverallNum)
summary(C.PSI90.aov)
# ANOVA results
C.D2.aov <- aov(CommittedRows$Domain2_Score ~ CommittedRows$OrgControlOverallNum)
summary(C.D2.aov)
C.HAC.aov <- aov(CommittedRows$Total_HAC_Score ~ CommittedRows$OrgControlOverallNum)
summary(C.HAC.aov)
# Tukey results
C.D2.tukey <- TukeyHSD(aov(CommittedRows$Domain2_Score ~ CommittedRows$OrgControlOverallNum))
C.HAC.tukey <- TukeyHSD(aov(CommittedRows$Total_HAC_Score ~ CommittedRows$OrgControlOverallNum))

# ANOVA results
NC.PSI4.aov <- aov(NonCommittedRows$PSI_4_SURG_COMP_Score ~ NonCommittedRows$OrgControlOverallNum)
summary(NC.PSI4.aov)
# Tukey results
NC.PSI4.tukey <- TukeyHSD(aov(NonCommittedRows$PSI_4_SURG_COMP_Score ~ NonCommittedRows$OrgControlOverallNum))

NC.PSI6.aov <- aov(NonCommittedRows$PSI_6_IAT_PTX_Score ~ NonCommittedRows$OrgControlOverallNum)
summary(NC.PSI6.aov)
NC.PSI12.aov <- aov(NonCommittedRows$PSI_12_POSTOP_PULMEMB_DVT_Score ~ NonCommittedRows$OrgControlOverallNum)
summary(NC.PSI12.aov)
NC.PSI14.aov <- aov(NonCommittedRows$PSI_14_POSTOP_DEHIS_Score ~ NonCommittedRows$OrgControlOverallNum)
summary(NC.PSI14.aov)
NC.PSI15.aov <- aov(NonCommittedRows$PSI_15_ACC_LAC_Score ~ NonCommittedRows$OrgControlOverallNum)
summary(NC.PSI15.aov)
NC.PSI90.aov <- aov(NonCommittedRows$PSI_90_SAFETY_Score ~ NonCommittedRows$OrgControlOverallNum)
summary(NC.PSI90.aov)
NC.D2.aov <- aov(NonCommittedRows$Domain2_Score ~ NonCommittedRows$OrgControlOverallNum)
summary(NC.D2.aov)
NC.HANC.aov <- aov(NonCommittedRows$Total_HAC_Score ~ NonCommittedRows$OrgControlOverallNum)
summary(NC.HANC.aov)

#TUKEY RESULT SPECIFIC ANALYSIS (NOT SURE IF THIS IS VALID! THESE COMMANDS ARE MISSING SOME COMPARISONS)
t.test(NonCommitted.OrgControl2$PSI_4_SURG_COMP_Score, NonCommitted.OrgControl3$PSI_4_SURG_COMP_Score, alternative = "greater")
t.test(Committed.OrgControl2$Domain2_Score, Committed.OrgControl2$Domain2_Score, alternative = "greater")

# split data among ownership types within committed vs non-committed
Committed.OrgControl1 <- filter(CommittedRows, CommittedRows$OrgControlOverallNum == "1")
NonCommitted.OrgControl1 <- filter(NonCommittedRows, NonCommittedRows$OrgControlOverallNum == "1")
Committed.OrgControl2 <- filter(CommittedRows, CommittedRows$OrgControlOverallNum == "2")
NonCommitted.OrgControl2 <- filter(NonCommittedRows, NonCommittedRows$OrgControlOverallNum == "2")
Committed.OrgControl3 <- filter(CommittedRows, CommittedRows$OrgControlOverallNum == "3")
NonCommitted.OrgControl3 <- filter(NonCommittedRows, NonCommittedRows$OrgControlOverallNum == "3")

#t-tests
PSI4.diff.org1 <- t.test(Committed.OrgControl1$PSI_4_SURG_COMP_Score, NonCommitted.OrgControl1$PSI_4_SURG_COMP_Score, alternative = "two.sided")
PSI6.diff.org1 <- t.test(Committed.OrgControl1$PSI_6_IAT_PTX_Score, NonCommitted.OrgControl1$PSI_6_IAT_PTX_Score, alternative = "two.sided")
PSI12.diff.org1 <- t.test(Committed.OrgControl1$PSI_12_POSTOP_PULMEMB_DVT_Score, NonCommitted.OrgControl1$PSI_12_POSTOP_PULMEMB_DVT_Score, alternative = "two.sided")
PSI14.diff.org1 <- t.test(Committed.OrgControl1$PSI_14_POSTOP_DEHIS_Score, NonCommitted.OrgControl1$PSI_14_POSTOP_DEHIS_Score, alternative = "two.sided")
PSI15.diff.org1 <- t.test(Committed.OrgControl1$PSI_15_ACC_LAC_Score, NonCommitted.OrgControl1$PSI_15_ACC_LAC_Score, alternative = "two.sided")
PSI90.diff.org1 <- t.test(Committed.OrgControl1$PSI_90_SAFETY_Score, NonCommitted.OrgControl1$PSI_90_SAFETY_Score, alternative = "two.sided")
D2.diff.org1 <- t.test(Committed.OrgControl1$Domain2_Score, NonCommitted.OrgControl1$Domain2_Score, alternative = "two.sided")
HAC.diff.org1 <- t.test(Committed.OrgControl1$Total_HAC_Score, NonCommitted.OrgControl1$Total_HAC_Score, alternative = "two.sided")

#Wilcox
#PSI4.diff.org1 <- wilcox.test(Committed.OrgControl1$PSI_4_SURG_COMP_Score, NonCommitted.OrgControl1$PSI_4_SURG_COMP_Score, alternative = "two.sided")
#var.test(Committed.OrgControl1$PSI_4_SURG_COMP_Score, NonCommitted.OrgControl1$PSI_4_SURG_COMP_Score)
#ansari.test(Committed.OrgControl1$PSI_4_SURG_COMP_Score, NonCommitted.OrgControl1$PSI_4_SURG_COMP_Score, alternative = "two.sided")
#PSI6.diff.org1 <- wilcox.test(Committed.OrgControl1$PSI_6_IAT_PTX_Score, NonCommitted.OrgControl1$PSI_6_IAT_PTX_Score, alternative = "two.sided")
#PSI12.diff.org1 <- wilcox.test(Committed.OrgControl1$PSI_12_POSTOP_PULMEMB_DVT_Score, NonCommitted.OrgControl1$PSI_12_POSTOP_PULMEMB_DVT_Score, alternative = "two.sided")
#PSI14.diff.org1 <- wilcox.test(Committed.OrgControl1$PSI_14_POSTOP_DEHIS_Score, NonCommitted.OrgControl1$PSI_14_POSTOP_DEHIS_Score, alternative = "two.sided")
#PSI15.diff.org1 <- wilcox.test(Committed.OrgControl1$PSI_15_ACC_LAC_Score, NonCommitted.OrgControl1$PSI_15_ACC_LAC_Score, alternative = "two.sided")
#PSI90.diff.org1 <- wilcox.test(Committed.OrgControl1$PSI_90_SAFETY_Score, NonCommitted.OrgControl1$PSI_90_SAFETY_Score, alternative = "two.sided")
#D2.diff.org1 <- wilcox.test(Committed.OrgControl1$Domain2_Score, NonCommitted.OrgControl1$Domain2_Score, alternative = "two.sided")
#HAC.diff.org1 <- wilcox.test(Committed.OrgControl1$Total_HAC_Score, NonCommitted.OrgControl1$Total_HAC_Score, alternative = "two.sided")

PSI4.diff.org1  # DIFFERENCE
PSI6.diff.org1
PSI12.diff.org1
PSI14.diff.org1
PSI15.diff.org1
PSI90.diff.org1
D2.diff.org1
HAC.diff.org1

PSI4.diff.org2 <- t.test(Committed.OrgControl2$PSI_4_SURG_COMP_Score, NonCommitted.OrgControl2$PSI_4_SURG_COMP_Score, alternative = "two.sided")
PSI6.diff.org2 <- t.test(Committed.OrgControl2$PSI_6_IAT_PTX_Score, NonCommitted.OrgControl2$PSI_6_IAT_PTX_Score, alternative = "two.sided")
PSI12.diff.org2 <- t.test(Committed.OrgControl2$PSI_12_POSTOP_PULMEMB_DVT_Score, NonCommitted.OrgControl2$PSI_12_POSTOP_PULMEMB_DVT_Score, alternative = "two.sided")
PSI14.diff.org2 <- t.test(Committed.OrgControl2$PSI_14_POSTOP_DEHIS_Score, NonCommitted.OrgControl2$PSI_14_POSTOP_DEHIS_Score, alternative = "two.sided")
PSI15.diff.org2 <- t.test(Committed.OrgControl2$PSI_15_ACC_LAC_Score, NonCommitted.OrgControl2$PSI_15_ACC_LAC_Score, alternative = "two.sided")
PSI90.diff.org2 <- t.test(Committed.OrgControl2$PSI_90_SAFETY_Score, NonCommitted.OrgControl2$PSI_90_SAFETY_Score, alternative = "two.sided")
D2.diff.org2 <- t.test(Committed.OrgControl2$Domain2_Score, NonCommitted.OrgControl2$Domain2_Score, alternative = "two.sided")
HAC.diff.org2 <- t.test(Committed.OrgControl2$Total_HAC_Score, NonCommitted.OrgControl2$Total_HAC_Score, alternative = "two.sided")

#PSI4.diff.org2 <- wilcox.test(Committed.OrgControl2$PSI_4_SURG_COMP_Score, NonCommitted.OrgControl2$PSI_4_SURG_COMP_Score, alternative = "two.sided")
#PSI6.diff.org2 <- wilcox.test(Committed.OrgControl2$PSI_6_IAT_PTX_Score, NonCommitted.OrgControl2$PSI_6_IAT_PTX_Score, alternative = "two.sided")
#PSI12.diff.org2 <- wilcox.test(Committed.OrgControl2$PSI_12_POSTOP_PULMEMB_DVT_Score, NonCommitted.OrgControl2$PSI_12_POSTOP_PULMEMB_DVT_Score, alternative = "two.sided")
#PSI14.diff.org2 <- wilcox.test(Committed.OrgControl2$PSI_14_POSTOP_DEHIS_Score, NonCommitted.OrgControl2$PSI_14_POSTOP_DEHIS_Score, alternative = "two.sided")
#PSI15.diff.org2 <- wilcox.test(Committed.OrgControl2$PSI_15_ACC_LAC_Score, NonCommitted.OrgControl2$PSI_15_ACC_LAC_Score, alternative = "two.sided")
#PSI90.diff.org2 <- wilcox.test(Committed.OrgControl2$PSI_90_SAFETY_Score, NonCommitted.OrgControl2$PSI_90_SAFETY_Score, alternative = "two.sided")
#D2.diff.org2 <- wilcox.test(Committed.OrgControl2$Domain2_Score, NonCommitted.OrgControl2$Domain2_Score, alternative = "two.sided")
#var.test(Committed.OrgControl2$Domain2_Score, NonCommitted.OrgControl2$Domain2_Score)
#ansari.test(Committed.OrgControl2$Domain2_Score, NonCommitted.OrgControl2$Domain2_Score, alternative = "two.sided")
#HAC.diff.org2 <- wilcox.test(Committed.OrgControl2$Total_HAC_Score, NonCommitted.OrgControl2$Total_HAC_Score, alternative = "two.sided")

PSI4.diff.org2
PSI6.diff.org2
PSI12.diff.org2 #Difference
PSI14.diff.org2
PSI15.diff.org2
PSI90.diff.org2
D2.diff.org2 # DIFFERENCE
HAC.diff.org2

PSI4.diff.org3 <- t.test(Committed.OrgControl3$PSI_4_SURG_COMP_Score, NonCommitted.OrgControl3$PSI_4_SURG_COMP_Score, alternative = "two.sided")
PSI6.diff.org3 <- t.test(Committed.OrgControl3$PSI_6_IAT_PTX_Score, NonCommitted.OrgControl3$PSI_6_IAT_PTX_Score, alternative = "two.sided")
PSI12.diff.org3 <- t.test(Committed.OrgControl3$PSI_12_POSTOP_PULMEMB_DVT_Score, NonCommitted.OrgControl3$PSI_12_POSTOP_PULMEMB_DVT_Score, alternative = "two.sided")
PSI14.diff.org3 <- t.test(Committed.OrgControl3$PSI_14_POSTOP_DEHIS_Score, NonCommitted.OrgControl3$PSI_14_POSTOP_DEHIS_Score, alternative = "two.sided")
PSI15.diff.org3 <- t.test(Committed.OrgControl3$PSI_15_ACC_LAC_Score, NonCommitted.OrgControl3$PSI_15_ACC_LAC_Score, alternative = "two.sided")
PSI90.diff.org3 <- t.test(Committed.OrgControl3$PSI_90_SAFETY_Score, NonCommitted.OrgControl3$PSI_90_SAFETY_Score, alternative = "two.sided")
D2.diff.org3 <- t.test(Committed.OrgControl3$Domain2_Score, NonCommitted.OrgControl3$Domain2_Score, alternative = "two.sided")
HAC.diff.org3 <- t.test(Committed.OrgControl3$Total_HAC_Score, NonCommitted.OrgControl3$Total_HAC_Score, alternative = "two.sided")

#PSI4.diff.org3 <- wilcox.test(Committed.OrgControl3$PSI_4_SURG_COMP_Score, NonCommitted.OrgControl3$PSI_4_SURG_COMP_Score, alternative = "two.sided")
#PSI6.diff.org3 <- wilcox.test(Committed.OrgControl3$PSI_6_IAT_PTX_Score, NonCommitted.OrgControl3$PSI_6_IAT_PTX_Score, alternative = "two.sided")
#PSI12.diff.org3 <- wilcox.test(Committed.OrgControl3$PSI_12_POSTOP_PULMEMB_DVT_Score, NonCommitted.OrgControl3$PSI_12_POSTOP_PULMEMB_DVT_Score, alternative = "two.sided")
#PSI14.diff.org3 <- wilcox.test(Committed.OrgControl3$PSI_14_POSTOP_DEHIS_Score, NonCommitted.OrgControl3$PSI_14_POSTOP_DEHIS_Score, alternative = "two.sided")
#PSI15.diff.org3 <- wilcox.test(Committed.OrgControl3$PSI_15_ACC_LAC_Score, NonCommitted.OrgControl3$PSI_15_ACC_LAC_Score, alternative = "two.sided")
#PSI90.diff.org3 <- wilcox.test(Committed.OrgControl3$PSI_90_SAFETY_Score, NonCommitted.OrgControl3$PSI_90_SAFETY_Score, alternative = "two.sided")
#D2.diff.org3 <- wilcox.test(Committed.OrgControl3$Domain2_Score, NonCommitted.OrgControl3$Domain2_Score, alternative = "two.sided")
#HAC.diff.org3 <- wilcox.test(Committed.OrgControl3$Total_HAC_Score, NonCommitted.OrgControl3$Total_HAC_Score, alternative = "two.sided")

PSI4.diff.org3
PSI6.diff.org3
PSI12.diff.org3
PSI14.diff.org3
PSI15.diff.org3
PSI90.diff.org3
D2.diff.org3
HAC.diff.org3

# summarize results

groupTableHeaders <- c("Ownership Type", "Code", "C($\\mu$/sd/n)", "NC($\\mu$/sd/n)", "t", "p")
#groupTableHeaders <- c("Ownership Type", "Code", "C($\\mu$)", "C(sd)", "C(n)", "NC($\\mu$)", "NC(sd)", "NC(n)", "W", "p")

#r1 <- data.frame(OrgControlOverallNames[1], row.names(measure.names)[1], round(PSI4.diff.org1$estimate[1], 2), round(sd(Committed.OrgControl1$PSI_4_SURG_COMP_Score, na.rm = TRUE), 2), nrow(Committed.OrgControl1), round(PSI4.diff.org1$estimate[2], 2), round(sd(NonCommitted.OrgControl1$PSI_4_SURG_COMP_Score, na.rm = TRUE), 2), nrow(NonCommitted.OrgControl1), round(PSI4.diff.org1$statistic, 2), as.character(round(PSI4.diff.org1$p.value, 10)))

r1 <- data.frame(OrgControlOverallNames[1], row.names(measure.names)[1], paste(round(mean(Committed.OrgControl1$PSI_4_SURG_COMP_Score, na.rm = TRUE), 2), round(sd(Committed.OrgControl1$PSI_4_SURG_COMP_Score, na.rm = TRUE), 2), sum(!is.na(Committed.OrgControl1[, "PSI_4_SURG_COMP_Score"])), sep = "/"), paste(round(mean(NonCommitted.OrgControl1$PSI_4_SURG_COMP_Score, na.rm = TRUE), 2), round(sd(NonCommitted.OrgControl1$PSI_4_SURG_COMP_Score, na.rm = TRUE), 2), sum(!is.na(NonCommitted.OrgControl1[, "PSI_4_SURG_COMP_Score"])), sep = "/"), round(PSI4.diff.org1$statistic, 2), as.character(round(PSI4.diff.org1$p.value, 10)))

rownames(r1) <- 1:nrow(r1)
colnames(r1) <- groupTableHeaders

r2 <- data.frame(OrgControlOverallNames[3], row.names(measure.names)[3], paste(round(PSI12.diff.org2$estimate[1], 2), round(sd(Committed.OrgControl2$PSI_12_POSTOP_PULMEMB_DVT_Score, na.rm = TRUE), 2), sum(!is.na(Committed.OrgControl2[, "PSI_12_POSTOP_PULMEMB_DVT_Score"])), sep = "/"), paste(round(PSI12.diff.org2$estimate[2], 2), round(sd(NonCommitted.OrgControl2$PSI_12_POSTOP_PULMEMB_DVT_Score, na.rm = TRUE), 2), sum(!is.na(NonCommitted.OrgControl2[, "PSI_12_POSTOP_PULMEMB_DVT_Score"])), sep = "/"), round(PSI12.diff.org2$statistic, 2), as.character(round(PSI12.diff.org2$p.value, 3)))

rownames(r2) <- 1:nrow(r2)
colnames(r2) <- groupTableHeaders

r3 <- data.frame(OrgControlOverallNames[3], row.names(measure.names)[7], paste(round(mean(Committed.OrgControl2$Domain2_Score, na.rm = TRUE), 2), round(sd(Committed.OrgControl2$Domain2_Score, na.rm = TRUE), 2), sum(!is.na(Committed.OrgControl2[, "Domain2_Score"])), sep = "/"), paste(round(mean(NonCommitted.OrgControl2$Domain2_Score, na.rm = TRUE), 2), round(sd(NonCommitted.OrgControl2$Domain2_Score, na.rm = TRUE), 2), sum(!is.na(NonCommitted.OrgControl2[, "Domain2_Score"])), sep = "/"), round(D2.diff.org2$statistic, 2), as.character(round(D2.diff.org2$p.value, 3)))

rownames(r3) <- 1:nrow(r3)
colnames(r3) <- groupTableHeaders

groupTable = rbind(r1, r2, r3)
#groupTable = rbind(r1, r3)


rownames(groupTable) <- 1:nrow(groupTable)
colnames(groupTable) <- groupTableHeaders

EligibleForReductionHeaders <- c("Commitment", "Reduction", "No Reduction", "Unknown")

C.Reduction <- CommittedRows %>% filter(Total_HAC_Score > 6.75) %>% summarise(n = n())
C.NoReduction <- CommittedRows %>% filter(Total_HAC_Score <= 6.75) %>% summarise(n = n())
C.Missing <- sum(is.na(CommittedRows[, "Total_HAC_Score"]))
C.E <- data.frame("Committed", C.Reduction, C.NoReduction, C.Missing)
rownames(C.E) <- 1:nrow(C.E)
colnames(C.E) <- EligibleForReductionHeaders

NC.Reduction <- NonCommittedRows %>% filter(Total_HAC_Score > 6.75) %>% summarise(n = n())
NC.NoReduction <- NonCommittedRows %>% filter(Total_HAC_Score <= 6.75) %>% summarise(n = n())
NC.Missing <- sum(is.na(NonCommittedRows[, "Total_HAC_Score"]))
NC.E <- data.frame("Non-committed", NC.Reduction, NC.NoReduction, NC.Missing)
rownames(NC.E) <- 1:nrow(NC.E)
colnames(NC.E) <- EligibleForReductionHeaders

EligibleForReduction <- rbind(C.E, NC.E)

#TESTING STUFF HERE

#ggplot(NonCommittedRows, aes(x = PSI_4_SURG_COMP_Score)) +
#geom_histogram()

#ggplot(CommittedRows, aes(x = OrgControlOverall)) +
    #geom_bar()

## check for normality
##qqPlot(CommittedRows$Domain2_Score)
##boxplot(CommittedRows$Domain2_Score)
#ad.test(CommittedRows$Domain2_Score)
## check for equality of variances
#leveneTest(CommittedRows$Domain2_Score ~ CommittedRows$OrgControlOverallNum)
##ggplot(CommittedRows, aes(x = log(Domain2_Score)) +
##geom_histogram()
## Tranformations
#library(MASS)
#out <- boxcox(CommittedRows$Domain2_Score ~ CommittedRows$OrgControlOverallNum)
#lambda <- round(out$x[which.max(out$y)], 1)
#lambda
## transform data - create new column yt using boxcox lambda
#CommittedRows <- mutate(CommittedRows, Domain2_Score_T = Domain2_Score ^ lambda)
## summarize data by Machine
#CommittedRows %>% group_by(OrgControlOverallNum) %>% summarise(Domain2_Score_T.avg = mean(Domain2_Score_T, na.rm = TRUE), Domain2_Score_T.sd = sd(Domain2_Score_T, na.rm = TRUE))
#CommittedRows %>% group_by(OrgControlOverallNum) %>% summarise(Domain2_Score.avg = mean(Domain2_Score, na.rm = TRUE), Domain2_Score.sd = sd(Domain2_Score, na.rm = TRUE))
## check for normality on transformed data (transformed data is normal)
#C.D2_T.aov <- aov(CommittedRows$Domain2_Score_T ~ CommittedRows$OrgControlOverallNum)
#summary(C.D2_T.aov)
#ad.test(residuals(C.D2_T.aov))
## levene test on transformed data (indicates there is no difference in variance)
#leveneTest(CommittedRows$Domain2_Score_T ~ CommittedRows$OrgControlOverallNum)
## bartlett test on transformed data (indicates there is no difference in variance)
#bartlett.test(CommittedRows$Domain2_Score_T ~ CommittedRows$OrgControlOverallNum)

#ggplot(CommittedRows, aes(x = Total_HAC_Score)) +
#geom_histogram()



## check for normality
#qqPlot(CommittedRows$Total_HAC_Score)
#boxplot(CommittedRows$Total_HAC_Score)
#ad.test(residuals(C.HAC.aov))
#leveneTest(CommittedRows$Total_HAC_Score ~ CommittedRows$OrgControlOverallNum)






