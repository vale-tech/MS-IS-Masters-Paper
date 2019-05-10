# Data Exploration for Project
# Working through STAT 757 Project List
myData.Predictors <- hospitalData %>% select(ISPlanCommittedToReduceMedicalErrors, NofBeds, NofSurgicalOperations, FullTimeEmployees, OrgControlOverall, OrgControlDetail)
myData.Response <- hospitalData %>% select(Domain2_Score, Total_HAC_Score, PSI_12_POSTOP_PULMEMB_DVT_Score, PSI_14_POSTOP_DEHIS_Score, PSI_15_ACC_LAC_Score, PSI_4_SURG_COMP_Score, PSI_6_IAT_PTX_Score, PSI_90_SAFETY_Score)

myData.Predictors.Summary <- dfSummary(myData.Predictors, style = "grid")
myData.Response.Summary <- dfSummary(myData.Response, style = "grid")

# appropriate plots
# Predictors
ggplot(myData.Predictors, aes(x = ISPlanCommittedToReduceMedicalErrors)) +
    geom_bar()
ggplot(myData.Predictors, aes(x = NofBeds)) +
    geom_histogram()
ggplot(myData.Predictors, aes(x = NofSurgicalOperations)) +
    geom_histogram()
ggplot(myData.Predictors, aes(x = FullTimeEmployees)) +
    geom_histogram()
ggplot(myData.Predictors, aes(x = OrgControlOverall)) +
    geom_bar()
ggplot(myData.Predictors, aes(x = OrgControlDetail)) +
    geom_bar()

# Response
ggplot(myData.Response, aes(x = Domain2_Score)) +
    geom_histogram()
ggplot(myData.Response, aes(x = Total_HAC_Score)) +
    geom_histogram()
ggplot(myData.Response, aes(x = PSI_12_POSTOP_PULMEMB_DVT_Score)) +
    geom_histogram()
ggplot(myData.Response, aes(x = PSI_14_POSTOP_DEHIS_Score)) +
    geom_histogram()
ggplot(myData.Response, aes(x = PSI_15_ACC_LAC_Score)) +
    geom_histogram()
ggplot(myData.Response, aes(x = PSI_4_SURG_COMP_Score)) +
    geom_histogram()
ggplot(myData.Response, aes(x = PSI_6_IAT_PTX_Score)) +
    geom_histogram()
ggplot(myData.Response, aes(x = PSI_90_SAFETY_Score)) +
    geom_histogram()

t.test(myData.Predictors$NofBeds)
t.test(myData.Predictors$NofSurgicalOperations)
t.test(myData.Predictors$FullTimeEmployees)
t.test(myData.Response$Domain2_Score)
t.test(myData.Response$Total_HAC_Score)
t.test(myData.Response$PSI_12_POSTOP_PULMEMB_DVT_Score)
t.test(myData.Response$PSI_14_POSTOP_DEHIS_Score)
t.test(myData.Response$PSI_15_ACC_LAC_Score)
t.test(myData.Response$PSI_4_SURG_COMP_Score)
t.test(myData.Response$PSI_6_IAT_PTX_Score)
t.test(myData.Response$PSI_90_SAFETY_Score)



#ISPlanCommittedToReduceMedicalErrors
x <- filter(hospitalData, hospitalData$ISPlanCommittedToReduceMedicalErrors == "1")
y <- filter(hospitalData, hospitalData$ISPlanCommittedToReduceMedicalErrors == "0")

t.test(x$NofBeds, y$NofBeds)
t.test(x$NofSurgicalOperations, y$NofSurgicalOperations)
t.test(x$FullTimeEmployees, y$FullTimeEmployees)

t.test(x$Domain2_Score, y$Domain2_Score)
t.test(x$Total_HAC_Score, y$Total_HAC_Score)
t.test(x$PSI_12_POSTOP_PULMEMB_DVT_Score, y$PSI_12_POSTOP_PULMEMB_DVT_Score)
t.test(x$PSI_14_POSTOP_DEHIS_Score, y$PSI_14_POSTOP_DEHIS_Score)
t.test(x$PSI_15_ACC_LAC_Score, y$PSI_15_ACC_LAC_Score)
t.test(x$PSI_4_SURG_COMP_Score, y$PSI_4_SURG_COMP_Score)
t.test(x$PSI_6_IAT_PTX_Score, y$PSI_6_IAT_PTX_Score)
t.test(x$PSI_90_SAFETY_Score, y$PSI_90_SAFETY_Score)

mean(x$PSI_90_SAFETY_Score)
sd(x$PSI_90_SAFETY_Score)

t.test(x$Domain2_Score, y$Domain2_Score)
t.test(x$Total_HAC_Score, y$Total_HAC_Score)
t.test(x$PSI_12_POSTOP_PULMEMB_DVT_Score, y$PSI_12_POSTOP_PULMEMB_DVT_Score)
t.test(x$PSI_14_POSTOP_DEHIS_Score, y$PSI_14_POSTOP_DEHIS_Score)
t.test(x$PSI_15_ACC_LAC_Score, y$PSI_15_ACC_LAC_Score)
t.test(x$PSI_4_SURG_COMP_Score, y$PSI_4_SURG_COMP_Score)
t.test(x$PSI_6_IAT_PTX_Score, y$PSI_6_IAT_PTX_Score)
t.test(x$PSI_90_SAFETY_Score, y$PSI_90_SAFETY_Score)

x.1 <- filter(x, x$OrgControlOverallNum == "1")
y.1 <- filter(y, y$OrgControlOverallNum == "1")

t.test(x.1$Domain2_Score, y.1$Domain2_Score)
t.test(x.1$Total_HAC_Score, y.1$Total_HAC_Score)
t.test(x.1$PSI_12_POSTOP_PULMEMB_DVT_Score, y.1$PSI_12_POSTOP_PULMEMB_DVT_Score)
t.test(x.1$PSI_14_POSTOP_DEHIS_Score, y.1$PSI_14_POSTOP_DEHIS_Score)
t.test(x.1$PSI_15_ACC_LAC_Score, y.1$PSI_15_ACC_LAC_Score)
t.test(x.1$PSI_4_SURG_COMP_Score, y.1$PSI_4_SURG_COMP_Score)
t.test(x.1$PSI_6_IAT_PTX_Score, y.1$PSI_6_IAT_PTX_Score)
t.test(x.1$PSI_90_SAFETY_Score, y.1$PSI_90_SAFETY_Score)

x.2 <- filter(x, x$OrgControlOverallNum == "2")
y.2 <- filter(y, y$OrgControlOverallNum == "2")

t.test(x.2$Domain2_Score, y.2$Domain2_Score)
t.test(x.2$Total_HAC_Score, y.2$Total_HAC_Score)
t.test(x.2$PSI_12_POSTOP_PULMEMB_DVT_Score, y.2$PSI_12_POSTOP_PULMEMB_DVT_Score)
t.test(x.2$PSI_14_POSTOP_DEHIS_Score, y.2$PSI_14_POSTOP_DEHIS_Score)
t.test(x.2$PSI_15_ACC_LAC_Score, y.2$PSI_15_ACC_LAC_Score)
t.test(x.2$PSI_4_SURG_COMP_Score, y.2$PSI_4_SURG_COMP_Score)
t.test(x.2$PSI_6_IAT_PTX_Score, y.2$PSI_6_IAT_PTX_Score)
t.test(x.2$PSI_90_SAFETY_Score, y.2$PSI_90_SAFETY_Score)

x.3 <- filter(x, x$OrgControlOverallNum == "3")
y.3 <- filter(y, y$OrgControlOverallNum == "3")

t.test(x.3$Domain2_Score, y.3$Domain2_Score)
t.test(x.3$Total_HAC_Score, y.3$Total_HAC_Score)
t.test(x.3$PSI_12_POSTOP_PULMEMB_DVT_Score, y.3$PSI_12_POSTOP_PULMEMB_DVT_Score)
t.test(x.3$PSI_14_POSTOP_DEHIS_Score, y.3$PSI_14_POSTOP_DEHIS_Score)
t.test(x.3$PSI_15_ACC_LAC_Score, y.3$PSI_15_ACC_LAC_Score)
t.test(x.3$PSI_4_SURG_COMP_Score, y.3$PSI_4_SURG_COMP_Score)
t.test(x.3$PSI_6_IAT_PTX_Score, y.3$PSI_6_IAT_PTX_Score)
t.test(x.3$PSI_90_SAFETY_Score, y.3$PSI_90_SAFETY_Score)


#OrgControlOverall
view(dfSummary(hospitalData))
x <- subset(hospitalData, select = c("OrgControlOverall", "Domain2_Score", "Total_HAC_Score", "PSI_12_POSTOP_PULMEMB_DVT_Score", "PSI_14_POSTOP_DEHIS_Score", "PSI_15_ACC_LAC_Score", "PSI_4_SURG_COMP_Score", "PSI_6_IAT_PTX_Score", "PSI_90_SAFETY_Score"))
#Domain2_Score
aov.out <- aov(x$Domain2_Score ~ x$OrgControlOverall)
summary(aov.out)
#Total_HAC_Score 
aov.out <- aov(x$Total_HAC_Score ~ x$OrgControlOverall)
summary(aov.out)
#PSI_12_POSTOP_PULMEMB_DVT_Score 
aov.out <- aov(x$PSI_12_POSTOP_PULMEMB_DVT_Score ~ x$OrgControlOverall)
summary(aov.out)
#PSI_14_POSTOP_DEHIS_Score 
aov.out <- aov(x$PSI_14_POSTOP_DEHIS_Score ~ x$OrgControlOverall)
summary(aov.out)
#PSI_15_ACC_LAC_Score 
aov.out <- aov(x$PSI_15_ACC_LAC_Score ~ x$OrgControlOverall)
summary(aov.out)
#PSI_4_SURG_COMP_Score
aov.out <- aov(x$PSI_4_SURG_COMP_Score ~ x$OrgControlOverall)
summary(aov.out)
#PSI_6_IAT_PTX_Score 
aov.out <- aov(x$PSI_6_IAT_PTX_Score ~ x$OrgControlOverall)
summary(aov.out)
#PSI_90_SAFETY_Score 
aov.out <- aov(x$PSI_90_SAFETY_Score ~ x$OrgControlOverall)
summary(aov.out)

#OrgControlDetail 
view(dfSummary(hospitalData))
x <- subset(hospitalData, select = c("OrgControlDetail", "Domain2_Score", "Total_HAC_Score", "PSI_12_POSTOP_PULMEMB_DVT_Score", "PSI_14_POSTOP_DEHIS_Score", "PSI_15_ACC_LAC_Score", "PSI_4_SURG_COMP_Score", "PSI_6_IAT_PTX_Score", "PSI_90_SAFETY_Score"))
#Domain2_Score
aov.out <- aov(x$Domain2_Score ~ x$OrgControlDetail )
summary(aov.out)
#Total_HAC_Score 
aov.out <- aov(x$Total_HAC_Score ~ x$OrgControlDetail )
summary(aov.out)
#PSI_12_POSTOP_PULMEMB_DVT_Score 
aov.out <- aov(x$PSI_12_POSTOP_PULMEMB_DVT_Score ~ x$OrgControlDetail )
summary(aov.out)
#PSI_14_POSTOP_DEHIS_Score 
aov.out <- aov(x$PSI_14_POSTOP_DEHIS_Score ~ x$OrgControlDetail )
summary(aov.out)
#PSI_15_ACC_LAC_Score 
aov.out <- aov(x$PSI_15_ACC_LAC_Score ~ x$OrgControlDetail )
summary(aov.out)
#PSI_4_SURG_COMP_Score
aov.out <- aov(x$PSI_4_SURG_COMP_Score ~ x$OrgControlDetail )
summary(aov.out)
#PSI_6_IAT_PTX_Score 
aov.out <- aov(x$PSI_6_IAT_PTX_Score ~ x$OrgControlDetail )
summary(aov.out)
#PSI_90_SAFETY_Score 
aov.out <- aov(x$PSI_90_SAFETY_Score ~ x$OrgControlDetail )
summary(aov.out)

# Misc
# Vizualiztion
ggplot(CommittedRows, aes(x = NofBeds)) +
    geom_histogram()

# bar chart for beds
ggplot(data = hospitalData, aes(x = PlanToReduceErrors, y = NofBeds, fill = OrgControlOverall)) +
    geom_bar(stat = "summary", fun.y = "mean", position = position_dodge(), colour = "black")

z.test(CommittedRows$NofBeds, y = NonCommittedRows$NofBeds, sigma.x = sd(CommittedRows$NofBeds), sigma.y = sd(NonCommittedRows$NofBeds))
t.test(CommittedRows$NofBeds, NonCommittedRows$NofBeds)