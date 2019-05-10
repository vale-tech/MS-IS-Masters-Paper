dat1 <- data.frame(
    sex = factor(c("Female", "Female", "Male", "Male")),
    time = factor(c("Lunch", "Dinner", "Lunch", "Dinner"), levels = c("Lunch", "Dinner")),
    total_bill = c(13.53, 16.81, 16.24, 17.42)
)
dat1

# Bar graph, time on x-axis, color fill grouped by sex -- use position_dodge()
ggplot(data = dat1, aes(x = time, y = total_bill, fill = sex)) +
geom_bar(stat = "identity", position = position_dodge())

ggplot(data = dat1, aes(x = time, y = total_bill)) +
    geom_bar(stat = "identity", position = position_dodge(), colour = "black")

ggplot(hospitalData, aes(x = NofBedsCategory, fill = PlanToReduceErrors)) +
geom_bar()

ggplot(hospitalData, aes(x = OrgControlOverall)) +
geom_bar()

ggplot(hospitalData, aes(x = OrgControlOverallNum)) +
geom_bar()

ggplot(Committed.OrgControl1, aes(x = PSI_4_SURG_COMP_Score)) +
geom_histogram()

ggplot(NonCommitted.OrgControl1, aes(x = PSI_4_SURG_COMP_Score)) +
geom_histogram()

median(Committed.OrgControl1$PSI_4_SURG_COMP_Score, na.rm = TRUE)
median(NonCommitted.OrgControl1$PSI_4_SURG_COMP_Score, na.rm = TRUE)
