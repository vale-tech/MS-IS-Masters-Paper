
# Create summary statisics
output <- descr(hospitalData$NofBeds, style = 'rmarkdown', transpose = TRUE)
output$stats[, 6:11] <- list(NULL)
attr(output$stats, "row.names") <- "Number of Beds"
