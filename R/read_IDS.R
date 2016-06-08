read_IDS <- function(){
    options(stringsAsFactors = FALSE)
    individual <- read.table("data/INDIVIDUAL.txt", header = FALSE)
    names(individual)[1:7] <- c("Id", "Id_D", "Id_I", "Source", "Type", "Value", "Value_Id_C")
    names(individual)[8:10] <- c("Day", "Month", "Year")
    names(individual)[11:13] <- c("Start_day", "Start_month", "Start_year")
    names(individual)[14:16] <- c("End_day", "End_month", "End_year")
    individual
}