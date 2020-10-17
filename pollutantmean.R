##Write a function named 'pollutantmean' that calculates the mean of
##a pollutant (sulfate or nitrate) across a specified list of monitors.
##The function 'pollutantmean' takes three arguments: 'directory', 
##pollutant', and 'id'. Given a vector monitor ID numbers, 'pollutant mean'
##reads that monitors' particulate matter data from the directory 
##specified in the 'directory' argument and returns the mean of the pollutant
##across all of the monitors, ignoring any missing values coded as NA.



pollutantmean <- function(directory, pollutant, id = 1:332)
{
    wd1 <- paste("~/", directory, sep="")
    setwd(wd1)
    means <- numeric(length(id))
    mat <- numeric()
    for(i in id)
    {
        if(i < 10)
        {
            data <- read.csv(file = paste("00", i, ".csv", sep = ""))
        }
        else if(i < 100)
        {
            data <- read.csv(file = paste("0", i, ".csv", sep = ""))
        }
        else
        {
            data <- read.csv(file = paste(i, ".csv", sep = ""))
        }
        if(pollutant == "sulfate")
        {
            mat <- c(mat, data$sulfate)
        }
        else
        {
            mat <- c(mat, data$nitrate)
        }
        
    }
    mean_final <- mean(mat, na.rm = TRUE)
    mean_final
}