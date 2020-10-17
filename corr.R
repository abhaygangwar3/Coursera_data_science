## Write a function that takes a directory of data files and a threshold
## for complete cases and calculates the correlation between sulfate and
## nitrate for monitor locations where the number of completely observed 
## cases (on all variables) is greater than the threshold. The function 
## should return a vector of correlations for the monitors that meet the 
## threshold requirement. If no monitors meet the threshold requirement, 
## then the function should return a numeric vector of length 0.

corr <- function(directory, threshold = 0)
{
    wd1 <- paste("~/", directory, sep="")
    setwd(wd1)
    id = 1:332
    df <- data.frame()
    corr_val <- numeric()
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
        complete.vals <- sum(complete.cases(data))
        if(complete.vals > threshold)
        {  
            corr_val <- c(corr_val, cor(data$nitrate[complete.cases(data)], data$sulfate[complete.cases(data)]))
        }
    }
    corr_val
}