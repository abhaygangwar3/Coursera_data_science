## Write a function that reads a directory full of files and reports the 
## number of completely observed cases in each data file. The function 
## should return a data frame where the first column is the name of the
## file and the second column is the number of complete cases.

complete <- function(directory, id = 1:332)
{
    wd1 <- paste("~/", directory, sep="")
    setwd(wd1)
    complete.vals <- numeric(length(id))
    j <- 1
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
        complete.vals[j] <- sum(complete.cases(data))
        j = j + 1
    }
    df1 <- cbind(id, complete.vals)
    colnames(df1) <- c("id", "nobs")
    df <- as.data.frame(df1)
    df
}