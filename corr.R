corr <- function(directory, threshold = 0) {
  setwd(file.path(getwd(), directory)) ## setting the directory
  
  correlationVector = NULL ## initializing the correlation matrix
  
  #Looping thru ALL the directory's files 
  for (i in 1:332)
  {
    
    
    
    if (i <10) { 
      data <- read.csv(
        paste("0","0", as.character(i), ".csv", sep=""),  ## for example, if 'id' =7, we get 007.csv
        header = T, 
        na.strings=c("NA","NaN", " ")
        
      )
    }
    
    else if (i>=10 & i<100) { 
      data <- read.csv(
        paste("0", as.character(i), ".csv", sep=""),  ## for example, if 'id' = 17, we get 017.csv
        header = T, 
        na.strings=c("NA","NaN", " ") 
        
      )
    }
    
    
    
    else       { 
      data <- read.csv(
        paste(as.character(i), ".csv", sep=""),     ## Normal
        header = T, 
        na.strings=c("NA","NaN", " ") 
        
      )
    }
    
    ## getting rid of all the "NA" values and, consequently, all the non-complete ovservations (the ones with at least one NA in row)
    data = na.omit(data) 
    
    ## if the number of complete observed cases meets the quota, find the correlation between the pollutants for the given monitor AND
    ## store the results in the correlation matrix
    if (nrow(data) > threshold) {
      correlationVector = c(correlationVector, cor(data[,2], data[,3]))
    }
    
    
  }
  
  
  
  setwd("..")  # reseting working directory path
  return (correlationVector)
}
cr<- corr("specdata",150)
head(cr); summary(cr)
cr <- corr("specdata", 400)
head(cr); summary(cr)
cr <- corr("specdata", 5000)
summary(cr); length(cr)
cr <- corr("specdata")
summary(cr); length(cr)