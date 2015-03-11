rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  validOutcomes <- c("heart attack","heart failure","pneumonia")
  if (! outcome %in% validOutcomes)
    stop("invalid outcome")
  
  validStates <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS",
                   "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC",
                   "ND", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VI", "VA", "WA", "WV",
                   "WI", "WY", "GU")
  if (! state %in% validStates)
    stop("invalid state")
  
  data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  if (outcome == "heart attack")
    #data <- data[data$State==state,c(2,11,17,23)]
    data <- data[data$State==state,c(2,11)]
  else if (outcome == "heart failure")
    data <- data[data$State==state,c(2,17)]
  else #pneumonia
    data <- data[data$State==state,c(2,23)]
  
  suppressWarnings(data[,2] <- as.numeric(data[,2]))
  data <- data[complete.cases(data),]
  data <- data[order(data[,2], data[,1], na.last=NA),]
  data[, "Rank"] <- 1:dim(data)[1]
  
  idx = 100000
  if (num == "best")
    idx <- 1
  else if (num == "worst")
    idx <- dim(data)[1]
  else
    idx <- num
  
  return(data[idx,1])
  
}
