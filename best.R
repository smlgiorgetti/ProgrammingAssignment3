best <- function(state, outcome) {
  ## Read outcome data
  
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  ## make tests case insensitive
#   outcome <- tolower(outcome)
#   state <- toupper(state)
  
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

  hospitalList <- data$Hospital.Name[data[,2]==min(data[,2], na.rm=TRUE)]
  
  if (length(hospitalList) > 1) {
    hospitalList <- sort(hospitalList)
    hospitalList <- hospitalList[1]
  }
  return(hospitalList)
}