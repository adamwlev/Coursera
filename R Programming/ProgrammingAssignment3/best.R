setwd("~/Documents/R Programming/ProgrammingAssignment3")
best <- function(state, outcome) {
	data <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character") 	
	data$State<-as.factor(data$State)
	data[,11]<-as.numeric(data[,11])
	data[,17]<-as.numeric(data[,17])
	data[,23]<-as.numeric(data[,23])

	
	## Check that state and outcome are valid
	states<-levels(data$State)
	if (!(state %in% states)){
		stop("invalid state")
	}
	outcomes<-c("heart attack","heart failure","pneumonia")
	if (!(outcome %in% outcomes)){
		stop("invalid outcome")
	}
	if(outcome=="heart attack"){
		output<-data[data$State==state,][order(data[data$State==state,11],data[data$State==state,2]),][1,2]
	}
	if(outcome=="heart failure"){
		output<-data[data$State==state,][order(data[data$State==state,17],data[data$State==state,2]),][1,2]	
	}
	if(outcome=="pneumonia"){
		output<-data[data$State==state,][order(data[data$State==state,23],data[data$State==state,2]),][1,2]
	}
		
	return(output)
}