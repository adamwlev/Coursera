setwd("~/Documents/R Programming/ProgrammingAssignment3")
rankhospital <- function(state, outcome,num="best") {
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
		ranking<-data[data$State==state,][order(data[data$State==state,11],data[data$State==state,2]),]
	}
	if(outcome=="heart failure"){
		ranking<-data[data$State==state,][order(data[data$State==state,17],data[data$State==state,2]),]	
	}
	if(outcome=="pneumonia"){
		ranking<-data[data$State==state,][order(data[data$State==state,23],data[data$State==state,2]),]
	}

	if (num=="best"){
		return(ranking[1,2])
	}
	else if (num=="worst"){
		return(ranking[sum(!is.na(ranking[,11])),2])
	}
	else {
		return(ranking[num,2])
	}	
	
}