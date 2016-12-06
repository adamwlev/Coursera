setwd("~/Documents/R Programming/ProgrammingAssignment3")
best <- function(state, outcome) {
	data <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character") 	
	data$State<-as.factor(data$State)
	
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
		best<-data[data$State==state,11][rank(data[data$State==state,11],ties.method="max")==min(rank(data[data$State==state,11],ties.method="max"))]
		output<-sort(data[data$State==state,][data[data$State==state,11]==best,2])[1]
	}
	if(outcome=="heart failure"){
		best<-data[data$State==state,17][rank(data[data$State==state,17],ties.method="max")==min(rank(data[data$State==state,17],ties.method="max"))]
		output<-sort(data[data$State==state,][data[data$State==state,17]==best,2])[1]
	}
	if(outcome=="heart failure"){
		best<-data[data$State==state,23][rank(data[data$State==state,23],ties.method="max")==min(rank(data[data$State==state,23],ties.method="max"))]
		output<-sort(data[data$State==state,][data[data$State==state,23]==best,2])[1]
	}
		
	return(output)
}