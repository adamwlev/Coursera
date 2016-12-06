setwd("~/Documents/R Programming/ProgrammingAssignment3")
rankall <- function(outcome,num="best") {
	data <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character") 	
	data$State<-as.factor(data$State)
	data[,11]<-as.numeric(data[,11])
	data[,17]<-as.numeric(data[,17])
	data[,23]<-as.numeric(data[,23])

	states<-levels(data$State)
	output<-data.frame(cbind(states),0)
	names(output)<-c("state","hospital")
	outcomes<-c("heart attack","heart failure","pneumonia")
	if (!(outcome %in% outcomes)){
		stop("invalid outcome")
	}
	if(outcome=="heart attack"){
		event<-11
	}
	if(outcome=="heart failure"){
		event<-17
	}
	if(outcome=="pneumonia"){
		event<-23
	}

	
	
	if (num=="best"){
		for (i in 1:length(states)){
			ranking<-data[data$State==states[i],][order(data[data$State==states[i],event],data[data$State==states[i],2]),]
			output[i,2]<-ranking[1,2]
		}
	}
	else if (num=="worst"){
		for (i in 1:length(states)){
			ranking<-data[data$State==states[i],][order(data[data$State==states[i],event],data[data$State==states[i],2]),]
			output[i,2]<-ranking[sum(!is.na(ranking[,event])),2]
		}
	}
	else {
		for (i in 1:length(states)){
			ranking<-data[data$State==states[i],][order(data[data$State==states[i],event],data[data$State==states[i],2]),]
			output[i,2]<-ranking[num,2]
		}	
	}
	return(output)
}