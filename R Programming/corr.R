corr<-function(directory,threshold=0){
	directory_andpath<-paste("~/Documents/R Programming/",directory,"/",sep="")	
	correlations<-numeric(length=0)
	for(i in 1:332){
		if(i<10){
			file_name<-paste("00",i,sep="")
		}
		else if(i<100){
			file_name<-paste("0",i,sep="")
		}
		else if(i>=100){
			file_name<-as.character(i)
		}
		
		data<-read.csv(paste(directory_andpath,file_name,".csv",sep=""))
		data<-data[complete.cases(data),]
		if(sum(complete.cases(data))>threshold){
			correlations<-append(correlations,cor(data$sulfate,data$nitrate))
		}
	}
	correlations
}