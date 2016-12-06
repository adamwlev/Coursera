complete<-function(directory,id=1:332){
	directory_andpath<-paste("~/Documents/R Programming/",directory,"/",sep="")	
	output<-data.frame(matrix(NA,nrow=length(id)),ncol=2)
	colnames(output)<-c("id","nobs")
	for(i in 1:length(id)){
		if(id[i]<10){
			file_name<-paste("00",id[i],sep="")
		}
		else if(id[i]<100){
			file_name<-paste("0",id[i],sep="")
		}
		else if(id[i]>=100){
			file_name<-as.character(id[i])
		}
		
		output[i,]<-c(id[i],sum(complete.cases(read.csv(paste(directory_andpath,file_name,".csv",sep="")))))
	}
	output
}