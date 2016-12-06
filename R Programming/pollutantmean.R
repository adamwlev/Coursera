pollutantmean<-function(directory,pollutant,id=1:332){
	list_of_values=c()
	directory_andpath<-paste("~/Documents/R Programming/",directory,"/",sep="")
	for(i in id){
		
		if(i<10){
			file_name<-paste("00",i,sep="")
		}
		else if(i<100){
			file_name<-paste("0",i,sep="")
		}
		else if(i>=100){
			file_name<-as.character(i)
		}
		
		list_of_values<-append(list_of_values,read.csv(paste(directory_andpath,file_name,".csv",sep=""))[[pollutant]])
		
	}
	mean(list_of_values,na.rm=TRUE)
}