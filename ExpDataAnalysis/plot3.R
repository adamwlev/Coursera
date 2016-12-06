data<-read.table("household_power_consumption.txt",sep=";",header=TRUE)
data1<-subset(data,Date=="1/2/2007"| Date =="2/2/2007")
png("plot3.png")
datetime<-numeric()
for (i in 1:nrow(data1)){
	datetime<-append(datetime,paste(c(as.character(data1$Date[i]),as.character(data1$Time[i])),collapse=":"))
}
datetime1<-strptime(datetime,'%d/%m/%Y:%H:%M:%S')
plot(datetime1,as.numeric(as.character(data1$Sub_metering_1)),type="n",ylab="Energy sub metering")
lines(datetime1,as.numeric(as.character(data1$Sub_metering_1)))
lines(datetime1,as.numeric(as.character(data1$Sub_metering_2)),col="red")
lines(datetime1,as.numeric(as.character(data1$Sub_metering_3)),col="blue")
legend("topright",c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lty=c(1,1,1),lwd=c(2.5,2.5,2.5),col=c("black","red","blue"))
dev.off()