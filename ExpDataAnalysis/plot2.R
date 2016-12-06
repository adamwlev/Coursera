data<-read.table("household_power_consumption.txt",sep=";",header=TRUE)
data1<-subset(data,Date=="1/2/2007"| Date =="2/2/2007")
png("plot2.png")
datetime<-numeric()
for (i in 1:nrow(data1)){
	datetime<-append(datetime,paste(c(as.character(data1$Date[i]),as.character(data1$Time[i])),collapse=":"))
}
datetime1<-strptime(datetime,'%d/%m/%Y:%H:%M:%S')
plot(datetime1,data1$Global_active_power,type="n")
lines(datetime1,data1$Global_active_power)
dev.off()