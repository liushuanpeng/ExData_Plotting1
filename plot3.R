#load data
data<-read.table("household_power_consumption.txt",sep=";")
#change colname
colname <- c()

for(x in data[1,])
{
	colname = c(colname ,as.character(x)) 
}
colnames(data)<-colname
data <- data[-1,]
data1 <- data[,1]
i <- 1
#result <- 0
#find tow days data
for(x in data1)
{
	x <- as.Date(x, "%d/%m/%Y")
		
	if(x == "2007-01-31")
	{
		begin <- i+1
	}
	if(x == "2007-02-02")
	{
		end <- i
	}
	i <- i+1
	
}
#print(begin)
#print(end)
intercept <- data[begin:end,]
#extract three row data
s1<-intercept$Sub_metering_1
s2<-intercept$Sub_metering_2
s3<-intercept$Sub_metering_3
#draw graphics
plot(e,s1,type="l",xlab="",ylab="Energy sub metering")
lines(e,s2, type="l", pch=22, lty=1, col="red")
lines(e,s3, type="l", pch=22, lty=1, col="blue")
legend("topright", c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), cex=0.8, col=c("black","red","blue"),  lty=1)
#save chart
dev.copy(png, file = "plot3.png",width =480,height =480,bg="white")
dev.off()
