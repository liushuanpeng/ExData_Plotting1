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
par(mfrow = c(2, 2))#define chart's format
#draw 4 chart
active <- intercept[,3]
a<-as.matrix(active)
a <- as.numeric(a)
plot(e,a,type="l",ylab="Global Active Power(kilowatts)",xlab = "")

vol <- intercept[,5]
b<-as.matrix(vol)
b <- as.numeric(b)
plot(e,b,type="l",ylab="Voltage",xlab = "")

s1<-intercept[,7]
s2<-intercept[,8]
s3<-intercept[,9]

plot(e,s1,type="l",xlab="",ylab="Energy sub metering")
lines(e,s2, type="l", pch=22, lty=1, col="red")
lines(e,s3, type="l", pch=22, lty=1, col="blue")
legend("topright", c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), cex=0.8, col=c("black","red","blue"),  lty=1)

vol <- intercept[,4]
d<-as.matrix(vol)
d <- as.numeric(d)
plot(e,d,type="l",xlab="",ylab="Global_reactive_power")
#save chart
dev.copy(png, file = "plot4.png",width =480,height =480,bg="white")
dev.off()

