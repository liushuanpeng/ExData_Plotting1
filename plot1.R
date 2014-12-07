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
#find two days data
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
options(warn=-1)
#clear up data
z <- intercept[,3]
a<-as.matrix(z)
a <- as.numeric(a)
#draw chart
hist(a,col="red",main = "Global Active Power",xlab="Global Active Power(kilowatts)",ylab="Frequency")
dev.copy(png, file = "plot1.png",width =480,height =480)
dev.off()
