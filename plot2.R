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
#extract two days data
intercept <- data[begin:end,]
da<-intercept[,1]
ti<-intercept[,2]
num <- end-begin+1
dati <- NULL
print(num)
for(x in 1:num)
{
	dati<-c(dati,paste(as.character(da[x]),as.character(ti[x]),sep=" "))
}
e <- as.POSIXct(strptime(dati, format="%d/%m/%Y %H:%M:%S"))
#print(nrow(dati))
options(warn=-1)
active <- intercept[,3]
a<-as.matrix(active)
a <- as.numeric(a)

#print(nrow(a))
#hist(a,col="red",main = "Global Active Power",xlab="Global Active Power(kilowatts)",ylab="Frequency")
#draw chart
plot(e,a,type="l",ylab="Global Active Power(kilowatts)",xlab = "")
dev.copy(png, file = "plot2.png",width =480,height =480)
dev.off()

