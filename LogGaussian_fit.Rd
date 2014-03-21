# Change the csv file names
# line 5
# and  6

loaddata="peak3.csv"
savedata="LogGausFitOut2.csv"

library("MASS")
library("fitdistrplus")




fLogGaussian <- function(x, a, b, c) {
	a*exp(-0.5*(log(x/b)/c)^2)
}

mydata = read.csv(loaddata)

out= matrix(0,3,ncol(mydata)-1)
x2 <- (11:10000)/1000

plot.new()
par(mfrow=c(1,(ncol(mydata)-1)))

i=1
while (i < ncol(mydata)) {
y <- mydata[[i+1]]
x <- mydata[[1]]
fit <- nls(y~fLogGaussian(x,a,b,c), data=data.frame(x,y), start=list(a=.3, b=.3, c=0.3))
fit 

plot(x,y,col="black", log="x")
d=coef(fit)
amplitude = (d[1])
center = (d[2])
width = (d[3])
lines(x2,fLogGaussian(x2,amplitude,center,width),col="red")
out[,i]=t(c(amplitude,center, width))
i = i+1
}

out2 = data.frame(out);
for (i in 1:(ncol(mydata)-1)){
names(out2)[i]=names(mydata)[i+1]
}


row.names(out2)[1]="amplitude"
row.names(out2)[2]="center"
row.names(out2)[3]="width"


write.csv(out2, file = savedata)

