#Make sure data os in the same directory 
#as the session directory
data=read.csv("forestfires.csv")

#-------(ii)---------
par(mfrow=c(3,4)) #Allocates plotting space for 
                  #multiple plots at once
hist(data[,1],xlab = names(data)[1],main="")
hist(data[,2],xlab = names(data)[2],main="")
for (ii in 5:length(data)) {
  hist(data[,ii],xlab = names(data)[ii],main="")
}

summary(data)

#-------(iii)---------

correlations=rep(0, 12)

for (ii in 1:(length(data)-1)) {
  if (is.numeric(data[,ii])==TRUE){
    correlations[ii]=cor(data$area,data[,ii])
  }
} 

#-------(iv)---------


par(mfrow=c(3,4))
for (ii in 1:(length(data)-1)) {
  plot(data[,ii],data$area,
       ylab="area",
       xlab = names(data)[ii],main="")
  
  
} 