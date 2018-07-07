data=read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/communities/communities.data"
              ,header=FALSE)
data_subset=data[,c(7,14,17,22,26,38,127,128)]
colnames(data_subset)= c("hhsize","age1624",
                         "urban","socsec",
                         "percapIncome","unemp",
                         "policebudget","crime")

data_subset[data_subset[,7]=="?",7]=NA
data_subset[,7]=as.numeric(data_subset[,7])

par(mfrow=c(3,3))
for (ii in 1:length(data_subset)){
  hist(data_subset[,ii]
       ,xlab = names(data_subset)[ii],main="")
}

correlations=rep(0, length(data_subset)-1)

for (ii in 1:(length(data_subset)-1)) {
    correlations[ii]=cor(data_subset[,8],data_subset[,ii]
                         , use = "complete.obs")
} 