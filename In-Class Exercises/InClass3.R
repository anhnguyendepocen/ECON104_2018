require(stargazer)


data=read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/communities/communities.data"
              ,header=FALSE)
data_subset=data[,c(7,14,17,22,26,38,127,128)]
colnames(data_subset)= c("hhsize","age1624",
                         "urban","socsec",
                         "percapIncome","unemp",
                         "policebudget","crime")

data_subset[data_subset[,7]=="?",7]=NA
data_subset[,7]=as.numeric(data_subset[,7])


ttest95=matrix(NA,2,length(data_subset)-1)
CI95LB=matrix(NA,2,length(data_subset)-1)
CI95UB=matrix(NA,2,length(data_subset)-1)

colnames(ttest95)=colnames(data_subset[-1])
colnames(CI95LB)=colnames(data_subset[-1])
colnames(CI95UB)=colnames(data_subset[-1])

for (jj in 1:(length(data_subset)-1)){
  reg0=lm(data_subset$crime~data_subset[,jj])
  reg=summary(reg0)
  
  ttest95[,jj]=reg$coefficients[,3]>=1.96
  CI95LB[,jj]=reg$coefficients[,1]-1.96*reg$coefficients[,2]
  CI95UB[,jj]=reg$coefficients[,1]+1.96*reg$coefficients[,2]
  
  names(reg0$coefficients)[2]=colnames(data_subset)[jj]
  name <- paste("reg", jj, sep = "")
  assign(name, reg0)
  
  
}

stargazer(reg1,reg2,reg3,reg4,reg5,reg6,reg7,type="text")
