# Base on Google research the range of Palmitoleic roughly from 30 to 350, becuase this dataset
# from Italy, the variance will be smaller than global variance, and we only estimate the mean 
# of Palmitoleic from Italian olive oil, so guess the variance = 169, and setting the marginal
# error to be 2, so for 95% confidence interval, we need to choose n=162 data from the dataset.

##########################################################################################
# SRS technique to estimate the variable Palmitoleic mean and its 95% confidence interval#
##########################################################################################
data(olive, package="pgmm")
pop.m <- mean(olive$Palmitoleic)
pop.m

pop.var <-1.96*var(olive$Palmitoleic)/nrow(olive)
pop.var

library(sampling)
set.seed(2345) # in order for grader get the same answer as I do

# Determination of sample size
ci = 0.95
E = 2
guess.var = 13^2
z.score = qnorm(1-(1-ci)/2)

# Our sample size
n = round(z.score^2*guess.var/E^2)
n

# Total observation
N = nrow(olive)
N

swo=srswor(n=n,N=N) # Randomly draw a sample size of n from N obervations
samplewo=olive[swo==1,] #Randomly draw a sample from dataset olive of size n
Mu=mean(samplewo$Palmitoleic) #calculate the mean of variable Palmitoleic
S2=var(samplewo$Palmitoleic) # calculate the standard deviation of my sample variable Palmitoleic
Var=S2*(1-n/N)/n
Var
# using correction factor to calculate sample standard error of variable Palmitoleic
SE=sqrt(Var)

#variance 
SE^2

#calculate 95% CI for the mean of variable Palmitoleic
upper=Mu+z.score*SE
lower=Mu-z.score*SE

cat("The estimated mean of varialbe Palmitoleic is:",Mu)
cat("The of 95% confidence interval for the mean of Palmitoleic is:","[",lower,",",upper,"]",sep="")


#########################################
#Stratified with proportional allocation#
#########################################

# Using Area 1,2,3,...9 as my strata, calculate small n and big N respectively
set.seed(2345)
data(olive,package="pgmm")

# Determination of sample size
ci = 0.95
E = 2
guess.var = 13^2
z.score = qnorm(1-(1-ci)/2)

# Our sample size
n = round(z.score^2*guess.var/E^2)
n

# Total observation
N = nrow(olive)
N

#using proportional allocation to calculate ni
n1=round(nrow(olive[olive$Area==1,])*n/N)
n2=round(nrow(olive[olive$Area==2,])*n/N)
n3=round(nrow(olive[olive$Area==3,])*n/N)
n4=round(nrow(olive[olive$Area==4,])*n/N)
n5=round(nrow(olive[olive$Area==5,])*n/N)
n6=round(nrow(olive[olive$Area==6,])*n/N)
n7=round(nrow(olive[olive$Area==7,])*n/N)
n8=round(nrow(olive[olive$Area==8,])*n/N)+1
n9=round(nrow(olive[olive$Area==9,])*n/N)+1
N1=nrow(olive[olive$Area==1,])
N2=nrow(olive[olive$Area==2,])
N3=nrow(olive[olive$Area==3,])
N4=nrow(olive[olive$Area==4,])
N5=nrow(olive[olive$Area==5,])
N6=nrow(olive[olive$Area==6,])
N7=nrow(olive[olive$Area==7,])
N8=nrow(olive[olive$Area==8,])
N9=nrow(olive[olive$Area==9,])

str1=srswor(n1,N1)  # choose ni data from strata Ni
str2=srswor(n2,N2)
str3=srswor(n3,N3)
str4=srswor(n4,N4)
str5=srswor(n5,N5)
str6=srswor(n6,N6)
str7=srswor(n7,N7)
str8=srswor(n8,N8)
str9=srswor(n9,N9)

sam1=olive[olive$Area==1,][str1==1,] # pick up samples for each strata
sam2=olive[olive$Area==2,][str2==1,]
sam3=olive[olive$Area==3,][str3==1,]
sam4=olive[olive$Area==4,][str4==1,]
sam5=olive[olive$Area==5,][str5==1,]
sam6=olive[olive$Area==6,][str6==1,]
sam7=olive[olive$Area==7,][str7==1,]
sam8=olive[olive$Area==8,][str8==1,]
sam9=olive[olive$Area==9,][str9==1,]

# plot to show strata 
sam=rbind(sam1,sam2,sam3,sam4,sam5,sam6,sam7,sam8,sam9)
plot(sam$Palmitic, sam$Palmitoleic, col=sam$Area, pch=sam$Area,ylab="Palmitoleic", xlab="Palmitic",xlim=c(500,1800))
legend(550, 270, c("Area 1", "Area 2", "Area 3", "Area 4", "Area 5", "Area 6", "Area 7", "Area 8", "Area 9"), col=1:9,pch=1:9)

# calculate stratified mean
Mu.strata={mean(sam1$Palmitoleic)*N1+mean(sam2$Palmitoleic)*N2+mean(sam3$Palmitoleic)*N3+
    mean(sam4$Palmitoleic)*N4+mean(sam5$Palmitoleic)*N5+mean(sam6$Palmitoleic)*N6+
    mean(sam7$Palmitoleic)*N7+mean(sam8$Palmitoleic)*N8+mean(sam9$Palmitoleic)*N9}/N
#calculate stratified variance of the mean
var_xbar=(N1/N)^2*(1-n1/N1)/n1*var(sam1$Palmitoleic)+(N2/N)^2*(1-n2/N2)/n2*var(sam2$Palmitoleic)+
  (N3/N)^2*(1-n3/N3)/n3*var(sam3$Palmitoleic)+(N4/N)^2*(1-n4/N4)/n4*var(sam4$Palmitoleic)+
  (N5/N)^2*(1-n5/N5)/n5*var(sam5$Palmitoleic)+(N6/N)^2*(1-n6/N6)/n6*var(sam6$Palmitoleic)+
  (N7/N)^2*(1-n7/N7)/n7*var(sam7$Palmitoleic)+(N8/N)^2*(1-n8/N8)/n8*var(sam8$Palmitoleic)+
  (N9/N)^2*(1-n9/N9)/n9*var(sam9$Palmitoleic)

# 95% confidence interval
lower.strata=Mu.strata-z.score*sqrt(var_xbar)
upper.strata=Mu.strata+z.score*sqrt(var_xbar)
cat("The estimated mean of varialbe Palmitoleic is:",Mu.strata)
cat("The of 95% confidence interval for the mean of Palmitoleic is:","[",lower.strata,",",upper.strata,"]",sep="")

#####################################
# stratified with Neyman allocation #
#####################################

# Using Area 1,2,3,...9 as my strata, calculate small n and big N respectively
data(olive,package="pgmm")
# Calculate standard deviation of each strata
set.seed(2345)

S1=sd(olive[olive$Area==1,]$Palmitoleic) #calculate standard deviation for each area
S2=sd(olive[olive$Area==2,]$Palmitoleic)
S3=sd(olive[olive$Area==3,]$Palmitoleic)
S4=sd(olive[olive$Area==4,]$Palmitoleic)
S5=sd(olive[olive$Area==5,]$Palmitoleic)
S6=sd(olive[olive$Area==6,]$Palmitoleic)
S7=sd(olive[olive$Area==7,]$Palmitoleic)
S8=sd(olive[olive$Area==8,]$Palmitoleic)
S9=sd(olive[olive$Area==9,]$Palmitoleic)

N1=nrow(olive[olive$Area==1,])  # calculate Ni for each strata
N2=nrow(olive[olive$Area==2,])
N3=nrow(olive[olive$Area==3,])
N4=nrow(olive[olive$Area==4,])
N5=nrow(olive[olive$Area==5,])
N6=nrow(olive[olive$Area==6,])
N7=nrow(olive[olive$Area==7,])
N8=nrow(olive[olive$Area==8,])
N9=nrow(olive[olive$Area==9,])


#using Neyman allocation to calculate ni for each strata
n1=round(N1*S1/(N1*S1+N2*S2+N3*S3+N4*S4+N5*S5+N6*S6+N7*S7+N8*S8+N9*S9)*n)
n2=round(N2*S2/(N1*S1+N2*S2+N3*S3+N4*S4+N5*S5+N6*S6+N7*S7+N8*S8+N9*S9)*n)
n3=round(N3*S3/(N1*S1+N2*S2+N3*S3+N4*S4+N5*S5+N6*S6+N7*S7+N8*S8+N9*S9)*n)
n4=round(N4*S4/(N1*S1+N2*S2+N3*S3+N4*S4+N5*S5+N6*S6+N7*S7+N8*S8+N9*S9)*n)
n5=round(N5*S5/(N1*S1+N2*S2+N3*S3+N4*S4+N5*S5+N6*S6+N7*S7+N8*S8+N9*S9)*n)
n6=round(N6*S6/(N1*S1+N2*S2+N3*S3+N4*S4+N5*S5+N6*S6+N7*S7+N8*S8+N9*S9)*n)
n7=round(N7*S7/(N1*S1+N2*S2+N3*S3+N4*S4+N5*S5+N6*S6+N7*S7+N8*S8+N9*S9)*n)
n8=round(N8*S8/(N1*S1+N2*S2+N3*S3+N4*S4+N5*S5+N6*S6+N7*S7+N8*S8+N9*S9)*n)+1
n9=round(N9*S9/(N1*S1+N2*S2+N3*S3+N4*S4+N5*S5+N6*S6+N7*S7+N8*S8+N9*S9)*n)+1

#choose sub-sample from each strata
str1=srswor(n1,N1) 
str2=srswor(n2,N2)
str3=srswor(n3,N3)
str4=srswor(n4,N4)
str5=srswor(n5,N5)
str6=srswor(n6,N6)
str7=srswor(n7,N7)
str8=srswor(n8,N8)
str9=srswor(n9,N9)
sam1=olive[olive$Area==1,][str1==1,]
sam2=olive[olive$Area==2,][str2==1,]
sam3=olive[olive$Area==3,][str3==1,]
sam4=olive[olive$Area==4,][str4==1,]
sam5=olive[olive$Area==5,][str5==1,]
sam6=olive[olive$Area==6,][str6==1,]
sam7=olive[olive$Area==7,][str7==1,]
sam8=olive[olive$Area==8,][str8==1,]
sam9=olive[olive$Area==9,][str9==1,]
# calculate stratified mean
Mu.strata2={mean(sam1$Palmitoleic)*N1+mean(sam2$Palmitoleic)*N2+mean(sam3$Palmitoleic)*N3+
    mean(sam4$Palmitoleic)*N4+mean(sam5$Palmitoleic)*N5+mean(sam6$Palmitoleic)*N6+
    mean(sam7$Palmitoleic)*N7+mean(sam8$Palmitoleic)*N8+mean(sam9$Palmitoleic)*N9}/N
#calculate stratified variance of the mean
var_xbar=(N1/N)^2*(1-n1/N1)/n1*var(sam1$Palmitoleic)+(N2/N)^2*(1-n2/N2)/n2*var(sam2$Palmitoleic)+
  (N3/N)^2*(1-n3/N3)/n3*var(sam3$Palmitoleic)+(N4/N)^2*(1-n4/N4)/n4*var(sam4$Palmitoleic)+
  (N5/N)^2*(1-n5/N5)/n5*var(sam5$Palmitoleic)+(N6/N)^2*(1-n6/N6)/n6*var(sam6$Palmitoleic)+
  (N7/N)^2*(1-n7/N7)/n7*var(sam7$Palmitoleic)+(N8/N)^2*(1-n8/N8)/n8*var(sam8$Palmitoleic)+
  (N9/N)^2*(1-n9/N9)/n9*var(sam9$Palmitoleic)
# 95% confidence interval
lower.strata2=Mu.strata2-z.score*sqrt(var_xbar)
upper.strata2=Mu.strata2+z.score*sqrt(var_xbar)
cat("The estimated mean of varialbe Palmitoleic is:",Mu.strata2)
cat("The of 95% confidence interval for the mean of Palmitoleic is:","[",lower.strata2,",",upper.strata2,"]",sep="")

##################
# Ratio estimate #
##################

# choose Palmitic as auxiliary variable
data(olive,package="pgmm")
# plot(olive$Palmitic,olive$Palmitoleic,xlab="Palmitic",ylab="Palmitoleic",main="Relation btween Palmitic and Palmitoleic")
# corr=cor(olive$Palmitic,olive$Palmitoleic)  # check the correlation of Palmitic and Palmitoleic
# corr

set.seed(2345)
swo=srswor(n,N)
sample=olive[swo==1,]

plot(sample$Palmitic, sample$Palmitoleic, xlab="Palmitic",ylab="Palmitoleic",main="Relation btween Palmitic and Palmitoleic", xlim=c(min(sample$Palmitic)+5,max(sample$Palmitic)+5), ylim=c(min(sample$Palmitoleic)+5, max(sample$Palmitoleic)+5))

CV_x=sd(sample$Palmitic)/mean(sample$Palmitic)  #CV(x)
CV_y=sd(sample$Palmitoleic)/mean(sample$Palmitic)  #CV(y)
corr=cor(sample$Palmitic,sample$Palmitoleic)  # check the correlation of Palmitic and Palmitoleic

y_bar=mean(sample$Palmitoleic)
x_bar=mean(sample$Palmitic)
B=y_bar/x_bar  # B hat
Mu.y=B*mean(olive$Palmitic)  # mean of Palmitoleic
y=sample$Palmitoleic
x=sample$Palmitic
SB=sum((y-B*x)^2)/(n-1)  # calculate Sb square
var_B=((N-n)/(n*N))/((mean(olive$Palmitic))^2)*SB
var_ybar=mean(olive$Palmitic)^2*var_B  # variance of ybar ratio
lower.ratio=Mu.y-z.score*sqrt(var_ybar)
upper.ratio=Mu.y+z.score*sqrt(var_ybar)
cat("The estimated mean of varialbe Palmitoleic is:",Mu.y)
cat("The of 95% confidence interval for the mean of Palmitoleic is:","[",lower.ratio,",",upper.ratio,"]",sep="")
if (corr>CV_x/(2*CV_y)) {cat("Because R > CV(x)/2CV(y), so we can use Ratio") }  else { cat("Because correlation does not greater than CV(x)/2CV(y), we should be cautious of using Ratio.")}

#######################
# regression estimate #
#######################

data(olive,package="pgmm")
# check the linear relationship of Palmitic and Palmitoleic
# plot(olive$Palmitic,olive$Palmitoleic,xlab="Palmitic",ylab="Palmitoleic",main="Relation btween Palmitic and Palmitoleic")
plot(sample$Palmitic, sample$Palmitoleic, xlab="Palmitic",ylab="Palmitoleic",main="Relation btween Palmitic and Palmitoleic", xlim=c(min(sample$Palmitic)+5,max(sample$Palmitic)+5), ylim=c(min(sample$Palmitoleic)+5, max(sample$Palmitoleic)+5))

set.seed(2345)
swo=srswor(n,N=N)
x.sample=olive[swo==1,]$Palmitic  # sample of x
y.sample=olive[swo==1,]$Palmitoleic  # sample of y
reg=lm(y.sample~x.sample)  # regression

# check assumptions of regression
par(mfrow = c(2, 2))
plot(reg)  # normality, reisudals independent of each other
par(mfrow=c(1,1))
mean(summary(reg)$residuals)  # Expected(residuals)=0
plot(summary(reg)$residuals, ylab="Residuals", main="Residual Plot", xlab="Observations") # constant variance of residuals
abline(h=0, col="red")

#get b0 and b1
b0=summary(reg)$coefficients[1,1]
b1=summary(reg)$coefficients[2,1]
y.bar=b0+b1*mean(olive$Palmitic)
# MSE=mean(summary(reg)$residuals^2)  #extract MSE from regression model
# Se.square=(n-2)*MSE/(n-1)   # calculate Se square = (n-2)*MSE/(n-1)

Se.square=sum(summary(reg)$residuals^2)/(n-1)

var_yreg=(1-n/N)*Se.square/n  # variance of ybar reg
lower.reg=y.bar-z.score*sqrt(var_yreg)
upper.reg=y.bar+z.score*sqrt(var_yreg)
cat("The estimated mean of varialbe Palmitoleic is:",y.bar)
cat("The of 95% confidence interval for the mean of Palmitoleic is:","[",lower.reg,",",upper.reg,"]",sep="")

###################
# Domain estimate #
###################

# Using Simple Random sample n=n to pick a sample and estimate mean of Palmitoleic in Area=5
swo=srswor(n=n,N=N)
set.seed(2345)
domain=olive[swo==1,]
xi=domain$Palmitoleic
xi=ifelse(domain$Area==5,xi/xi,xi*0)  # vector of xi
yi=domain$Palmitoleic                 # vector of yi
# ui=ifelse(domain$Area==5,yi*1,yi*0)   # vector of ui
# ui=yi*xi

yi=ifelse(domain$Area==5, yi, 0)
ybar_domain=sum(yi)/sum(xi)
# ybar_domain=sum(ui)/sum(xi)   # y bar
# nd=length(which(xi!=0))  # nd
nd=sum(xi)

yi_d=yi[yi!=0]
Sd.square=sum((yi_d-ybar_domain)^2)/(nd-1)  # Sd square
var_yd=(1-n/N)*(n/nd^2)*((nd-1)/(n-1))*Sd.square # variance of yd bar
var_yd=(1-n/N)*Sd.square

lower.domain=ybar_domain-z.score*sqrt(var_yd)
upper.domain=ybar_domain+z.score*sqrt(var_yd)
cat("The estimated mean of varialbe Palmitoleic is:",ybar_domain)
cat("The of 95% confidence interval for the mean of Palmitoleic is:","[",lower.domain,",",upper.domain,"]",sep="")


####################
# Cluster estimate #
####################

# Using Area as clusters, so N=max(Area)=9
# Choose n=4
set.seed(2345)

# Using the cluster command from the sampling package
library(sampling)
n=4
N=max(olive$Area)
s=cluster(olive, "Area", n)

ls(s)

sample = olive[s$ID_unit,]

# choose Area 1,2,5,9
clus1=sample[sample$Area==1,]
clus2=sample[sample$Area==2,]
clus3=sample[sample$Area==5,]
clus4=sample[sample$Area==9,]

# swo=srswor(n=n,N=N)
# swo
# #because swo is 1 1 0 0 1 0 0 1, so choose Area 1,2,5,9
# clus1=olive[olive$Area==1,]
# clus2=olive[olive$Area==2,]
# clus3=olive[olive$Area==5,]
# clus4=olive[olive$Area==9,]


# calculate the total for each cluster names ti
t1=sum(clus1$Palmitoleic)
t2=sum(clus2$Palmitoleic)
t3=sum(clus3$Palmitoleic)
t4=sum(clus4$Palmitoleic)
# calculate the Mi for each cluster
M1=nrow(clus1)
M2=nrow(clus2)
M3=nrow(clus3)
M4=nrow(clus4)
# calculate y bar
y.cluster=sum(t1,t2,t3,t4)/sum(M1,M2,M3,M4)
M.bar=mean(M1,M2,M3,M4)  # M bar
Mi=c(M1,M2,M3,M4)  # vector of Mi
ti=c(t1,t2,t3,t4)   # vector of ti
#because Mi's are not equal in each clusters, so need to use the formula on page 180 of text book
#to find the variance of ybar
var.cluster=((1-n/N)/(n*M.bar^2))*(sum((ti-y.cluster*Mi)^2)/(n-1))
#because the data inside each cluster is greater tha 30, so use Z value instead t value
lower.cluster=y.cluster-z.score*sqrt(var.cluster)
upper.cluster=y.cluster+z.score*sqrt(var.cluster)
cat("The estimated mean of varialbe Palmitoleic is:",y.cluster)
cat("The of 95% confidence interval for the mean of Palmitoleic is:","[",lower.cluster,",",upper.cluster,"]",sep="")

#################
# CI comparsion #
#################

F <- c(Mu, Mu.strata,Mu.strata2,Mu.y,y.bar,ybar_domain,y.cluster)
x <- c("SRS","Stra. Prop.", "Stra. Neyman", "Ratio", "Reg", "Domain", "Cluster")
u <- c(upper,upper.strata,upper.strata2,upper.ratio,upper.reg,upper.domain,upper.cluster)
l <- c(lower,lower.strata,lower.strata2,lower.ratio,lower.reg,lower.domain,lower.cluster)

df <- data.frame(x, F, l, u)

library(ggplot2)

ggplot(df, aes(x=x, y=F))+geom_point(size=4)+geom_errorbar(aes(ymax=u, ymin=l))

