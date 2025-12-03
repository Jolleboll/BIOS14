# library(car)
# library(tidyverse)
library(glmmTMB)
library(MuMIn)

rm(list=ls())
options(echo = TRUE)
options(na.action = "na.fail")

plants=read.csv('input/alpineplants.csv',header=TRUE)
plants=na.omit(plants)
plants=plants[plants$max_T_winter < 4,]


# full_fc=Carex.bigelowii~mean_T_winter+max_T_winter+min_T_winter+mean_T_summer+max_T_summer+min_T_summer+light+snow+soil_moist+altitude
full_fc=Carex.bigelowii~max_T_winter+max_T_summer+min_T_summer+light+snow+soil_moist+altitude

full_ft=Thalictrum.alpinum~mean_T_winter+max_T_winter+min_T_winter+mean_T_summer+max_T_summer+min_T_summer+light+snow+soil_moist+altitude
full_ft=Thalictrum.alpinum~max_T_winter+max_T_summer+min_T_summer+light+snow+soil_moist+altitude

mc=lm(full_fc,plants)
mt=lm(full_ft,plants)

# # vif() outputs predictor correlation. VIF values higher than 3 are problematic.
# # Does not change with different response variables if predictors are the same.
# vif(mc)
# # These were removed: mean_T_winter, min_T_Winter, mean_T_summer

c_dredgemodels=dredge(mc)
t_dredgemodels=dredge(mt)
c_dredgemodels

# Best: Altitude + min_T_summer
# Best: Altitude + min_T_summer + mean_T_summer

quit()



set.seed(1337)
x1 = rnorm(200, 10, 3)
group = as.factor(sample(c("A", "B"), 200, replace=T))
y = 0.5*x1 + rnorm(200, 0, 4)


y[group=="A"] = y[group=="A"] + rnorm(length(y[group=="A"]), 2, 1)

options(na.action = "na.fail")

fullmodel=lm(y ~ x1 * group)
x1
group
model_set = dredge(fullmodel)
model_set
quit()


m1 = lm(y ~ x1 * group) # Catches slope differences between groups (ANCOVA)
m1r = glmmTMB(y ~ x1 + (1|group)) # Randomises intercept differences (LinReg+RandomEffect)
m2 = lm(y ~ x1 + group) # Catches intercept differences betw groups (ANCOVA)
m3 = lm(y ~ x1) # Ignores groups (LinReg)
m4 = lm(y ~ group) # Ignores x (ANOVA? Needs 3 or more groups tho?)
m5 = lm(y ~ 1) # Null model

mlist = list(m1, m1r, m2, m3, m4, m5)
AICTab = AIC(m1, m1r,m2, m3, m4, m5)
AICTab$logLik = unlist(lapply(mlist, logLik))
AICTab = AICTab[order(AICTab$AIC, decreasing=F),]
AICTab$delta = round(AICTab$AIC - min(AICTab$AIC), 2)
lh = exp(-0.5*AICTab$delta)
AICTab$w = round(lh/sum(lh), 2)
AICTab

















quit()
logit = function(x) log(x/(1-x))
invlogit = function(x) 1/(1+exp(-x))

x = runif(500,0,20)
# x = rnorm(500, 10, 5)
eta = 1 + 0.15*x
# eta = 1 + 0.15*x + rnorm(500, 0, 0.1)
y1 = ceiling(exp(eta)+rpois(500,0.3))
y2 = rpois(500,exp(eta))
y2
# y1 = ceiling(exp(eta))
# y2 = ceiling(exp(eta))
svg('arrrrplot.svg',width=18,height=8,pointsize=16)
par(mfrow=c(1,3))
plot(x, eta, las=1)
plot(x, y1, las=1)
plot(x, y2, las=1)
dev.off()








quit()

x = runif(5000)
logit_x = logit(x)

x = rnorm(2000, 15, 3)
eta = 1 + 0.4*x + rnorm(2000, 0, 3)
p = invlogit(eta)
y = rbinom(2000, 1, p)
svg('arrrrplot.svg',width=18,height=10,pointsize=16)
par(mfrow=c(1,3))
plot(x, eta, las=1)
plot(x, p, las=1)
plot(x, y, pch='|',las=1)
dev.off()
# quit()
m = glm(y~x, family=binomial(link="logit"))
summary(m)

coefs = summary(m)$coef
x_pred = seq(from=min(x)-10, to=max(x)+10, by=0.01)
y_hat = coefs[1,1] + coefs[2,1]*x_pred
p_hat = invlogit(y_hat)
xofhalfy=-coefs[1,1]/coefs[2,1]


svg('arrrrplot.svg',width=18,height=10,pointsize=16)
plot(x,y,las=1,pch='|',xlim=c(-20,20))
lines(x_pred,p_hat)
abline(v=xofhalfy,lty=3)
abline(h=0.5,lty=3)
dev.off()

options(echo = FALSE)
# svg('arrrrplot.svg',width=18,height=10,pointsize=16)
# par(mfrow=c(2,2))
# hist(x, las=1)
# hist(logit_x, las=1)


# xx = seq(-5, 5, 0.01)
# plot(xx, invlogit(xx), type="l", las=1,xlab="Logit (x)",ylab="P")
# plot(x, invlogit(logit_x), las=1)
# dev.off()













options(echo = FALSE)

# plants=read.csv('input/alpineplants.csv',header=TRUE)
# plants=plants[plants$max_T_winter < 4,]

# # fc=quote(Carex.bigelowii~.-Thalictrum.alpinum-min_T_winter-mean_T_winter-mean_T_summer)
# fc=Carex.bigelowii~max_T_winter+mean_T_summer+max_T_summer+min_T_summer+light+snow+soil_moist+altitude
# # fc=Carex.bigelowii~light+snow++altitude
# ft=Thalictrum.alpinum~max_T_winter+mean_T_summer+max_T_summer+min_T_summer+light+snow+soil_moist+altitude
# # ft=Thalictrum.alpinum~light+snow+altitude
# # ft=quote(Thalictrum.alpinum~.-Carex.bigelowii-min_T_winter-mean_T_winter-mean_T_summer)
# mc=lm(fc,plants)
# mt=lm(ft,plants)

# # vif() calculates the VIF values for every predictor variable,
# # essentially doing a linear regression per predictor in turn,
# # each time pretending the predictor is the response. It is not
# # quite related to Pearson's r^2 but is still a measure of
# # correlation, kind of. VIF values higher than 3 are problematic.
# vif(mc)
# vif(mt)


# # summary(plants)
# summary(mc)
# summary(mt)

# svg('arrrrplot.svg',width=18,height=10,pointsize=16)
# par(mfrow=c(2,2))
# plot(mc)
# dev.off()

# svg('arrrrplot1.svg',width=18,height=10,pointsize=16)
# par(mfrow=c(3,3))
# plot(fc,plants,las=1,ylim=c(0,10))
# dev.off()

# svg('arrrrplot2.svg',width=18,height=10,pointsize=16)
# par(mfrow=c(3,3))
# plot(ft,plants,las=1,ylim=c(0,10))
# dev.off()


# plantsL=pivot_longer(plants,Carex.bigelowii:Thalictrum.alpinum,
# 						names_to="species",
# 						values_to="abundance")
# plantsL$species=as.factor(plantsL$species)
# head(plantsL)
# # summary(plantsL)

# # anova(lm(abundance~.^2,plantsL))

# nm=lm(abundance~ -1 + species*.,plantsL)
# anova(nm)
# summary(nm)
# # anova(~light+snow+altitude)

# options(echo=FALSE)
 

# set.seed(1337)
# x1 = rnorm(200, 10, 4)
# # x2 = 0.5*x1 + rnorm(200, 0, 2)
# x2 = rnorm(200, 7, 3)
# xi=x1*x2

# y = 0.7*x1 + 2.2*x2 + 0.9*xi + rnorm(200, 0, 3)
# # y = 0.7*x1 + 2.2*x2 + rnorm(200, 0, 2)
# m = lm(y~x1*x2)
# coefs = summary(m)$coef

# summary(m)

# y_pred=coefs[1]+coefs[2][1]*x1+coefs[3][1]*x2+coefs[4][1]*x1*x2

# y_pred1=coefs[1]+coefs[2][1]*mean(x1)+coefs[3][1]*mean(x2)+coefs[4][1]*x1*x2
# y_pred2=coefs[1]+coefs[2][1]*x1+coefs[3][1]*mean(x2)+coefs[4][1]*mean(x1)*mean(x2)
# y_pred3=coefs[1]+coefs[2][1]*mean(x1)+coefs[3][1]*x2+coefs[4][1]*mean(x1)*mean(x2)
# var(y_pred1)/var(y_pred)
# var(y_pred2)/var(y_pred)
# var(y_pred3)/var(y_pred)

# var_ypred=t(coefs[2:4,1]) %*% cov(cbind(x1,x2,xi)) %*% coefs[2:4,1]
# var_ypred=t(coefs[4:4,1]) %*% cov(cbind(xi)) %*% coefs[4:4,1]
# var_ypred=t(coefs[2:2,1]) %*% cov(cbind(x1)) %*% coefs[2:2,1]
# var_ypred/var(y_pred)
# var_ypred/var(y)

# # r^2 is the measure of 
# # how much of the response variance is predicted/explained
# # by the model. It doesn't work here because the formula
# # for adding more than two variances is a mess and I won't have it.


# x1 = rnorm(200, 10, 4) ; x1z=scale(x1) #; x1=(x1-(mean(x1)))/sd(x1)
# x2 = rnorm(200, 7, 3) ; x2z=scale(x2) #; x2=(x2-(mean(x2)))/sd(x2)
# xi=x1*x2
# xiz=x1z*x2z
# # xi=x1*x2 ; xiz=(x1*x2-mean(x1)*x2-mean(x2)*x1+mean(x1)*mean(x2))/(sd(x1)*sd(x2))

# y = 0.7*x1 + 2.2*x2 + 0.9*xi + rnorm(200, 0, 3)
# # y = 0.7*x1z + 2.2*x2z + 0.9*xiz + rnorm(200, 0, 3)
# m = lm(y~x1z*x2z)
# coefs = summary(m)$coef
# summary(m)

options(echo=FALSE)


# set.seed(1337)
# groups=as.factor(rep(c("Low","Medium","High"),each=50))
# groups=factor(groups,levels=c('Medium','Low','High'))
# x=c(rnorm(50,10,6),rnorm(50,14,6),rnorm(50,18,6))

# svg('arrrrplot.svg',width=15,height=8,pointsize=16)

# plot(groups,x,las=1,xlab="Group",ylab="Body size (g)")

# dev.off()

# m = lm(x~groups)
# # m = lm(x~groups-1)

# # model.matrix(m)

# anova(m)
# summary(m)

# SS_T=1070.6+5666.8
# var(x)
# SS_T/(length(x)-1)
# 1070.6/SS_T




# df=read.csv("input/chapter3/butterflies.csv")
# names(df)

# df$MaternalHost = paste0(df$MaternalHost, "M")
# df$LarvalHost = paste0(df$LarvalHost, "L")
# # list(df$MaternalHost, df$LarvalHost)
# # df$DevelopmentTime

# tapply(df$DevelopmentTime, list(df$MaternalHost, df$LarvalHost), mean)
# # tapply(df,DevelopmentTime ~ MaternalHost*LarvalHost, mean)

# mdev=lm(DevelopmentTime~MaternalHost*LarvalHost,df)
# mgro=lm(GrowthRate~MaternalHost*LarvalHost,df)
# msiz=lm(AdultWeight~MaternalHost*LarvalHost,df)

# adev=anova(mdev)
# agro=anova(mgro)
# asiz=anova(msiz)

# adev
# agro
# asiz

# # Conclusion: larval host accounts for most of the variance
# # and while there is some support for an interaction it is
# # weak relative to the direct effects.
# # Particularly, final size of larvae was not correlated with
# # maternal host.
# # Suggest a short-generation component explains the maternal
# # host effects, i.e. healthier mom > healthier larvae,
# # but healthier grand-grandmom is likely less impactful. Stands to reason.

# options(echo=FALSE)





















# x=rnorm(1000,100,30)
# y=2*x+rnorm(1000,0,10)

# svg('arrrrplot.svg',width=18,height=9,pointsize=16)

# plot(x,y,
# 	las=1,
# 	xlab="Leaf length (mm)",
# 	ylab="Leaf width (mm)")

# dev.off()


# slopes=NULL
# corrslopes=NULL
# for(i in 1:40){

# 	x_i=x+rnorm(1000,0,i)
# 	slopes[i]=lm(y~x_i)$coeff[2]
# 	# y_i=y+rnorm(1000,0,i)
# 	# slopes[i]=lm(y_i~x)$coeff[2]

# 	corrslopes[i]=slopes[i]/(1-(i^2/var(x_i)))
# 	# corrslopes[i]=slopes[i]/(1-(i^2/var(x)))
# }

# xaxis=seq(length(slopes))

# svg('arrrrplot.svg',width=18,height=9,pointsize=16)

# plot(slopes,
# 	pch=16,
# 	las=1,
# 	xlab="Predictor variable measurement error",
# 	ylab="Linear model fit slope",
# 	xlim=c(1,41),ylim=c(0,2.5))
# points(corrslopes)
# segments(xaxis,slopes,xaxis,corrslopes)


# dev.off()










options(echo = FALSE)

# rm(list=ls())
# options(echo = TRUE)

# set.seed(1337)





# x=rnorm(500,10,2) # N, mean, sd
# y=0.4*x+rnorm(500,0,1) # Element-wise vector/matrix addition, very cool

# m=lm(y~x)
# # str(m) # Lists the contents of m, whatever m is, apparently
# summary(m)

# coefficients=m$co
# strait=coefficients[1]+coefficients[2]*x # A vector containing the y-values
# # predicted by the model m, mostly for plotting purposes, I think.
# # Maybe some residuals calculations, too. Who knows.

# pdf('optional.pdf',width=9,height=5)
# par(mfrow=c(1,2))
# plot(x,y,
# 	las=1,
# 	xlab='Leaf length (mm)',
# 	ylab='Leaf width (mm)')
# segments(x,y,x,strait,col="blue",lwd=0.1)

# # abline(m,col="red") # Draws a line of infinite length
# shortline_x=c(min(x),max(x))
# shortline_y=coefficients[1]+coefficients[2]*shortline_x
# lines(shortline_x,shortline_y,col='red') # Draws a line of finite length
# # (You shouldn't do drugs or extrapolate)

# hist(residuals(m),
# 	las=1,
# 	xlab='')

# plot(m)
# dev.off()




# df=data.frame(x,y)

# sampling_slopes=NULL
# for(i in 1:50){
# 	sample_i=df[sample(nrow(df),1000,replace=TRUE),]
# 	linm=lm(sample_i$y~sample_i$x) # Should work
# 	sampling_slopes[i]=linm$co[2]
# }
# summary(sampling_slopes)
# slope_se=sd(sampling_slopes)
# m$co
# slope_se

# coefficients[2]*(mean(x)+sd(x)) - coefficients[2]*mean(x)
# cor(x,y)^2

# coefficients[2]^2*var(x)


# bird_df=read.csv("input/chapter2/bird_allometry.csv",header=TRUE)

# m=lm(brain_mass ~ body_mass,bird_df)
# summary(m)

# coefficients=m$co
# strait=coefficients[1]+coefficients[2]*bird_df$body_mass
# x=bird_df$body_mass
# y=bird_df$brain_mass

# pdf('optional.pdf',width=9,height=5)
# par(mfrow=c(1,2))
# plot(x,y,
# 	las=1,
# 	xlab='Body mass (kg)',
# 	ylab='Brain mass (kg)')
# segments(x,y,x,strait,col="blue",lwd=0.1)

# # abline(m,col="red") # Draws a line of infinite length
# shortline_x=c(min(x),max(x))
# shortline_y=coefficients[1]+coefficients[2]*shortline_x
# lines(shortline_x,shortline_y,col='red') # Draws a line of finite length
# # (You shouldn't do drugs or extrapolate)

# hist(residuals(m),
# 	las=1,
# 	xlab='')

# plot(m)
# dev.off()



# males = bird_df[bird_df$Sex=="m",]
# females = bird_df[bird_df$Sex=="f",]
# mm = lm(log(brain_mass)~log(body_mass), data=males)
# mf = lm(log(brain_mass)~log(body_mass), data=females)


# pdf('optional.pdf',width=9,height=5)
# hist(residuals(mm))
# dev.off()



# birds = read.csv('input/bird_allometry.csv')
# head(birds)

# males = birds[birds$Sex=="m",]
# females = birds[birds$Sex=="f",]

# mm = lm(log(brain_mass)~log(body_mass), data=males)
# mf = lm(log(brain_mass)~log(body_mass), data=females)

# svg('arrrrplot.svg',width=18,height=9,pointsize=16)
# # hist(residuals(mm))
# plot(log(males$body_mass),log(males$brain_mass),pch=1,cex=1,col='blue',xlab='log(Body mass)',ylab='log(Brain mass)')
# points(log(females$body_mass),log(females$brain_mass),col='red')
# abline(mm,col='blue')
# abline(mf,col='red')

# dev.off()

# # Check if the slopes are significantly different
# maleslopes=NULL
# faleslopes=NULL
# for(i in 1:1000){
# 	msample=males[sample(nrow(males),nrow(males),replace=TRUE),]
# 	fsample=females[sample(nrow(females),nrow(females),replace=TRUE),]
# 	mm_i=lm(log(brain_mass)~log(body_mass), data=msample)
# 	mf_i=lm(log(brain_mass)~log(body_mass), data=fsample)
# 	maleslopes[i]=mm_i$coeff[2]
# 	faleslopes[i]=mf_i$coeff[2]
# }

# summary(maleslopes)
# summary(faleslopes)
# t.test(maleslopes,faleslopes)




# options(echo=FALSE)
# set.seed(1)
# x=rnorm(50,10,2)
# se_x=sqrt(var(x)/length(x))

# out=NULL
# for(i in 1:1000){
# 	sample=sample(x,replace=TRUE)
# 	out[i]=mean(sample)
# }

# pdf()
# hist(out,las=1,main='')
# sd(out)
# se_x
# quantile(out,c(0.025,0.975))
# qnorm(c(0.025,0.975))
# mean(x)-1.96*se_x
# mean(x)+1.96*se_x

# # Coefficient of variation, CV=sd/mean
# get_CV=function(datavector) {

# 	return(sd(datavector)/mean(datavector))
# }

# set.seed(1337)
# mydata=rnorm(500,500,50)
# # mydata=rnorm(500,50,10)
# # mydata=runif(500,1,10)


# CV_samples=NULL
# SD_logsamples=NULL
# CV_samplesO=NULL
# SD_logsamplesO=NULL
# for(i in 1:200){

# 	mysample=sample(mydata,replace=TRUE)
# 	mydist=rnorm(500,1000,i)
# 	CV_samples[i]=get_CV(mysample)
# 	SD_logsamples[i]=sd(log(mysample))

# 	CV_samplesO[i]=get_CV(mydist)
# 	SD_logsamplesO[i]=sd(log(mydist))

# }


# quantile(CV_samples,c(0.025,0.975)) # The vector corresponds to 95%CI
# se_x=sqrt(var(CV_samples)/length(CV_samples))
# mean(CV_samples)-1.96*se_x
# mean(CV_samples)+1.96*se_x
# # mean(CV_samples)-1.96*sd(CV_samples)
# # mean(CV_samples)+1.96*sd(CV_samples)

# df=data.frame(CV=CV_samplesO,SD=SD_logsamplesO)
# lm_opt=lm(SD_logsamplesO~CV_samplesO,data=df)


# pdf('optional.pdf')
# plot(CV_samplesO,SD_logsamplesO,pch=1,cex=1,xlab='CV',ylab='SD(log)')


# abline(0,1,col='#D2042D')
# abline(lm_opt,col='#D2042D')
# dev.off()
# warnings()