rm(list=ls())
options(echo = TRUE)

set.seed(1337)
groups=as.factor(rep(c("Low","Medium","High"),each=50))
groups=factor(groups,levels=c('Medium','Low','High'))
x=c(rnorm(50,10,6),rnorm(50,14,6),rnorm(50,18,6))

svg('arrrrplot.svg',width=15,height=8,pointsize=16)

plot(groups,x,las=1,xlab="Group",ylab="Body size (g)")

dev.off()

m = lm(x~groups)
# m = lm(x~groups-1)

# model.matrix(m)

anova(m)
summary(m)

SS_T=1070.6+5666.8
var(x)
SS_T/(length(x)-1)
1070.6/SS_T




df=read.csv("input/chapter3/butterflies.csv")
names(df)

df$MaternalHost = paste0(df$MaternalHost, "M")
df$LarvalHost = paste0(df$LarvalHost, "L")
# list(df$MaternalHost, df$LarvalHost)
# df$DevelopmentTime

tapply(df$DevelopmentTime, list(df$MaternalHost, df$LarvalHost), mean)
# tapply(df,DevelopmentTime ~ MaternalHost*LarvalHost, mean)

mdev=lm(DevelopmentTime~MaternalHost*LarvalHost,df)
mgro=lm(GrowthRate~MaternalHost*LarvalHost,df)
msiz=lm(AdultWeight~MaternalHost*LarvalHost,df)

adev=anova(mdev)
agro=anova(mgro)
asiz=anova(msiz)

adev
agro
asiz

# Conclusion: larval host accounts for most of the variance
# and while there is some support for an interaction it is
# weak relative to the direct effects.
# Particularly, final size of larvae was not correlated with
# maternal host.
# Suggest a short-generation component explains the maternal
# host effects, i.e. healthier mom > healthier larvae,
# but healthier grand-grandmom is likely less impactful. Stands to reason.

options(echo=FALSE)





















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