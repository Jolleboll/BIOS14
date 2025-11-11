rm(list=ls())
options(echo = TRUE)


x=rnorm(1000,100,30)
y=2*x+rnorm(1000,0,10)

svg('arrrrplot.svg',width=18,height=9,pointsize=16)

plot(x,y,
	las=1,
	xlab="Leaf length (mm)",
	ylab="Leaf width (mm)")

dev.off()


slopes=NULL
corrslopes=NULL
for(i in 1:40){

	x_i=x+rnorm(1000,0,i)
	slopes[i]=lm(y~x_i)$coeff[2]
	# y_i=y+rnorm(1000,0,i)
	# slopes[i]=lm(y_i~x)$coeff[2]

	corrslopes[i]=slopes[i]/(1-(i^2/var(x_i)))
	# corrslopes[i]=slopes[i]/(1-(i^2/var(x)))
}

xaxis=seq(length(slopes))

svg('arrrrplot.svg',width=18,height=9,pointsize=16)

plot(slopes,
	pch=16,
	las=1,
	xlab="Predictor variable measurement error",
	ylab="Linear model fit slope",
	xlim=c(1,41),ylim=c(0,2.5))
points(corrslopes)
segments(xaxis,slopes,xaxis,corrslopes)


dev.off()


















options(echo = FALSE)

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