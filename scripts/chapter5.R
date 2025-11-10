rm(list=ls())
options(echo = TRUE)

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

# Coefficient of variation, CV=sd/mean
get_CV=function(datavector) {

	return(sd(datavector)/mean(datavector))
}

set.seed(1337)
mydata=rnorm(500,500,50)
# mydata=rnorm(500,50,10)
# mydata=runif(500,1,10)


CV_samples=NULL
SD_logsamples=NULL
CV_samplesO=NULL
SD_logsamplesO=NULL
for(i in 1:200){

	mysample=sample(mydata,replace=TRUE)
	mydist=rnorm(500,1000,i)
	CV_samples[i]=get_CV(mysample)
	SD_logsamples[i]=sd(log(mysample))

	CV_samplesO[i]=get_CV(mydist)
	SD_logsamplesO[i]=sd(log(mydist))

}


quantile(CV_samples,c(0.025,0.975)) # The vector corresponds to 95%CI
se_x=sqrt(var(CV_samples)/length(CV_samples))
mean(CV_samples)-1.96*se_x
mean(CV_samples)+1.96*se_x
# mean(CV_samples)-1.96*sd(CV_samples)
# mean(CV_samples)+1.96*sd(CV_samples)

df=data.frame(CV=CV_samplesO,SD=SD_logsamplesO)
lm_opt=lm(SD_logsamplesO~CV_samplesO,data=df)


pdf('optional.pdf')
plot(CV_samplesO,SD_logsamplesO,pch=1,cex=1,xlab='CV',ylab='SD(log)')


abline(0,1,col='#D2042D')
abline(lm_opt,col='#D2042D')
dev.off()
warnings()