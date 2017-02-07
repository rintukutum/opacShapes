rm(list=ls())
#plot(1:10,1:10,type='n',xlim=c(0,50))
#source('./point.R')
#point(x=5,y=5,r=2)
dir.create('./figures', showWarnings=FALSE)
pdf('./figures/test.pdf', width=6,height=6)
plot(1:10,1:10,type='n',xlim=c(0,50))
source('./R/point.R')
point(x=8,y=8)
dev.off()
#------
rm(list=ls())
pdf('./figures/test_6_10.pdf', width=6,height=10)
plot(1:10,1:10,type='n',xlim=c(0,50))
source('./R/point.R')
point(x=8,y=8)
dev.off()

#------
rm(list=ls())
pdf('./figures/test_6_10_split.pdf', width=10,height=8)
source('./R/point.R')
par(mfrow=c(2,1))
for(i in 1:2){
plot(1:10,1:10,type='n',xlim=c(0,50))
point(x=8,y=8)
}
dev.off()
#---------------------
png('./figures/rescaledPoint.png',
    width=400,height=600,
    res=100)
plot(1:10,1:10,type='n',xlim=c(0,50))
point(
	x = 8,
	y = 8,
	r = 10)
dev.off()
