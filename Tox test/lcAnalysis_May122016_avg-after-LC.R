library(MASS)
toxTestalt<-read.csv('toxTestalt.csv')

glmlist<-vector(mode="list",length=41)
doselist<-vector(mode="list",length=41)
for (i in 2:42){
  tox.temp<-toxTestalt[complete.cases(toxTestalt[,i]),c(1,i)]
  tox.temp<-tox.temp[-1,]
  dead<-tox.temp[,2]
  alive<-5-tox.temp[,2]
  glmlist[[i-1]]<-glm(cbind(dead,alive) ~ log(tox.temp$Conc), family = binomial(link = "probit"))
  doselist[[i-1]]<-dose.p(glmlist[[i-1]],1:2,c(0.1,0.5))
}

lc50<-numeric(length=41)
lc50SE<-numeric(length=41)
lc10<-numeric(length=41)
lc10SE<-numeric(length=41)
for (i in 1:41){
  lc50[i]<-exp(doselist[[i]][2])
  lc50SE[i]<-attr(doselist[[i]],"SE")[2]
  lc10[i]<-exp(doselist[[i]][1])
  lc10SE[i]<-attr(doselist[[i]],"SE")[1]
}


mean.50<-c()
var.50<-c()
mean.10<-c()
var.10<-c()
i<-1
while(i<19){
  mean.50<-c(mean.50,mean(lc50[i:(i+1)]))
  var.50<-c(var.50,var(lc50[i:(i+1)]))
  mean.10<-c(mean.10,mean(lc10[i:(i+1)]))
  var.10<-c(var.10,var(lc10[i:(i+1)]))
  i<-i+2
}
mean.50<-c(mean.50,lc50[19],mean(lc50[20:22]))
mean.10<-c(mean.10,lc10[19],mean(lc10[20:22]))
var.50<-c(var.50,lc50[19],var(lc50[20:22]))
var.10<-c(var.10,lc10[19],var(lc10[20:22]))

i<-23
while(i<33){
mean.50<-c(mean.50,mean(lc50[i:(i+1)]))
var.50<-c(var.50,var(lc50[i:(i+1)]))
mean.10<-c(mean.10,mean(lc10[i:(i+1)]))
var.10<-c(var.10,var(lc10[i:(i+1)]))
i<-i+2
}

mean.50<-c(mean.50,lc50[33],lc50[34],mean(lc50[35:36]),mean(lc50[37:39]),mean(lc50[40:41]))
mean.10<-c(mean.10,lc10[33],lc10[34],mean(lc10[35:36]),mean(lc10[37:39]),mean(lc10[40:41]))
var.50<-c(var.50,lc50[33],lc50[34],var(lc50[35:36]),var(lc50[37:39]),var(lc50[40:41]))
var.10<-c(var.10,lc10[33],lc10[34],var(lc10[35:36]),var(lc10[37:39]),var(lc10[40:41]))

toxTest<-read.csv('toxTest.csv')

LCalt<-data.frame(Type=rep(c("Pond","Lake"),c(12,9)),
               Habitat=gsub('.[0-9]+',"",names(toxTest[3:23])),
               Clone=names(toxTest[3:23]),
               lc10=mean.10,
               lc10SD=sqrt(var.10),
               lc50=mean.50,
               lc50SD=sqrt(var.50),
               diff=mean.50-mean.10)

##make graph
data<-LCalt[,c(4,6)]
data<-t(data)
colnames(data)<-as.character(LCalt$Clone)
col<-c(rep(c("darkolivegreen2","forestgreen"),12),rep(c("cadetblue1","cornflowerblue"),9))


barplot(data, beside=TRUE, ylim=c(140,170),
        xpd=FALSE, col=col,
        las=2, ylab="Cu Concentration(ug/L)",
        xlab="Clonal Line",cex.names = 0.7,
        offset=0)
legend("topleft",
       c("LC10 Ponds","LC50 Ponds", "LC10 Lakes", "LC50 Lakes"),
       fill=c("darkolivegreen2","forestgreen","cadetblue1","cornflowerblue"),
       cex=0.7,
       inset=0.01)

LCalt$Type<-as.factor(LCalt$Type)
ponds<-subset(LCalt,Type=="Pond")
lakes<-subset(LCalt,Type=="Lake")
t.test(ponds$lc50,lakes$lc50,paired=FALSE) ##non signif

shapiro.test(ponds$lc50) ##non-normal
shapiro.test(lakes$lc50) ##normal
leveneTest(lc50~Type,LCalt) ##variances equal
wilcox.test(ponds$lc50,lakes$lc50, paired=FALSE) ##signif diff

t.test(ponds$lc10,lakes$lc10,paired=FALSE) #non signif
shapiro.test(ponds$lc10) ##non-normal
shapiro.test(lakes$lc10) ##normal
leveneTest(lc10~Type,LCalt) ##variance equal
wilcox.test(ponds$lc10,lakes$lc10) ##non signif

LC1<-LCalt[-c(5,8,21),]
LC1$Habitat<-as.factor(LC1$Habitat)
LC1$Habitat<-droplevels(LC1$Habitat)
leveneTest(lc50~Habitat,LC1)
leveneTest(lc10~Habitat,LC1)
oneway.test(lc50~Habitat, data=LC1, na.action=na.omit, var.equal=FALSE)
oneway.test(lc10~Habitat,LC1,var.equal = FALSE)
##Habitats not significantly different