toxTest<-read.csv('toxTest.csv')
attach(toxTest)

glmlist<-vector(mode="list",length=21)
doselist<-vector(mode="list",length=21)
dose10percent<-vector(mode="list",length=21)
for (i in 2:22){
  toxTest[complete.cases(toxTest[,i]),]
  glmlist<-glm(cbind(toxTest[,i],100-toxTest[,i]) ~ toxTest[,1], family = binomial(link = "probit"))
  doselist[[i-1]]<-dose.p(glmlist,1:2,0.5)
  dose10percent[[i-1]]<-dose.p(glmlist,1:2,0.1)
}

lc50<-numeric(length=21)
lc50SE<-numeric(length=21)
lc10<-numeric(length=21)
lc10SE<-numeric(length=21)
for (i in 1:21){
  doseobject<-doselist[[i]]
  lc50[i]<-doseobject[1]
  lc50SE[i]<-attr(doseobject,"SE")
  doseobject<-dose10percent[[i]]
  lc10[i]<-doseobject[1]
  lc10SE[i]<-attr(doseobject,"SE")
}

LC<-read.csv("LCresults.csv")
LC$Type<-as.factor(LC$Type)
ponds<-subset(LC,Type=="Pond")
lakes<-subset(LC,Type=="Lake")
t.test(ponds$LC50,lakes$LC50,paired=FALSE)
t.test(ponds$LC10,lakes$LC10,paired=FALSE)

LC$Habitat<-as.factor(LC$Habitat)
a<-aov(LC50~Habitat,data=LC)
summary(a)
b<-aov(LC10~Habitat,data=LC)
summary(b)
plot(TukeyHSD(a))
TukeyHSD(b)
summary(manova(cbind(LC10,LC50)~Habitat,data=LC))
summary(aov(cbind(LC10,LC50)~Habitat,data=LC))

tankCounts<-read.csv("TankCounts.csv")
tankCounts$Treatment<-as.factor(tankCounts$Treatment)
tankCounts$Pair<-as.factor(tankCounts$Pair)
tankCounts$ID<-as.factor(tankCounts$ID)

summary(manova(cbind(Means,littler)~Treatment*ID,tankCounts))
treatmt<-aov(cbind(Means,littler)~Treatment*ID,tankCounts)

pair1<-subset(tankCounts,Pair==1)
pair2<-subset(tankCounts,Pair==2)
t.test(pair1$Means,pair2$Means,paired=TRUE)
t.test(pair1$littler,pair2$littler,paired=TRUE)