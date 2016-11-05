library(MASS)
toxTest<-read.csv('toxTest.csv')

glmlist<-vector(mode="list",length=21)
doselist<-vector(mode="list",length=21)
for (i in 3:23){
  tox.temp<-toxTest[complete.cases(toxTest[,i]),c(2,i)]
  tox.temp<-tox.temp[-1,]
  dead<-tox.temp[,2]
  alive<-100-tox.temp[,2]
  glmlist[[i-2]]<-glm(cbind(dead,alive) ~ log(tox.temp$Conc), family = binomial(link = "probit"))
  doselist[[i-2]]<-dose.p(glmlist[[i-2]],1:2,c(0.1,0.5))
}

lc50<-numeric(length=21)
lc50SE<-numeric(length=21)
lc10<-numeric(length=21)
lc10SE<-numeric(length=21)
for (i in 1:21){
  lc50[i]<-exp(doselist[[i]][2])
  lc50SE[i]<-attr(doselist[[i]],"SE")[2]
  lc10[i]<-exp(doselist[[i]][1])
  lc10SE[i]<-attr(doselist[[i]],"SE")[1]
}
diff<-lc50-lc10

LC<-data.frame(Type=rep(c("Pond","Lake"),c(12,9)),
               Habitat=gsub('.[0-9]+',"",names(toxTest[3:23])),
               Clone=names(toxTest[3:23]),
               lc10,
               lc10SE,
               lc50,
               lc50SE,
               diff)

##make graph
data<-LC[,c(4,6)]
data<-t(data)
colnames(data)<-as.character(LC$Clone)
col<-c(rep(c("darkolivegreen2","forestgreen"),12),rep(c("cadetblue1","cornflowerblue"),9))


# def.par <- par(no.readonly = TRUE)
# 
# ## divide device into two rows and 1 column 
# ## allocate figure 1  for barplot
# ## allocate figure 2 for barplot labels
# ## respect relations between widths and heights
# 
# nf <- layout(matrix(c(1,1,2,2),2,2,byrow = TRUE), c(1,3), c(3,1), TRUE)
# layout.show(nf)
# 
# ## barplot 
# par(mar = c(0,1,1,1))
# 
# xx <- barplot(data, beside=TRUE, ylim=c(140,170),
#               xpd=FALSE, col=col,
#               las=2, ylab="Cu Concentration(ug/L)", 
#               xlab="Clonal Line", xaxt='n')
# 
# legend("topright",
#        c("LC10 Ponds","LC50 Ponds", "LC10 Lakes", "LC50 Lakes"),
#        fill=c("darkolivegreen2","forestgreen","cadetblue1","cornflowerblue"),
#        cex=0.8,
#        inset=0.05)
# 
# ## labels , create d ummy plot for sacles
# par(mar = c(1,1,0,1))
# plot(seq_len(length(xx)),rep(1,length(xx)),type='n',axes=FALSE)
# ## Create some text labels 
# labels <- colnames(data)
# ## Plot text labels with some rotation at the top of the current figure
# text(seq(1.5,42,by=2),rep(1.4,length(xx)/2), srt = 90, adj = 1,
#      labels = labels, xpd = TRUE,cex=0.8,srt=60)
# 
# par(def.par)  #- reset to default

barplot(data, beside=TRUE, ylim=c(140,170),
        xpd=FALSE, col=col,
        las=2, ylab="Cu Concentration(ug/L)",
        xlab="Clonal Line",cex.names = 0.7,
        offset=0)
legend("topright",
       c("LC10 Ponds","LC50 Ponds", "LC10 Lakes", "LC50 Lakes"),
       fill=c("darkolivegreen2","forestgreen","cadetblue1","cornflowerblue"),
       cex=0.7,
       inset=0.01)



LC$Type<-as.factor(LC$Type)
ponds<-subset(LC,Type=="Pond")
lakes<-subset(LC,Type=="Lake")
t.test(ponds$lc50,lakes$lc50,paired=FALSE) ##non signif

shapiro.test(ponds$lc50) ##non-normal
shapiro.test(lakes$lc50) ##normal
leveneTest(lc50~Type,LC) ##variances equal
wilcox.test(ponds$lc50,lakes$lc50, paired=FALSE) ##signif diff

t.test(ponds$lc10,lakes$lc10,paired=FALSE) #non signif
shapiro.test(ponds$lc10) ##normal
shapiro.test(lakes$lc10) ##normal
leveneTest(lc10~Type,LC) ##variance equal
wilcox.test(ponds$lc10,lakes$lc10) ##non signif

mean(LC$lc50);sd(LC$lc50)
# [1] 158.7248
# [1] 1.452439
mean(LC$lc10);sd(LC$lc10)
# [1] 156.043
# [1] 1.770172
mean(ponds$lc50); mean(lakes$lc50) #lakes have a higher mean
# [1] 158.4791
# [1] 159.0522
mean(ponds$lc10); mean(lakes$lc10) #lakes have a higher mean
# [1] 155.5011
# [1] 156.7654
sd(ponds$lc50); sd(lakes$lc50) #ponds have a higher sd
# [1] 1.873205
# [1] 0.4879347
sd(ponds$lc10); sd(lakes$lc10) #ponsd have a higher sd
# [1] 1.995999
# [1] 1.152455

LC1<-LC[-c(5,8,21),]
LC1$Habitat<-as.factor(LC1$Habitat)
LC1$Habitat<-droplevels(LC1$Habitat)
##calculate sample means per Habitat of lc50 and lc10
means.lc50<-with(LC1, tapply(lc50, Habitat, mean))
means.lc10<-with(LC1, tapply(lc10, Habitat, mean))
##lc50 are normal but lc10 not
shapiro.test(means.lc50);shapiro.test(means.lc10)
##calculate sd's per habitat
sd.lc50<-with(LC1, tapply(lc50, Habitat, sd))
sd.lc10<-with(LC1, tapply(lc10, Habitat, sd))
##high unequal variances
library(car)
leveneTest(lc50~Habitat,LC1)
leveneTest(lc10~Habitat,LC1)
oneway.test(lc50~Habitat, data=LC1, na.action=na.omit, var.equal=FALSE)

##lc50 not different between habitats
a<-aov(lc50~Habitat,data=LC1)
summary(a)
oneway.test(lc50~Habitat,LC1,var.equal = FALSE)
datastdres<-rstandard(lm(lc50~Habitat,LC))
qqnorm(datastdres)
qqline(datastdres) ##close to normal but outliers at end

##anova finds barely signif diff between habitats for lc10
##but lack of normality means KW preferred
b<-aov(lc10~Habitat,data=LC1)
summary(b)
oneway.test(lc10~Habitat,LC1,var.equal = FALSE)

##Tukey comparison
##http://stackoverflow.com/questions/28587498/post-hoc-tests-for-one-way-anova-with-welchs-correction-in-r
allPairs <- expand.grid(levels(LC1$Habitat), levels(LC1$Habitat))
allPairs <- unique(t(apply(allPairs, 1, sort)))
allPairs <- allPairs[ allPairs[,1] != allPairs[,2], ]
allPairs
allResults <- apply(allPairs, 1, function(p) {
  dat <- LC1[ LC1$Habitat %in% p, ]
  ret <- oneway.test(lc10 ~ Habitat, data = dat)
  ret$groups <- p
  ret
})
length(allResults)
## [1] 3
allResults[[1]]
mm <- diag(length(levels(LC1$Habitat)))
dimnames(mm) <- list(levels(LC1$Habitat), levels(LC1$Habitat))
pMatrix <- lapply(allResults, function(res) {
  ## not fond of out-of-scope assignment ...
  mm[res$groups[1], res$groups[2]] <<- mm[res$groups[2], res$groups[1]] <<- res$p.value
})
mm<(0.05/36)

datastdres<-rstandard(lm(lc10~Habitat,LC))
qqnorm(datastdres)
qqline(datastdres) ##below the line on left, above the line on right
shapiro.test(residuals(lm(lc10~Habitat,LC)))

##no point in doing post hoc because no signif diffs found
TukeyHSD(a)
TukeyHSD(b)

##not safe to assume normality for these parametric tests
summary(manova(cbind(lc10,lc50)~Habitat,data=LC))
summary(aov(cbind(lc10,lc50)~Habitat,data=LC))


##=======================================================================
##Tank data
##=======================================================================

tankCounts<-read.csv("TankCounts2.csv")
tankCounts$Diversity<-as.factor(tankCounts$Diversity)
tankCounts$ID<-as.factor(tankCounts$ID)
tankCounts$Copper<-as.factor(tankCounts$Copper)


dates<-c(0,7,14,20,36)
max.days<-character(length=18)
r1<-numeric(length=18)
r2<-numeric(length=18)
r3<-numeric(length=18)
for (i in 1:18){
  ind<-which.max(tankCounts[i,5:7])
  r1[i]<-log(tankCounts[i,ind+4]/48)/dates[ind]
  ind<-which.max(tankCounts[i,5:8])
  r2[i]<-log(tankCounts[i,ind+4]/48)/dates[ind]
  ind<-which.max(tankCounts[i,5:9])
  r3[i]<-log(tankCounts[i,ind+4]/48)/dates[ind]
}
tankCounts$r1<-r1
tankCounts$r2<-r2
tankCounts$r3<-r3

fit<-lmerTest::lmer(COV~Diversity+Copper+Copper*Diversity+(1|ID),
                    tankCounts)
lmerTest::anova(fit,ddf="Satterthwaite")
summary(fit)
plot(fit)
hist(resid(fit))
qqnorm(resid(fit));qqline(resid(fit))

fit.adj<-lm(COV~Diversity+Copper+Copper*Diversity,
                    tankCounts)
summary(fit.adj)

fit2<-lmerTest::lmer(r1~Diversity+Copper+Copper*Diversity+(1|ID),
                    tankCounts)
lmerTest::anova(fit2)
shapiro.test(resid(fit2))

fit3<-lmerTest::lmer(r2~Diversity+Copper+Copper*Diversity+(1|ID),
                     tankCounts)
lmerTest::anova(fit3)

fit4<-lmerTest::lmer(r3~Diversity+Copper+Copper*Diversity+(1|ID),
                     tankCounts)
lmerTest::anova(fit4,ddf="Kenward-Roger")
plot(fit4)
hist(resid(fit4))
qqnorm(resid(fit4));qqline(resid(fit4))


fit<-aov(COV~Diversity+Error(ID),
        tankCounts)
summary(fit)
shapiro.test(resid(fit))

fit2<-aov(r3~Diversity+Error(ID),
         tankCounts)
summary(fit2)

no.cu<-subset(tankCounts,Copper=="no")
yes.cu<-subset(tankCounts,Copper=="yes")
t.test(no.cu$r3,yes.cu$r3,paired=TRUE)
t.test(no.cu$COV,yes.cu$COV,paired=TRUE)
shapiro.test(no.cu$r3);shapiro.test(yes.cu$r3)
shapiro.test(no.cu$COV);shapiro.test(yes.cu$COV) ##no Cu COV is non-normal
wilcox.test(no.cu$COV,yes.cu$COV,paired = TRUE)
wilcox.test(no.cu$r3,yes.cu$r3,paired = TRUE)

all.res<-numeric(nrow(tankCounts))
for(i in 1:nrow(tankCounts)){
  mymodel<-lmerTest::lmer(COV~Diversity+Copper+Copper*Diversity+(1|ID),
                          tankCounts[-i,])
  all.res[i]<-fixef(mymodel)[4]
}
all.res

tankCounts$Copper<-c(seq(1,9,1),seq(1,9,1))
summary(manova(cbind(COV,r3)~Diversity+Diversity/ID,tankCounts))
fit<-aov(COV~Diversity+Error(ID),tankCounts)
summary(fit)


fit<-aov(r3~Diversity+Error(ID),tankCounts)
summary(fit)
t.test(r3~Copper,tankCounts,paired=TRUE)

library(TukeyC)

tuk = TukeyC(tankCounts,
             model = 'r3 ~ Diversity + Error(ID)',
             error = 'ID',
             which = 'Diversity',
             fl1=1,
             sig.level = 0.05
)

summary(tuk)
