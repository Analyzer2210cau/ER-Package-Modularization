



data2=NULL

data2 <- read.csv("F:\\IEICIE(Special-Issue)\\WIN\\Experimental-Data\\Dataset\\Eclipse-3.0.csv")
data2

a<-data2["bugrpon"]
b<-data2["pkgKLOC"]
result <- list()

nProness <- cumsum(data2$bugrpon)
kLoc <- cumsum(data2$pkgKLOC)

plot(kLoc, nProness)
colnames(data2)

#######################################################RC Martin Model#######################################



data2=NULL
#D:\MultiObjective-Effort-Aware-Performance of Predictors
data2 <- read.csv("F:\\IEICIE(Special-Issue)\\WIN\\Experimental-Data\\Dataset\\Eclipse-3.0.csv")
data2

data2=data.frame(data2)
colnames(data2)
data2<-data2[, c("A", "N", "post", "Ce", "D", "I", "Ca")]
data2

colnames(data2)
library("dplyr")

attach(data2)

n.folds<-10
folds <- cut(sample(seq_len(nrow(data2))),  breaks=n.folds, labels=FALSE)
all.confusion.tables
TP
FN
FP
TN
accuracy
precission
recall
MFM
all.confusion.tables=NULL
TP=NULL
FN=NULL
FP=NULL
TN=NULL
MFM=NULL
accuracy=NULL
precission=NULL
recall=NULL


sensitivity
specificity

sensitivity=NULL
specificity=NULL

thresh.pred=NULL
logit.prob=NULL

all.confusion.tables <- list()

colnames(data2)
#all.response <- all.predictor <- aucs <- c()

for(i in seq_len(n.folds))
  
{
  
  
  train <-filter(data2, folds !=i)
  
  test <- filter(data2, folds==i)
  
  
  #glm.model <- glm((post>0) ~   N+A+Ce+Ca+I+D,  data=train, family= "binomial")
  
  #glm.model <- glm((post>0)~N+A+Ce+Ca+I+D,  data=train, family= "binomial")
  
  glm.model <- glm((post>0)~N+A+Ce+Ca+I+D,  data=train, family= "binomial")
  
  #glm.model <- glm((post>0) ~   N+A+Ce+Ca+I+D+PGF+PSC,  data=train, family= "binomial")
  # glm.model <- glm((post>0) ~   N+A+Ce+Ca+I+D+IIPUD+IIPED+PGF+PSC,  data=train, family= "binomial")
  # glm.model <- glm((Post>0) ~  IIPUD+IIPED+IPCI+PGF+PSC,  data=train, family= "binomial")
  # glm.model <- glm((post>0) ~   N+A+Ce+Ca+I+D+MII+NC+BCF+IC+AIC+SAVI+PPI+APIUP+IIPUD+IIPED+IPCI+PGF+PSC,  data=train, family= "binomial")
  
  
  
  model.pred<-predict(glm.model, newdata=test[,-3])
  
  thresh.pred <-model.pred>=0.5
  #mapply(result,thresh.pred, SIMPLIFY = TRUE)
  
  all.confusion.tables[[i]] <- table(factor(test$post>0, levels=c(F,T)), factor(thresh.pred, levels=c(F,T)))
  
  TN[i]=all.confusion.tables[[i]][1,1]
  FN[i]=all.confusion.tables[[i]][2,1]
  FP[i]=all.confusion.tables[[i]][1,2]
  TP[i]=all.confusion.tables[[i]][2,2]
  
  
  accuracy[i] <- (TP[i] + TN[i]) / (TN[i] + FN[i] + FP[i] + TP[i])
  precission[i] <-  (TP[i]) / (TP[i] + FP[i])
  recall[i] <- TP[i] / (TP[i] + FN[i])}

  MFM[i]<- (2*recall[i]*precission[i])/(recall[i]+precission[i])
  
  sensitivity[i] <-  (TP[i]) / ( FN[i]+ TP[i])
  specificity[i] <- (TN[i]) / (TN[i]+FP[i]) 
  
  
}

thresh.pred

test 
train

TP
TN
FP
FN

all.confusion.tables
specificity
sensitivity
MFM
accuracy
precission
recall

ac<-summary(accuracy)
ac
pr<-mean(precission)
rec<-mean(recall)
sn<-mean(sensitivity)
sp<-mean(specificity)

ac
pr
rec

fmeasure=NULL
fmeasure= 2*rec*pr/(rec+pr)
fmeasure
blance=NULL

balance= 1- sqrt((0-sn)^2+(1-sp)^2)
max(MFM)
balance


#Eclipse 2.0: MFM=0.62, BPP=0.61 -----Eclipse 2.1: MFM= ,0.64  BPP= 0.59
#Eclipse 2.1: MFM=0.52, BPP=0.73 -----Eclipse 2.1: MFM=0.46 ,  BPP=0.74
#Eclipse 3.0: MFM=0.62, BPP=0.60 -----Eclipse 2.1: MFM=0.64 ,  BPP=0.52
#Ant 1.6: MFM=0.62, BPP=0.61 -----Eclipse 2.1: MFM= ,  BPP=
#Ant 1.7: MFM=0.62, BPP=0.61 -----Eclipse 2.1: MFM= ,  BPP=

