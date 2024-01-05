
setwd("")
load("KrukUWr2023.RData")

library(data.table)
library(ggplot2)
library(gam)
library(pROC)
library(ada)
library(corrplot)
library(cluster)



################ Analiza eksploracyjna ----

summary(cases)
summary(events)

Cases <- data.table(cases)
Events <- data.table(events)


# Decoding variables 

Cases[,CreditCard := ifelse(Product=="Credit card",1,0)]
Cases[,Female := ifelse(Gender=="FEMALE",1,0)]


# Handling missing data

Variables = c(         "LoanAmount",
                       "TOA",
                       "Principal",
                       "Interest",
                       "Other",
                       "D_ContractDateToImportDate",
                       "DPD",
                       "PopulationInCity",
                       "Age",
                       "LastPaymentAmount",
                       "M_LastPaymentToImportDate",
                       "GDPPerCapita",
                       "MeanSalary",
                       "CreditCard",
                       "Female",
                       "Bailiff",
                       "ClosedExecution"
)



nullCounts <- lapply(Cases[,.SD,.SDcols=Variables], function(x) sum(is.na(x)))


# Imputation with avg

variables <- c(        "LoanAmount",
                       "TOA",
                       "Principal",
                       "Interest",
                       "Other",
                       "D_ContractDateToImportDate",
                       "DPD",
                       "PopulationInCity",
                       "Age",
                       "LastPaymentAmount",
                       "M_LastPaymentToImportDate",
                       "GDPPerCapita",
                       "MeanSalary"
)



for (variable in variables) {      ## variable = 'Age'
  if (eval(parse(text=paste("nullCounts$",variable,sep=""))) > 0) {
    avg <- eval(parse(text=paste("mean(Cases[,",variable,"],na.rm=TRUE)",sep="")))
    eval(parse(text=paste("Cases[is.na(",variable,"), ",variable,":=avg]",sep="")))
  }           
}


#  Other imputation

summary(Cases)

Cases[is.na(Female),Female:= ifelse(runif(nullCounts$Female,0,1)<Cases[,mean(Female,na.rm=TRUE)],1L,0L)]
Cases[is.na(Bailiff),Bailiff:= ifelse(runif(nullCounts$Bailiff,0,1)<Cases[,mean(Bailiff,na.rm=TRUE)],1L,0L)]

Cases[is.na(ClosedExecution) & Bailiff==0, ClosedExecution:= 0L]
Cases[is.na(ClosedExecution), ClosedExecution:= ifelse(runif(dim(Cases[is.na(ClosedExecution),])[1],0,1)<Cases[,mean(ClosedExecution,na.rm=TRUE)],1L,0L)]


#  Proportion of tail data to be removed from the dataset

summary(Cases)
Proportion = 0.001

Cases <- Cases[LoanAmount<quantile(Cases[,LoanAmount], probs=1-Proportion),]
Cases <- Cases[DPD<quantile(Cases[,DPD], probs=1-Proportion),]
Cases <- Cases[LastPaymentAmount<quantile(Cases[,LastPaymentAmount], probs=1-Proportion),]


# Korelacja 

corrplot(cor(Cases[,.SD,.SDcols = Variables]), order = "hclust", tl.col='black', tl.cex=.75)



################ Cechy objaśniane ----

setkey(Cases,CaseId)
setkey(Events,CaseId)

Payments <- Events[Month <= 12
                   ,.(P12M = sum(ifelse(is.na(PaymentAmount),0,PaymentAmount)), Qty12M = sum(ifelse(is.na(PaymentAmount),0,1)))
                   ,by=.(CaseId)]
setkey(Payments,CaseId)


Cases <- Cases[Payments[,.(CaseId,P12M,Qty12M)],nomatch=0][,Client := 'B']
Cases[P12M*1.0/TOA > 0.005 | Qty12M >= 3, Client := 'G']
Cases[, Good:=ifelse(Client=='G',1,0)]
Cases[, SR12M:=P12M*1.0/TOA]

Cases <- Cases[SR12M<quantile(Cases[,SR12M], probs=1-Proportion),]
Cases <- Cases[SR12M >= 0,]

summary(Cases)




################ Zbiór trn/val ----

n <- Cases[, .N]
indexTrn <- sample(1:n, 0.5*n)
CasesTrn <- Cases[indexTrn,]
CasesTst <- Cases[-indexTrn,]

summary(CasesTrn)
summary(CasesTst)



################ Sztuczne portfele ----

skupienia = clara(CasesTst[,.SD,.SDcols = Variables], k = 20, metric = "euclidean", stand = FALSE, samples = 5, sampsize = 1000, trace = 0, medoids.x = TRUE, keep.data = FALSE, rngR = TRUE)


#skupienia$clusinfo   #Info o skupieniach 
#skupienia$medoids    #Współrzędne medoidów
#skupienia$i.med      #Medoidy
#skupienia$clustering #NumerySkupień

CasesTstP <- copy(CasesTst)
CasesTstP[,skupienie := skupienia$clustering]



# Skupienia jako portfele

CasesTstP[,.N,by=skupienie]





################ Regresja SR12M ----


#  Model

model_SR12M <- gam(as.formula(paste("SR12M~",paste(Variables,collapse="+"))), family=gaussian(link=identity), data = CasesTrn)
                                       
summary(model_SR12M)
summary(CasesTrn)


#  Stepwise selection

Variables_Discrete = c("CreditCard","Female","Bailiff","ClosedExecution")

Lista <- c()
for (i in 1:length(Variables)) {
                           
    if (sum(Variables[i] == Variables_Discrete) == 0) {
       Lista[[i]] = as.formula(paste("~1+", Variables[i], 
                  "+s(", Variables[i], ",2)", "+s(", 
                  Variables[i], ",3)", sep = ""))
                  }
    else {
                Lista[[i]] <- as.formula(paste("~1+", Variables[i], 
                  sep = ""))
    }
}

step.model_SR12M <-step.Gam(model_SR12M, scope=Lista, dir='both')

summary(step.model_SR12M)


#  Final model

model_SR12M_gam <- gam(
                   SR12M~
                          s(LoanAmount,3) +                    
                          s(TOA,3) +                            
                          s(Principal,3) +                       
                          s(D_ContractDateToImportDate,3) +     
                          s(DPD,3) +                             
                          PopulationInCity +                                                         
                          s(Age,3) +                             
                          s(LastPaymentAmount,3) +               
                          s(M_LastPaymentToImportDate,3) +       
                          GDPPerCapita +                                                             
                          MeanSalary +                                                               
                          Female                              
, family=gaussian(link=identity), data = CasesTrn)

summary(model_SR12M_gam)


#  Partial prediction plots

plot.Gam(model_SR12M_gam,ask=TRUE)


#  Payments 12M Forecast

forecast_gam <- predict.Gam(model_SR12M_gam, newdata=CasesTst, type='response')*CasesTst[,TOA]
CasesTstP <- data.table(cbind(CasesTstP, as.data.frame(forecast_gam)))


# Obliczenie średniego odchylenia per portfel

dev_gam <- CasesTstP[,.(dev=(abs(sum(P12M)-sum(forecast_gam)))/sum(P12M)),by=skupienie][,dev]
(mean(dev_gam))
dev_gam_v2 <- CasesTstP[,.(dev=(abs(sum(P12M)-sum(ifelse(forecast_gam<0,0,forecast_gam))))/sum(P12M)),by=skupienie][,dev]
(mean(dev_gam_v2))



#  Final model - GLM

model_SR12M_glm <- glm(
  SR12M~
    LoanAmount +                    
    TOA +                            
    Principal +                       
    D_ContractDateToImportDate +     
    DPD +                             
    PopulationInCity +                                                         
    Age +                             
    LastPaymentAmount +               
    M_LastPaymentToImportDate +       
    GDPPerCapita +                                                             
    MeanSalary +                                                               
    Female                              
  , family=gaussian(link=identity), data = CasesTrn)

summary(model_SR12M_glm)


#  Payments 12M Forecast

forecast_glm <- predict.glm(model_SR12M_glm, newdata=CasesTst, type='response')*CasesTst[,TOA]
CasesTstP <- data.table(cbind(CasesTstP, as.data.frame(forecast_glm)))


# Obliczenie średniego odchylenia per portfel

dev_glm <- CasesTstP[,.(dev=(abs(sum(P12M)-sum(forecast_glm)))/sum(P12M)),by=skupienie][,dev]
(mean(dev_glm))
dev_glm_v2 <- CasesTstP[,.(dev=(abs(sum(P12M)-sum(ifelse(forecast_glm<0,0,forecast_glm))))/sum(P12M)),by=skupienie][,dev]
(mean(dev_glm_v2))


# Porównanie

summary=data.table()
summary=cbind(data.table(dev_gam=dev_gam, dev_glm=dev_glm, NoOfCases=CasesTstP[,.N,by=skupienie][,N]))

ggplot(data = summary, aes(x = dev_gam, y = dev_glm)) +
  geom_point(aes(size=NoOfCases)) 





################ Classification Good ----

#  Model

model_SR12M <- gam(as.formula(paste("Good~",paste(Variables,collapse="+"))), family=binomial(link = "logit"), data = CasesTrn)

summary(model_SR12M)


#  Stepwise selection

Variables_Discrete = c("CreditCard","Female","Bailiff","ClosedExecution")

Lista <- c()
for (i in 1:length(Variables)) {

    if (sum(Variables[i] == Variables_Discrete) == 0) {
       Lista[[i]] = as.formula(paste("~1+", Variables[i],
                  "+s(", Variables[i], ",2)", "+s(",
                  Variables[i], ",3)", sep = ""))
                  }
    else {
                Lista[[i]] <- as.formula(paste("~1+", Variables[i],
                  sep = ""))
    }
}

step.model_SR12M <-step.Gam(model_SR12M, scope=Lista, dir='both')

summary(step.model_SR12M)


#  Final model

model_SR12M_gam <- gam(
                   Good ~
                        s(LoanAmount,3) +
                        Interest +
                        s(Other, 3) +
                        s(D_ContractDateToImportDate, 3) +
                        s(DPD, 3) +
                        s(Age, 3) +
                        s(LastPaymentAmount, 3) +
                        s(M_LastPaymentToImportDate, 3) +
                        s(GDPPerCapita, 3) +
                        s(MeanSalary, 3) +
                        CreditCard +
                        Female
, family=binomial(link = "logit"), data = CasesTrn)


summary(model_SR12M_gam)


#  Partial prediction plots

plot.Gam(model_SR12M_gam,ask=TRUE)


#  Payments 12M Forecast

Cases[,Forecast := predict.Gam(model_SR12M_gam, newdata=Cases, type='response')]

Cases[,Band := cut(Forecast, breaks = 10)]
CasesTrn[,Band := Cases[indexTrn,Band]]
CasesTst[,Band := Cases[-indexTrn,Band]]
CasesTstP[,Band := Cases[-indexTrn,Band]]

Payments_Group <- tapply(CasesTrn[,P12M],CasesTrn[,Band],sum)
TOA_Group <- tapply(CasesTrn[,TOA],CasesTrn[,Band],sum)
SR_Group <- Payments_Group/TOA_Group

Forecast <- data.table(cbind(Band=row.names(SR_Group),SR=SR_Group*1.0))
CasesTstP <- CasesTstP[Forecast, on = "Band"]
CasesTstP[, forecast_logit := as.numeric(SR)*as.numeric(TOA)]


# Obliczenie średniego odchylenia per portfel

dev_logit_gam <- CasesTstP[,.(dev=(abs(sum(P12M)-sum(forecast_logit)))/sum(P12M)),by=skupienie][,dev]
(mean(dev_logit_gam))



#  Final model - GLM-Logit

model_SR12M_glm <- glm(
  Good ~
    LoanAmount +
    Interest +
    Other +
    D_ContractDateToImportDate +
    DPD +
    Age +
    LastPaymentAmount +
    M_LastPaymentToImportDate +
    GDPPerCapita +
    MeanSalary +
    CreditCard +
    Female
  , family=binomial(link = "logit"), data = CasesTrn)


summary(model_SR12M_glm)



#  Payments 12M Forecast

Cases[,Forecast := predict.glm(model_SR12M_glm, newdata=Cases, type='response')]

Cases[,Band := cut(Forecast, breaks = 10)]
CasesTrn[,Band := Cases[indexTrn,Band]]
CasesTst[,Band := Cases[-indexTrn,Band]]
CasesTstP[,Band := Cases[-indexTrn,Band]]

Payments_Group <- tapply(CasesTrn[,P12M],CasesTrn[,Band],sum)
TOA_Group <- tapply(CasesTrn[,TOA],CasesTrn[,Band],sum)
SR_Group <- Payments_Group/TOA_Group

Forecast_glm <- data.table(cbind(Band=row.names(SR_Group),SR_glm=SR_Group*1.0))
CasesTstP <- CasesTstP[Forecast_glm, on = "Band"]
CasesTstP[, forecast_logit_glm := as.numeric(SR_glm)*as.numeric(TOA)]


# Obliczenie średniego odchylenia per portfel

dev_logit_glm <- CasesTstP[,.(dev=(abs(sum(P12M)-sum(forecast_logit_glm)))/sum(P12M)),by=skupienie][,dev]
(mean(dev_logit_glm))




################ Porównanie modeli ----

# Dodamy jeszcze Boosting Rzeczywisty

model_ADA <- ada(CasesTrn[,.SD,.SDcols = Variables],CasesTrn[,Good],type="real",nu=0.1,iter=150,loss="exponential")

scores_gam <- predict.Gam(model_SR12M_gam, CasesTst, type = "response")
scores_glm <- predict.glm(model_SR12M_glm, CasesTst, type = "response")
scores_ada <- predict(model_ADA, CasesTst[,.SD,.SDcols = Variables],type="prob")[,2]

roc_GAM <- roc(CasesTst[,Good], scores_gam, direction="<")
roc_GLM <- roc(CasesTst[,Good], scores_glm, direction="<")
roc_ADA <- roc(CasesTst[,Good], scores_ada, direction="<")

plot(roc_GAM, col=2, lwd=3, main="ROC_comparison", asp=1)
plot(roc_GLM, col=3, lwd=3, add=TRUE)
plot(roc_ADA, col=4, lwd=3, add=TRUE)
legend(0.3, 0.67, c('GAM', 'GLM', 'ADA'), 2:5)





# Confusion matrix

table(ifelse(scores_gam>0.35,1,0), CasesTst[,Good])


#McNemar's Test

Qual_GAM <- (ifelse(scores_gam>0.35,1,0) == CasesTst[,Good])*1
Qual_ADA <- (ifelse(scores_ada>0.35,1,0) == CasesTst[,Good])*1

tab <- table(Qual_GAM, Qual_ADA)

chi_test <- ((abs(tab[2,1]-tab[1,2])-1)^2)/(tab[2,1]+tab[1,2])
qchisq(.95, df=1)
mcnemar.test(tab, correct = TRUE)



# Resampled paired t-test

noOfTrials <- 10
n <- dim(Cases)[1] 

p <- c()

for (i in 1:noOfTrials) { # i=1
  
  
  index.learn = sample(1:n, dim(Cases)[1]/2)
  CasesTst = copy(Cases[-index.learn,])
  CasesTrn = copy(Cases[index.learn,])
  
  
  # Model A
  
  model_A <- gam(
    Client ~
      s(TOA,2) +
      LoanAmount +
      Interest +
      s(Other, 3) +
      s(D_ContractDateToImportDate, 3) +
      s(DPD, 3) +
      s(Age, 3) +
      s(LastPaymentAmount, 3) +
      s(M_LastPaymentToImportDate, 3) +
      s(GDPPerCapita, 3) +
      s(MeanSalary, 3) +
      CreditCard +
      Female
    , family=binomial(link = "logit"), data = CasesTrn)    
  
  scores_A <- predict.Gam(model_GAM, CasesTst, type = "response")
  
  
  
  # Model B  
  
  model_B <- lm( Client ~
                   LoanAmount +
                   Interest +
                   Other +
                   D_ContractDateToImportDate +
                   DPD +
                   Age +
                   LastPaymentAmount +
                   M_LastPaymentToImportDate +
                   GDPPerCapita +
                   MeanSalary +
                   CreditCard +
                   Female
                 , data = CasesTrn)
  
  
  scores_B = predict(model_LM, CasesTst)
  
  
  
  Tab_A <- table(ifelse(scores_A>0.35,1,0), CasesTst[,Client])
  Tab_B <- table(ifelse(scores_B>0.35,1,0), CasesTst[,Client])
  
  Misclass_A <- (Tab_A[2,1]+Tab_A[1,2])/dim(CasesTst)[1]
  Misclass_B <- (Tab_B[2,1]+Tab_B[1,2])/dim(CasesTst)[1]
  
  p <- c(p, Misclass_A-Misclass_B)
  
}


t_stat <- mean(p)*sqrt(noOfTrials)/sqrt(sum((p-mean(p))^2)/(noOfTrials-1))
qt(.025, df=noOfTrials-1)





################ Concurvity ----

library(mgcv)

model_SR12M <- gam(
                   Good ~
                        LoanAmount +
                        Interest +
                        s(Other) +
                        s(D_ContractDateToImportDate) +
                        s(DPD) +
                        s(Age) +
                        s(LastPaymentAmount) +
                        s(M_LastPaymentToImportDate) +
                        s(GDPPerCapita) +
                        s(MeanSalary) +
                        CreditCard +
                        Female
, family=binomial(link = "logit"), data = CasesTrn)


summary(model_SR12M)
plot(model_SR12M)

concurvity(model_SR12M,full=TRUE) # cecha vs reszta cech
concurvity(model_SR12M,full=FALSE) # cechy parami
# 0 - brak, 1 - krzywoliniowość



vis.concurvity(model_SR12M)



vis.concurvity <- function(b, type="estimate"){
   cc <- concurvity(b, full=FALSE)[[type]]

   diag(cc) <- NA
   cc[lower.tri(cc)]<-NA

   layout(matrix(1:2, ncol=2), widths=c(5,1))
   opar <- par(mar=c(5, 6, 5, 0) + 0.1)
   # main plot
   image(z=cc, x=1:ncol(cc), y=1:nrow(cc), ylab="", xlab="",
         axes=FALSE, asp=1, zlim=c(0,1))
   axis(1, at=1:ncol(cc), labels = colnames(cc), las=2)
   axis(2, at=1:nrow(cc), labels = rownames(cc), las=2)
   # legend
   opar <- par(mar=c(5, 0, 4, 3) + 0.1)
   image(t(matrix(rep(seq(0, 1, len=100), 2), ncol=2)),
         x=1:3, y=1:101, zlim=c(0,1), axes=FALSE, xlab="", ylab="")
   axis(4, at=seq(1,101,len=5), labels = round(seq(0,1,len=5),1), las=2)
   par(opar)
}


