rm(list=ls())

setwd("")
load("KrukUWr2023.RData")
library(data.table)
library(ggplot2)

set.seed(1)
######################################################## Zadanie 1

summary(cases)
summary(events)

Cases <- data.table(cases)
Events <- data.table(events)

# Cases <- Cases[sample((1:dim(Cases)[1]),0.5*dim(Cases)[1])]

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



######################################################## Zadanie 2

setkey(Cases,CaseId)
setkey(Events,CaseId)

Payments <- Events[Month <= 12,.(P12M = sum(ifelse(is.na(PaymentAmount),0,PaymentAmount)), Qty12M = sum(ifelse(is.na(PaymentAmount),0,1))),by=.(CaseId)]
setkey(Payments,CaseId)

Cases <- Cases[Payments[,.(CaseId,P12M,Qty12M)],nomatch=0][,Client := 'B']
Cases[P12M*1.0/TOA > 0.005 | Qty12M >= 3, Client := 'G']
Cases[, Good:=ifelse(Client=='G',1,0)]
Cases[, SR12M:=P12M*1.0/TOA]

Cases <- Cases[SR12M<quantile(Cases[,SR12M], probs=1-Proportion),]
Cases <- Cases[SR12M >= 0,]


summary(Cases)


# Korelacja

library(corrplot)
corrplot(cor(Cases[,.SD,.SDcols = Variables]), order = "hclust", tl.col='black', tl.cex=.75)


# W³¹czenie H2O

# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download packages that H2O depends on.
pkgs <- c("RCurl","jsonlite")
for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}

# Now we download, install and initialize the H2O package for R.
install.packages("h2o", type="source", repos="http://h2o-release.s3.amazonaws.com/h2o/rel-yau/10/R")

# Finally, let's load H2O and start up an H2O cluster
library(h2o)
h2o.init(nthreads = -1)


# Wybór cech

Variables <- c(        "LoanAmount",
                       "TOA",
                       "Principal",
                       #"Interest",
                       #"Other",
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
                       "Bailiff"
                       #"ClosedExecution"
)




# Zbiory trn/val

n <- Cases[, .N]
indexTrn <- sample(1:n, 0.5*n)
CasesTrn <- Cases[indexTrn,]
CasesTst <- Cases[-indexTrn,]

summary(CasesTrn)
summary(CasesTst)


# Sztuczne portfele w zbiorze Tst

library(cluster)
skupienia = clara(CasesTst[,.SD,.SDcols = Variables], k = 20, metric = "euclidean", stand = FALSE, samples = 5, sampsize = 1000, trace = 0, medoids.x = TRUE, keep.data = FALSE, rngR = TRUE)

#skupienia$clusinfo   #Info o skupieniach 
#skupienia$medoids    #Wspó³rzêdne medoidów
#skupienia$i.med      #Medoidy
#skupienia$clustering #NumerySkupieñ

CasesTstP <- copy(CasesTst)
CasesTstP[,skupienie := skupienia$clustering]
CasesTstP[,.N,by=skupienie]




library(scatterplot3d)
library(rgl)
library(car)
CasesStd <- data.table(scale(CasesTstP[,.SD,.SDcols = Variables]))
pca <- prcomp(CasesStd, center = FALSE, scale = FALSE)
CasesPCA <- data.table(as.matrix(CasesStd) %*% pca$rotation)
CasesPCA[, skupienie:=CasesTstP[,skupienie]]
scatter3d(x = CasesPCA[,PC1], y = CasesPCA[,PC4], z = CasesPCA[,PC3], groups = as.factor(CasesPCA[,skupienie]), surface=FALSE)




# Cechy objaœniana i objaœniaj¹ca
y <- 'SR12M'
x <- Variables

# Wczytanie zbiorów jako h2o

train <- as.h2o(CasesTrn) 
val <- as.h2o(CasesTst) 
valP <- as.h2o(CasesTstP) 



# Przeszukiwanie po kracie (Cartesian Grid Search)

# hyperparameters - okrojone
hidden_opt <- list(c(10)) 
l1_opt <- c(1e-4,1e-3) 
epochs_opt <- c(5,50)
input_dropout_ratio_opt = c(0.1, 0.4)
hidden_dropout_ratio_opt = c(0.1, 0.4)

# hyperparameters
hidden_opt <- list(c(5), c(10)) 
l1_opt <- c(1e-4,1e-3) 
epochs_opt <- c(5,10,30,50)
input_dropout_ratio_opt = c(0.1, 0.2, 0.3, 0.4)
hidden_dropout_ratio_opt = c(0.1, 0.2, 0.3, 0.4)

hyper_params <- list(hidden = hidden_opt, 
                     l1 = l1_opt, 
                     epochs = epochs_opt, 
                     hidden_dropout_ratios = hidden_dropout_ratio_opt, 
                     input_dropout_ratio = input_dropout_ratio_opt) 

model_grid <- h2o.grid("deeplearning", 
                       grid_id = "uwr15", 
                       hyper_params = hyper_params, 
                       x = x, 
                       y = y, 
                       distribution = "gaussian", 
                       loss = "Quadratic",
                       activation = "RectifierWithDropout", 
                       training_frame = train, 
                       validation_frame = val, 
                       #rate = 0.2,
                       #score_interval = 2, 
                       stopping_rounds = 3, 
                       stopping_tolerance = 0.05, 
                       stopping_metric = "MAE")  


head(model_grid@summary_table)
tail(model_grid@summary_table)



model_grid_result <- data.table(model_grid@summary_table)

summary <- data.table()
for (i in 1:dim(model_grid_result)[1]) { #i=1
  
  # Predykcja
  
  model <- h2o.getModel(model_grid_result[i,model_ids])
  forecast <- h2o.predict(model, newdata = valP)
  as.data.frame(forecast$predict)
  CasesTstP <- data.table(cbind(CasesTstP, as.data.frame(forecast$predict)))
  
  #summary(CasesTstP)
  
  dev <- mean(CasesTstP[,.(dev=(abs(sum(P12M)-sum(predict*TOA)))/sum(P12M)),by=skupienie][,dev])
  
  summary <- rbind(summary,data.table(
    id=model_grid_result[i,model_ids], 
    epochs=model_grid_result[i,epochs], 
    hidden=model_grid_result[i,hidden], 
    input_dropout_ratio=model_grid_result[i,input_dropout_ratio], 
    l1=model_grid_result[i,l1], 
    residual_deviance=as.numeric(model_grid_result[i,residual_deviance]),
    dev12M=dev,
    portfolio_type='clusters'
  ))
  
  CasesTstP[,predict:=NULL]
}



# Sprawdzenie zale¿noœci per portfel

ggplot(data = summary, aes(x = residual_deviance, y = dev12M)) +
  geom_point()








# Sztuczne portfele losowo

CasesTstP <- copy(CasesTst)
CasesTstP[, skupienie := sample(c(1:20), nrow(CasesTstP), replace = TRUE)]
CasesTstP[,.N,by=skupienie]


# Wczytanie zbiorÃ³w jako h2o

#train <- as.h2o(CasesTrn) 
#val <- as.h2o(CasesTst) 
valP <- as.h2o(CasesTstP) 


# prognoza

for (i in 1:dim(model_grid_result)[1]) { #i=1
  
  # Predykcja
  
  model <- h2o.getModel(model_grid_result[i,model_ids])
  forecast <- h2o.predict(model, newdata = valP)
  as.data.frame(forecast$predict)
  CasesTstP <- data.table(cbind(CasesTstP, as.data.frame(forecast$predict)))
  
  #summary(CasesTstP)
  
  dev <- mean(CasesTstP[,.(dev=(abs(sum(P12M)-sum(predict*TOA)))/sum(P12M)),by=skupienie][,dev])
  
  summary <- rbind(summary,data.table(
    id=model_grid_result[i,model_ids], 
    epochs=model_grid_result[i,epochs], 
    hidden=model_grid_result[i,hidden], 
    input_dropout_ratio=model_grid_result[i,input_dropout_ratio], 
    l1=model_grid_result[i,l1], 
    residual_deviance=as.numeric(model_grid_result[i,residual_deviance]),
    dev12M=dev,
    portfolio_type='random'
  ))
  
  CasesTstP[,predict:=NULL]
}



# Sprawdzenie zale¿noœci per portfel

ggplot(data = summary, aes(x = residual_deviance, y = dev12M, color = portfolio_type)) +
  geom_point() +
  geom_smooth(method=lm, aes(fill=portfolio_type)) 
  



