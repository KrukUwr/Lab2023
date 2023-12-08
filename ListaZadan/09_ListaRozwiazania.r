
setwd("")

library(data.table)
library(car)
library(MASS)
load("KrukUWr2023.Rdata")
set.seed(1)


### Zadanie 1 ####       

#  Przygotuj ramke danych `cases_loanamount` bazujac na tabeli `cases` tylko z przypadkami kredytow gotowkowych. 

cases_loanamount <- cases[Product == "Cash loan",]

# * Sprawdz miary pozycyjne wszystkich cech w utworzonej ramce.

summary(cases_loanamount)

# * Sprawdz licznosc brakow danych dla wszystkich cech.

sapply(cases_loanamount, function(x){sum(is.na(x))})




#### Zadanie 2 ####

# Interesujaca nas cecha bedzie `LoanAmount`, ktorej wartosc bedziemy modelowac w celu zastapienia `NA's`.

# * Metoda losowania z rozkladu zastap braki danych w cesze `Land`.

 dist <- prop.table(table(cases_loanamount$Land))
 sampled <- sample(x = sort(unique(cases_loanamount[!is.na(Land), Land])),
                   size = sum(is.na(cases_loanamount[,Land])),
                   prob = dist,
                   replace = TRUE)
 
 cases_loanamount[is.na(Land) , Land := sampled]
 
 
# * Metoda ekspercka zastap braki danych w cechach: `Other`,`Gender`, `MeanSalary` i `GDPPerCapita`.
 
 tmp <- cases[!is.na(MeanSalary), .(MS = min(MeanSalary), GDP= min(GDPPerCapita)), by = Land]
 cases_loanamount <- cases_loanamount[tmp, on = "Land"]
 cases_loanamount[, ':='(MeanSalary = MS, GDPPerCapita = GDP)]
 cases_loanamount[,':='(MS = NULL, GDP = NULL)]
 
 cases_loanamount[is.na(Other), Other:=TOA-Principal-Interest]
 cases_loanamount[is.na(Gender), Gender:="Company"]
 
 
# * Dokonaj dyskretyzacji cechy DPD a wartosciom `NA` przypisz poziom `brak danych`, ceche zapisz jako `D_DPD`

cases_loanamount[, D_DPD := cut(DPD, breaks = c(-Inf, 180, 360, 720, Inf))]
 
cases_loanamount[is.na(D_DPD), "D_DPD"] <- "brak danych"
cases_loanamount[,DPD:=NULL]
 




 #### Zadanie 3 ####
 
 # Na podstawie `cases_loanamount` przygotuj ramki danych:

 #   * `cases_loanamount_nas`, ktora zawiera wszystkie przypadki brakujacych wartosci zmiennej `LoanAmount`.
 
cases_loanamount_nas <- cases_loanamount[is.na(LoanAmount),]
cases_loanamount_wonas <- cases_loanamount[!is.na(LoanAmount),]
cases_loanamount_wonas <- cases_loanamount_wonas[sample(1:nrow(cases_loanamount_wonas), 10000),]
 
ix_trn <- sample(1:nrow(cases_loanamount_wonas), nrow(cases_loanamount_wonas)*0.7)
ix_tst <- c(1:nrow(cases_loanamount_wonas))[-ix_trn]





#### Zadanie 4 ####

# Zbadaj rozklad  `cases_loanamount_wonas$LoanAmount`. Jezeli jest taka potrzeba zaproponuj transformacje. 
 
summary(cases_loanamount_wonas$LoanAmount)
boxplot(cases_loanamount_wonas$LoanAmount)
plot(density(cases_loanamount_wonas$LoanAmount))

cases_loanamount_wonas[LoanAmount==0, LoanAmount:=1]

plot(density(log(cases_loanamount_wonas$LoanAmount)))

cases_loanamount_wonas[, LoanAmount_log := log(LoanAmount)]

boxplot(cases_loanamount_wonas$LoanAmount_log)




#### Zadanie 5 ####

# Zbuduj model regresji  liniowej `m1` gdzie zmienna modelowana jest `LoanAmount` a zmiennymi objasniajacymi :
# `TOA`, `Principal`, `Interest`, `Other`, `GDPPerCapita`, `MeanSalry`, `D_DPD`, `Age`, `Gender` 
options(scipen=999)

fmla <- as.formula(LoanAmount_log ~ TOA + Other + Interest + Principal + D_DPD + Age + Gender + GDPPerCapita)

m1 <- lm(fmla, data = cases_loanamount_wonas, subset = ix_trn)
m1
summary(m1) 
 




#### Zadanie 6 #####

plot(density(m1$residuals))

shapiro.test(sample(m1$residuals, 5000))


# p-value < 0.05 - odrzucamy hipoteze o normalnosci rozkladu




#### Zadanie 7 #####

# Korzystajac ze zbioru testowego dokonaj predykcji (wyniki zapisz w obiekcie `m1_pred`), 
# a nastepnie oblicz bez uzywania gotowych funkcji: RSS, RSE, TSS i R^2.

RSS_trn <- sum(m1$residuals^2)

p <- length(m1$coefficients)-1
n <- nrow(cases_loanamount_wonas[ix_trn])

RSE_trn <- sqrt(RSS_trn/(n - p - 1))
TSS_trn <- sum((cases_loanamount_wonas[ix_trn]$LoanAmount_log - mean(cases_loanamount_wonas[ix_trn]$LoanAmount_log))^2)
(R2_trn <- 1 - RSS_trn/TSS_trn)


n <- nrow(cases_loanamount_wonas[-ix_trn])

m1_pred_tst <- predict.lm(m1, newdata = cases_loanamount_wonas[-ix_trn,])
rsids_tst <- cases_loanamount_wonas[-ix_trn]$LoanAmount_log - m1_pred_tst

RSS_tst <- sum(rsids_tst^2)
RSE_tst <- sqrt(RSS_tst/(n - p - 1))

TSS_tst <- sum((cases_loanamount_wonas[-ix_trn]$LoanAmount_log - mean(cases_loanamount_wonas[-ix_trn]$LoanAmount_log))^2)

(R2 <- 1 - RSS_tst/TSS_tst) #zgodnie z oczekiwaniami R^2 gorszy na testowym




#### Zadanie 8 #####

# Dokonaj oceny jakosci predykcji za pomoca znanych Ci miar


# Zmierzymy za pomoca RMSE (Root Mean Square Error) i MAPE (Mean Absolute Percentage Error)

m1_pred_tst <- exp(m1_pred_tst)
rsids_tst <- cases_loanamount_wonas[-ix_trn]$LoanAmount - m1_pred_tst
RMSE <- sqrt(mean(rsids_tst^2))

APE <- abs(rsids_tst)/cases_loanamount_wonas[-ix_trn]$LoanAmount
summary(APE)
MAPE <-mean(APE)
quantile(APE, seq(0, 1, .05))




############################################### Checking the avg predictor


AvgTrain <- mean(cases_loanamount_wonas[ix_trn]$LoanAmount)
APE_avg <- abs(AvgTrain-cases_loanamount_wonas[-ix_trn]$LoanAmount)/cases_loanamount_wonas[-ix_trn]$LoanAmount
summary(APE_avg)
quantile(APE_avg, seq(0, 1, .05))
MAPE_avg <-mean(APE_avg)




