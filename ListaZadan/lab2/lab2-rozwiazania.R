library(data.table)
load("../../KrukUWr2023.RData")



# Zadanie #1 ------------------------------------------------------------------------------------------------------
# Sprawdź poprawność danych w zakresie punktów podanych ze slajdu *poprawność danych*. Na zajęciach wybierz 1-2 punkty.


# 1. skladowe zadluzenia
cases[, SumOfToaComponents := Principal+Interest+Other]
cases[, ToaDiff := TOA - SumOfToaComponents]

cases[abs(ToaDiff) >= 0.01, .N]

cases[abs(ToaDiff) >= 0.01, .(CaseId, Principal, Interest, Other, TOA, SumOfToaComponents, ToaDiff)]

# 1221 spraw gdzie różnica między TOA a sumą komponentów zadłużenia jest większa lub równa 1 grosz


# b. Relacja kapitał vs kwota pożyczki

cases[Principal-LoanAmount>0.01, .N]

cases[Principal-LoanAmount>0.01, .(CaseId, Product, LoanAmount, Principal, Diff=LoanAmount-Principal)]

# Liczba spraw, gdzie kapitał do spłaty jest większy niż kwota pożyczki: 8940


# c. Chronologia dat
cases[DPD > D_ContractDateToImportDate, .(CaseId, D_ContractDateToImportDate, DPD)]

# 131 spraw z nieprawidłową relacja DPD - liczba dni od podpisania umowy


# d. Relacja zmiennych związanych z egzekucją komorniczą

cases[,.N,by=.(Bailiff, ClosedExecution)]

# Brak relacji, które wskazywałyby na nieprawidłowości w danych.


# e. Relacja wartości w kolumnach związanych z jednostką terytorialną (Land)
# Kolumny: Land, GDPPerCapita, MeanSalary

land_info <- cases[,.N, by=.(Land, GDPPerCapita, MeanSalary)]
setorder(land_info, Land, na.last = TRUE)

# każdy land powienien wystąpić dokładnie raz, jeśli wystepuje więcej razy tzn. że mamy jakieś błędy

sum(duplicated(land_info$Land))

print(land_info[31:38])

# Są braki danych natomiast nie ma zmultiplikowanych wartości dla pojedynczego landu, więc wszystko wygląda OK.


# f. Relacja wartości w kolumnach związanych z ostatnią wpłatą

cases[LastPaymentAmount<0.0, ]
# Brak ujemtnych wpłat - OK

cases[LastPaymentAmount > 0 & M_LastPaymentToImportDate==50]
# Brak kwot wpłat tam gdzie liczba miesięcy od ostatniej wplaty przyjmuje sztuczną wartość 50 - OK

cases[is.na(LastPaymentAmount) & !is.na(M_LastPaymentToImportDate), .(M_LastPaymentToImportDate, LastPaymentAmount)]
cases[!is.na(LastPaymentAmount) & is.na(M_LastPaymentToImportDate), .(M_LastPaymentToImportDate, LastPaymentAmount)]
# Brak info w jednej z kolumn gdy druga jest NA. Również OK, choć hipotetycznie mogłoby się tak zdarzyć 
# i nie oznaczaloby to bledu

# g. Kwoty wpłat z tabeli events
events[PaymentAmount<0, .(CaseId, Month, PaymentAmount, NumberOfPayment)]

# Są sprawy, gdzie są ujemne wpłaty. Należy zdecydować co w takiej sytuacji zrobić. 
# Zostawić, przypisać 0.0 lub rozłożyć ujemne wpłaty na pozostałe dodatnie wpłaty w innych miesiącach dla danej sprawy.



## Zadanie #2
# Zdiagnozuj kolumny, które najlepiej różnicują 12 miesięczną skuteczność. 
# Mogą to być kolumny zarówno z tabeli *cases* jak i *events* (oprócz liczby wpłat).
# Z Tabeli events weź informacje z wierszy do max 6 miesiąca.

load("../../KrukUWr2023.RData")
source("Lab2_Functions.R")

# przygotowanie danych
events[is.na(PaymentAmount), PaymentAmount:=0.0]
payments <- aggregatePayments(events = events, horizons = c(6L, 12L))

setkey(cases, CaseId)
setkey(payments, CaseId)

# Wyznaczenie istotnych cech w kontekście wpływu na SR 12M, np. na podstawie współczynników korelacji:
cases <- cases[payments]
cases[,`:=`(
  IsLoan = as.integer(Product == "Cash loan"),
  Female = as.integer(Gender == "FEMALE"),
  SR6M=SumOfPayments6M/TOA,
  SR12M=SumOfPayments12M/TOA)]

cases[, `:=`(
  Product=NULL,
  Gender=NULL,
  SumOfPayments12M=NULL)]

corr_matrix <- cor(x = cases, use = "pairwise.complete.obs")

sort(corr_matrix[,"SR12M"])

# Wybrane cechy do podsumowania (po jednej ze zbioru cases i events):
# SR6M
# M_LastPaymentToImportDate                        

# Stworzenie grup wg ww cech:
cases[,`:=`(
  M_LastPaymentToImportDateGroup = cut(
    x = M_LastPaymentToImportDate, 
    breaks = c(1, 6, 12, 18, 24, 36, 50), 
    include.lowest=TRUE),
  SR6MGroup = cut(
    x = SR6M, 
    breaks = c(-Inf, 0:10/10, Inf), 
    include.lowest=TRUE)
)]


summarizeCasesByVariable(cases = cases, events = events, 
  variable_name = "M_LastPaymentToImportDateGroup")

summarizeCasesByVariable(cases = cases, events = events, variable_name = "SR6MGroup")




## Zadanie #3
# Napisz funkcję, która przyjmie jako argumenty (1) wektor oraz 
#   (2) metodę przekształcenia danych w tym wektorze. W zależności od wartości z argumentu (2) funkcja zwróci:
# -   wektor *zestandaryzowany* (średnia=0, wariancja=1)
# -   wektor *znormalizowany* (przekształcenie wartości zmiennej 
#   na odcinek \[0, 1\])
# -   wektor zlogarytmowany
# -   wektor spierwiastkowany
# Wyznacz macierz współczynników korelacji między wybraną zmienną 
# o wartościach oryginalnych i jej wartościami przekształconymi. 
# Chodzi o wybrana zmienną numeryczną z tabeli cases.

transformVariable <- function(x, method, na.rm=TRUE) {
  
  if(method == "standardize") {
    y <- standardize(x = x, na.rm = na.rm)
  } else if(method == "normalize") {
    y <- normalize(x = x, na.rm = na.rm)
  } else if(method == "log") {
    y <- log(x)
  } else if(method == "sqrt") {
    y <- sqrt(x)
  } else {
    stop("Unknown method.")
  }
  
  return(y)
  
}

standardize <- function(x, na.rm=TRUE) {
  
  y <- (x - mean(x, na.rm=na.rm)) / sd(x, na.rm=na.rm)
  
  return(y)
  
}

normalize <- function(x, na.rm=TRUE) {
  
  y <- (x - min(x, na.rm=na.rm)) / (max(x, na.rm=na.rm) - min(x, na.rm=na.rm))
  
  return(y)
  
}

# sprawdzenie
summary(transformVariable(cases$TOA, method = "standardize"))
summary(transformVariable(cases$TOA, method = "normalize"))
summary(transformVariable(cases$TOA, method = "log"))
summary(transformVariable(cases$TOA, method = "sqrt"))


cor(cases$TOA, transformVariable(cases$TOA, method = "standardize"))
cor(cases$TOA, transformVariable(cases$TOA, method = "normalize"))
cor(cases$TOA, transformVariable(cases$TOA, method = "log"))
cor(cases$TOA, transformVariable(cases$TOA, method = "sqrt"))



## Zadanie #4
# Wyznacz krzywe skuteczności wyników w horyzoncie 12 miesięcy w rozbiciu na grupy wg grup wyznaczonych w zadaniu #2


load("../../KrukUWr2023.RData")
source("Lab2_Functions.R")

events[is.na(PaymentAmount), PaymentAmount:=0.0]

payments <- aggregatePayments(events, horizons = 1L:12L)

groups <- cases[,.(
  CaseId,
  TOA,
  M_LastPaymentToImportDateGroup = cut(
    x = M_LastPaymentToImportDate, 
    breaks = c(1, 6, 12, 18, 24, 36, 50), 
    include.lowest=TRUE)
)]

setkey(payments, CaseId)
setkey(groups, CaseId)

payments <- payments[groups]

sr <- payments[, .(
  M1 = sum(SumOfPayments1M)/sum(TOA),
  M2 = sum(SumOfPayments2M)/sum(TOA),
  M3 = sum(SumOfPayments3M)/sum(TOA),
  M4 = sum(SumOfPayments4M)/sum(TOA),
  M5 = sum(SumOfPayments5M)/sum(TOA),
  M6 = sum(SumOfPayments6M)/sum(TOA),
  M7 = sum(SumOfPayments7M)/sum(TOA),
  M8 = sum(SumOfPayments8M)/sum(TOA),
  M9 = sum(SumOfPayments9M)/sum(TOA),
  M10 = sum(SumOfPayments10M)/sum(TOA),
  M11 = sum(SumOfPayments11M)/sum(TOA),
  M12 = sum(SumOfPayments12M)/sum(TOA)
), by = M_LastPaymentToImportDateGroup]

setorder(sr, M_LastPaymentToImportDateGroup)

print(sr)


  
## Zadanie #5
# Wyniki z zadania 5 przedstaw w formie wykresu / wykresów.

sr_long <- melt(data=sr, id.vars="M_LastPaymentToImportDateGroup", variable.name = "Month", value.name = "SR")

sr_long[,Month:=as.integer(sub("M", "", Month))]


library(ggplot2)

ggplot() +
  geom_line(
    data = sr_long,
    aes(
      x = Month, 
      y = SR, 
      color = M_LastPaymentToImportDateGroup, 
      group = M_LastPaymentToImportDateGroup), 
    size=0.8) +
  ggtitle("Cumulative SR") +
  theme_bw() +
  theme(legend.position = "bottom")


