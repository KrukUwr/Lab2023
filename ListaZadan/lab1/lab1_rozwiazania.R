library(data.table)
load("KrukUWr2023.RData")

source("funkcje.R")

# 1.  Ile jest spraw w analizowanych zbiorach?

cases[,.N] # skladnia data.table
nrow(cases)

length(unique(events$CaseId))
cases[!duplicated(CaseId), .N]

# przyklad zastosowania funkcji czyRownaLiczbaSpraw wczytanej ze skryptu funkcje.R
czyRownaLiczbaSpraw(sprawy = cases, zdarzenia = events)

stopifnot(czyRownaLiczbaSpraw(sprawy = cases, zdarzenia = events))

events_w_missings <- events[CaseId!=11478344]

stopifnot(czyRownaLiczbaSpraw(sprawy = cases, zdarzenia = events_w_missings))

# 2.  Jakie jest ich łączne zadłużenie?

cases[,sum(TOA)]

# 3.  Ile jest spraw kobiet i mężczyzn? Czy są sprawy bez przypisanej płci klienta?

cases[,.N,by=.(Gender)]

# 4.  Jaka jest suma wpłat wszystkich spraw w horyzoncie 6 miesięcy od ich zakupu?

events[Month<=6, sum(PaymentAmount, na.rm=TRUE)]


# 5.  Ile wpłaciła sprawa `CaseId=11478344` w pierwszym miesiącu obsługi, 
# ile do 5 włącznie, a ile w całym dostępnym horyzoncie?

events[CaseId==11478344 & Month==1, sum(PaymentAmount, na.rm=TRUE)]
events[CaseId==11478344 & Month<=5, sum(PaymentAmount, na.rm=TRUE)]
events[CaseId==11478344, sum(PaymentAmount, na.rm=TRUE)]


# 6.  Podzielcie zbiór spraw na 10 portfeli o nazwach:
#   | Portfolio       |
#   |-----------------|
#   | PKO1            |
#   | PKO2            |
#   | mBank1          |
#   | Provident1      |
#   | Provident2      |
#   | Wonga6          |
#   | Santander3      |
#   | Pekao2          |
#   | BocianPozyczki4 |
#   | HapiPozyczki22  |
  

portfolios <- c(
  "PKO1", "PKO2", "mBank1", "Provident1", "Provident2", 
  "Wonga6", "Santander3", "Pekao2", "BocianPozyczki4", "HapiPozyczki22")
cases[, Portfolio:= sample(x = portfolios, size = .N, replace=TRUE)]


# 7.  Jakie jest średnie zadłużenie w portfelu?

cases[,.(AvgTOA = mean(TOA)), by=.(Portfolio)][order(AvgTOA)]

# 8.  Jaka jest suma wpłat w 12 miesięcy per portfel?

events[is.na(PaymentAmount),PaymentAmount:=0.0]

payments_12m <- events[, .(SumOfPayments=sum(PaymentAmount)), by=.(CaseId)]

stopifnot(czyRownaLiczbaSpraw(sprawy = cases, zdarzenia = payments_12m))

cases_w_payments_12m <- cases[payments_12m, on = c("CaseId" = "CaseId")]
cases_w_payments_12m[,.(SumOfPayments=sum(SumOfPayments)), by=.(Portfolio)][order(SumOfPayments)]

# 9.  Jaka jest skuteczność w 12 miesięcy per portfel?

cases_w_payments_12m[,.(SR=sum(SumOfPayments)/sum(TOA)), by=.(Portfolio)][order(SR)]

# 10. Ile spraw dokonało wpłaty w 12 miesięcy w każdym portfelu? Jaki stanowią udział wszystkich spraw pod względem liczby spraw oraz zadłużenia?

events[is.na(NumberOfPayment),NumberOfPayment:=0]
number_of_payments_by_cases <- events[,.(NoOfPayments=sum(NumberOfPayment)), by=.(CaseId)]

cases_w_no_of_payments <-  cases[number_of_payments_by_cases, on = c("CaseId" = "CaseId")]

# Udzial spraw z wplatami wg zadluzenia

cases_w_no_of_payments[, .(
  NoOfCasesWithPayment = sum(NoOfPayments>0),
  ShareOfCasesWithPayment = sum(NoOfPayments>0)/.N
), by=.(Portfolio)]

toa_of_all_cases_by_portfolio <- cases_w_no_of_payments[,.(TOA_all_cases=sum(TOA)), by=.(Portfolio)]
toa_of_cases_w_payments_by_portfolio <- cases_w_no_of_payments[NoOfPayments>0,.(TOA_cases_w_pmt=sum(TOA)), by=.(Portfolio)]


toa_of_all_cases_by_portfolio[toa_of_cases_w_payments_by_portfolio, on = c("Portfolio" = "Portfolio")][
  ,.(
    Portfolio, 
    TOAShareOfCasesWithPayments = TOA_cases_w_pmt/TOA_all_cases
    )]
