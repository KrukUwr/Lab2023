
library(data.table)
library(caret)
library(Metrics)

library(mlr)


load("../../KrukUWr2023.RData")



# Ad1 -------------------------------------------------------------------------------------------------------------

# dla kart zmienna LoanAmount może oznaczać limit na karcie,
# zamiast rzeczywiście pożyczone kwoty
# dlatego dla wszystkich kart ustawiam sztuczną wartość -1
cases[Product=="Credit card", LoanAmount:=-1]

cases[is.na(Other), Other := max(TOA - Principal - Interest, 0.0)]


# Tworzymy możliwe kombinacje zmiennych i wyznaczamy prawdopodobieństwo wystapienia 
# na podstawie liczności w naszym zbiorze
land_values <- cases[!is.na(Land),.(
  Prob = .N/cases[,.N]
  ), by = .(Land_pattern = Land, GDP_pattern = GDPPerCapita, Salary_pattern = MeanSalary)]

# zapisujemy nazwy oryginalnych cech w wektorze - przydadzą się do odtworzenia formy tabeli cases
cases_colnames <- names(cases)

# losowanie
cases[is.na(Land), `:=`(
  Land2 = sample(land_values$Land_pattern, .N, replace=TRUE, prob = land_values$Prob)
)]

# dopasowanie pozostałych cech
cases <- land_values[cases, on = c("Land_pattern" = "Land2")][is.na(Land), `:=`(
  Land = Land_pattern, 
  GDPPerCapita=GDP_pattern, 
  MeanSalary=Salary_pattern)][,.SD, .SDcols=cases_colnames]


summary(cases)


# Ad2 -------------------------------------------------------------------------------------------------------------

# Od 2 punktu zadania wykonane z użyciem pakietu mlr
# mlr nie wspiera data.table dlatego zmieniamy na data.frame

cases <- data.frame(cases)

# prosta sworzenie imputera
imp <- mlr::impute(obj = cases, cols = list(D_ContractDateToImportDate = imputeMean()))

summary(cases$D_ContractDateToImportDate)

# predykcja na nasz zbiór
cases <- reimpute(cases, imp$desc)

summary(cases$D_ContractDateToImportDate)

# Ad3 -------------------------------------------------------------------------------------------------------------

# jakie obiekty klasy learners są dostępne. Jest to zależne od lokalnie zainstalowanych pakietów
available_learnsers <- listLearners()

default_rf_hp <- list(
  ntree = 100, nodesize=500, maxnodes=2^6
)

variables  <- c("TOA", "D_ContractDateToImportDate", "MeanSalary", "Age")

# należny wskazać odpowiednia nazwę learner'a dla odpowiedniego algorytmu
lrn_dpd <- makeLearner(cl = "regr.randomForest", id = "lrn-rf-dpd-imputer", par.vals = default_rf_hp)

imp_dpd <- mlr::impute(obj = cases, cols = list(DPD = imputeLearner(learner = lrn, features = variables)))

summary(cases)

cases <- reimpute(cases, imp_dpd$desc)

summary(cases)


# Ad4 -------------------------------------------------------------------------------------------------------------

# wymagany pakiet FNN do learnera "regr.fnn"
# install.packages("FNN")

default_knn_hp <- list(
 k=20
)

variables  <- c("TOA", "D_ContractDateToImportDate", "MeanSalary", "Age", "DPD")

# należny wskazać odpowiednia nazwe learner'a
lrn_pop <- makeLearner(cl = "regr.fnn", id = "lrn-knn-pop-imputer", par.vals = default_knn_hp)

imp_pop <- mlr::impute(
  obj = cases, 
  cols = list(PopulationInCity = imputeLearner(learner = lrn_pop, features = variables)))

summary(cases)

cases <- reimpute(cases, imp_pop$desc)

summary(cases)



# powrót do data.table
setDT(cases)




