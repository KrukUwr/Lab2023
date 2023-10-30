
# Zad 2 -----------------------------------------------------------------------------------------------------------

summarizeCasesByVariable <- function(cases, events, variable_name, na.rm=TRUE) {
  
  stopifnot(is.data.table(cases))
  stopifnot("CaseId" %in% names(cases))
  stopifnot("CaseId" %in% names(events))
  stopifnot(is.data.table(events))
  stopifnot(all(!is.na(events$PaymentAmount)))
  stopifnot(is.character(variable_name), length(variable_name)==1)
  stopifnot(variable_name %in% names(cases) || variable_name %in% names(events))
  
  payments <- aggregatePayments(events = events, horizons = c(6L, 12L))
  
  setkey(cases, CaseId)
  setkey(payments, CaseId)
  
  cases <- payments[cases]
  
  total_row <- summarizeTotalCases(cases, na.rm=na.rm)
  setnames(total_row, "Group", variable_name)
  
  summary_table_groups <- cases[,.(
    NoOfCases = .N,
    ShareOfCases = .N / total_row$ShareOfCases,
    Principal = sum(Principal, na.rm=na.rm),
    ShareOfPrincipal = sum(Principal, na.rm=na.rm) / sum(TOA),
    TOA = sum(TOA),
    ShareOfTOA = sum(TOA) / total_row$TOA,
    AvgDPD = mean(DPD, na.rm=na.rm),
    SR6M = sum(SumOfPayments6M) / sum(TOA),
    SR12M = sum(SumOfPayments12M) / sum(TOA)
  ), by = c(variable_name)]
  
  summary_table <- rbindlist(list(summary_table_groups, total_row))
  
  setorderv(summary_table, variable_name)
  
  return(summary_table)
  
} 


summarizeTotalCases <- function(cases, na.rm) {
  
  summary_total <- cases[,.(
    Group="TOTAL",
    NoOfCases = .N,
    ShareOfCases = 1.0,
    Principal = sum(Principal, na.rm=na.rm),
    ShareOfPrincipal = sum(Principal, na.rm=na.rm) / sum(TOA),
    TOA = sum(TOA),
    ShareOfTOA = 1.0,
    AvgDPD = mean(DPD, na.rm=na.rm),
    SR6M = sum(SumOfPayments6M) / sum(TOA),
    SR12M = sum(SumOfPayments12M) / sum(TOA)
  )]
  
  return(summary_total)
  
}


aggregatePayments <- function(events, horizons = c(6L, 12L)) {
  
  stopifnot(is.integer(horizons))
  stopifnot(all(horizons>=1 & horizons <= 12))
  
  payments <- events[,.(CaseId=unique(CaseId))]
  setkey(payments, CaseId)
  
  for(horizon in horizons) {
    
    payments_horizon <- createPaymentsInHorizonTable(events = events, horizon = horizon)
    
    payments <- payments[payments_horizon]
    
  }
  
  return(payments)
  
}


createPaymentsInHorizonTable <- function(events, horizon) {
  
  payments_horizon <- events[Month <= horizon, .(PaymentsHorizon = sum(PaymentAmount)), by=CaseId]
  
  data.table::setnames(
    x = payments_horizon, 
    old = "PaymentsHorizon", 
    new = paste0("SumOfPayments", horizon, "M"))
  
  setkey(payments_horizon, CaseId)
  
  return(payments_horizon)
  
}

