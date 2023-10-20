
czyRownaLiczbaSpraw <- function(sprawy, zdarzenia) {
  
  odp <- TRUE
  if(sprawy[,.N] != length(unique(zdarzenia$CaseId))) {
    odp <- FALSE
  }
  
  return(odp)
  
}
