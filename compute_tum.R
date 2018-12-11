source("compute_dum.R")
source("compute_dtm.R")

ComputeTUM <-function(dum,dtm) {
  # Ensures that the DUM is colwise normalized
  # Note that the DTM may always be rowwise normalized
  # Therefore the TUM will be rowwise normalized
  dum_n <- NormalizeDUM(dum)
  tum <- t(dum_n) %*% dtm
}



