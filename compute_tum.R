source("compute_dum.R")
source("compute_dtm.R")

ComputeTUM <-function(dum,dtm) {
  # Ensures that the DUM is colwise normalized
  # Note that the DTM may always be rowwise normalized
  # Therefore the TUM will be rowwise normalized
  dum_n <- t(NormalizeDUM(dum))
  available_users_id <- as.numeric(colnames(dum_n))
  filtered_dtm <- dtm[available_users_id,]
  rownames(filtered_dtm) <- available_users_id
  tum <- dum_n %*% filtered_dtm
}



