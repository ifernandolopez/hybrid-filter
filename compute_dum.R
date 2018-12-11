library(dplyr)
library(reshape2)

ComputeDUM <- function(indicators_df) {
  indicators_count_df <- indicators_df %>%
    group_by(url_id, user_id) %>%
    count(url_id, user_id) 
  dum <- acast(indicators_count_df, url_id ~ user_id, value.var="n")
  # Replace NA with 0.0
  dum[is.na(dum)] <- 0.0
  return(dum)
}

NormalizeDUM <- function(dum) {
  # Normalize the columns
  dum <- (t(t(dum)/colSums(dum)))
  return(dum)
}


