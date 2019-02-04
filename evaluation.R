source('compute_tum.R')

NormalizedDUMpredict <- function(tum,dtm) {
  # Compute the DUMpredict and normalize it
  dum_predict <- tum %*% t(dtm) 
  dum_predict <- dum_predict/rowSums(dum_predict)
  colnames(dum_predict) <- 1:ncol(dum_predict)
  return (dum_predict)
}

UserList <- function(user_id, dum_n) {
  # dum_n - the normalized DUM
  # Return the user list where names indicate the doc_id and values the user indicators score
  indexes <- which(dum_n[user_id,] > 0.0)
  user_list <- dum_n[user_id,][indexes]
  return(user_list)
}

TopNRecommendation <-function(user_id, n, dum_predict) {
  # Return the top-N recommendation for the user_id 
  # where names indicae the doc_id and values the prediction score
  top_n <- dum_predict[user_id,]
  top_n <- sort(top_n, decreasing = TRUE)[1:n]
  return (top_n)
}

Evaluate <- function(dum_n, dum_predict, min_user_rated_docs = 2) {
  # Measure performance for all the users
  tp <- tn <- fp <- fn <- 0
  n_docs <- ncol(dum_n)
  all_docs <-1:n_docs
  for (user_id in rownames(dum_n)) {
    # Obtain the user list
    user_list <- UserList(user_id, dum_n)
    if (length(user_list) <= min_user_rated_docs) 
      next
    for (removed_doc_id in names(user_list)) {
      # Leave one out
      user_list <- user_list[which(names(user_list) != removed_doc_id)]
      # Obtain the top-n recommendation
      top_n <- TopNRecommendation(user_id, length(user_list), dum_predict)
      # Evaluate
      user_list_docs <- names(user_list)
      non_user_list_docs <- setdiff(all_docs,user_list_docs)
      top_n_docs <- names(top_n)
      non_top_n_docs <- setdiff(all_docs,top_n_docs)
      #tp - The recommended documents that are in the user list
      tp <- tp + length(intersect(user_list_docs, top_n_docs))
      #tn - Non-recommended documents that are not in the user list
      tn <- tn + length(setdiff(non_top_n_docs,user_list_docs))
      #fp - Recommended documents that are not in the user list
      fp <- fp + length(setdiff(top_n_docs,user_list_docs))
      #fn - Non.recommended documents that are in the user list
      fn <- fn + length(intersect(non_top_n_docs, user_list_docs))
    }
  }
  accuracy <- (tp+tn) / (tp+tn+fp+fn)
  precision <- tp / (tp+fp)
  recall <- tp / (tp+fn)
  f1 <- 2*precision*recall / (precision+recall)
  return(list(accuracy = accuracy, precision = precision, recall = recall, f1 = f1))
}

set.seed(116)

# Obtain the DTM
urls_df = read.csv('urls.csv')
lex <- ComputeTDM(urls_df)
topics <- ComputeTopics(lex, 100)
dtm <- ComputeDTM(topics)

# Determine the days range
indicators_df = read.csv('indicators.csv')
first_day_number <- as.integer(as.Date('2018-11-30'))
last_day <-  top_n(indicators_df,1,date)
x_dates <- seq(as.Date('2018-11-30'), as.Date(last_day$date), by=1)

# Evaluate for each day
evaluation_df <- data_frame(day = numeric(), accuracy = numeric(),precicion = numeric(), recall = numeric(), f1 = numeric()) 
for (x_date in x_dates) {
  x_indicators_df <- indicators_df %>%
    subset(as.Date(date) <= x_date)
  # Obtain the DUM
  dum <- ComputeDUM(x_indicators_df)
  # Obtain the TUM
  tum <- ComputeTUM(dum,dtm)
  # Compute the normalized DUM and DUMpredict
  dum_n <- t(NormalizeDUM(dum))
  dum_predict <- NormalizedDUMpredict(tum,dtm)
  # Evaluate
  result <- Evaluate(dum_n, dum_predict)
  result$day <- x_date- first_day_number + 1
  evaluation_df <- rbind(evaluation_df, result)
}

# Plot the result
library(ggplot2)
ggplot(evaluation_df) + 
  geom_line(aes(x = day, y = accuracy, color = 'Accuracy'), size = 2) + 
  geom_line(aes(x = day, y = precision, color = 'Precision'), size = 2) +
  geom_line(aes(x = day, y = recall, color = 'Recall'), size = 2) +
  scale_color_discrete(name = 'Measures', labels = c('Accuracy', 'Precision', 'Recall')) +
  labs(x = 'Days', y= 'Scores') + expand_limits(y = 0)


