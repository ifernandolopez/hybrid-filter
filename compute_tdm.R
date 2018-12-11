library(dplyr)
library(tidyr)
library(tidytext)
library(readtext)
library(dplyr)
library(stringr)
library(lda)

LoadPosts <- function(urls_df, directory = "posts") {
  files = sapply(urls_df$url_id,function(id) {paste0("posts","/post",id,".txt")})
  texts <-readtext(files)
  docs_df <- cbind(urls_df,text=texts$text, stringsAsFactors = FALSE)
  return (docs_df)
}

ComputeTF_IDF <- function(docs_df) {
  # Use unnest_tokens() to find work occurrences with one token per row
  # and also remove stop words
  stop.words <- data_frame(word = c('si', stopwords::stopwords(language = 'es')))
  words <- docs_df %>%
    unnest_tokens(word,text) %>%
    anti_join(stop.words, by = "word")
  
  # Remove words occuring only once in the whole corpus: imputed as misspelling
  misspelling <- names(table(words$word))[table(words$word)==1]
  words <-words %>%
    filter(!(word %in% misspelling))
  
  # Count the word ocurrences per doument
  words_count_df <- words %>%
    count(url_id, word, sort = TRUE) 
  
  # Compute the TF-IDF index
  words_tf_idf_df <- words_count_df %>%
    bind_tf_idf(word,url_id,n) %>%
    arrange(-tf_idf)
  return (words_tf_idf_df)
}

ComputeTDM <- function(urls_df, tf_idf_threshold = 0.005) {
  # Compute TD-IDF
  docs_df <- LoadPosts(urls_df)
  words_tf_idf_df <- ComputeTF_IDF(docs_df)
  
  # Select characteristic words (cw) of each document: those with a higher TF-IDF
  cw_df <- words_tf_idf_df %>%
    filter(tf_idf > 0.005)
  
  # Ungroup characteristic words count
  rep_cw_df <- cw_df %>%
    rowwise() %>%
    mutate(rep_word = paste0(rep(word,n),collapse = " ")) %>%
    ungroup()
  
  # Recreate characteristic word documents (cwd)
  # by collapsing words separated by space and grouping them together based on the common key
  cwd_df <- rep_cw_df %>% 
    group_by(url_id) %>% 
    summarise(text = str_c(rep_word, collapse = " "))
  
  # Computes TDM and vocabulary
  documents <- cwd_df$text
  lex = lexicalize(documents)
  return(lex) 
}


