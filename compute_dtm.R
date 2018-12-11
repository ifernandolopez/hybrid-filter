source("compute_tdm.R")

ComputeTopics <- function(lex, iterations = 50) {
  # Model fitting with the Gibbs sampler ( a sampler for obtaining observations with the same distribution)
  D = length(lex$documents) # Nº of documents in the corpus
  K = ceiling(log(D,2)) # Nº of topics is the binary log of the number of documents
  topics = lda.collapsed.gibbs.sampler(lex$documents,
                                       K,
                                       lex$vocab,
                                       iterations, # number of iterations of Gibbs sampling over the entire corpus
                                       0.1, # Alpha 
                                       0.1, # Beta
                                       compute.log.likelihood = TRUE # Useful for assessing convergence of the iterative process
  )
  return(topics)
}

ComputeDTM <- function(topics) {
  dtm <- t(topics$document_sums)
  # Normalize the rows so that all topics sum up to 1.0
  dtm <- dtm/rowSums(dtm)
  return(dtm)
}
