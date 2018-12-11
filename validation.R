source("compute_dtm.R")

set.seed(118)
urls_df = read.csv("urls.csv")
lex <- ComputeTDM(urls_df)
topics <- ComputeTopics(lex, 100)
#dtm <- t(topics$document_sums)
dtm <- ComputeDTM(topics)

# Topics representation
# Get the top 5 words in each topic to obtain a description of the topic
ttw = t(top.topic.words(topics$topics, num.words = 10, by.score = TRUE))

# Get the most representative document for each topic
ttd <- urls_df[top.topic.documents(topics$document_sums, num.documents = 1),]

# Distribution of topics in the documents
library(reshape2)
library(RColorBrewer)
library(ggplot2)
dist <- melt(t(dtm), varnames = c('topic','document'))
dist$topic <- as.factor(dist$topic)
dist$document <- as.factor(dist$document)
ggplot(dist, aes(x=document)) + geom_bar(aes(weight=value, fill=topic), position='fill') +
  labs(x = "Documents", y= 'Topic distribution') +
  scale_fill_manual(values = rainbow(ncol(dtm)))

# Coherence of the documents
CosineDistance <- function(m) {
  mat <- m %*% t(m)
  d <- diag(mat)
  sim <- t(t(mat/sqrt(d))/sqrt(d))
  dis <- 1-sim
}

docs_df <- LoadPosts(urls_df)
words_tf_idf_df <- ComputeTF_IDF(docs_df)
tdm <- acast(words_tf_idf_df, url_id ~ word, value.var="tf_idf")
tdm[is.na(tdm)] <- 0.0
ddm_tf_idf <- CosineDistance(tdm)
ddm_topics <- CosineDistance(dtm)
diff <- abs(ddm_tf_idf - ddm_topics)
library(gplots)
heatmap.2(diff,Rowv=FALSE,Colv=FALSE,col=colorRampPalette(c("white","yellow","orange"))(100),dendrogram='none', trace='none')

# Verify convergence
library(ggplot2)
library(reshape2)
likelihoods <- melt(topics$log.likelihoods, varnames=c('Likelihood', 'iteration'))
likelihoods$Likelihood = as.factor(likelihoods$Likelihood)
ggplot(likelihoods, aes(x=iteration ,y=value, color=Likelihood)) + 
  geom_point() + labs(x = "Iterations", y= 'Log likelihood values') +
  scale_color_manual(labels = c("Full", "Assigment"), values = c("blue", "red"))

