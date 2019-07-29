# Text as Data
# Author: Kamwoo Lee
# Last modified: 11/1/2016

library(rvest)
library(tm)
library(stringr)
library(XML)
library(cluster) 
library(proxy)
library(dplyr)
library(ggplot2)
library(topicmodels)



######################
# 1. Choose a Corpus #
######################

load("ecorpt.RData")
# This code uses the result of analysis in the previous problem set. (the economic report of the president)




###################
# 2. Topic Model  #
###################

# Start with previous stemmed corpus, make a DocumentTermMatrix
eco_report.DTM <- DocumentTermMatrix(eco_report.corpus.stem.comp, control=list(bounds=list(global=c(3,Inf))))


# Comparing marginal log-likehoods with Gibbs sampling
# based on the harmonic mean estimator
library(Rmpfr)
harmonicMean <- function(logLikelihoods, precision=2000L) {
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
}

# Fit the model via Gibbs samplin, using a sequence of k and fixing the Gibbs controls
burnin = 1000
iter = 1000
keep = 50
ks <- seq(5, 50, 5) # a sequence of ks from 5 to 20 (in increments of 1)

# Topic models with different numbers of topics (k)
seed1 <- 123
chooseKgibbs <- lapply(ks, function(k) LDA(eco_report.DTM, k=k, method="Gibbs",
                                           control=list(burnin=burnin, iter=iter, keep=keep, seed=seed1)))

# Extract logliks from each topic model
chooseKgibbs.logLik <- lapply(chooseKgibbs, function(L)  L@logLiks[-c(1:(burnin/keep))])
# "keep" in the LDA function asked that every iteration the log-likelihood generates be stored,
# including burnin; omit the burn-in iterations before calculating the harmonic mean

# Compute harmonic means, plot, return maximum
chooseKgibbs.hm <- sapply(chooseKgibbs.logLik, function(h) harmonicMean(h))
plot(ks, chooseKgibbs.hm, type = "l")
ks[which.max(chooseKgibbs.hm)]
# k = 25




#####################################
# 3. Evaluation and Interpretation  #
#####################################


dcLDAv <- LDA(eco_report.DTM, k=10, control=list(seed=seed1))
eco_report.LDA <- LDA(eco_report.DTM, k=25, control=list(seed=seed1))

# Top 10 for each topic
terms(eco_report.LDA, 10)
# Almost all topics have terms that are common in an economic article such as "economic", "price", "will", "year"
# I guess the main difference is the order of the terms
# So, the analysis below focuses on difference of the order for each document

# Topic assignment for each document
topiclab <- as.data.frame(t(terms(eco_report.LDA, 5)))
topiclab$lab <- paste0(topiclab$V1, "-", topiclab$V2, "-", topiclab$V3, "-", topiclab$V4, "-", topiclab$V5)

# Probability of topic in document
prob <- as.data.frame(t(posterior(eco_report.LDA)$topics))
prob$topic <- as.factor(rownames(prob))
prob$topiclab <- topiclab$lab

# Graph topic probabilities for a document (Harry S. Truman's 1947 economic report)
ggplot(prob, aes(x=topiclab, y=prob[,1])) + 
  geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=-90, hjust=0)) +
  labs(y=colnames(prob[1]), title="Topic Assignment")




#####################
# 4. Visualization  #
#####################

library(reshape2)
probLong <- melt(prob, id.vars = c("topic", "topiclab"), 
                 value.name="topicProb", variable.name="report")

# President Barack Obama's topic
ggplot(probLong[1576:1750,], aes(x=topiclab, y=topicProb)) + 
  geom_bar(stat="identity") + facet_wrap(~report, ncol=4) +
  labs(title="Topic Assignment") + 
  theme(axis.text.x=element_text(angle=-90, hjust=0))

# President George W. Bush's topic
ggplot(probLong[1376:1575,], aes(x=topiclab, y=topicProb)) + 
  geom_bar(stat="identity") + facet_wrap(~report, ncol=4) +
  labs(title="Topic Assignment") + 
  theme(axis.text.x=element_text(angle=-90, hjust=0))

# Topics (at least priority of terms) of President Bush are more diverse than that of President Obama




save.image("ecorpt.RData")














