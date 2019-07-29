# Text as Data
# Author: Kamwoo Lee
# Last modified: 10/13/2016
#

library(rvest)
library(tm)
library(stringr)
library(XML)



###########################
# 1. Processing the Text  #
###########################

# Create data frame to hold report and metadata
eco_report.files <- DirSource() # create a list of files to read
eco_report.df <- data.frame(report=character(length(eco_report.files$filelist)),
                            year=character(length(eco_report.files$filelist)), 
                            president=character(length(eco_report.files$filelist)), stringsAsFactors=FALSE) # create an empty data frame
for(i in 1:length(eco_report.files$filelist)){
  eco_report.df$report[i] <- readLines(eco_report.files$filelist[i]) # write report to data frame
  eco_report.df$year[i] <- str_extract(eco_report.files$filelist[i], "[0-9]+") # write year to data frame
  eco_report.df$president[i] <- sub(" *.txt", "", sub("./[0-9]+-", "", eco_report.files$filelist[i])) # write name of the president to data frame
}

# Create a corpus (from a data frame), from "tm" package
docvar <- list(content="report", year="year", president="president")
myReader <- readTabular(mapping=docvar)
eco_report.corpus <- Corpus(DataframeSource(eco_report.df), readerControl=list(reader=myReader))

# Remove capitalization
eco_report.corpus <- tm_map(eco_report.corpus,content_transformer(tolower))
# Remove punctuation
eco_report.corpus <- tm_map(eco_report.corpus, removePunctuation) 
# Remove stop words
eco_report.corpus <- tm_map(eco_report.corpus, removeWords, stopwords("english")) 
# Strip whitespace
eco_report.corpus <- tm_map(eco_report.corpus, stripWhitespace)
# I chose not to remove numbers because certain year or percentage might be important


# Stemming
library(SnowballC)
eco_report.corpus.stem <- tm_map(eco_report.corpus, stemDocument)

# Stem completion code from class
stemCompletion2 <- function(x, d) {
  x <- unlist(strsplit(as.character(x), " "))
  x <- x[x != ""]
  x <- stemCompletion(x, dictionary=d)
  x <- paste(x, sep="", collapse=" ")
  PlainTextDocument(stripWhitespace(x))
}
# This code takes 2~3 hours to run.
eco_report.corpus.stem.comp <- lapply(eco_report.corpus.stem, stemCompletion2, eco_report.corpus)
eco_report.corpus.stem.comp <- Corpus(VectorSource(eco_report.corpus.stem.comp))


# Create Term Document Matrix
eco_report.stem.comp.TDM <- TermDocumentMatrix(eco_report.corpus.stem.comp)
# After comparing stemmed and stem completed terms, 
# I chose to analyze stem completed terms because words are more understandable with voice (active, passive).





########################
# 2. Initial Analysis  #
########################

# Frequent terms
eco_report.freq <- findFreqTerms(eco_report.stem.comp.TDM, lowfreq=210, highfreq=Inf)
# There are 70 reports, so what are the words that mentioned more than 3 times, average, in a report.

library(wordcloud)
# Stemmed wordcloud from frequency data frame
wordfreq.matrix <- as.matrix(eco_report.stem.comp.TDM)
colnames(wordfreq.matrix) <- c(1947:2016)
v <- sort(rowSums(wordfreq.matrix), decreasing=TRUE)
wordfreq.df <- data.frame(word=names(v), freq=v)
wordcloud(wordfreq.df$word, wordfreq.df$freq, min.freq=210, scale=c(4,.33)) 

# Plot
library(ggplot2)
p <- ggplot(wordfreq.df[1:25,], aes(x = reorder(word, freq), y=freq))
p + geom_bar(stat="identity") + coord_flip()

# "economic" is by far the most frequent word as expected since this is "economic report".
# "year", "increase", "product", "price", "tax" are among the most frequent words
# and I wouldn't be surprised if I read some of these words in this kind of report.
# "will" is the second most frequent word, which implies that this report predicts the future event or status.




############################
# 3. Comparative Analysis  #
############################


comparison.cloud(wordfreq.matrix[,c("1999", "2002", "2008", "2007")], max.words=200, 
                 scale=c(3,.25), random.order=FALSE, 
                 colors=c("blue", "red", "red3", "blue3"), title.size=1)

# I decided to compare words in the reports between when the economy was upturn (1999 and 2007) and downturn (2002 and 2008).
# During the financial crisis, many frequent words have negative connotation such as "attack", "must", recessed", "war".
# When the economy was godd, "invest", "health", "surplus", "standard" are amont the frequent words, which have positive meanings.




save.image("../ecorpt.RData")
