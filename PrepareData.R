### Purging and retrieving word prediction data

# loading libraries
library(stringr)
library(quanteda)
# Set the number of processor cores
quanteda_options("threads" = 6)

# Download source databases
pathEnTwit <- "final/en_US/en_US.twitter.txt"
pathEnNews <- "final/en_US/en_US.news.txt"
pathEnBlog <- "final/en_US/en_US.blogs.txt"
allBlogs <- readLines(pathEnBlog, warn = F, encoding = "UTF-8")
allNews <-  readLines(pathEnNews, warn = F, encoding = "UTF-8")
allTwitter <- readLines(pathEnTwit, warn = F, encoding = "UTF-8")
summary(allBlogs)
summary(allNews)
summary(allTwitter)
# Create database corpus
corpBlogs <- corpus(allBlogs)
corpNews <- corpus(allNews)
corpTwit <- corpus(allTwitter)

rm(allBlogs, allNews, allTwitter)
# Сreate 10% of selection from databases
set.seed(1234)
corpMinBlog <- corpus_sample(corpBlogs, 90000)
corpMinNews <- corpus_sample(corpNews, 100000)
corpMinTwit <- corpus_sample(corpTwit, 200000)

rm(corpBlogs, corpNews, corpTwit)

## Create token objects for each database and clear them
tokenBlog <- tokens(corpMinBlog, 
                    remove_punct = TRUE,
                    remove_numbers = TRUE, 
                    remove_symbols = TRUE,
                    remove_twitter = TRUE,
                    remove_hyphens  =  TRUE,
                    remove_url  =  TRUE,
)
tokenBlog <- tokens_tolower(tokenBlog)
tokenBlog <- tokens_remove(tokenBlog, pattern = stopwords('en'))

tokenTwit <- tokens(corpMinTwit, 
                    remove_punct = TRUE,
                    remove_numbers = TRUE, 
                    remove_symbols = TRUE,
                    remove_twitter = TRUE,
                    remove_hyphens  =  TRUE,
                    remove_url  =  TRUE,
)
tokenTwit <- tokens_tolower(tokenTwit)
tokenTwit <- tokens_remove(tokenTwit, pattern = stopwords('en'))

tokenNews <- tokens(corpMinNews, 
                    remove_punct = TRUE,
                    remove_numbers = TRUE, 
                    remove_symbols = TRUE,
                    remove_twitter = TRUE,
                    remove_hyphens  =  TRUE,
                    remove_url  =  TRUE,
)
tokenNews <- tokens_tolower(tokenNews)
tokenNews <- tokens_remove(tokenNews, pattern = stopwords('en'))

# The function of creating n-gram databases for word prediction
nGramFunc <- function(ng, size, token) {
  # creating n-grams
  nGrams <- tokens_ngrams(token, n = ng, concatenator = " ")
  # We distribute n-grams in frequencies from largest to smaller
  dfm_nGrams <- dfm(nGrams)
  statBlog <- textstat_frequency(dfm_nGrams)
  # Delete extra columns
  statBlog <- statBlog[, c(1,2)]
  # Create a column with a prediction base (feature1)and a column with a predicted word (feature2)
  statBlog$feature2 <- word(statBlog$feature, -1)
  statBlog$feature1 <- word(statBlog$feature, 1, ng-1)
  statBlog <- subset(statBlog, select = c(feature1, feature2))
  # Delete repetitions in feature1
  statBlog <- statBlog[!duplicated(statBlog$feature1)]
  #dim(statBlog)
  rowToKeep <- c(1:size)
  statBlog <- statBlog[rowToKeep,] 
  dim(statBlog)
  return(statBlog)
}

# We create databases for prediction by limiting the number of rows 5000
biBlog <- nGramFunc(2, 5000, tokenBlog)
head(biBlog)
tail(biBlog)
saveRDS(biBlog, file = "Tidy/biBlog.rds")

triBlog <- nGramFunc(3, 5000, tokenBlog)
head(triBlog)
tail(triBlog)
saveRDS(triBlog, file = "Tidy/triBlog.rds")

fourBlog <- nGramFunc(4, 5000, tokenBlog)
head(fourBlog)
tail(fourBlog)
saveRDS(fourBlog, file = "Tidy/fourBlog.rds")

##################################################
biTwit <- nGramFunc(2, 5000, tokenTwit)
head(biTwit)
tail(biTwit)
saveRDS(biTwit, file = "Tidy/biTwit.rds")

triTwit <- nGramFunc(3, 5000, tokenTwit)
head(triTwit)
tail(triTwit)
saveRDS(triTwit, file = "Tidy/triTwit.rds")

fourTwit <- nGramFunc(4, 5000, tokenTwit)
head(fourTwit)
tail(fourTwit)
saveRDS(fourTwit, file = "Tidy/fourTwit.rds")

#####################################################
biNews <- nGramFunc(2, 5000, tokenNews)
head(biNews)
tail(biNews)
saveRDS(biNews, file = "Tidy/biNews.rds")

triNews <- nGramFunc(3, 5000, tokenNews)
head(triNews)
tail(triNews)
saveRDS(triNews, file = "Tidy/triNews.rds")

fourNews <- nGramFunc(4, 5000, tokenNews)
head(fourNews)
tail(fourNews)
saveRDS(fourNews, file = "Tidy/fourNews.rds")
