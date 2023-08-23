library(topicmodels)
library(quanteda)
library(tidyr)
library(dplyr)
library(tidytext)
library(NLP)
library(tm)
library(ggplot2)
library(forcats)
library(RColorBrewer)
library(wordcloud)
library(lda)
library(stringr)
data<- read.csv("Label-titles-abstracts-tokenized-filtered.csv")
data$words <- gsub("\\\\", "", data$words)
data$words <- gsub("\"", "", data$words)
data$words <- gsub("\"", "", data$words)
data$words <- str_replace_all(data$words, "(\\\\|\")", "")
data$FundingBody <- gsub("Innovate-UK", 3, data$FundingBody)
data$FundingBody <- gsub("BBSRC", 5, data$FundingBody)
data$FundingBody <- gsub("NERC", 1, data$FundingBody)
data$FundingBody <- gsub("STFC", 4, data$FundingBody)
data$FundingBody <- gsub("EPSRC", 2, data$FundingBody)

train_indices <- sample(1:nrow(data), nrow(data) * 0.75)  # 75% 的样本作为训练集
saveRDS(train_indices, "train_indices.rds")
train_indices<-readRDS("train_indices.rds")
train_set <- data[train_indices, ]  # 训练集的文档-词频矩阵
train_corpus <- corpus(train_set$words)
train_tokens<- tokens(train_corpus)
train_dtm <- dfm(train_tokens)

validation_indices <- setdiff(1:nrow(data), train_indices)  # 剩下的样本作为验证集
validation_set <- data[validation_indices, ]  # 验证集的文档-词频矩阵
validation_corpus <- corpus(validation_set$words)
validation_tokens<- tokens(validation_corpus)
validation_dtm <- dfm(validation_tokens)

sLDA_input <- lexicalize(train_set$words, lower = TRUE)
sLDA_valid_input <- lexicalize(validation_set$words, lower = TRUE)

kNum<-seq(50,500,25)
for (k in kNum){
  print(paste0("generate ",k,"-topic LDA model"))
  lda_out <- LDA(
    train_dtm,
    k,
    method = "Gibbs",
    control = list(seed=42)
  )
  lda_name<-paste0("lda_out",k,".rds")
  saveRDS(lda_out, lda_name)
  print("generate sLDA model labelled funding body ")
  slda_mod <- slda.em(documents = sLDA_input$documents,k, vocab = sLDA_input$vocab,
                        num.e.iterations = 100, num.m.iterations = 2, alpha = 1,
                        eta = 0.1, params = sample(c(-1,1),k, replace = TRUE),
                        annotations  = train_set$FundingBody,
                        variance = var(train_set$FundingBody),
                        method = "sLDA")
  slda_name<-paste0("slda_mod",k,".rds")
  saveRDS(slda_mod, slda_name)

  print("generate sLDA model labelled project duration ")
  Durslda_mod <- slda.em(documents = sLDA_input$documents, k, vocab = sLDA_input$vocab,
                            num.e.iterations = 100, num.m.iterations = 2, alpha = 1,
                            eta = 0.1, params = sample(c(-1,1),k, replace = TRUE),
                            annotations  = train_set$Duration,
                            variance = var(train_set$Duration),
                            method = "sLDA")
  Durslda_name<-paste0("Durslda_mod",k,".rds")
  saveRDS(Durslda_mod, Durslda_name)

  print("generate sLDA model labelled funding amount ")
  FAslda_mod <- slda.em(documents = sLDA_input$documents, k, vocab = sLDA_input$vocab,
                          num.e.iterations = 100, num.m.iterations = 2, alpha = 1,
                          eta = 0.1, params = sample(c(-1,1),k, replace = TRUE),
                          annotations  = train_set$FundingAmount,
                          variance = var(train_set$FundingAmount),
                          method = "sLDA")
  FAslda_name<-paste0("FAslda_mod",k,".rds")
  saveRDS(FAslda_mod, FAslda_name)

  print("generate sLDA model labelled funding per year ")
  FPslda_mod <- slda.em(documents = sLDA_input$documents, k, vocab = sLDA_input$vocab,
                           num.e.iterations = 100, num.m.iterations = 2, alpha = 1,
                           eta = 0.1, params = sample(c(-1,1),k, replace = TRUE),
                           annotations  = train_set$Fundingpy,
                           variance = var(train_set$Fundingpy),
                           method = "sLDA")
  FPslda_name<-paste0("FPslda_mod",k,".rds")
  saveRDS(FPslda_mod, FPslda_name)



}


