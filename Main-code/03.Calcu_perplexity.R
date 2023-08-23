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
library(text2vec)
library(Matrix)
library(patchwork)
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


lda_perplexity <- c()
for (k in seq(50, 500, 25)) {
  filename <- paste0("lda_out", k, ".rds")
  
  lda_model <- readRDS(filename)
  log_likelihood <- as.numeric(logLik(lda_model))
  perplexity<-exp(-log_likelihood / sum(validation_dtm))
  lda_perplexity <- c(lda_perplexity, perplexity)
}
df <- data.frame(k = seq(50, 500, 25), perplexity = lda_perplexity)


slda_perplexity <- c()
for (k in seq(50, 500, 25)) {
  filename <- paste0("slda_mod", k, ".rds")
  
  slda_model <- readRDS(filename)
  log_likelihood <- as.numeric(logLik(slda_model$model))
  perplexity<-exp(-log_likelihood / sum(validation_dtm))
  slda_perplexity <- c(slda_perplexity, perplexity)
}
sdf <- data.frame(k = seq(50, 500, 25), perplexity=slda_perplexity)


Durslda_perplexity <- c()
for (k in seq(50, 500, 25)) {
  filename <- paste0("Durslda_mod", k, ".rds")
  
  Durslda_model <- readRDS(filename)
  log_likelihood <- as.numeric(logLik(Durslda_model$model))
  perplexity<-exp(-log_likelihood / sum(validation_dtm))
  Durslda_perplexity <- c(Durslda_perplexity, perplexity)
}
Dursdf <- data.frame(k = seq(50, 500, 25), perplexity=Durslda_perplexity)


FPslda_perplexity <- c()
for (k in seq(50, 500, 25)) {
  filename <- paste0("FPslda_mod", k, ".rds")
  
  FPslda_model <- readRDS(filename)
  log_likelihood <- as.numeric(logLik(FPslda_model$model))
  perplexity<-exp(-log_likelihood / sum(validation_dtm))
  FPslda_perplexity <- c(FPslda_perplexity, perplexity)
}
FPsdf <- data.frame(k = seq(50, 500, 25), perplexity=FPslda_perplexity)


FAslda_perplexity <- c()
for (k in seq(50, 500, 25)) {
  filename <- paste0("FAslda_mod", k, ".rds")
  
  FAslda_model <- readRDS(filename)
  log_likelihood <- as.numeric(logLik(FAslda_model$model))
  perplexity<-exp(-log_likelihood / sum(validation_dtm))
  FAslda_perplexity <- c(FAslda_perplexity, perplexity)
}
FAsdf <- data.frame(k = seq(50, 500, 25), perplexity=FAslda_perplexity)


p1 <- ggplot(df, aes(x = k, y = perplexity)) +
  geom_line( linetype = "solid") +  
  geom_point(shape = 16, size = 3) +  
  labs(x = "Number of Topics (k)", y = "Perplexity", color = "Legend Title", linetype = "Legend Title") +  # 设置图例名称
  ggtitle("Perplexity in LDA model") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 16, face = "bold")
  )

p2 <- ggplot() +
  geom_line(data = sdf, aes(x = k, y = perplexity),color = "#757fa0", linetype = "solid") +
  geom_point(data = sdf, aes(x = k, y = perplexity),shape = 16, size = 3,color = "#757fa0") +
  labs(x = "Number of Topics (k)", y = "Perplexity", color = "Legend Title", linetype = "Legend Title") +  # 设置图例名称
  ggtitle("Labelled by funding body") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 16, face = "bold")
  )


p3 <- ggplot() +
  geom_line(data = Dursdf, aes(x = k, y = perplexity), color = "#DDacaa", linetype = "solid") +  # Dursdf 数据的线条
  geom_point(data = Dursdf, aes(x = k, y = perplexity), shape = 16, size = 3, color = "#ddacaa")+
  labs(x = "Number of Topics (k)", y = "Perplexity", color = "Legend Title", linetype = "Legend Title") +  # 设置图例名称
  ggtitle("Labelled by funding Duration") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 16, face = "bold")
  )


p4 <- ggplot() +
  geom_line(data = FPsdf, aes(x = k, y = perplexity), color = "#F2a869", linetype = "solid") +  # Dursdf 数据的线条
  geom_point(data = FPsdf, aes(x = k, y = perplexity), shape = 16, size = 3, color = "#F2a869")+
  labs(x = "Number of Topics (k)", y = "Perplexity", color = "Legend Title", linetype = "Legend Title") +  # 设置图例名称
  ggtitle("Labelled by funding per year") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 16, face = "bold")
  )

p5 <- ggplot() +
  geom_line(data = FAsdf, aes(x = k, y = perplexity), color = "#b783bd", linetype = "solid") +  # Dursdf 数据的线条
  geom_point(data = FAsdf, aes(x = k, y = perplexity), shape = 16, size = 3, color = "#b783bd")+
  labs(x = "Number of Topics (k)", y = "Perplexity", color = "Legend Title", linetype = "Legend Title") +  # 设置图例名称
  ggtitle("Labelled by funding amount") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 16, face = "bold")
  )

p3 <- p3 + labs(y = NULL)
p5 <- p5 + labs(y = NULL)

p2 <- p2 + labs(x = NULL)
p3 <- p3 + labs(x = NULL)


combined_plot <- p1 / (p2 + p3) / (p4 + p5)


pdf("lda-slda.pdf", width = 10, height = 5)

combined_plot
dev.off()
