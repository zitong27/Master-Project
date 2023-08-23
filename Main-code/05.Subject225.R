lda225t<- read.csv("lda225topic.csv",header=TRUE,sep = ",", skip = 1)
lda225topic<-lda225t[,2]
words <- sapply(strsplit(lda225t[,2], "/"), function(x) gsub("\"", "", x))
word_counts <- table(unlist(words))
word_counts_df <- data.frame(Word = names(word_counts), Count = as.numeric(word_counts))

word_df <- data.frame(Word = word_counts_df[,1], Count = word_counts_df[,2])


bar225_plot <- ggplot(word_df, aes(x = Count, y = reorder(Word, +Count),fill=Count)) +
  geom_bar(stat = "identity") +
  labs(title = "Word Frequency in LDA model with 225 topics", x = "Count", y = "Word") + 
  scale_fill_gradient(low = "#a3aed1", high = "#484b8a")+
  theme_minimal() +
  theme(legend.position = "none")



Durslda225t<- read.csv("Durslda225topic.csv",header=TRUE,sep = ",", skip = 1)
Durslda225topic<-Durslda225t[,2]
Durwords <- sapply(strsplit(Durslda225t[,2], "/"), function(x) gsub("\"", "", x))
Durword_counts <- table(unlist(Durwords))

Durword_counts_df <- data.frame(Word = names(Durword_counts), Count = as.numeric(Durword_counts))

Durword_counts_df <- Durword_counts_df[order(-Durword_counts_df$Count), ]
Durword_df <- data.frame(Word = Durword_counts_df[,1], Count = Durword_counts_df[,2])


Durbar225_plot <- ggplot(Durword_df, aes(x = Count, y = reorder(Word, +Count),fill=Count)) +
  geom_bar(stat = "identity") +
  labs(title = "Word Frequency in LDA model with 225 topics", x = "Count", y = "Word") + 
  scale_fill_gradient(low = "#a3aed1", high = "#484b8a")+
  theme_minimal() +
  theme(legend.position = "none")

bar225_plot<-bar225_plot+labs(x=NULL,y=NULL,title=NULL)
Durbar225_plot<-Durbar225_plot+labs(x="Count",y=NULL,title=NULL)
final_plot <- bar225_plot/Durbar225_plot +
  plot_annotation(title = "Distribution of Subject Categories in LDA & sLDA with 225 Topics")&
  theme(plot.title= element_text(face = "bold",size=16))

final_plot
ggsave("subject225.pdf",final_plot,width = 9, height = 7)
