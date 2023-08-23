Durslda_mod75<-readRDS("Durslda_mod75.rds")

Durtopics <- Durslda_mod75$topics %>% top.topic.words(5, by.score = TRUE) %>% 
  apply(2, paste, collapse = ",")

Durcoefs <- data.frame(coef(summary(Durslda_mod75$model)))
Durcoefs <- cbind(Durcoefs, Durtopics = factor(Durtopics, Durtopics[order(Durcoefs$Estimate, Durcoefs$Std..Error)]))
Durcoefs <- Durcoefs[order(Durcoefs$Estimate), ]
DurationPlot75<- Durcoefs %>% ggplot(aes(Durtopics, Estimate, colour = Estimate)) + geom_point() + 
  geom_errorbar(width = 0.5, aes(ymin = Estimate - 1.96 * Std..Error,
                                 ymax = Estimate + 1.96 * Std..Error)) + 
  coord_flip() + theme_bw()+labs(x = NULL)


ggsave("75-Duration.jpg",DurationPlot75,width = 10, height = 6, dpi = 300)


Durslda_mod225<-readRDS("Durslda_mod225.rds")
Durtopics <- Durslda_mod225$topics %>% top.topic.words(5, by.score = TRUE) %>% 
  apply(2, paste, collapse = ",")

Durcoefs <- data.frame(coef(summary(Durslda_mod225$model)))
Durcoefs <- cbind(Durcoefs, Durtopics = factor(Durtopics, Durtopics[order(Durcoefs$Estimate, Durcoefs$Std..Error)]))
Durcoefs <- Durcoefs[order(Durcoefs$Estimate), ]

DurationPlot225<- Durcoefs %>% ggplot(aes(Durtopics, Estimate, colour = Estimate)) + geom_point() + 
  geom_errorbar(width = 0.5, aes(ymin = Estimate - 1.96 * Std..Error,
                                 ymax = Estimate + 1.96 * Std..Error)) + 
  coord_flip() + theme_bw()+
  theme(axis.text.y = element_blank())+labs(x = NULL,title = "Topic number is 225")



Durslda_mod400<-readRDS("Durslda_mod400.rds")
Durtopics <- Durslda_mod400$topics %>% top.topic.words(5, by.score = TRUE) %>% 
  apply(2, paste, collapse = ",")

Durcoefs <- data.frame(coef(summary(Durslda_mod400$model)))
Durcoefs <- cbind(Durcoefs, Durtopics = factor(Durtopics, Durtopics[order(Durcoefs$Estimate, Durcoefs$Std..Error)]))
Durcoefs <- Durcoefs[order(Durcoefs$Estimate), ]

DurationPlot400<- Durcoefs %>% ggplot(aes(Durtopics, Estimate, colour = Estimate)) + geom_point() + 
  geom_errorbar(width = 0.5, aes(ymin = Estimate - 1.96 * Std..Error,
                                 ymax = Estimate + 1.96 * Std..Error)) + 
  coord_flip() + theme_bw()+
  theme(axis.text.y = element_blank())+labs(x = NULL,title = "Topic number is 400")



combplotDur<-(DurationPlot225+DurationPlot400)
pdf("Durslda225&400.pdf")
combplotDur
dev.off()