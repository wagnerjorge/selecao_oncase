library(jsonlite)
library(plyr)
library(dplyr)
library(ggplot2)

setwd("G:/Dropbox_Novo/Dropbox/selecao_oncase/selecao_oncase")
receitas <- fromJSON('receitas.json')

set.seed(1)
receitas <- receitas[sample(seq_along(receitas$fat), 2000, replace = FALSE), ]

receitas <- receitas %>% mutate(id = seq_along(fat))
receitas %>% select(fat, calories, protein, sodium) %>% summary()

id_error <- receitas %>% select(fat, calories, protein, sodium) %>% apply(2, which.max)

threshold <- receitas[-id_error, ] %>% 
  select(fat, calories, protein, sodium) %>% 
  apply(2, function(x) mean(x, na.rm = TRUE) + 3 * sd(x, na.rm = TRUE))

receitas[-id_error, ] %>% ggplot(aes(x = seq_along(fat) , y = fat)) + 
  geom_point() + xlab('Index') +
  geom_text(aes(label=ifelse(fat > 2 *sd(fat, na.rm = TRUE) ,
                             as.character(id), '')), hjust=0, vjust=0)

receitas[-id_error, ] %>% ggplot(aes(x = seq_along(calories) , y = calories)) + 
  geom_point() + xlab('Index')+
  geom_text(aes(label=ifelse(calories > 2 * sd(calories, na.rm = TRUE) ,
                             as.character(id), '')), hjust=0, vjust=0)

receitas[-id_error, ] %>% ggplot(aes(x = seq_along(protein) , y = protein)) + 
  geom_point() + xlab('Index')+
  geom_text(aes(label=ifelse(protein > 2 *sd(protein, na.rm = TRUE) ,
                             as.character(id), '')), hjust=0, vjust=0)

receitas[-id_error, ] %>% ggplot(aes(x = seq_along(sodium) , y = sodium)) + 
  geom_point() + xlab('Index')+
  geom_text(aes(label=ifelse(sodium > 2 *sd(sodium, na.rm = TRUE) ,
                             as.character(id), '')), hjust=0, vjust=0)



receitas <- receitas %>% 
  mutate(fat = ifelse(is.na(fat), mean(fat, na.rm = TRUE), fat))
receitas <- receitas %>%
  mutate(calories = ifelse(is.na(calories), mean(calories, na.rm = TRUE), fat))
receitas <- receitas %>% 
  mutate(protein = ifelse(is.na(protein), mean(protein, na.rm = TRUE), fat))
receitas <- receitas %>% 
  mutate(sodium = ifelse(is.na(sodium), mean(sodium, na.rm = TRUE), fat))


