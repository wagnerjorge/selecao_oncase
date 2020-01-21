#install.packages('jsonlite')
#install.packages('udpipe')
#install.packages('textrank')
#install.packages('rpart')
#install.packages('rattle')
#install.packages('plyr')
#install.packages('dplyr')
#install.packages('ggplot2')
#install.packages('stringr')
#install.packages('tibble')
#install.packages('tm')
#install.packages('GGally')
#install.packages('lattice')
#install.packages('data.table')
#install.packages('XML')
#install.packages('xml2')
#install.packages('fastDummies')

library(jsonlite)
library(udpipe)
library(textrank)
library(rpart)
library(rattle)
library(plyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(tibble)
library(tm)
library(GGally)
library(lattice)
library(data.table)
library(XML)
library(xml2)
library(fastDummies)

# Leitura dos dados
setwd("G:/Dropbox_Novo/Dropbox/selecao_oncase/selecao_oncase")

recipes <- fromJSON('data/raw/receitas.json')

recipes %>% select(rating, fat, calories, protein, sodium) %>% summary()

recipes <- recipes %>% mutate(id = seq_along(fat))
id_error <- recipes %>% select(fat, calories, protein, sodium) %>% apply(2, which.max)
id_error

recipes[-id_error[1], ] %>% ggplot(aes(y = fat)) + geom_boxplot() + theme_bw()
recipes[-id_error[2], ] %>% ggplot(aes(y = calories)) + geom_boxplot() + theme_bw()
recipes[-id_error[3], ] %>% ggplot(aes(y = protein)) + geom_boxplot() + theme_bw()
recipes[-id_error[4], ] %>% ggplot(aes(y = sodium)) + geom_boxplot() + theme_bw()

# Imputacao
c(sum(na.omit(recipes$fat) >= 93), sum(na.omit(recipes$calories) >= 2500),
  sum(na.omit(recipes$protein) >= 100), sum(na.omit(recipes$sodium) >= 3400))

id_error_fat <- which(recipes$fat >= 93)
id_error_calories <- which(recipes$calories >= 2500)
id_error_protein <- which(recipes$protein >= 100)
id_error_sodium <- which(recipes$sodium >= 3400)

fat_trim_mean <- mean(recipes$fat, na.rm = TRUE, trim = .05)
calories_trim_mean <- mean(recipes$calories, na.rm = TRUE, trim = .05)
protein_trim_mean <- mean(recipes$protein, na.rm = TRUE, trim = .05)
sodium_trim_mean <- mean(recipes$sodium, na.rm = TRUE, trim = .05)

fat <- ifelse(is.na(recipes$fat), fat_trim_mean, recipes$fat)
calories <- ifelse(is.na(recipes$calories), fat_trim_mean, recipes$calories)
protein <- ifelse(is.na(recipes$protein), fat_trim_mean, recipes$protein)
sodium <- ifelse(is.na(recipes$sodium), fat_trim_mean, recipes$sodium)

fat[id_error_fat] <- fat_trim_mean
calories[id_error_calories] <- calories_trim_mean
protein[id_error_protein] <- protein_trim_mean
sodium[id_error_sodium] <- sodium_trim_mean

#Feature feature engineering
num_process <- sapply(recipes[[1]], function(x) length(x))
baked <- str_detect(recipes$directions, '°F|°C|°K')
conciseness <- str_length(recipes$directions)

hour <- sapply(str_extract_all(recipes$directions, "\\d+(?=\\shour)"),
               function(x) sum(as.numeric(x))) 
minutes <- sapply(str_extract_all(recipes$directions, "\\d+(?=\\sminute)"),
                  function(x) sum(as.numeric(x)))
time <- 24 * hour + minutes

date <- recipes %>% mutate(date, date1 = as.Date(date)) %>% select(date1)
num_days <- as.numeric(Sys.Date() - date$date1)
num_days[which(is.na(num_days))] <- mean(num_days, na.rm = TRUE)
old_recipe <- (num_days >= 3650) %>% as.integer() %>% as.factor()

num_categories <- sapply(recipes$categories,
                         function(x) length(x))

vegetarian <- recipes %>% 
  select(categories) %>% 
  sapply(function(x) str_detect(x, pattern = 'Vegetarian')) %>% 
  as.integer() %>% as.factor()

cat <- sapply(recipes$categories, 
              function(x) stringi::stri_trans_general(x, "Latin-ASCII")  )

freq_cat <- factor(unlist(recipes$categories), 
                   levels = unique(unlist(recipes$categories)))
summary(freq_cat) %>% head(10)



desc <- recipes %>% select(desc) %>% is.na() %>% as.numeric()

phrases <- recipes %>% 
  select(desc) %>% 
  apply(1, 
        function(x) str_count(x, pattern = '\\.|\\:|\\?|\\!|\\;'))
phrases <- ifelse(is.na(phrases), 0, phrases)

desc_len <- recipes %>% select(desc) %>% apply(1,
                                               function(x) str_length(x)) 
desc_len <- ifelse(is.na(desc_len), 0, desc_len)


# Novo dataset
rwi <- which(sapply(recipes$ingredients, length) == 0)
rwi
recipes2 <- recipes[-rwi, ]

num_ingredients <- recipes2$ingredients %>% sapply(function(x) length(x))


recipes2 %>% 
  select(rating) %>% 
  summary()

recipes2 <- recipes2 %>% mutate(rating, rating = ifelse(is.na(rating), 
                                                        mean(rating, na.rm = TRUE), rating))


recipes3 <- data.table(recipes2$title, recipes2$rating, fat[-rwi], calories[-rwi], 
                       protein[-rwi], sodium[-rwi], num_categories[-rwi],num_days[-rwi],
                       num_ingredients, num_process[-rwi], baked[-rwi], 
                       conciseness[-rwi], desc[-rwi], desc_len[-rwi], old_recipe[-rwi], 
                       phrases[-rwi], time[-rwi], vegetarian[-rwi], 
                       recipes2$categories, recipes2$ingredients)

names(recipes3) <- c('title', 'rating', 'fat', 'calories', 'protein', 'sodium',
                     'num_categories', 'num_days', 'num_ingredients', 'num_process', 
                     'baked', 'conciseness', 'desc', 'desc_len', 'old_recipe', 'phrases',
                     'time', 'vegetarian', 'categories', 'ingredients')

recipes3$desc <- recipes3$desc %>% 
  as.integer() %>% 
  as.factor() 

recipes3$baked <- recipes3$baked %>% 
  as.integer() %>% 
  as.factor() 

write_json(recipes2, 'data/interm/recipes2.json')

write_json(recipes3, 'data/interm/recipes3.json')

recipes4 <- recipes3 %>% dplyr::select(-one_of('categories', 'ingredients', 'title')) %>% 
  mutate(rating, rating = ifelse(rating >= 4.0, 1, 0) %>% as.factor()) 


write_json(recipes4, 'data/processed/recipes4.json')

#Remove all objects
rm(list=ls(all=TRUE))
