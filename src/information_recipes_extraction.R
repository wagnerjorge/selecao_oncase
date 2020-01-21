
# Crawler function
source('G:/Dropbox_Novo/Dropbox/selecao_oncase/selecao_oncase/src/crawler.R', echo=TRUE)

setwd('G:/Dropbox_Novo/Dropbox/selecao_oncase/selecao_oncase/')
recipes3 <- fromJSON('data/interm/recipes3.json')
recipes4 <- fromJSON('data/processed/recipes4.json')

# Caloric recipes
caloric_recipes <- recipes3 %>% 
  arrange(desc(calories)) %>% slice(1 : 10)
caloric_recipes


# Categories of the most caloric recipes
stats <- txt_freq(unlist(caloric_recipes$categories))
library(lattice)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 10), 
         col = "cadetblue", main = "Categorias mais frequentes", xlab = "Frequência")
stats %>% slice(1 : 10)


# Top ingredients in most caloric recipes

drop_words <- c('cup', 'cups', 'teaspoon', 'teaspoons', 'tablespoons', 'tablespoon',
                'peel', 'pound', 'ground', 'preserve', 'inch', 'piece', 'stick', 'slice',
                'large', 'optional', 'ounce', 'leave', 'purpose', 'powder', 'broth',
                'baking', 'ounces', 'pieces', 'cube')


ingredients <- caloric_recipes$ingredients %>% 
  lapply(function(x) removeWords(x, drop_words)) %>% 
  lapply(function(x) removeWords(x, stopwords('english'))) %>% 
  lapply(function(x) str_replace_all(x, '[^[:alnum:]]', ' ')) %>% 
  lapply(function(x) removeNumbers(x)) %>% 
  lapply(function(x) stripWhitespace(x))


if(file.exists('english-ewt-ud-2.4-190531.udpipe')){
  ud_model <- udpipe_load_model('english-ewt-ud-2.4-190531.udpipe')
}else{
  ud_model_temp <- udpipe_download_model(language = "english")
  ud_model <- udpipe_load_model(ud_model_temp$file_model)
}

x <- udpipe_annotate(ud_model, x = unlist(ingredients))
x <- as.data.frame(x)

stats <- subset(x, upos %in% "NOUN")
stats <- txt_freq(x = stats$lemma)

stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 15), 
         col = "cadetblue", main = "Ingredientes mais presentes nas receitas altamente calóricas", xlab = "Frequência")

# Three recipes recommendation
recipes3[with(recipes3, order(-rating, fat, calories, sodium,
                              num_process, conciseness)), ] %>% 
  slice(1 : 3)

# Rules extraction
fit_tree <- rpart(formula = rating ~., data = recipes4)
asRules(fit_tree)

# One hundred best recipes
recipes_top100 <- recipes3 %>% 
arrange(desc(rating)) %>% 
  slice(1 : 100)

title_top100 <- recipes_top100 %>% dplyr::select(title)

urls <- rbindlist(lapply(title_top100, function(x){
  url <- paste('https://www.epicurious.com/search/', x, sep = '')
  tibble(url)
}), fill = TRUE)


setwd('G:/Dropbox_Novo/Dropbox/selecao_oncase/selecao_oncase/data/processed')
recipes_in_epicurious <- epicurious(title_top100$title)
recipes_in_epicurious$inside_epicurious

setwd('G:/Dropbox_Novo/Dropbox/selecao_oncase/selecao_oncase/')

# Category with most recipes
cat_existents <- unique(unlist(recipes_top100[recipes_in_epicurious$inside_epicurious,                                                                                     ]$categories))
num_recipes_cat <- lapply(recipes_top100[recipes_in_epicurious$inside_epicurious,
                                         ]$categories, 
                          function(x) factor(x, cat_existents))

num_recipes_cat <- lapply(num_recipes_cat, function(x) dummy_cols(x))
num_recipes_cat <- lapply(num_recipes_cat, function(x) data.matrix(x, 
                                                                   rownames.force = NA))

num_recipes_cat <- t(sapply(num_recipes_cat, function(y) apply(y, 2, sum)))
num_recipes_cat <- apply(num_recipes_cat, 2,  function(x) sum(x))[-1]

recipes_by_cat <- data.frame(categorie = as.factor(cat_existents), 
                             num_recipes_cat = num_recipes_cat) %>% 
  arrange(desc(num_recipes_cat))

g <- recipes_by_cat[1:30, ] %>% ggplot(aes(x = reorder(categorie, num_recipes_cat), y = num_recipes_cat)) + 
  geom_bar(stat = 'identity') +  coord_flip() + theme_bw() + 
  xlab('Frequência') + ylab('Categoria') + ggtitle('Trinta categorias com mais receitas')
g
