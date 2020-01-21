epicurious <- function(recipe_title){
  recipe_title <- recipe_title %>% stringi::stri_trans_general('Latin-ASCII')
  recipe_title <- recipe_title %>% str_replace('\'s', '')
  
  names_recipes <- paste(paste('recipe', seq_along(recipe_title), 
                               sep = ''), '.xml', sep = '')
  out <- rep(0, length(recipe_title))
  
  for(i in seq_along(recipe_title)){
    recipe_url <- recipe_title[i] %>% 
      as.character()
    
    recipe_url <- paste('https://www.epicurious.com/search/', recipe_url, sep = '')
    
    if(!(file.exists(names_recipes[i]))){
      download.file(recipe_url, destfile = names_recipes[i])
    }
    
    recipes_xml <- read_html(names_recipes[i]) %>% 
      htmlParse()
    
    links <- getHTMLLinks(recipes_xml, externalOnly = TRUE, 
                          xpQuery = "//a/@href", baseURL = docName(recipes_xml), 
                          relative = FALSE) 
    url_recipe <- links[links %>% str_detect('/recipes/food/views/')][1]
    out[i] <- paste('https://www.epicurious.com', url_recipe, sep = '')
    
    first_word_recipe_title <- recipe_title[i] %>% 
      str_replace('-', ' ') %>% 
      str_replace('[:punct:]', '') %>% 
      str_to_lower() %>% 
      removeWords(stopwords('english')) %>% 
      str_replace_all('[^[:alnum:]]', ' ') %>% 
      removeNumbers() %>% 
      stripWhitespace() %>% 
      word()
    
    out[i] <- ifelse(out[i] %>% 
                       str_detect(first_word_recipe_title), out[i], 0)
  }
  list('link' = out, 
       'inside_epicurious' = ifelse(out != 0, TRUE, FALSE) )
}
