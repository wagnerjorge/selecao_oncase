# Desafio Oncase - Receitas

Autor: Wagner J.F. Silva

Afiliação: Centro de Informática - UFPE

Palavras-chave: cientista de dados, extração de conhecimento, receitas, epicurious

abstract: Estatístico, mestre e doutorando em Ciência da Computação apaixonado por ciência de dados e entusiasta no uso e criação de modelos/algoritmos whitebox para extração e mineração de conhecimento a partir de dados diversos. Debruçado, até o final de sete dias, sobre o estudo da base de dados de receitas com objetivo de entender suas peculiaridades e pepitas escondidas como forma de expor parte do conhecimento obtido na academia para a Oncase.

# O problema

Este notebook foi construído com objetivo de solucionar questões de negócios como método de seleção da Oncase. Em linhas gerais, o problema consiste em extrair conhecimento a partir de dados de receitas de comidas. Devido a quantidade imensurável de conhecimento disponível através deste conjunto de dados a Oncase propôs seis perguntas a serem respondidas a partir dos dados, sendo a última de caráter opcional, são elas:

* A categorias pertencem as comidas mais calóricas?
* Quais os top 10 ingredientes contidos nas receitas mais calóricas? 
* Se você tivesse que recomendar 3 receitas baseando-se nos dados, quais seriam? 
* Alguma característica presente nos dados determina a alta nota de uma receita? 
* Considerando-se as categorias das top 100 receitas em avaliação, quantas receitas há atualmente no site [Epicurious](https://www.epicurious.com) para cada categoria? 
* [opcional] Construa um classificador para recomendar tags (categorias) para as receitas.

Adiantamos de antemão que todas questões exceto a sexta foram respondidas.

## Preparação das ferramentas e dos dados
Inicialmente vamos instalar todos os pacotes necessários para as análises, mudar o diretório de trabalho e tratar os dados através de limpeza e criação de features. Uma vez que os pacotes foram instalados estes devem ser disponibilizados para o uso.

## Instalação dos pacotes necessários
```{r, results = 'hide', message =  FALSE, include = FALSE}
start <- Sys.time()
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
```


## Leitura dos dados
A leitura dos dados é feita pelo pacote jsonlite e transformado num `data.frame`. Dessa forma, cada coluna descreve uma variável. Consta na base de dados 20.130 receitas e 12 variáveis em formatos diversos: texto, contínuas, datas. 

```{r}
setwd("G:/Dropbox_Novo/Dropbox/selecao_oncase/selecao_oncase")
recipes <- fromJSON('receitas.json')
```

# Análise descritiva preliminar

Para as variáveis contínuas nós calculamos as principais medidas resumo. No entando, é possível observar que há valores extremamente fora do esperado para cada variável, estes valores indicam algum erro de medição visto que a distância do terceiro quartil é muito grande, o que não configura um outlier, mas um erro claro de medição. Para identificá-los, nós criamos uma variabel `id`. A partir dela conseguimos identificar, inicialmente, dois indivíduos com valores altamente discrepante.

```{r}
recipes %>% select(rating, fat, calories, protein, sodium) %>% summary()

recipes <- recipes %>% mutate(id = seq_along(fat))
id_error <- recipes %>% select(fat, calories, protein, sodium) %>% apply(2, which.max)
id_error
```

Além destes, podemos observar outros valores estranhos nas variáveis contínuas, a saber: `fat, calories, protein` e `sodium`. Estes valores tem deformado a distribuição de probabilidade conforme podemos observar nos boxplots abaixo. Isto impede a identificação do uso adequado de modelos para explicação de comportamentos, caso necessário.

```{r}
recipes[-id_error[1], ] %>% ggplot(aes(y = fat)) + geom_boxplot() + theme_bw()
recipes[-id_error[2], ] %>% ggplot(aes(y = calories)) + geom_boxplot() + theme_bw()
recipes[-id_error[3], ] %>% ggplot(aes(y = protein)) + geom_boxplot() + theme_bw()
recipes[-id_error[4], ] %>% ggplot(aes(y = sodium)) + geom_boxplot() + theme_bw()
```


## Imputação de dados

Então, nós utilizamos fontes nutricionais externas para detectar estes valores discrepantes para cada variável descrita. Todas medidas consideram a média ingerida por uma pessoa por dia. Obviamente esta medida pode ser subestimada, pois dependende diretamente do tamanho da porção que serve cada receita e sobrestimada considerando que as métricas são diárias e não por refeição/receita. Dessa forma, supomos que há um equilíbrio nas métricas externas utilizadas. Em detalhes, as métricas são:

* Total de gordura (`fat`) - Segundo a [Health](https://health.gov/dietaryguidelines/dga2000/document/choose.htm) a média de gordura para uma dieta diária de 2.800 calorias é de 93g. Baseado nessa informação trataremos 186g de gordura como limiar.

* Calorias (`calories`) - Ainda de acordo com a [Health](https://health.gov/dietaryguidelines/2015/guidelines/appendix-2/) a média de calorias para um homem e uma mulher de um jovem é 2.800 e 2200 calorias, respectivamente. Devido a ausência do gênero como variável na base, utilizaremos como limiar a média entre homens e mulheres que é 2500 calorias.

* Proteína (`protein`) - Em entrevista para o [NY Times](https://www.nytimes.com/2017/07/28/well/eat/how-much-protein-do-we-need.html) a autora do livro “Devoured: How What We Eat Defines Who We Are”, Sophie Egan afirma que em média um norte americano consome em média 100g de proteínas pode dia logo, o limiar adotado é 200g. 

* Total de Sódio (`sodium`) - A American Heart Association conhecida como [Heart](https://www.heart.org/en/healthy-living/healthy-eating/eat-smart/sodium/how-much-sodium-should-i-eat-per-day) informa que os americanos consomem em média 3.400mg de sódio por dia assim, este foi escolhido como valor limitante para definição de erro de medida ou não.


Em outras palavras, receitas que ultrapassem os limiares citados acima, nas suas respectivas variáveis, serão substituídas pela média truncada em 5%. O número de receitas que ultrapassam estes limiares são 678, 234, 441 e 328 para `fat`, `calories`, `protein` e `sodium`, respectivamente.


```{r}
c(sum(na.omit(recipes$fat) >= 93), sum(na.omit(recipes$calories) >= 2500),
  sum(na.omit(recipes$protein) >= 100), sum(na.omit(recipes$sodium) >= 3400))

id_error_fat <- which(recipes$fat >= 93)
id_error_calories <- which(recipes$calories >= 2500)
id_error_protein <- which(recipes$protein >= 100)
id_error_sodium <- which(recipes$sodium >= 3400)

```

```{r}
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
```


## Criação de novas features

Acreditamos que a nota da receita está relacionada a dificuldade da mesma, pois receitas muito difíceis podem não ser concluídas com sucesso e com isso ter sua nota diminuída e vice versa. Então, elencamos algumas medidas para avaliar a dificuldade da receita, são elas: (1) número de processos (`num_process`); (2) cozida, assada, ou vai ao forno (`baked`); (3) número de caracteres (concisão) (`conciseness`); (4) preparation time (`time`).

```{r}
num_process <- sapply(recipes[[1]], function(x) length(x))
baked <- str_detect(recipes$directions, '°F|°C|°K')
conciseness <- str_length(recipes$directions)

hour <- sapply(str_extract_all(recipes$directions, "\\d+(?=\\shour)"),
               function(x) sum(as.numeric(x))) 
minutes <- sapply(str_extract_all(recipes$directions, "\\d+(?=\\sminute)"),
               function(x) sum(as.numeric(x)))
time <- 24 * hour + minutes
```

Número de dias desde a publicação da receita pode ser um fator decisivo na nota da receita. Além disso, criamos uma variável que determina se uma "receita é clássica" ou não, elas são consideradas clássicas se tem mais de 10 anos (3650 dias) desde a publicação. Através de uma variável dummy `old_recipe` sabemos se as receitas são clássicas ou não.

```{r}
date <- recipes %>% mutate(date, date1 = as.Date(date)) %>% select(date1)
num_days <- as.numeric(Sys.Date() - date$date1)
num_days[which(is.na(num_days))] <- mean(num_days, na.rm = TRUE)
old_recipe <- (num_days >= 3650) %>% as.integer() %>% as.factor()
```

Outra importante variável para a nota da receita são as categorias que ela está inserida. Nós calculamos e armazenamos o número de categorias em que uma receita está inserida (`num_categories`) e verificamos também se está é vegetariana ou não (`vegetarian`). Além disso, verificamos quais as categorias são mais frequentes nas receitas através da variável `freq_cat`

```{r}
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
```

A descrição da receita funciona como um subtítulo da receita e pode ser um fator de engajamento a executar a receita. Dessa forma, pode ter influência sobre a avaliação da receita. Portanto, três variáveis foram criadas, a primeira verifica se há descrição na receita, a segunda indica o número de orações de sentido completo na descrição e a terceira revela o número de caracteres na descrição. Elas são descritas pelas variáveis `desc`, `phrases` e `desc_len`, respectivamente.

```{r}
desc <- recipes %>% select(desc) %>% is.na() %>% as.numeric()

phrases <- recipes %>% 
  select(desc) %>% 
  apply(1, 
        function(x) str_count(x, pattern = '\\.|\\:|\\?|\\!|\\;'))
phrases <- ifelse(is.na(phrases), 0, phrases)

desc_len <- recipes %>% select(desc) %>% apply(1,
                                                function(x) str_length(x)) 
desc_len <- ifelse(is.na(desc_len), 0, desc_len)
```


Estudando a variável ingredientes podemos detectar que algumas receitas não apresentam nenhum ingrediente assim, não foram consideradas receitas válidas e foram deletadas do banco de dados. Para execução do processo de deleção utilizamos a variável `rwi` que significa "recipes wihtout ingredients". A base de dados `recipes2` armazena a nova base de dados sem as receitas inválidas. Em seguida, contamos o número de ingredientes por receita.

```{r}
rwi <- which(sapply(recipes$ingredients, length) == 0)
rwi
recipes2 <- recipes[-rwi, ]

num_ingredients <- recipes2$ingredients %>% sapply(function(x) length(x))
```



A variável resposta tem algumas notas (`rating`) faltando, supondo que esta ausência é aleatória, imputamos estes valores pela média. Além disso, observamos que há uma assimetria positiva na variável resposta, notamos também que a nota mínima recebida por uma receita é zero e máxima igual a 5.0.

```{r}
recipes2 %>% 
  select(rating) %>% 
  summary()

recipes2 <- recipes2 %>% mutate(rating, rating = ifelse(is.na(rating), 
                                             mean(rating, na.rm = TRUE), rating))

recipes2 %>%
  ggplot(aes(rating)) + geom_histogram(bins = 6) + ylab('Frequency') + 
  ylab('Frequência') + theme_bw()
```

Através das variáveis originais e das criadas neste estudo (features) montamos um novo dataset denominado de  `recipes3`. Com este novo dataset estudamos quais variáveis podem influenciar na nota de uma receita.

```{r}
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
```


As 10 receitas com maior teor calórico são descritas abaixo.

```{r}
caloric_recipes <- recipes3 %>% 
  arrange(desc(calories)) %>% slice(1 : 10)
caloric_recipes
```

Considerando as 10 receitas com maior teor calórico verificamos as principais categorias que estas receitas pertencem, respectivamente, são: (1) Peanut Free; (2) Soy Free; (3) Tree Nut Free; (4) Dinner; (5) Vegetarian; (6) Pescatarian; (7) Kosher; (8) Bon Appétit; (9) Party; (10) Sugar Conscious.

```{r}
stats <- txt_freq(unlist(caloric_recipes$categories))
library(lattice)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 10), 
         col = "cadetblue", main = "Categorias mais frequentes", xlab = "Frequência")
stats %>% slice(1 : 10)
```

Considerando as 10 receitas com maior teor calórico podemos observar através do gráfico abaixo que o ingrediente mais presente nelas é `chicken` seguido de `pepper`, `butter`, `sugar`, `salt` e outros.

```{r}
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

```



Seguindo o conceito da [fronteira de Pareto](https://link.springer.com/article/10.1007/s11047-018-9685-y), recomendamos três receitas segundo 5 critérios por ordem de prioridade, a saber: (1) maiores notas; (2) baixo teor de gordura; (3) baixo teor de calorias; (4) pouco sódio; (5) poucos processos; (6) receita simples (consisa).

```{r}
recipes3[with(recipes3, order(-rating, fat, calories, sodium,
                              num_process, conciseness)), ] %>% 
  slice(1 : 3)

```


# Modelagem 
Com objetivo de identificar se há algum fator determinante para a alta nota de uma receita transformamos a nota numa variável dummy, em que receitas com notas iguais ou maiores que 4.0 são consideradas receitas bem avaliadas, caso contrário não. Em seguida, extraímos as regras mais importantes de uma árvore de decisão que fazem uma receita ser ou não bem avaliada.

Selecionando as regras mais importantes notamos, por exemplo, que as receitas que tem mais de 8.5 ingredientes tendem, com probabilidade igual a 0.59, de serem bem avaliadas, com cobertura de 57%. É interessante mencionar também que as receitas tendem a serem mal avaliadas, com probabilidade igual a 0.46, quando tem menos de 8.5 ingredientes, levam mais de 8.5 horas para serem feitas, estão no site a mais e 1726 dias e tem menos de 125.5 calorias.

 Rule number: 22 [rating=0 cover=1776 (9%) prob=0.46]
   num_ingredients< 8.5
   time>=8.5
   num_days>=1726
   calories< 125.5

```{r}
recipes4 <- recipes3 %>% dplyr::select(-one_of('categories', 'ingredients', 'title')) %>% 
  mutate(rating, rating = ifelse(rating >= 4.0, 1, 0) %>% as.factor()) 
  

fit_tree <- rpart(formula = rating ~., data = recipes4)
asRules(fit_tree)
```

Neste ponto, selecionamoas as cem melhores receitas segundo a nota de avaliação para o estudo das categorias que possuem mais receitas.

```{r}
recipes_top100 <- recipes3 %>% 
  arrange(desc(rating)) %>% 
  slice(1 : 100)
```

Com a função abaixo fizemos um crawler com objetivo de verificar quais das 100 primeiras receitas estão no site [epicurious](https://www.epicurious.com). A função `epicurious` recebe o nome das receitas (`title`) e retorna o link da receitas no site, caso ela se encontre nele, é retornado também uma variável booleana que indica se a variável foi encontrada na busca do site ou não.

```{r}
title_top100 <- recipes_top100 %>% dplyr::select(title)

urls <- rbindlist(lapply(title_top100, function(x){
  url <- paste('https://www.epicurious.com/search/', x, sep = '')
  tibble(url)
}), fill = TRUE)


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
```

O nome das receitas que estão no site podem ser vistos abaixo:

```{r, results = 'hide', message =  FALSE, include = FALSE}
recipes_in_epicurious <- epicurious(title_top100$title)
recipes_in_epicurious$inside_epicurious
```

A seguir, vemos as trinta categorias que possuem mais receitas.

```{r}
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
 
```

# Considerações, limitações da solução e feedback 
O presente desafio deu oportunidade de mostrar conhecimento em áreas diversas de ciência de dados, desde a limpeza de dados, passando pela necessidade de conhecimento de especialistas até a identificação de fatores que influenciam uma variável resposta.

Esta solução não é definitiva podendo assim, ser aprimorada em tempo oportuno. Portanto, há algumas limitações que podem ser melhoradas em etapas futuras, tais como:

* Definição do limiar para detecção de erro de medição - esta etapa pode ser facilmente refinada quando se há conhecimento melhorado do domínio, disponibilidade de consulta a especialista (cliente); ou ainda através de investigação de modelos estatísticos avançados não usuais na literatura de ciência de dados;

* O crawler que verifica a existência de uma receita no site epicurious é um protótipo, podendo ser melhorado;

* A sexta questão pode ser resolvia através de sistemas de recomendação, por exemplo, cgontent-based filtering. Porém há limitações técnicas e teóricas, temporárias, que impossibilitam a solução do problema no tempo estabelecido;

* Feedback - A variável `rating` de modo purista deve ser analisado por um modelo estatístico cuja distribuição de probabilidade tenha suporte no intervalo [0, 5], ou seja, modelos dessa natureza garantem matematicamente que as predições e previsões não estaram fora do domínio da variável. Portanto, a detecção de fatores importantes que influenciam na nota de uma receita pode ser feita através de modelos desta natureza. Contudo, a carga teórica para apredizagem de modelos deste tipo pode durar de uma semana a um mês. Uma possível solução para isto é perder um pouco de informação binarizando (tornando dummy) a variável em estudo com isso, podemos aplicar um modelo de regressão logística ou árvore de decisão para extração de regras. Sendo o último a abordagem utilizada.

Por fim, abaixo temos o tempo para solução de todas as questões obrigatórias.
```{r}
Sys.time() - start
```
