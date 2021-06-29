library(plyr)
library(shiny)
library(stringr)
library(mgsub)
library(tidyverse)
library(quanteda)
library(tidytext)
library(plyr)
library(shiny)
library(stringr)
library(mgsub)
library(tidyverse)
library(quanteda)
library(tidytext)
library(wordcloud)
library(grDevices)
library(quanteda)
library(DT)
library(ggwordcloud)
library(ggplot2)
library(readxl)
library(rtweet)
#install.packages("pryr")
#install.packages("devtools")
#devtools::install_github("hadley/lineprof")

#setwd("/home/ricardo/Documents/UNESCO/apps/plataforma_unesco")




###### función para procesar y guardar base, va adentro de la función de descarga ####
#query = "queer"
#cantidad = 100

  busqueda_tweets_free <- function(query,cantidad,retryonratelimit = F){

   ##### función para arreglar el téxto
   
   fix_text <- function(data) {
      a = c(" ", "-", "á","é","í","ó","ú")
      b= c("_", "", "a", "e", "i", "o", "u")
      data =  mgsub(data, a,b)
      data = tolower(data)
      return(data)
   }
   
   #### funcion clean Tweets
   
   tweets_cleaner <- function(x){
      
      tweets_txt <- x
      tweets_txt <- tolower(tweets_txt)
      clean_tweet = gsub("&amp", "", tweets_txt) # Remove Amp
      clean_tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet) # Remove Retweet
      clean_tweet = gsub("@\\w+", "", clean_tweet) # Remove @
      clean_tweet = gsub("#", " ", clean_tweet) # Before removing punctuations, add a space before every hashtag
      clean_tweet = gsub("[[:punct:]]", "", clean_tweet) # Remove Punct
      clean_tweet = gsub("[[:digit:]]", "", clean_tweet) # Remove Digit/Numbers
      clean_tweet = gsub("http\\w+", "", clean_tweet) # Remove Links
      clean_tweet = gsub("[ \t]{2,}", " ", clean_tweet) # Remove tabs
      clean_tweet = gsub("^\\s+|\\s+$", " ", clean_tweet) # Remove extra white spaces
      clean_tweet = gsub("^ ", "", clean_tweet)  # remove blank spaces at the beginning
      clean_tweet = gsub(" $", "", clean_tweet) # remove blank spaces at the end
      clean_tweet = gsub("[^[:alnum:][:blank:]?&/\\-]", "", clean_tweet) # Remove Unicode Char
      
      
      clean_tweet <- str_replace_all(clean_tweet," "," ") #get rid of unnecessary spaces
      clean_tweet <- str_replace_all(clean_tweet, "https://t.co/[a-z,A-Z,0-9]*","") # Get rid of URLs
      clean_tweet <- str_replace_all(clean_tweet, "http://t.co/[a-z,A-Z,0-9]*","")
      clean_tweet <- str_replace(clean_tweet,"RT @[a-z,A-Z]*: ","") # Take out retweet header, there is only one
      clean_tweet <- str_replace_all(clean_tweet,"#[a-z,A-Z]*","") # Get rid of hashtags
      clean_tweet <- str_replace_all(clean_tweet,"@[a-z,A-Z]*","") # Get rid of references to other screennames
      
      clean_tweet
   }
   
   
n = cantidad
x = query

# Identificación y obtención de tokens
appname <- "quantuscl"
key     <- "BAuwk29vuK1QgNXk2AjyHhtFm"
secret  <- "mu8gF7AcddtveaQHF1q2VgUFu9ImGXmEdcMbZvZt9CrBhLWKbB"
acces_token <- "123905060-9DeRvtzH78V8zA08xRTHRscHGwYyWBfIVHWLUNdF"
acces <- "7OqGawz0IxPLdF576zH4Lqr6S06w11bJnf9M9VaYVfgBd"

token <- create_token(app = appname, 
                      consumer_key = key,
                      consumer_secret = secret, 
                      access_token = acces_token, 
                      access_secret = acces, 
                      set_renv = T)

d <- search_tweets(q = x, n = n, lang = "es",  type = "mixed", include_rts = F, token = token, retryonratelimit = F) 
hoy = format(Sys.time(), "%y%m%d%H%M%S")

d$fecha_busq = hoy
d$nom_busq = x
d$id_tweet = seq(1000001,1000000+nrow(d))
d$ubicacion_buscada = NA

   #  Corregimos y extraemos nombres
gsub(".rds|AND|OR","_", x) -> terminos; terminos
str_replace_all(terminos," ","") -> terminos; terminos
str_split(terminos, pattern = "_", simplify = TRUE) -> terminos; terminos
as.vector(terminos) -> terminos
 
d$concepto1 = terminos[1]
d$concepto2 = terminos[2]
d$concepto3 = terminos[3]

   quanteda::stopwords(language = "es") -> stop_words 
   remove_reg <- "&amp;|&lt;|&gt;"
   
   d %>% 
     select(id_tweet,fecha_busq, nom_busq,  concepto1, concepto2, concepto3, text, location, coords_coords, geo_coords,country, favorite_count, retweet_count, ubicacion_buscada,) %>%
     distinct(text, .keep_all = T) %>% 
     mutate( concepto1 =  fix_text(concepto1),
          concepto2 =  fix_text(concepto2),
          concepto3 =  fix_text(concepto3) )-> b1_1
   
   b1_1$text = tweets_cleaner(b1_1$text)

   ##### tokenizamos reparamos valores, eliminamos stopwords, corremigos volvemos a anidar en grupos los tokens ####
   tweets = b1_1
   
   tidy_tweets  = tweets %>% 
     distinct(text, .keep_all = T) %>% 
     filter(!str_detect(text, "^RT")) %>%
     mutate(text = str_remove_all(text, remove_reg)) %>%
     unnest_tokens(word, text, token = "tweets", to_lower = T) %>%
     filter(!word %in% stop_words,
            !word %in% str_remove_all(stop_words, "'"),
            str_detect(word, "[a-z]"),
            !grepl("@", word),
            !grepl("http",word),
            nchar(word)> 2  ) #  %>%   mutate(word = replace(word, word == "clitoris", "clítoris")) 
   
   tidy_tweets$word = gsub("#", "", tidy_tweets$word)
   tidy_tweets$word = fix_text(tidy_tweets$word)
   
   #### creamos variable de palabras que coinciden con sexdic
   sexdic = readRDS("sexdic_tidy.rds")
   data.frame(table(sexdic$token)) -> df_sexdic_tokens
   

   tidy_tweets <- tidy_tweets %>% 
     mutate(e_sexdic =  ifelse(word %in% df_sexdic_tokens$Var1[df_sexdic_tokens$Freq > 4],1,0)) #### tienen que ser palabras que en SEXDIC tengan mayr frecuencia de 1
   
   #### construimos variable para darle puntaje a los tuis dependiendo de la cantidad de palabras del SEXDIC que tengan
   tidy_tweets = dplyr::left_join(tidy_tweets,
                                  tidy_tweets %>% 
                                    group_by(id_tweet) %>% 
                                    summarise(n_SEX_tweet = sum(e_sexdic)), by = "id_tweet")  
   
   
   #### agregamos analisis de sentimiento 
   sent_dic2 = read_excel("emociones.xlsx") %>% 
      select(word = `Spanish Translation (Google Translate)`, Positive, Negative, Anticipation,	Disgust,	Fear,	Joy,	Sadness,	Surprise,	Trust)
   
   tidy_tweets = left_join(tidy_tweets, sent_dic2, by = "word")
   
   tidy_tweets = tidy_tweets  %>% left_join(tidy_tweets %>% 
      group_by(id_tweet) %>% 
      summarise(t_pos = sum(Positive, na.rm = T),
                t_neg = sum(Negative, na.rm = T),
                t_expect = sum(Anticipation, na.rm = T),
                t_disgusto = sum(Disgust, na.rm = T),
                t_temor = sum(Fear, na.rm = T),
                t_alegria = sum(Joy, na.rm = T),
                t_tristeza = sum(Sadness, na.rm = T),
                t_sorpresa = sum(Surprise, na.rm = T),
                t_confiar = sum(Trust, na.rm = T),
                t_sexDIC = sum(e_sexdic, na.rm = T)), by = "id_tweet")
   
   tidy_tweets = tidy_tweets %>% left_join(tweets[,c("id_tweet", "text")], by = "id_tweet")


saveDATAtidy <- function(data){
   filename <- paste0("BBDD/",data[[2]][[1]],"_",data[[4]][[1]],"_tidy.rds")
   saveRDS(data,filename)
}
  
# saveDATAtweets <- function(data){
#    filename <- paste0("BBDD/tweets/",data[[1]][[1]],"_",data[[4]][[1]],"_tweets.rds")
#    data = data[,c("id_tweet", "concepto1", "concepto2", "concepto3","text")]
#    saveRDS(data,filename)
# }
# 
 saveDATAtidy(tidy_tweets)
#   
# #  saveDATAtweets(tweets)
   
return(tidy_tweets)
}

#busqueda_tweets_free("queer", 100)
