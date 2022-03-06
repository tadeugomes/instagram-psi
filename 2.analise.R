############################### Carregamento dos pacotes utilizados na analise #######################################

library(quanteda)
library(stopwords)
library(quanteda.textplots)
library(quanteda.textstats)
library(magrittr)
library(topicmodels)
library(textcat)  # Para filtrar por idioma. Nao foi utilizado e deixei no script para registro da possibilidade. 

options(stringsAsFactors = FALSE)


################ Explorar os dados ############

# Leitura dos datasets.  Cada dataset foi carregado individualmente. Os resultados foram produzidos e incorporados ao texto da dissertacao. Entao o processo deve ser repetido para cada dataset.
psi <-read.csv("table-psicologia-2021-01-13_20_23_11.csv", header = TRUE, sep = ",", encoding = "UTF-8")

# Verificar dimensao dos dados e colunas

dim(psi)

colnames(psi)


## Selecionar apenas as publicaces em portugues, caso seja o recorte. Nao foi aplicado ao dataset para analise na dissertacao ###
#psi.port <- psi %>% 
  #mutate(idioma = textcat(psi$Text)) %>% 
  #filter(idioma=="portuguese")


## Retirar a postagem com mais comentários em janeiro, pois é um outlier. Em novembro não houve outliers.

psi.2<- psi %>% 
  filter(Comments<3700)

# Selecionar a coluna Text e converter em corpus com funcao do Quanteda 

psi.corpus <- corpus(psi.2$Text) # dataset janeiro. As alteracoes para cada mes de analise sao individuais. 

psi.corpus <- corpus (psi$Text)

summary(psi.corpus)


#  Transformar em um document-feature matrix

psi.dfm <- tokens(psi.corpus, remove_punct = TRUE) %>%
  dfm()

head(psi.dfm)

class(psi.dfm)

# Extrair as hashtags relacionadas 

tag_dfm <- dfm_select(psi.dfm, pattern = "#*")

toptag <- names(topfeatures(tag_dfm, 30))

head(toptag, 20)

toptag

#library(quanteda.textplots) # para as redes de usuarios e hashtags

tag_fcm <- fcm(tag_dfm)
head(tag_fcm)


topgat_fcm <- fcm_select(tag_fcm, pattern = toptag)
textplot_network(topgat_fcm, min_freq = 0.1, edge_alpha = 0.5, edge_size = 3)


# Rede de coocorrência de usuarios mencionados

user_dfm <- dfm_select(psi.dfm, pattern = "@*")

# Seleciona-se os 100 mais 
topuser <- names(topfeatures(user_dfm, 100))
head(topuser)

user_fcm <- fcm(user_dfm) # cria-se objeto fcm para o grafico 
head(user_fcm, 20)

user_fcm <- fcm_select(user_fcm, pattern = topuser)
user_fcm
textplot_network(user_fcm, min_freq = 1, edge_color = "blue", edge_alpha = 0.6, edge_size = 3)


############################ Verificar frequencias de palavras ############################

# Criar objeto dfm (usado no pacote Quanteda), remover stopwords e aplicar stemming

psi.dfm <- dfm(psi.corpus,
                            remove = stopwords(language = "portuguese", source = "nltk"),
                            remove_punct = TRUE, remove_numbers = T, remove_symbols = T, stem = TRUE)

psi.dfm2 <- dfm_remove(psi.dfm, pattern = stopwords("en", source = "nltk"))

psi.dfm3 <-dfm_remove(psi.dfm2, pattern = stopwords("spanish", source = "nltk"))
                      
topfeatures(psi.dfm3, 20) # 20 palavras mais frequentes

textstat_frequency(psi.dfm3, n = 20) %>% 
  ggplot(aes(x = reorder(feature, -rank), y = frequency)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip()+ 
  labs(x = "", y = "")+ 
  theme_bw()


########## Criar dataframe para frequencia de palavras ######

## Similar ao grafico acima, porem com melhor visualizacao

# Soma as colunas com contagem de palavras
freqs <- colSums(psi.dfm3)
# Pega um vetor com vocabulario
words <- colnames(psi.dfm3)
# Combina as palavras e suas frequencias em um dataframe 
wordlist <- data.frame(words, freqs)
# Reordena a lista de palabras em ordem decrescente
wordIndexes <- order(wordlist[, "freqs"], decreasing = TRUE)
wordlist <- wordlist[wordIndexes, ]
# Mostra as palavras mais frequentes
head(wordlist, 25)


#library("quanteda.textplots")

# Nuvem de palavra para auxiliar a analise. Nao inserido na dissertacao

textplot_wordcloud(psi.dfm3, min_freq = 2000, random_order = FALSE,
                   rotation = .25,
                   colors = RColorBrewer::brewer.pal(15, "Dark2"))


############ Verificar e Extrair no corpus textual o contexto das palavras e/ou usuarios  ##################

#### Funcao do pacote Quanteda

k <- kwic(tokens(psi.corpus), '#amorproprio', window = 3)

head(k, 6)

kwic(tokens(psi.corpus), '@desiderata_ilustrata', window = 7)


# Imprimir uma sentenca de texto mapeada pela funcao Kwic
cat(as.character(psi.corpus[301]))


##################### Clusterizacao de texto com LDA do Topic Models ##########


psi_model <- convert(psi.dfm3, to = "topicmodels") %>% 
  LDA(k = 3)   # 3 topicos 

########## extrai os termos por topico em uma lista 

termos_psi_model <- get_terms(psi_model)

terms(psi_model, 10)
class(psi_model)

get_terms(psi_model, 15)

library(tidytext)

topics <- tidy(psi_model, matrix = "beta")
topics

top_termos <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 15) %>% 
  ungroup() %>%
  arrange(topic, -beta)

top_termos %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = TRUE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()+
  scale_fill_discrete(name = "", labels = c("1.Profissional", "2.Bem-estar", "3.Generalista"))+
  labs(x ="", y="")



