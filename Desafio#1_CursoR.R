# Curso R -------------------------------------------------------------------------------------------------------------------------------
# Manipulando a base de filmes do IMDB --------------------------------------------------------------------------------------------------
# Desafio #1 ----------------------------------------------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ------------------------------------------------------------------------------------------------------
# Data: 16/04/2022 ----------------------------------------------------------------------------------------------------------------------

# Pacote necessário ------------------------------------------------------------------------------------------------------------------------

library(tidyverse)

# Desafio:  --------------------------------------------------------------------------------------------------------------------------------

## Escreva um código que transforme a base do IMDB em uma base nomeada IMDB_atores, que deve ser 
## formada pelas seguintes colunas:

#### ator: nome de um(a) ator/atriz (cada nome deve aparecer uma vez só)

base <- readRDS("imdb.rds")
View(base)

base %>% 
  select(starts_with("ator")) %>% 
  head(3)

base %>% 
  pivot_longer(
    cols = starts_with("ator"), 
    names_to = "protagonismo",
    values_to = "ator_atriz"
  ) %>% 
  select(ator_atriz) %>%
  arrange(ator_atriz)

#### nota_media_imdb: nota imdb média dos filmes que o(a) ator/atriz participou

base %>% 
  pivot_longer(
    cols = starts_with("ator"), 
    names_to = "protagonismo",
    values_to = "ator_atriz"
  ) %>% 
  select(ator_atriz, nota_imdb) %>%
  group_by(ator_atriz) %>%
  summarise(nota_media_imdb = mean(nota_imdb, na.rm = TRUE)) %>%
  arrange(ator_atriz, nota_media_imdb)


#### media_lucro: lucro médio dos filmes que o(a) ator/atriz participou

base %>% 
  pivot_longer(
    cols = starts_with("ator"), 
    names_to = "protagonismo",
    values_to = "ator_atriz"
  ) %>% 
  select(ator_atriz, receita, orcamento) %>%
  mutate(lucro = receita - orcamento) %>%
  group_by(ator_atriz) %>%
  summarise(media_lucro = mean(lucro, na.rm = TRUE)) %>%
  arrange(ator_atriz, media_lucro)

#### top1_genero: gênero mais frequente entre os filmes que o(a) ator/atriz participou

generos <- base %>% 
  pivot_longer(
    cols = starts_with("ator"), 
    names_to = "protagonismo",
    values_to = "ator_atriz"
  ) %>% 
  select(ator_atriz, generos) %>%
    separate( 
    col = generos,
    into = c("genero1", "genero2", "genero3"), 
    sep = "\\|") 
view(generos)
tibble(generos)

generos$ator_atriz <- as.factor(generos$ator_atriz)
generos$genero1 <- as.factor(generos$genero1)
generos$genero2 <- as.factor(generos$genero2)
generos$genero3 <- as.factor(generos$genero3)

levels(generos$genero1)
levels(generos$genero2)
levels(generos$genero3)

top1_genero <- generos %>%
  select(ator_atriz, genero1) %>%
  group_by(ator_atriz, genero1) %>%
  summarise(Frequencia1 = n())
view(top1_genero)

#### top2_genero: segundo gênero mais frequente entre os filmes que o(a) ator/atriz participou

top2_genero <- generos %>%
  select(ator_atriz, genero2) %>%
  group_by(ator_atriz, genero2) %>%
  summarise(Frequencia2 = n())
view(top2_genero)

#### top3_genero: terceiro gênero mais frequente entre os filmes que o(a) ator/atriz particiou

top3_genero <- generos %>%
  select(ator_atriz, genero3) %>%
  group_by(ator_atriz, genero3) %>%
  summarise(Frequencia3 = n())
view(top3_genero)

#### primeiro_registro: ano do primeiro filme que o(a) ator/atriz participou

prim_reg <- base %>% 
  pivot_longer(
    cols = starts_with("ator"), 
    names_to = "protagonismo",
    values_to = "ator_atriz"
  ) %>% 
  select(ator_atriz, ano, titulo) %>%
  arrange(ator_atriz, ano, titulo)
view(prim_reg)

#### ultimo_registro: ano do último filme que o(a) ator/atriz particiou

ult_reg <- base %>% 
  pivot_longer(
    cols = starts_with("ator"), 
    names_to = "protagonismo",
    values_to = "ator_atriz"
  ) %>% 
  select(ator_atriz, ano, titulo) %>%
  arrange(ator_atriz, ano, titulo)
view(ult_reg)

#### filmes: um data frame com informações de todos os filmes que o(a) ator/atriz participou, 
#### as colunas desse data frame devem ser as seguintes:

# - título: nome do filme
# - ano: ano do filme
# - diretor: diretor(a) do filme
# - duracao: duração do filme
# - cor: cor do filme (Color ou Black and White)
# - generos: generos do filme
# - pais: país do filme
# - classificacao: classificação etária do filme
# - nota_imdb: nota imbd do filme
# - orcamento: orcamento do filme
# - receita: receita do filme
# - lucro: lucro do filme

filmes <- base %>% 
  pivot_longer(
    cols = starts_with("ator"), 
    names_to = "protagonismo",
    values_to = "ator_atriz") %>% 
  select(ator_atriz, titulo, ano, diretor, duracao, cor, generos, pais, 
         classificacao, nota_imdb, orcamento, receita) %>%
  mutate(lucro = receita - orcamento) %>%
  arrange(ator_atriz, titulo, ano, diretor, duracao, cor, generos, pais, 
         classificacao, nota_imdb, orcamento, receita, lucro)
view(filmes)

#### contracenou: um vetor com os nomes de todos os(as) atores/atrizes que o(a) ator/atriz já 
#### contracenou (ou seja, participou do mesmo filme)