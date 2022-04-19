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
  head(6)

#### nota_media_imdb: nota imdb média dos filmes que o(a) ator/atriz participou

base %>% 
  pivot_longer(
    cols = starts_with("ator"), 
    names_to = "protagonismo",
    values_to = "ator_atriz"
  ) %>% 
  select(ator_atriz, nota_imdb) %>%
  group_by(ator_atriz) %>%
  summarise(nota_media_imdb = mean(nota_imdb, na.rm = TRUE))


#### media_lucro: lucro médio dos filmes que o(a) ator/atriz participou

base %>% 
  pivot_longer(
    cols = starts_with("ator"), 
    names_to = "protagonismo",
    values_to = "ator_atriz"
  ) %>% 
  select(ator_atriz, nota_imdb, receita, orcamento) %>%
  mutate(lucro = receita - orcamento) %>%
  group_by(ator_atriz) %>%
  summarise(media_lucro = mean(lucro, na.rm = TRUE),
            nota_media_imdb = mean(nota_imdb, na.rm = TRUE))

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
