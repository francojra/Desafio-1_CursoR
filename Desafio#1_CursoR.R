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
    sep = "\\|"
  ) 
view(generos)

ggplo