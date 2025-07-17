library(tidyverse)
library(readxl)
library(magrittr)
library(showtext)

font_add_google(name = 'Montserrat', family = 'Montserrat')
showtext_auto()

setwd('d:\\Giovanni\\Downloads\\posição relativa Brasil')

dados <- read_excel('GM-GDP per capita - Dataset - v27.xlsx', sheet = 'data-GDP-per-capita-in-columns')
pop <- read_excel('population.xls')

dados <- dados[-c(198:204), -c(1)]

# Manipulando ----

# Vendo quais nomes não batem
teste1 <- inner_join(pop, dados, by = c('Country Name' = 'Country Name')) %>%
  mutate(dummy = 1)

teste2 <- left_join(pop, dados, by = c('Country Name' = 'Country Name'))

teste3 <- left_join(teste2, teste1, by = c('Country Name' = 'Country Name')) %>%
  filter(is.na(dummy))

# Arrumando os nomes
dados$`Country Name` <- gsub("Russia", "Russian Federation", dados$`Country Name`)
dados$`Country Name` <- gsub("Egypt", "Egypt, Arab Rep.", dados$`Country Name`)
dados$`Country Name` <- gsub("Turkey", "Turkiye", dados$`Country Name`)
dados$`Country Name` <- gsub("Iran", "Iran, Islamic Rep.", dados$`Country Name`)
dados$`Country Name` <- gsub("South Korea", "Korea, Rep.", dados$`Country Name`)
dados$`Country Name` <- gsub("Yemen", "Yemen, Rep.", dados$`Country Name`)
dados$`Country Name` <- gsub("Venezuela", "Venezuela, RB", dados$`Country Name`)
dados$`Country Name` <- gsub("North Korea", "Korea, Dem. People's Rep.", dados$`Country Name`)
dados$`Country Name` <- gsub("Syria", "Syrian Arab Republic", dados$`Country Name`)
dados$`Country Name` <- gsub("Hong Kong, China", "Hong Kong SAR, China", dados$`Country Name`)
dados$`Country Name` <- gsub("Lao", "Lao PDR", dados$`Country Name`)
dados$`Country Name` <- gsub("Gambia", "Gambia, The", dados$`Country Name`)
dados$`Country Name` <- gsub("Macedonia, FYR", "North Macedonia", dados$`Country Name`)

# Mergeando população com PIB per capita
base <- inner_join(pop, dados, by = c('Country Name' = 'Country Name'))

# Gráfico 1 (posição relativa do Brasil) ----
lista <- list()

for(i in as.character(1800:2021)){
  lista[[paste0(i)]] <- base %>%
    select(`Country Name`, `i`) %>%
    mutate(ano = i) %>%
    rename(pib = `i`) %>%
    arrange(-pib) %>%
    mutate(posição = c(1:155)) %>%
    filter(`Country Name` == 'Brazil') %>%
    select(posição, ano, `Country Name`)
}

pa <- bind_rows(lista)

# Colocando a posição em termos relativos
pa %<>% mutate(percentil = 100 - (posição * 100 / 155))

# Pegando de 5 em 5 anos (melhora a visualização no gráfico)
p1 <- function(x){
  if(x%%5 == 0){
    return (1)
  } else {
    return(0)
  }
}

p2 <- Vectorize(p1)
p <- p2(1800:2021)

pa2 <- bind_cols(pa, p) %>%
  filter(`...5` == 1) %>%
  select(-`...5`)

# Gráfico
ggplot(pa2, aes(x = ano, y = percentil)) +
  geom_line(aes(group = `Country Name`)) +
  geom_point(size = 2) +
  labs(x = "", y = "Percentil") +
  scale_x_discrete(breaks = seq(1800, 2021, 20)) +
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100), limits = c(0,100)) +
  theme(text = element_text(family = 'Montserrat'),
        axis.title = element_text(size = 70),
        axis.text = element_text(size = 60))

ggsave(plot = last_plot(), file = "posição_Brazil.png", width = 20, height = 10, bg = 'white')

# Gráfico 2 (crescimento do Brasil) ----
dados <- read_excel('dados.xlsx') %>%
  mutate(dummy = 1)

# Calculando a média móvel
dados <- dados[-c(1),]

x <- c(0, cumsum(dados$perc))
média <- (x[(5 + 1):length(x)] - x[1:(length(x) - 5)]) * 100 / 5

dados <- dados[-c(1:4),]

base <- bind_cols(dados, média) %>%
  rename(perc_movel = `...5`)

# Gráfico
ggplot(base, aes(x = ano, y = perc_movel)) +
  geom_line(aes(group = dummy), size = 1, color = '#F8766D') +
  labs(x = "", y = "%") +
  geom_hline(yintercept = 0, size = .4, linetype = 1, colour = "black") +
  scale_x_continuous(breaks = seq(1850, 2021, 10)) +
  scale_y_continuous(breaks = c(-6,-4,-2,0,2,4,6,8,10)) +
  theme(text = element_text(family = 'Montserrat'),
        axis.title = element_text(size = 90),
        axis.text = element_text(size = 60))

ggsave(plot = last_plot(), file = "crescimento_Brazil.png", width = 20, height = 10, bg = 'white')
