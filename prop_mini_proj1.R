
# Diretório ---------------------------------------------------------------

getwd()
setwd('~/Documents/portfolio/analise_socioeconomica_R')

# Dados -------------------------------------------------------------------

df <- read.csv('dataset.csv')
View(df)


# Packages -------------------------------------------------------------
# install.packages('naniar')
# install.packages('simputation')
# install.packages('skimr')
# install.packages('tidyverse')
# install.packages('visdat')

library(dplyr)
require(ggplot2)
library(magrittr)
require(naniar)
library(simputation)
library(visdat) # Graf. visual.

# Looking data --------------------------------------------------------------

sum(is.na(df)) # qtde de Na
str(df)
glimpse(df)
summary(df)
dim(df)
skimr::skim(df)
complete <- sum(complete.cases(df)) # lines without NaN
not_complete <- sum(!complete.cases(df)) # lines with NaN
# Percent data with NaN
perc <- (not_complete / complete) * 100
perc
rm(complete, not_complete)


# Columns names -----------------------------------------------------------
colnames(df)
myColumns <- colnames(df)

myColumns[1] <- "Estado"
myColumns[2] <- "Ano"
myColumns[3] <- "Indice_Nivel_Vida"
myColumns[4] <- "PIB_Per_Capita"
myColumns[5] <- "Suporte_Social"
myColumns[6] <- "Expectativa_Vida"
myColumns[7] <- "Indice_Liberdade"
myColumns[8] <- "Indice_Generosidade"
myColumns[9] <- "Indice_Corrupcao"
myColumns[10] <- "Indice_Emocoes_Pos"
myColumns[11] <- "Indice_Emocoes_Neg"

colnames(df) <- myColumns
colnames(df)

rm(myColumns)


# visdat ------------------------------------------------------------------
# https://cran.r-project.org/web/packages/visdat/vignettes/using_visdat.html

data_vis_dat(df)
vis_dat(df) # graphic
vis_miss(df) # Graphic Missing %
vis_dat(df, sort_type = FALSE)
vis_miss(df, sort_miss = TRUE) # Missing sort desc

vis_miss(df, cluster = TRUE, sort_miss = TRUE)

# Correlation
#### !Estado because not numeric
df %>% 
  select_if(is.numeric) %>% 
  vis_cor(cor_method = 'pearson') # “pearson” (default), “kendall”, or “spearman”

df %>% 
  select_if(is.numeric) %>% 
  vis_value() # range of values

vis_guess(df) # mostra tipo de dado


# Columns Types -----------------------------------------------------------

sum(is.na(df$Ano))
sum(is.na(df$Estado))
df$Ano <- factor(df$Ano)
df$Estado <- factor(df$Estado)

# Missing relationships ---------------------------------------------------

ggplot(df,
       aes(x= Indice_Corrupcao,
           y= Indice_Generosidade)) +
  geom_point()


# Naniar 
ggplot(df,
       aes(x= Indice_Corrupcao,
           y= Indice_Generosidade)) +
  geom_miss_point() # naniar


# Naniar ------------------------------------------------------------------
# https://cran.r-project.org/web/packages/naniar/vignettes/getting-started-w-naniar.html

# Show for year
# gg_miss_var(df, facet = Ano, show_pct = FALSE) +
#   labs(y = 'NAN Values')
gg_miss_var(df, show_pct = FALSE) +
  labs(y = 'NAN Values')



# Cria uma sobra dos dados com referências NA e !NA
as_shadow(df)
bind_shadow(df) # attach shadow to dataframe


df_shadow <- bind_shadow(df) # nabular(df) == bind_shadow()

glimpse(df_shadow)


# Impute values ------------------------------------------------------------

df_shadow %>% 
  as.data.frame() %>% 
  impute_rlm(Indice_Corrupcao ~ -Ano) %>% 
  ggplot(aes(x = Indice_Corrupcao,
             colour = Indice_Corrupcao_NA)) +
  geom_histogram()


df_test <- df_shadow %>% 
  impute_rlm(Suporte_Social ~ -Indice_Generosidade)

sum(is.na(df_test$Suporte_Social))

df_test <- df_test %>% 
  impute_rlm(Indice_Emocoes_Neg ~ -Indice_Generosidade)

df_test <- df_test %>% 
  impute_rlm(Indice_Emocoes_Pos ~ Indice_Liberdade
             + Indice_Nivel_Vida)

df_test <- df_test %>% 
  impute_rlm(Indice_Liberdade ~ - Indice_Generosidade
             - Indice_Emocoes_Neg)

df_test <- df_test %>% 
  impute_rlm(PIB_Per_Capita ~ Expectativa_Vida
             + Indice_Corrupcao
             + Indice_Nivel_Vida
             + Suporte_Social)

df_test <- df_test %>% 
  impute_rlm(Expectativa_Vida ~ PIB_Per_Capita
             + Indice_Corrupcao
             + Indice_Nivel_Vida
             + Suporte_Social)

df_test <- df_test %>% 
  impute_rlm(Indice_Generosidade ~ Indice_Corrupcao
             + Indice_Emocoes_Pos
             + Indice_Liberdade)

df_test <- df_test %>% 
  impute_rlm(Indice_Corrupcao ~ - Suporte_Social)

sum(is.na(df_test))

gg_miss_var(df_test, show_pct = FALSE) +
  labs(y = 'NAN Values DF_test')

vis_miss(df_test)
vis_dat(df_test)
vis_guess(df_test)

perc_2 <- (sum(!complete.cases(df_test)) / sum(complete.cases(df_test)) * 100)
perc_2

# NA omit -----------------------------------------------------------------
dados <- na.omit(df_test)
sum(is.na(dados))

dados <- dados %>% 
  select(1:11)

# Remove data -------------------------------------------------------------
rm(df_shadow, df_test)

View(dados)

######## Parte 2 ########

# Respostas ---------------------------------------------------------------

# Pergunta 1
# O aumento do PIB per capita de um país afeta positivamente a expectativa de vida dos cidadãos ao nascer?
# Qual a correlação entre essas duas variáveis?

dados %>% 
  select_if(is.numeric) %>% 
  vis_cor()

# Sim, tem correlação positiva forte

# Pergunta 2
# Existe uma correlação entre a escala de vida e a conscientização do público em geral sobre a corrupção 
# nos negócios e no governo? 
# Qual a correlação entre essas duas variáveis?

# Sim tem correlação, é negativa de média para fraca.

# Pergunta 3
# O aumento na escala de vida tem algum efeito na média de felicidade entre o público em geral?
# Qual a correlação entre essas duas variáveis?

# Sim, relação positiva com intensidade média

# Pergunta 4
# O país com o menor índice de suporte social tem maior percepção de corrupção em relação 
# às empresas e ao governo no país?

tab_Social <- dados %>% 
  group_by(Estado) %>% 
  summarise(Mean_Sup_Social = mean(Suporte_Social))

dados_soc_mean <- left_join(dados, tab_Social, by = 'Estado')
rm(tab_Social)

dados_soc_mean %>% 
  select(Mean_Sup_Social) %>% 
  min()

dados_soc_mean[dados_soc_mean$Suporte_Social == min(dados_soc_mean$Suporte_Social),]
df_prop <- dados_soc_mean[dados_soc_mean$Estado == 'Central African Republic',]
View(df_prop)
mean(df_prop$Suporte_Social)
mean(df_prop$Indice_Corrupcao)
max(dados_soc_mean$Suporte_Social)
max(dados_soc_mean$Indice_Corrupcao)

cor.test(df_prop$Suporte_Social, df_prop$Indice_Corrupcao, method = "pearson")

df_prop %>% 
  select(Suporte_Social, Indice_Corrupcao, Indice_Liberdade, Indice_Generosidade, PIB_Per_Capita) %>% 
  vis_cor(cor_method = 'pearson')

# País com menor índice de suporte social tem maior percepeção de corrupção.


# Pergunta 5
# Pessoas generosas são mais felizes?

dados %>% 
  select(Indice_Generosidade, Indice_Emocoes_Pos) %>% 
  vis_cor()

# Há uma correlação positiva média com relação a generosidade e Felicidade.


dados %>% 
  group_by('Estado') %>% 
  arrange(desc(Indice_Corrupcao))

