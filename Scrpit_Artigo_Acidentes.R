gc()

###CArregando as Bibliotecas
x <- c("tidyverse","janitor", "writexl", "stringr",
       "aweek", "zoo", "rio", "lubridate", "slider",
       "tidyquant", "tsibble", "forcats", "gtsummary",
       "foreign", "DescTools", "data.table","tidymodels",
       "poissonreg", "readxl")


lapply(x, require, character.only = TRUE)


####Lendo os Bancos
load("D:/Episus Avançado/Saúde do Trabalhador/Boletim Epidemiológico/Banco Óbitos/Bancos AT (Sinan) e SIM/Banco_Base_Acidentes.RData")

banco_es <- read_excel("D:/Episus Avançado/Saúde do Trabalhador/Boletim Epidemiológico/banco_acidentes_es.xlsx")

acgran <- acgran %>% 
  clean_names()

banco_es <- banco_es %>% 
  clean_names()

acgran2 <- rbind(acgran, banco_es)

#####Analise do banco de dados
acgran2 %>% tabyl(nu_ano)

df <- acgran2 %>% drop_na(nu_ano)

skimr::skim(df)

rm(acgran, acgran2, banco_es)

df %>% tabyl(nu_ano) #%>% write_clip()

######Calculo da completude

df <- df %>% 
  mutate(
    across(where(is.factor), fct_explicit_na, "Ignorado"))

df %>% tabyl(cs_sexo) %>% adorn_totals()

#####Selecionando as variáveis de interesse

var_obr <- df %>% select(sg_uf_not, dt_nasc, nu_idade_n, cs_sexo, cs_gestant, 
                         id_ocupa_n, local_acid, cid_acid, part_corp1, cid_lesao, 
                         cat, cnae)

var_essen <- df %>% select(cs_raca, cs_escol_n, sit_trab, uf_emp, mun_emp,
                           dis_emp, nobaiemp, end_emp, hora_acid, hora_acid,
                           min_acid, hora_jor, min_jor, uf_acid, mun_acid, tipo_acid,
                           mais_trab, atende_med, dt_atende, uf_atende, mun_atende,
                           uni_atende, part_corp2, part_corp3, regime, evolucao, dt_obito)

skimr::skim(var_obr)

skimr::skim(var_essen)

df %>% tabyl(cs_gestant)

#####Verificando as inconsistências entre as datas
df <- df %>% separate(nu_idade_n, c("tp_idade", "nu_idade_n"), 1, convert = TRUE)

df <- df %>% mutate(idade_dias = case_when(
  tp_idade == 1 ~ nu_idade_n/24,
  tp_idade == 2 ~ nu_idade_n*1,
  tp_idade == 3 ~ nu_idade_n*30,
  tp_idade == 4 ~ nu_idade_n*365,
))

summary(df$idade_dias)

df <- df %>% mutate(idade_anos = idade_dias/365.25)

summary(df$idade_anos)

df$idade_anos <- as.integer(df$idade_anos)

df$faixa <- cut(x = df$idade_anos, breaks = c(0,10,20,30,40,50,60,70,80,1000),
                labels=c('0-9 anos', '10-19 anos', '20 a 29 anos',
                         '30 a 39 anos', '40 a 49 anos', '50 a 59 anos', '60 a 69 anos',
                         '70 a 79 anos', '80 anos e mais'), ordered_result = FALSE, right = FALSE)

df %>% tabyl(faixa)

df_inconsitencias_1 <- df %>% filter(df$dt_notific - df$dt_nasc < 1825)

df %>% filter(df$dt_obito - df$dt_notific < 0)

df_a %>% tabyl(cs_gestant, cs_sexo) %>% adorn_totals()

library(clipr)

df_a %>% tabyl(sit_trab, cat) %>% adorn_totals() #%>% write_clip()

df_a %>% select(sit_trab, cat) %>% tbl_summary(by=cat) %>% as_flex_table() %>%
  flextable::save_as_docx(path = "D:/t3.docx")

df %>% filter(evolucao %in% c(1, 2, 3, 4, 7, 9)) %>%  tabyl(dt_obito, evolucao)

df <- df %>% mutate(oportunidade_1 = dt_notific - dt_acid)

df %>% mutate(oportunidade_1 = dt_notific - dt_acid) %>% 
  group_by(nu_ano) %>% 
  summarize(mean(oportunidade_1))

df_a$oportunidade_1 <-as.numeric(df_a$oportunidade_1)

summary(df$oportunidade_1)

n_inorportunas <- df_a %>% filter(oportunidade_1 > 1)

df$sg_uf_nome = fct_collapse(
  df$sg_uf,
  "Rondônia" = "11", "Acre" = "12", "Amazonas" = "13",
  "Roraima" = "14",  "Pará" = "15", "Amapá" = "16",
  "Tocantins" = "17", "Maranhão" = "21", "Piauí" = "22",
  "Ceará" = "23", "Rio Grande do Norte" = "24", "Paraíba" = "25",
  "Pernambuco" = "26", "Alagoas"= "27", "Sergipe" = "28", "Bahia" = "29",
  "Minas Gerais" = "31", "Espírito Santo" = "32", "Rio de Janeiro" = "33",
  "São Paulo" = "35", "Paraná" = "41", "Santa Catarina" = "42", "Rio Grande do Sul" = "43",
  "Mato Grosso do Sul" = "50", "Mato Grosso" = "51", "Goiás" = "52", "Distrito Federal" = "53",
  Ignorado = "Ignorado")

regiao_norte <- c("11", "12", "13", "14", "15", "16", "17")
regiao_nordeste <- c("21", "22", "23", "24", "25", "26", "27", "28", "29")
regiao_sudeste <- c("31", "32", "33", "35")
regiao_centro_oeste <- c("50", "51", "52", "53")
regiao_sul <- c("43", "41", "42")

df <- df %>% 
  mutate(
    regiao_uf = case_when(
      sg_uf %in% regiao_norte ~ "Região Norte",
      sg_uf %in% regiao_nordeste ~ "Região Nordeste",
      sg_uf %in% regiao_sudeste ~ "Região Sudeste",
      sg_uf %in% regiao_centro_oeste ~ "Região Centro Oeste",
      sg_uf %in% regiao_sul ~ "Região Sul",
    ))

df %>% tabyl(nu_ano, regiao_uf) #%>% write_clip()

df %>% tabyl(nu_ano, sg_uf_nome) #%>% write_clip()

df %>% drop_na(sg_uf_nome) %>% group_by(sg_uf_nome) #%>% summarize(n = mean(as.numeric(oportunidade_1))) #%>% write_clip()

df_a %>% drop_na(sg_uf_nome) %>% 
  group_by(sg_uf_nome) %>% 
  summarize(n = mean(as.numeric(oportunidade_1))) %>% 
  mutate(sg_uf_nome = fct_reorder(sg_uf_nome, n)) %>%
  ggplot(aes(x = sg_uf_nome, y = n)) +
  geom_bar(stat="identity", fill="steelblue") +
  coord_flip() +
  geom_text(stat= "identity", aes(label = paste(round(..y..))), hjust=-0.3) +
  labs(x = "Esatdo Notificação", y = "Média de Dias Oportunidade de Notificação") +
  theme_minimal()

df %>% tabyl(sg_uf_nome, nu_ano) %>% write_clip()

df %>% tabyl(id_mn_resi, nu_ano) %>% write_clip()

n_inorportunas %>% tabyl(sg_uf_nome, nu_ano) %>% adorn_totals()



df_a %>% filter(oportunidade_1 %in% (0:1)) %>% 
  ggplot(aes(x = sg_uf_nome, y=oportunidade_1)) +
  geom_boxplot(fill = "gray")+
  theme_bw()


tabela_desc <- df %>% drop_na(sg_uf_nome) %>%
  tabyl(nu_ano, sg_uf_nome)

skimr::skim(tabela_desc)

summary(tabela_desc)

Desc(tabela_desc)

df_a %>% drop_na(sg_uf_nome) %>% 
  count(nu_ano, sg_uf_nome) %>%
  mutate(sg_uf_nome = fct_reorder(sg_uf_nome, n)) %>%
  ggplot(aes(x = sg_uf_nome, y=n)) +
  geom_boxplot(fill = "steelblue") +
  coord_flip() +
  theme_bw() +
  labs(x = "Estado Notificação", y = "Número de Notificações por ano")

df %>% tabyl(sg_uf) %>% arrange(desc(n))

options(scipen = 100, digits = 4)

df %>% group_by(sg_uf) %>% 
  summarise_all(~sum(is.na(.))/length(.)) %>% 
  mutate(soma = rowSums(.)) #%>% write_clip()

df %>% group_by(nu_ano) %>% 
  summarise_all(~mean(is.na(.))) %>% 
  mutate(soma = rowSums())

             
#######Analise de completude (Artigo - Qualidade do dado)

df %>% tabyl(cnae) %>% arrange(n)

#### Analise Sensibilidade
df %>% tabyl(sit_trab)

df$sit_trab <- replace_na(df$sit_trab, "Ignorado")

df$sit_trab = fct_collapse(
  df$sit_trab,
  "Empregado registrado" = "01",
  "Empregado registrado" = "1",
  "Empregado não registrado" = "02",
  "Autônomo" = "03",
  "Servidor publico estatutário" = "04",
  "Servidor público celetista" = "05",
  "Aposentado" = "06",
  "Desempregado" = "07",
  "Trabalho temporário" = "08",
  "Cooperativado" = "09",
  "Trablhador avulso" = "10",
  "Empregador" = "11",
  "Outros" = "12",
  Ignorado = "99",
  Ignorado = "Ignorado")

df = df %>% mutate(clt = case_when(
  sit_trab == "Empregado registrado" ~ "Sim",
  sit_trab == "Trabalho temporário" ~ "Sim",
  sit_trab == "Servidor público celetista" ~ "Sim",
  sit_trab == "Trablhador avulso" ~ "Sim",
  sit_trab == "Empregado não registrado" ~ "Não",
  sit_trab == "Autônomo" ~ "Não",
  sit_trab == "Servidor publico estatutário" ~ "Não",
  sit_trab == "Aposentado" ~ "Não",
  sit_trab == "Desempregado" ~ "Não",
  sit_trab == "Cooperativado" ~ "Não",
  sit_trab == "Empregador" ~ "Não",
  sit_trab == "Outros" ~ "Não",
  sit_trab == "Ignorado" ~ "Ignorado")
)

df %>% filter(clt == "Sim") %>% tabyl(nu_ano) #%>% write_clip()

####3Correções apresentação


scale_y_continuous(labels = scales::number_format(scale = 100, accuracy = 1))

df_a %>%
  mutate(oportunidade_1 = dt_notific - dt_acid) %>%
  group_by(sg_uf_nome, nu_ano) %>%
  summarize(n = mean(oportunidade_1)) %>%
  filter(sg_uf_nome != "Ignorado") %>%
  ggplot(aes(x = reorder(sg_uf_nome, n), y = n)) +
  geom_boxplot(fill = "steelblue") +
  scale_y_continuous(
    limits = c(0, 250),
    breaks = seq(0, 250, by = 50)) +
  theme_bw() +
  labs(x = "Estado Notificação", y = "Oportunidade (dias)")
  
df$sg_uf_nome = fct_collapse(
    df$sg_uf,
    "RO" = "11", "AC" = "12", "AM" = "13",
    "RR" = "14", "PA" = "15", "AP" = "16",
    "TO" = "17", "MA" = "21", "PI" = "22",
    "CE" = "23", "RN" = "24", "PB" = "25",
    "PE" = "26", "AL" = "27", "SE" = "28",
    "BA" = "29", "MG" = "31", "ES" = "32",
    "RJ" = "33", "SP" = "35", "PR" = "41",
    "SC" = "42", "RS" = "43", "MS" = "50",
    "MT" = "51", "GO" = "52", "DF" = "53",
    Ignorado = "Ignorado")

df <- df %>% 
  mutate(oportunidade_cat = case_when(
    oportunidade_1 <= 1  ~ "Oportuna",
    oportunidade_1 > 1 ~ "Não oportuna"
  ))

df %>% tabyl(oportunidade_cat)

df %>%
  group_by(nu_ano) %>%
  summarise(n = n(),
            n_oportunidade  = sum(oportunidade_cat == "Oportuna"),
            Proporção = (n_oportunidade/ sum(n = n()))*100) %>%
  write_clip()


teste <- df %>%
  group_by(nu_ano) %>% 
  select(where(is.factor)) %>% 
  summarise(total = n(),
            Validas = across(everything(), ~mean(. != "Ignorado")),
            Proporção = (Validas/ sum(n = n()))*100)


library(writexl)
library(clipr)

teste_df <- as.data.frame(t(teste))

write_xlsx(teste_df, "teste.xlsx")


colunas_texto <- df %>%
  select(where(is.factor))

df %>% group_by(nu_ano) %>%
  select(sg_uf_not, nu_idade_n, cs_sexo, cs_gestant, 
         id_ocupa_n, local_acid, cid_acid, part_corp1, cid_lesao, 
         cat, cnae) %>%
  summarise(across(everything(), ~mean(. != "Ignorado",  na.rm = TRUE)))


df_a %>% 
  group_by(nu_ano) %>%
  select(sg_uf_not, nu_idade_n, cs_sexo, cs_gestant, 
         id_ocupa_n, local_acid, cid_acid, part_corp1, cid_lesao, 
         cat, cnae) %>%
  summarise_all(~mean(.!= "Ignorado",  na.rm = TRUE))

df %>% 
  filter(nu_ano %in% c(2013, 2014)) %>% 
  group_by(sg_uf_nome) %>%
  select(cs_raca, cs_escol_n, sit_trab, uf_emp, mun_emp,
         dis_emp, nobaiemp, end_emp, hora_acid, hora_acid,
         min_acid, hora_jor, min_jor, uf_acid, mun_acid, tipo_acid,
         mais_trab, atende_med, uf_atende, mun_atende,
         uni_atende, part_corp2, part_corp3, regime, evolucao) %>%
  summarise_all(~mean(.!= "Ignorado",  na.rm = TRUE))
 
df %>%
  group_by(sg_uf_nome, nu_ano) %>%
  select(cs_raca, cs_escol_n, sit_trab, uf_emp, mun_emp,
         dis_emp, nobaiemp, end_emp, hora_acid, hora_acid,
         min_acid, hora_jor, min_jor, uf_acid, mun_acid, tipo_acid,
         mais_trab, atende_med, uf_atende, mun_atende,
         uni_atende, part_corp2, part_corp3, regime, evolucao) %>%
  summarise_all(~mean(.!= "Ignorado",  na.rm = TRUE))

df %>%
  select(cs_raca, cs_escol_n, sit_trab, uf_emp, mun_emp,
         dis_emp, nobaiemp, end_emp, hora_acid, hora_acid,
         min_acid, hora_jor, min_jor, uf_acid, mun_acid, tipo_acid,
         mais_trab, atende_med, uf_atende, mun_atende,
         uni_atende, part_corp2, part_corp3, regime, evolucao) %>%
  summarise_all(~mean(.!= "Ignorado",  na.rm = TRUE))


df %>%
  group_by(sg_uf_nome, nu_ano) %>%
  summarise(across(c(cs_raca, cs_escol_n, sit_trab, uf_emp, mun_emp,
                     dis_emp, nobaiemp, end_emp, hora_acid, hora_acid,
                     min_acid, hora_jor, min_jor, uf_acid, mun_acid, tipo_acid,
                     mais_trab, atende_med, uf_atende, mun_atende,
                     uni_atende, part_corp2, part_corp3, regime, evolucao), 
                   ~sum(. != "Ignorado", na.rm = TRUE))) %>%
  pivot_wider(names_from = nu_ano, values_from = c(cs_raca, cs_escol_n, sit_trab, uf_emp, mun_emp,
                                                   dis_emp, nobaiemp, end_emp, hora_acid, hora_acid,
                                                   min_acid, hora_jor, min_jor, uf_acid, mun_acid, tipo_acid,
                                                   mais_trab, atende_med, uf_atende, mun_atende,
                                                   uni_atende, part_corp2, part_corp3, regime, evolucao))


resultados <- as.data.frame(t(resultados))

resultados <- resultados %>% mutate(classificacao <- case_when(
  V1 < 0.5 ~ "Muito Ruim",
  V1 >= 0.5 & V1 < 0.7 ~ "Ruim",
  V1 >= 0.7 & V1 < 0.9 ~ "Regular",
  V1 >= 0.9 & V1 < 0.95 ~ "Bom",
  V1 >= 0.95 ~ "Excelente"
))

count(resultados)

####calculo da completude

df %>%
  group_by(sg_uf_nome, nu_ano) %>% 
  summarise(
    n = n(),
    Ignorado  = sum(cs_gestant == "Ignorado"),
    Proporção = (sum(cs_gestant == "Ignorado") / sum(n = n()))*100) %>%
  ggplot(aes(x = reorder(sg_uf_nome, Proporção), y = Proporção)) +
  geom_boxplot(fill = "steelblue") +
  scale_y_continuous(
    limits = c(0, 250),
    breaks = seq(0, 250, by = 50)) +
  theme_bw() +
  labs(x = "Estado Notificação", y = "Oportunidade (dias)")

df  %>%
  group_by(sg_uf_nome) %>% 
  summarise(
    n = n(),
    Ignorado  = sum(cs_gestant == "Ignorado"),
    Proporção = (sum(cs_gestant == "Ignorado") / sum(n = n()))*100) %>%
  mutate(sg_uf_nome = fct_reorder(sg_uf_nome, Proporção)) %>%
  ggplot(aes(x = sg_uf_nome, y = Proporção)) +
  geom_bar(stat="identity", fill="steelblue") +
  coord_flip() +
  geom_text(stat= "identity", aes(label = paste(round(..y..))), hjust=-0.3) +
  labs(x = "Esatdo Notificação", y = "Média de Dias Oportunidade de Notificação") +
  theme_minimal()



#####Ver a tendência da completude por estado (média das variáveis selecionadas)
completude <- df %>%
  group_by(sg_uf_nome) %>%
  select(id_ocupa_n, local_acid, 
         cat, cnae, cs_raca, cs_escol_n, sit_trab,
         uf_acid, mun_acid, tipo_acid,
         atende_med, evolucao) %>%
  summarise_all(~mean(.!= "Ignorado",  na.rm = TRUE))

completude$media <- rowMeans(completude[,2:13])

completude_3 <- completude[, c("sg_uf_nome", "media")]

completude_3 <- completude_3[1:27,]

library(clipr)

completude %>% write_clip()

library(dendextend)
library(cluster)

# Use map_dbl to run many models with varying value of k
sil_width <- map_dbl(2:10,  function(k){
  model <- pam(completude_3, k = k)
  model$silinfo$avg.width
})

# Generate a data frame containing both k and sil_width
sil_df <- data.frame(
  k = 2:10,
  sil_width = sil_width
)

# Plot the relationship between k and sil_width
ggplot(sil_df, aes(x = k, y = sil_width)) +
  geom_line() +
  scale_x_continuous(breaks = 2:10)

# Use map_dbl to run many models with varying value of k (centers)
tot_withinss <- map_dbl(1:10,  function(k){
  model <- kmeans(x = completude_3, centers = k)
  model$tot.withinss
})

# Generate a data frame containing both k and tot_withinss
elbow_df <- data.frame(
  k = 1:10,
  tot_withinss = tot_withinss
)

# Plot the elbow plot
ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() +
  scale_x_continuous(breaks = 1:10)

completude_pam <- pam(completude_3, k = 2)

sil_plot <- silhouette(completude_pam)

plot(sil_plot)

completude_3.matriz <- as.matrix(completude_3)

row.names(completude_3.matriz) <- completude_3$sg_uf_nome

dist_completude <- dist(completude_3.matriz, method = 'euclidean')

hc_completude <- hclust(dist_completude, method = 'complete')

cluster_assignments <- cutree(hc_completude, k = 2)

print(cluster_assignments)

completude_cluster <- mutate(completude_3, cluster = cluster_assignments)

plot(hc_completude, main = 'Dendograma', xlab = 'Unidades da Federação',
     ylab = "Distancia Euclideana")

completude_cluster %>% group_by(cluster) %>% 
  summarize(mean(media))

library(ggplot2)

ggplot(completude_cluster, aes(x= cluster, y = media)) +
  geom_point(size = 3) +
  theme_bw()


completude$media <- rowMeans(completude[,2:9])

df_pw <- Completude_log %>% pivot_wider(names_from = sg_uf_nome, values_from = media_log)

df_pw$time <- seq(1:15)

library(prais)

library(plm)

df_pw$nu_ano <- NULL

results <- list()

for(col in names(df_pw)[!names(df_pw) %in% "time"]) {
  formula <- as.formula(paste(col, "~ time"))
  results[[col]] <- prais_winsten(formula, data = df_pw, index = "time")
}

coefficients_and_se_list <- lapply(names(results), function(name) {
  result <- results[[name]]
  summary_result <- summary(result)
  data.frame(
    variable = name, 
    coef = coef(summary_result)[, "Estimate"], 
    std_error = coef(summary_result)[, "Std. Error"]
  )
})

coefficients_and_se_df <- do.call(rbind, coefficients_and_se_list)

coefficients_and_se_df <- coefficients_and_se_df %>%
  mutate(
    VPA = (-1 + 10^(coef)) * 100,
    IC95_sup = (-1 + 10^(coef + 1.96 * std_error)) * 100,
    IC95_inf = (-1 + 10^(coef - 1.96 * std_error)) * 100
  )

#####Ver a tendência da completude das variáveis por ano
df_comp <- df_e %>%
  group_by(nu_ano) %>%
  select(id_ocupa_n_2, local_acid, 
         cat, cnae, cs_raca, cs_escol_n, sit_trab,
         uf_acid, mun_acid, tipo_acid, evolucao) %>%
  summarise_all(~mean(.!= "Ignorado",  na.rm = TRUE))

df_comp$nu_ano <- NULL

df_comp <- df_comp %>%
  mutate_all(~ . * 100)

df_comp <- df_comp %>%
  mutate_all(~ log(.))

df_comp$time <- seq(1:16)

options(scipen = 9999)

library(prais)

results <- list()

for(col in names(df_comp)[!names(df_comp) %in% "time"]) {
  formula <- as.formula(paste(col, "~ time"))
  results[[col]] <- prais_winsten(formula, data = df_comp, index = "time")
}

coefficients_and_se_list <- lapply(names(results), function(name) {
  result <- results[[name]]
  summary_result <- summary(result)
  data.frame(
    variable = name, 
    coef = coef(summary_result)[, "Estimate"], 
    std_error = coef(summary_result)[, "Std. Error"],
    pvalue = coef(summary_result)[, "Pr(>|t|)"]
  )
})

coefficients_and_se_df <- do.call(rbind, coefficients_and_se_list)

coefficients_and_se_df <- coefficients_and_se_df %>%
  mutate(
    VPA = (-1 + 10^(coef)) * 100,
    IC95_sup = (-1 + 10^(coef + 1.96 * std_error)) * 100,
    IC95_inf = (-1 + 10^(coef - 1.96 * std_error)) * 100
  )

library(writexl)

write_xlsx(coefficients_and_se_df, "pw_variáveis.xlsx")

#####Corrigindo o Campo Ocupação

df_e$id_ocupa_n_2 = fct_collapse(
  df_e$id_ocupa_n,
  Ignorado = "9999",
  Ignorado = "9989",
  Ignorado = "Igno",
  Ignorado = "XXX")


######ver Completude por Região
df_comp <- df_e %>%
  group_by(nu_ano) %>%
  select(id_ocupa_n_2, local_acid, 
         cat, cnae, cs_raca, cs_escol_n, sit_trab,
         uf_acid, mun_acid, tipo_acid, evolucao, atende_med) %>%
  summarise_all(~mean(.!= "Ignorado",  na.rm = TRUE))
  
df_comp$media <- rowMeans(df_comp[,3:13])

df_comp <- df_comp[, c("regiao_uf", "nu_ano", "media")]

df_comp <- df_comp %>% pivot_wider(names_from = regiao_uf, values_from = media)

df_comp$Ignorado <- NULL
df_comp$nu_ano <- NULL

df_comp <- df_comp %>% clean_names()

df_comp <- df_comp %>%
  mutate_all(~ . * 100)

df_comp <- df_comp %>%
  mutate_all(~ log(.))

df_comp$time <- seq(1:16)

options(scipen = 9999)

library(prais)

results <- list()

for(col in names(df_comp)[!names(df_comp) %in% "time"]) {
  formula <- as.formula(paste(col, "~ time"))
  results[[col]] <- prais_winsten(formula, data = df_comp, index = "time")
}

# Criar um novo dataframe para os valores ajustados
df_pred = data.frame(time = df_comp$time)

# Calcular e salvar os valores preditos
for(col in names(df_comp)[!names(df_comp) %in% "time"]) {
  # Obter os valores ajustados
  fitted_values = fitted.values(results[[col]])
  
  # Retransformar os valores ajustados, se necessário
  predicted_values = exp(fitted_values)
  
  # Adicionar ao dataframe
  df_pred[[col]] = predicted_values
}



coefficients_and_se_list <- lapply(names(results), function(name) {
  result <- results[[name]]
  summary_result <- summary(result)
  data.frame(
    variable = name, 
    coef = coef(summary_result)[, "Estimate"], 
    std_error = coef(summary_result)[, "Std. Error"],
    pvalue = coef(summary_result)[, "Pr(>|t|)"]
  )
})

coefficients_and_se_df <- do.call(rbind, coefficients_and_se_list)

coefficients_and_se_df <- coefficients_and_se_df %>%
  mutate(
    VPA = (-1 + 10^(coef)) * 100,
    IC95_sup = (-1 + 10^(coef + 1.96 * std_error)) * 100,
    IC95_inf = (-1 + 10^(coef - 1.96 * std_error)) * 100
  )

library(writexl)

write_xlsx(coefficients_and_se_df, "pw_variáveis_regiao.xlsx")

id_ociupa <- df_e %>% tabyl(id_ocupa_n)

library(tidyr)

library(tidyr)

# Transformar df_comp para formato longo
df_comp_long <- gather(df_comp, key = "region", value = "value", -time)

# Transformar df_pred para formato longo
df_pred_long <- gather(df_pred, key = "region", value = "predicted_value", -time)

df_comp_long$type <- 'Original'
df_pred_long$type <- 'Predicted'

df_pred_long$value <- df_pred_long$predicted_value
df_pred_long$predicted_value <- NULL

df_combined <- rbind(df_comp_long, df_pred_long)

ggplot(data = df_combined, aes(x = time, y = value, color = region)) + 
  geom_point(data = subset(df_combined, type == 'Original')) +
  geom_line(data = subset(df_combined, type == 'Predicted')) +
  facet_wrap(~region) +
  labs(x = "ano", y = "Value", color = "Region") +
  theme_minimal()

####Prais_winsten para o Brasil
df_comp_original <- df_e %>%
  group_by(nu_ano) %>%
  select(id_ocupa_n_2, local_acid, 
         cat, cnae, cs_raca, cs_escol_n, sit_trab,
         uf_acid, mun_acid, tipo_acid, evolucao) %>%
  summarise_all(~mean(.!= "Ignorado",  na.rm = TRUE)) %>% write_clip()

df_comp$media <- rowMeans(df_comp[,2:12])

df_comp <- df_comp[, c("nu_ano", "media")]

df_comp$Casos_ano <- df_comp$media * 100

df_comp$Casos_ano_log <- log(df_comp$Casos_ano)

df_comp$time <- seq(1:16)

library(prais)

############################################################################
# Realizar a regressão de Prais-Winsten
pw_ano <- prais_winsten(Casos_ano_log ~ time, data = df_comp, index = "time")
summary(pw_ano)

df_comp$fitted_Cases_ano = exp(pw_ano$fitted.values)


# Coeficiente estimado e erro padrão
b <- 0.003203                   
ErroPadrao <- 0.002006           

# Valor crítico t para IC95%
t_value <- 1.96

# Calcular VPA
VPA <- (-1 + 10^(b))*100

# Calcular IC95% superior e inferior
IC95_sup <- (-1 + 10^(b + t_value * ErroPadrao)) * 100
IC95_inf <- (-1 + 10^(b - t_value * ErroPadrao)) * 100

# Imprimir resultados
cat("Variação Percentual Anual (VPA):", VPA, "%\n")
cat("IC95% Superior:", IC95_sup, "%\n")
cat("IC95% Inferior:", IC95_inf, "%\n")

##   PLOT
ggplot(data = df_comp, aes(x = nu_ano, y = Casos_ano)) + 
  geom_point(size = 2, aes(color = "Casos ao ano")) +  
  geom_line(aes(y = fitted_Cases_ano, color = "Casos preditos"), size = 1) +
  scale_x_continuous(breaks = 2007:2022, limits = c(2007, 2022)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +
  theme_classic(base_size = 14) +
  theme(plot.caption = element_text(hjust = 0, margin = margin(t = 10, l = 10, unit = "pt")),
        legend.position = "bottom",
        legend.background = element_rect(fill = "white", color = NA)) +
  xlab("Ano") +
  ylab("% Completude") +
  ggtitle("Completude por Ano")

####################### Prais-Winsten Núemero de Notificação

df_not <- df_e %>% tabyl(nu_ano)

df_not$percent <- NULL

df_not$Casos_ano_log <- log(df_not$n)

df_not$time <- seq(1:16)

library(prais)

############################################################################
# Realizar a regressão de Prais-Winsten
pw_ano_not <- prais_winsten(Casos_ano_log ~ time, data = df_not, index = "time")
summary(pw_ano_not)

df_not$fitted_Cases_ano = exp(pw_ano_not$fitted.values)


# Coeficiente estimado e erro padrão
b <- 0.1725                  
ErroPadrao <- 0.0234           

# Valor crítico t para IC95%
t_value <- 1.96

# Calcular VPA
VPA <- (-1 + 10^(b))*100

# Calcular IC95% superior e inferior
IC95_sup <- (-1 + 10^(b + t_value * ErroPadrao)) * 100
IC95_inf <- (-1 + 10^(b - t_value * ErroPadrao)) * 100

# Imprimir resultados
cat("Variação Percentual Anual (VPA):", VPA, "%\n")
cat("IC95% Superior:", IC95_sup, "%\n")
cat("IC95% Inferior:", IC95_inf, "%\n")

##   PLOT
ggplot(data = df_not, aes(x = nu_ano, y = n)) + 
  geom_point(size = 2, aes(color = "Casos ao ano")) +  
  geom_line(aes(y = fitted_Cases_ano, color = "Casos preditos"), size = 1) +
  scale_x_continuous(breaks = 2007:2022, limits = c(2007, 2022)) +
  scale_y_continuous(breaks = seq(19000, 319000, by = 50000), limits = c(19000, 319000)) +
  theme_classic(base_size = 14) +
  theme(plot.caption = element_text(hjust = 0, margin = margin(t = 10, l = 10, unit = "pt")),
        legend.position = "bottom",
        legend.background = element_rect(fill = "white", color = NA)) +
  xlab("Ano") +
  ylab("Número de Notificações") +
  ggtitle("Notificações por Ano")

#####Completude por Municipio

CADMUN <- read_excel("D:/CADMUN.xlsx")

cadmun <- clean_names(CADMUN)

cadmun <- cadmun %>% select(muncod, munnome, latitude, longitude)

df_f <- merge(df_e, cadmun, by.x="id_municip", by.y="muncod", all.x = TRUE)

df_comp_municipios <- df_e %>%
  group_by(id_municip) %>%
  select(id_ocupa_n_2, local_acid, 
         cat, cnae, cs_raca, cs_escol_n, sit_trab,
         uf_acid, mun_acid, tipo_acid, evolucao) %>%
  summarise_all(~mean(.!= "Ignorado",  na.rm = TRUE)) %>% write_clip()

df_comp_municipios$media <- rowMeans(df_comp_municipios[,2:12])

df_comp_municipios_mapa <- df_comp_municipios %>% select(id_municip, media)

df_comp_municipios_mapa_vf <- merge(df_comp_municipios_mapa, cadmun, by.x="id_municip", by.y="muncod", all.x = TRUE)

write_xlsx(df_comp_municipios_mapa_vf, "mapa_municipio_completude_2.xlsx")


write_xlsx(df_comp, "df_comp_variáveis.xlsx")

