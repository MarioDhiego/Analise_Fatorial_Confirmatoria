# ==============================================================================
# SCRIPT R MESTRE DEFINITIVO E REPRODUTÍVEL DE ANÁLISE DE DADOS
# Pesquisa: Adoção de Práticas Socioambientais em PMEs
# Autor: Marcelo Vianna (2026)
# ==============================================================================

# ------------------------------------------------------------------------------
# BLOCO 1: CARREGAMENTO DE PACOTES E BASE DE DADOS
# ------------------------------------------------------------------------------


library(readxl)
library(dplyr)
library(rstatix)
library(writexl)
library(likert)
library(ggplot2)
library(patchwork)
library(lavaan)
library(semTools)
library(semPlot)
library(reshape2)
library(sjPlot)

# Leitura e preparação da base de dados
dados <- read_excel("Análise_Marcelo_Vianna.xlsx", sheet = "Banco1")

# Mapeando as colunas das 9 dimensões originais do instrumento
itens_cols <- c(paste0("P", 1:6), paste0("B", 1:6), paste0("PA", 1:8),
                paste0("PS", 1:5), paste0("D", 1:10), paste0("INT", 1:4),
                paste0("ATT", 1:4), paste0("NS", 1:5), paste0("CCP", 1:5))

df_itens <- dados %>% select(all_of(itens_cols)) %>% na.omit()
cat("\n[✓] Dados carregados com sucesso. Amostra N =", nrow(df_itens), "\n")


# ------------------------------------------------------------------------------
# BLOCO 2: ANÁLISE COMPLETA DO PERFIL SOCIOECONÔMICO E INVARIÂNCIA POPULACIONAL
# ------------------------------------------------------------------------------
dados <- dados %>%
  mutate(
    RAMO_CLEAN = case_when(
      RAMO_ATIVIDADE %in% c("Comércio", "Comercio", "Cafe", "Feirante") ~ "Comércio",
      RAMO_ATIVIDADE %in% c("Serviço", "Salão de Beleza", "Dentista", "Beleza", "Saúde") ~ "Serviço",
      TRUE ~ RAMO_ATIVIDADE
    ),
    ESCOLARIDADE_REC = case_when(
      ESCOLARIDADE %in% c("Mestrado", "Doutorado") ~ "Stricto Sensu (Mestrado/Doutorado)",
      TRUE ~ ESCOLARIDADE
    ),
    FAIXA_FUNCIONARIOS = case_when(
      `Nº FUNCIONÁRIOS` == 0 ~ "Sem funcionários fixos (0)",
      `Nº FUNCIONÁRIOS` >= 1 & `Nº FUNCIONÁRIOS` <= 4 ~ "De 1 a 4 funcionários",
      `Nº FUNCIONÁRIOS` >= 5 & `Nº FUNCIONÁRIOS` <= 9 ~ "De 5 a 9 funcionários",
      `Nº FUNCIONÁRIOS` >= 10 & `Nº FUNCIONÁRIOS` <= 49 ~ "De 10 a 49 funcionários",
      `Nº FUNCIONÁRIOS` >= 50 ~ "50 ou mais funcionários"
    )
  )

# 2.1 - Caracterização Univariada (Tabela 1)
cat("\n====================================================================\n")
cat(" PASSO 1: CARACTERIZAÇÃO SOCIOECONÔMICA DA AMOSTRA (N =", nrow(dados), ")")
cat("\n====================================================================\n")

vars_socio <- c("GENERO", "IDADE", "RAÇA", "ESCOLARIDADE_REC", "GARGO", 
                "TEMPO_CARGO", "LOCAL_EMPRESA", "RAMO_CLEAN", "FAIXA_FUNCIONARIOS", "FAT_ANUAL_EMPRESA")

df_tabela1 <- data.frame()

for(v in vars_socio) {
  cat("\n--- Variável:", v, "---\n")
  tab <- dados %>%
    group_by(Variavel = v, Categoria = as.character(.data[[v]])) %>%
    summarise(
      Frequencia_N = n(),
      Percentual_Pct = round((n() / nrow(dados)) * 100, 1),
      .groups = 'drop'
    ) %>%
    arrange(desc(Frequencia_N))
  
  print(as.data.frame(tab))
  df_tabela1 <- rbind(df_tabela1, tab)
}

# 2.2 - Invariância Populacional (Tabela 2)
dados <- dados %>%
  mutate(
    Dim_Pressao    = rowMeans(select(., P2, P4, P5), na.rm = TRUE),
    Dim_Barreiras  = rowMeans(select(., B1, B3, B4, B5), na.rm = TRUE),
    Dim_Praticas   = rowMeans(select(., PA1, PA2, PA3, PA5, PA6, PA7, PA8), na.rm = TRUE),
    Dim_Desempenho = rowMeans(select(., D2, D3, D4, D5, D7, D9, D10), na.rm = TRUE),
    Dim_Intencao   = rowMeans(select(., INT1, INT2, INT3, INT4), na.rm = TRUE)
  )

dims <- c("Dim_Pressao", "Dim_Barreiras", "Dim_Praticas", "Dim_Desempenho", "Dim_Intencao")

cat("\n====================================================================\n")
cat(" PASSO 2: TESTES DE HOMOGENEIDADE E DIFERENÇA DE MÉDIAS (p-valores)")
cat("\n====================================================================\n")

df_invariancia <- data.frame()

for(d in dims) {
  teste <- wilcox.test(get(d) ~ GARGO, data = dados)
  df_invariancia <- rbind(df_invariancia, data.frame(
    Variavel_Demografica = "Cargo no Empreendimento",
    Dimensao_Avaliada = d, Teste_Aplicado = "Mann-Whitney U",
    Estatistica_Chi2_W = round(teste$statistic, 2), p_valor = round(teste$p.value, 4),
    Diagnostico = ifelse(teste$p.value < 0.05, "Diferença Significativa", "Invariante (p > 0.05)")
  ))
}

multinomiais <- c("GENERO", "IDADE", "RAÇA", "ESCOLARIDADE_REC", "TEMPO_CARGO", 
                  "LOCAL_EMPRESA", "RAMO_CLEAN", "FAIXA_FUNCIONARIOS", "FAT_ANUAL_EMPRESA")

for(v_demo in multinomiais) {
  for(d in dims) {
    kw <- kruskal.test(get(d) ~ get(v_demo), data = dados)
    df_invariancia <- rbind(df_invariancia, data.frame(
      Variavel_Demografica = v_demo, Dimensao_Avaliada = d, Teste_Aplicado = "Kruskal-Wallis",
      Estatistica_Chi2_W = round(kw$statistic, 2), p_valor = round(kw$p.value, 4),
      Diagnostico = ifelse(kw$p.value < 0.05, "Diferença Significativa", "Invariante (p > 0.05)")
    ))
  }
}
print(df_invariancia %>% filter(p_valor < 0.05))

# 2.3 - Tabelas Cruzadas (Crosstabs)
crosstab_gc_abs <- table(dados$GENERO, dados$GARGO)
crosstab_gc_pct <- prop.table(crosstab_gc_abs, margin = 1) * 100
df_gc <- as.data.frame.matrix(round(crosstab_gc_pct, 1))
df_gc$GENERO <- rownames(df_gc)
df_gc <- df_gc %>% select(GENERO, everything())

crosstab_fr_abs <- table(dados$FAT_ANUAL_EMPRESA, dados$RAMO_CLEAN)
crosstab_fr_pct <- prop.table(crosstab_fr_abs, margin = 1) * 100
df_fr <- as.data.frame.matrix(round(crosstab_fr_pct, 1))
df_fr$FATURAMENTO <- rownames(df_fr)
df_fr <- df_fr %>% select(FATURAMENTO, everything())

# 2.4 - Avaliação Granular Item por Item
todos_itens <- c("P1","P2","P3","P4","P5","P6","B1","B2","B3","B4","B5","B6",
                 "PA1","PA2","PA3","PA4","PA5","PA6","PA7","PA8","INT1","INT2","INT3","INT4")

res_itens <- data.frame(Item = character(), Chi2_Genero = numeric(), p_val_Genero = numeric(), Chi2_Cargo = numeric(), p_val_Cargo = numeric(), stringsAsFactors = FALSE)

for(item in todos_itens) {
  t_gen <- table(dados$GENERO, dados[[item]])
  chi_gen <- chisq.test(t_gen)
  t_car <- table(dados$GARGO, dados[[item]])
  chi_car <- chisq.test(t_car)
  res_itens <- rbind(res_itens, data.frame(
    Item = item, Chi2_Genero = round(chi_gen$statistic, 2), p_val_Genero = round(chi_gen$p.value, 4),
    Chi2_Cargo = round(chi_car$statistic, 2), p_val_Cargo = round(chi_car$p.value, 4)
  ))
}

# 2.5 - Exportação Consolidada para Excel
lista_relatorios <- list(
  "Tabela 1 - Perfil Socio" = df_tabela1,
  "Tabela 2 - Invariancia"  = df_invariancia,
  "Cruzamento Genero x Cargo" = df_gc,
  "Cruzamento Fat x Ramo"   = df_fr,
  "QuiQuadrado Itens"       = res_itens
)
write_xlsx(lista_relatorios, "Analise_Socioeconomica_Completa.xlsx")

# 2.6 - Mapa de Calor de Spearman
dados_num <- dados %>%
  mutate(
    Idade_Ord = as.numeric(factor(IDADE, levels = c("De 25 anos a 35 anos de idade", "De 36 anos a 45 anos de idade", "De 46 anos a 55 anos de idade", "Acima de 55 anos de idade"))),
    Escolaridade_Ord = as.numeric(factor(ESCOLARIDADE_REC, levels = c("Ensino Fundamental Completo", "Ensino Médio Completo", "Ensino Superior Completo", "Pós Graduação Completa", "Stricto Sensu (Mestrado/Doutorado)"))),
    TempoCargo_Ord = as.numeric(factor(TEMPO_CARGO, levels = c("De 01 a 03 anos", "De 04 a 06 anos", "De 07 a 09 anos", "Acima de 10 anos"))),
    Funcionarios_Num = `Nº FUNCIONÁRIOS`,
    Faturamento_Ord = as.numeric(factor(FAT_ANUAL_EMPRESA, levels = c("Menor ou igual a R$ 360.000,00/ano", "De R$ 360.000,01 até R$ 2.400.000,00/ano", "De R$ 2.400.000,01 até R$ 7.200.000,00/ano", "De R$ 7.200.000,01 até R$ 9.600.000,00/ano")))
  )

vars_socio_num <- c("Idade_Ord", "Escolaridade_Ord", "TempoCargo_Ord", "Funcionarios_Num", "Faturamento_Ord")
vars_dims <- c("Dim_Pressao", "Dim_Barreiras", "Dim_Praticas", "Dim_Desempenho", "Dim_Intencao")
matriz_corr <- cor(dados_num[, vars_socio_num], dados_num[, vars_dims], method = "spearman", use = "complete.obs")
rownames(matriz_corr) <- c("Idade", "Escolaridade", "Tempo no Cargo", "Nº Funcionários", "Faturamento Anual")
colnames(matriz_corr) <- c("Pressão", "Barreiras", "Práticas", "Desempenho", "Intenção")

df_heat <- melt(matriz_corr)
ggplot(df_heat, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.3f", value)), color = "black", size = 3.8) +
  scale_fill_gradient2(low = "#4575b4", mid = "#ffffbf", high = "#d73027", midpoint = 0, limit = c(-0.15, 0.15), name = "Spearman (rho)") +
  theme_minimal() +
  labs(title = "Mapa de Calor: Correlação de Spearman (Perfil Socioeconômico vs. Construtos)", x = "", y = "") +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), axis.text = element_text(size = 10, color = "black", face = "bold"), legend.position = "right")


# ------------------------------------------------------------------------------
# BLOCO 3: ANÁLISE DESCRITIVA DAS RESPOSTAS DOS ITENS (GRÁFICOS DE LIKERT)
# ------------------------------------------------------------------------------
cat("\n====================================================================\n")
cat(" GERAÇÃO DOS GRÁFICOS DE DIVERGÊNCIA DE LIKERT (9 DIMENSÕES)")
cat("\n====================================================================\n")

labels_likert <- c("Discordo Totalmente", "Discordo", "Neutro", "Concordo", "Concordo Totalmente")

df_graficos <- df_itens %>%
  mutate(across(everything(), ~ factor(.x, levels = 1:5, labels = labels_likert, ordered = TRUE))) %>%
  as.data.frame()

minhas_cores <- c("#D7191C", "#F46D43", "#E0E0E0", "#74C476", "#238B45")

# 3.1 - Pressão (P1 a P6)
obj_pressao <- likert(df_graficos[, paste0("P", 1:6)])
grafico_pressao <- plot(obj_pressao, colors = minhas_cores) + 
  ggtitle("PRESSÃO") + theme_minimal() + 
  theme(legend.position = "bottom", legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.y = element_text(size = 12, face = "bold"))
print(grafico_pressao)

# 3.2 - Barreiras (B1 a B6)
obj_barreiras <- likert(df_graficos[, paste0("B", 1:6)])
grafico_barreiras <- plot(obj_barreiras, colors = minhas_cores) + 
  ggtitle("BARREIRAS") + theme_minimal() + 
  theme(legend.position = "bottom", legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.y = element_text(size = 12, face = "bold"))
print(grafico_barreiras)

# 3.3 - Práticas (PA1 a PA8)
obj_praticas <- likert(df_graficos[, paste0("PA", 1:8)])
grafico_praticas <- plot(obj_praticas, colors = minhas_cores) + 
  ggtitle("PRÁTICAS") + theme_minimal() + 
  theme(legend.position = "bottom", legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.y = element_text(size = 12, face = "bold"))
print(grafico_praticas)

# 3.4 - Práticas Sociais (PS1 a PS5)
obj_praticas_sociais <- likert(df_graficos[, paste0("PS", 1:5)])
grafico_praticas_sociais <- plot(obj_praticas_sociais, colors = minhas_cores) + 
  ggtitle("Práticas Sociais") + theme_minimal() + 
  theme(legend.position = "bottom", legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.y = element_text(size = 12, face = "bold"))
print(grafico_praticas_sociais)

# 3.5 - Desempenho Operacional (D1 a D10)
obj_desempenho <- likert(df_graficos[, paste0("D", 1:10)])
grafico_desempenho <- plot(obj_desempenho, colors = minhas_cores) + 
  ggtitle("Desempenho Operacional") + theme_minimal() + 
  theme(legend.position = "bottom", legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.y = element_text(size = 12, face = "bold"))
print(grafico_desempenho)

# 3.6 - Intenção (INT1 a INT4)
obj_intencao <- likert(df_graficos[, paste0("INT", 1:4)])
grafico_intencao <- plot(obj_intencao, colors = minhas_cores) + 
  ggtitle("Intenção") + theme_minimal() + 
  theme(legend.position = "bottom", legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.y = element_text(size = 12, face = "bold"))
print(grafico_intencao)

# 3.7 - Atitudes (ATT1 a ATT4)
obj_atitudes <- likert(df_graficos[, paste0("ATT", 1:4)])
grafico_atitudes <- plot(obj_atitudes, colors = minhas_cores) + 
  ggtitle("Atitudes") + theme_minimal() + 
  theme(legend.position = "bottom", legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.y = element_text(size = 12, face = "bold"))
print(grafico_atitudes)

# 3.8 - Norma Subjetiva (NS1 a NS5)
obj_norma <- likert(df_graficos[, paste0("NS", 1:5)])
grafico_norma <- plot(obj_norma, colors = minhas_cores) + 
  ggtitle("Norma Subjetiva") + theme_minimal() + 
  theme(legend.position = "bottom", legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.y = element_text(size = 12, face = "bold"))
print(grafico_norma)

# 3.9 - Controle Comportamental Percebido (CCP1 a CCP5)
obj_controle <- likert(df_graficos[, paste0("CCP", 1:5)])
grafico_controle <- plot(obj_controle, colors = minhas_cores) + 
  ggtitle("Controle Comportamental Percebido") + theme_minimal() + 
  theme(legend.position = "bottom", legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.y = element_text(size = 12, face = "bold"))
print(grafico_controle)


# ------------------------------------------------------------------------------
# BLOCO 4: PURIFICAÇÃO DO MODELO DE MENSURAÇÃO (MODELOS V1 A V6 DETALHADOS)
# ------------------------------------------------------------------------------

# MODELO V1 (6 DIMENSÕES - TODOS OS ITENS)
cat("\n====================================================================\n")
cat(" MODELO AFC V1 (6 DIMENSÕES - TODOS OS ITENS)")
cat("\n====================================================================\n")

modelo_afc_6dim <- '
  Pressao                 =~ P1 + P2 + P3 + P4 + P5
  Barreiras               =~ B1 + B3 + B4 + B5
  Praticas                =~ PA1 + PA2 + PA3 + PA5 + PA6 + PA7 + PA8
  Desempenho_Operacional  =~ D1 + D2 + D3 + D4 + D5 + D6 + D7 + D8 + D9 + D10
  Intencao                =~ INT1 + INT2 + INT3 + INT4 
  Praticas_Sociais        =~ PS1 + PS2 + PS3 + PS4
'
ajuste_afc_6dim <- cfa(modelo_afc_6dim, data = dados, estimator = "WLSMV", ordered = TRUE)
summary(ajuste_afc_6dim, standardized = TRUE, fit.measures = TRUE)
print(round(t(reliability(ajuste_afc_6dim)), 3))
print(round(lavInspect(ajuste_afc_6dim, "cor.lv"), 3))
print(round(htmt(modelo_afc_6dim, data = dados, ordered = TRUE), 3))

semPaths(ajuste_afc_6dim, what = "paths", whatLabels = "std", layout = "tree2", edge.color = "black",
         color = list(lat = "#EAEAEA", man = "#FFFFFF"), sizeMan = 3, sizeLat = 7, intercepts = FALSE,
         residuals = FALSE, edge.label.cex = 0.8, label.cex = 1.0, label.scale = FALSE, mar = c(2, 4, 2, 4), title = FALSE)


# MODELO V2 (6 DIMENSÕES - SEM P1)
cat("\n====================================================================\n")
cat(" MODELO AFC V2 (6 DIMENSÕES - SEM P1)")
cat("\n====================================================================\n")

modelo_afc_6dim_v2 <- '
  Pressao                 =~ P2 + P3 + P4 + P5
  Barreiras               =~ B1 + B3 + B4 + B5
  Praticas                =~ PA1 + PA2 + PA3 + PA5 + PA6 + PA7 + PA8
  Desempenho_Operacional  =~ D1 + D2 + D3 + D4 + D5 + D6 + D7 + D8 + D9 + D10
  Intencao                =~ INT1 + INT2 + INT3 + INT4 
  Praticas_Sociais        =~ PS1 + PS2 + PS3 + PS4
'
ajuste_afc_6dim_v2 <- cfa(modelo_afc_6dim_v2, data = dados, estimator = "WLSMV", ordered = TRUE)
summary(ajuste_afc_6dim_v2, standardized = TRUE, fit.measures = TRUE)
print(round(t(reliability(ajuste_afc_6dim_v2)), 3))
print(round(lavInspect(ajuste_afc_6dim_v2, "cor.lv"), 3))
print(round(htmt(modelo_afc_6dim_v2, data = dados, ordered = TRUE), 3))

semPaths(ajuste_afc_6dim_v2, what = "paths", whatLabels = "std", layout = "tree2", edge.color = "black",
         color = list(lat = "#EAEAEA", man = "#FFFFFF"), sizeMan = 3, sizeLat = 7, intercepts = FALSE,
         residuals = FALSE, edge.label.cex = 0.8, label.cex = 1.0, label.scale = FALSE, mar = c(2, 4, 2, 4), title = FALSE)


# MODELO V3 (6 DIMENSÕES - SEM P1 E P3)
cat("\n====================================================================\n")
cat(" MODELO AFC V3 (6 DIMENSÕES - SEM P1 E P3)")
cat("\n====================================================================\n")

modelo_afc_6dim_v3 <- '
  Pressao                 =~ P2 + P4 + P5
  Barreiras               =~ B1 + B3 + B4 + B5
  Praticas                =~ PA1 + PA2 + PA3 + PA5 + PA6 + PA7 + PA8
  Desempenho_Operacional  =~ D1 + D2 + D3 + D4 + D5 + D6 + D7 + D8 + D9 + D10
  Intencao                =~ INT1 + INT2 + INT3 + INT4 
  Praticas_Sociais        =~ PS1 + PS2 + PS3 + PS4
'
ajuste_afc_6dim_v3 <- cfa(modelo_afc_6dim_v3, data = dados, estimator = "WLSMV", ordered = TRUE)
summary(ajuste_afc_6dim_v3, standardized = TRUE, fit.measures = TRUE)
print(round(t(reliability(ajuste_afc_6dim_v3)), 3))
print(round(lavInspect(ajuste_afc_6dim_v3, "cor.lv"), 3))
print(round(htmt(modelo_afc_6dim_v3, data = dados, ordered = TRUE), 3))

semPaths(ajuste_afc_6dim_v3, what = "paths", whatLabels = "std", layout = "tree2", edge.color = "black",
         color = list(lat = "#EAEAEA", man = "#FFFFFF"), sizeMan = 3, sizeLat = 7, intercepts = FALSE,
         residuals = FALSE, edge.label.cex = 0.8, label.cex = 1.0, label.scale = FALSE, mar = c(2, 4, 2, 4), title = FALSE)

indices_mod <- modindices(ajuste_afc_6dim_v3)
cargas_cruzadas <- subset(indices_mod, op == "=~")
cat("\n[!] MAIORES CARGAS CRUZADAS (MI > 10):\n")
print(head(cargas_cruzadas[order(-cargas_cruzadas$mi), ], 15))

itens_alvo <- c("PS1", "PS2", "PS3", "PS4", "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10")
erros_correlacionados <- subset(indices_mod, op == "~~" & (lhs %in% itens_alvo | rhs %in% itens_alvo))
cat("\n[!] MAIORES CORRELAÇÕES DE ERRO (MI > 15):\n")
print(head(erros_correlacionados[order(-erros_correlacionados$mi), ], 15))


# MODELO V4 (6 DIMENSÕES - SEM P1, P3 E D8)
cat("\n====================================================================\n")
cat(" MODELO AFC V4 (6 DIMENSÕES - SEM P1, P3 E D8)")
cat("\n====================================================================\n")

modelo_afc_6dim_v4 <- '
  Pressao                 =~ P2 + P4 + P5
  Barreiras               =~ B1 + B3 + B4 + B5
  Praticas                =~ PA1 + PA2 + PA3 + PA5 + PA6 + PA7 + PA8
  Desempenho_Operacional  =~ D1 + D2 + D3 + D4 + D5 + D6 + D7 + D9 + D10
  Intencao                =~ INT1 + INT2 + INT3 + INT4 
  Praticas_Sociais        =~ PS1 + PS2 + PS3 + PS4
'
ajuste_afc_6dim_v4 <- cfa(modelo_afc_6dim_v4, data = dados, estimator = "WLSMV", ordered = TRUE)
summary(ajuste_afc_6dim_v4, standardized = TRUE, fit.measures = TRUE)
print(round(t(reliability(ajuste_afc_6dim_v4)), 3))
print(round(lavInspect(ajuste_afc_6dim_v4, "cor.lv"), 3))
print(round(htmt(modelo_afc_6dim_v4, data = dados, ordered = TRUE), 3))

semPaths(ajuste_afc_6dim_v4, what = "paths", whatLabels = "std", layout = "tree2", edge.color = "black",
         color = list(lat = "#EAEAEA", man = "#FFFFFF"), sizeMan = 3, sizeLat = 7, intercepts = FALSE,
         residuals = FALSE, edge.label.cex = 0.8, label.cex = 1.0, label.scale = FALSE, mar = c(2, 4, 2, 4), title = FALSE)


# MODELO V5 (6 DIMENSÕES - SEM P1, P3, D8 E D1)
cat("\n====================================================================\n")
cat(" MODELO AFC V5 (6 DIMENSÕES - SEM P1, P3, D8 E D1)")
cat("\n====================================================================\n")

modelo_afc_6dim_v5 <- '
  Pressao                 =~ P2 + P4 + P5
  Barreiras               =~ B1 + B3 + B4 + B5
  Praticas                =~ PA1 + PA2 + PA3 + PA5 + PA6 + PA7 + PA8
  Desempenho_Operacional  =~ D2 + D3 + D4 + D5 + D6 + D7 + D9 + D10
  Intencao                =~ INT1 + INT2 + INT3 + INT4 
  Praticas_Sociais        =~ PS1 + PS2 + PS3 + PS4
'
ajuste_afc_6dim_v5 <- cfa(modelo_afc_6dim_v5, data = dados, estimator = "WLSMV", ordered = TRUE)
summary(ajuste_afc_6dim_v5, standardized = TRUE, fit.measures = TRUE)
print(round(t(reliability(ajuste_afc_6dim_v5)), 3))
print(round(lavInspect(ajuste_afc_6dim_v5, "cor.lv"), 3))
print(round(htmt(modelo_afc_6dim_v5, data = dados, ordered = TRUE), 3))

semPaths(ajuste_afc_6dim_v5, what = "paths", whatLabels = "std", layout = "tree2", edge.color = "black",
         color = list(lat = "#EAEAEA", man = "#FFFFFF"), sizeMan = 3, sizeLat = 7, intercepts = FALSE,
         residuals = FALSE, edge.label.cex = 0.8, label.cex = 1.0, label.scale = FALSE, mar = c(2, 4, 2, 4), title = FALSE)


# MODELO V6 (6 DIMENSÕES - SEM P1, P3, D8, D1 E D6)
cat("\n====================================================================\n")
cat(" MODELO AFC V6 (6 DIMENSÕES - SEM P1, P3, D8, D1 E D6)")
cat("\n====================================================================\n")

modelo_afc_6dim_v6 <- '
  Pressao                 =~ P2 + P4 + P5
  Barreiras               =~ B1 + B3 + B4 + B5
  Praticas                =~ PA1 + PA2 + PA3 + PA5 + PA6 + PA7 + PA8
  Desempenho_Operacional  =~ D2 + D3 + D4 + D5 + D7 + D9 + D10
  Intencao                =~ INT1 + INT2 + INT3 + INT4 
  Praticas_Sociais        =~ PS1 + PS2 + PS3 + PS4
'
ajuste_afc_6dim_v6 <- cfa(modelo_afc_6dim_v6, data = dados, estimator = "WLSMV", ordered = TRUE)
summary(ajuste_afc_6dim_v6, standardized = TRUE, fit.measures = TRUE)
print(round(t(reliability(ajuste_afc_6dim_v6)), 3))
print(round(lavInspect(ajuste_afc_6dim_v6, "cor.lv"), 3))
print(round(htmt(modelo_afc_6dim_v6, data = dados, ordered = TRUE), 3))

semPaths(ajuste_afc_6dim_v6, what = "paths", whatLabels = "std", layout = "tree2", edge.color = "black",
         color = list(lat = "#EAEAEA", man = "#FFFFFF"), sizeMan = 3, sizeLat = 7, intercepts = FALSE,
         residuals = FALSE, edge.label.cex = 0.8, label.cex = 1.0, label.scale = FALSE, mar = c(2, 4, 2, 4), title = FALSE)


# ------------------------------------------------------------------------------
# BLOCO 5: MODELO DE MENSURAÇÃO FINAL DEFINITIVO (5 DIMENSÕES)
# ------------------------------------------------------------------------------
cat("\n====================================================================\n")
cat(" MODELO DEFINITIVO DE MENSURAÇÃO (5 DIMENSÕES)")
cat("\n====================================================================\n")

modelo_afc_final <- '
  Pressao                 =~ P2 + P4 + P5
  Barreiras               =~ B1 + B3 + B4 + B5
  Praticas                =~ PA1 + PA2 + PA3 + PA5 + PA6 + PA7 + PA8
  Desempenho_Operacional  =~ D2 + D3 + D4 + D5 + D7 + D9 + D10
  Intencao                =~ INT1 + INT2 + INT3 + INT4 
'

ajuste_afc_final <- cfa(modelo_afc_final, data = dados, estimator = "WLSMV", ordered = TRUE)

summary(ajuste_afc_final, standardized = TRUE, fit.measures = TRUE)
metricas_final <- reliability(ajuste_afc_final)
print(round(t(metricas_final), 3))

matriz_correlacao_final <- lavInspect(ajuste_afc_final, "cor.lv")
print(round(matriz_correlacao_final, 3))

matriz_htmt_final <- htmt(modelo_afc_final, data = dados, ordered = TRUE)
print(round(matriz_htmt_final, 3))

# Diagramas do Modelo Final
semPaths(ajuste_afc_final, what = "paths", whatLabels = "std", layout = "tree2",
         edge.color = "black", color = list(lat = "#EAEAEA", man = "#FFFFFF"),
         sizeMan = 3.5, sizeLat = 8, intercepts = FALSE, residuals = FALSE,
         edge.label.cex = 0.9, label.cex = 1.1, label.scale = FALSE, mar = c(3, 5, 3, 5), title = FALSE)

cores_pls <- list(lat = "#3399FF", man = "#FFFF99")
semPaths(ajuste_afc_final, what = "paths", whatLabels = "std", layout = "tree", nCharNodes = 0,
         shapeLat = "circle", shapeMan = "rectangle", sizeLat = 11, sizeMan = 6,
         color = cores_pls, edge.color = "black", border.color = "black", label.color = "black",
         label.cex = 0.9, edge.label.cex = 0.8, intercepts = FALSE, residuals = FALSE,
         curvePivot = TRUE, mar = c(3, 3, 3, 3), title = FALSE)


# ------------------------------------------------------------------------------
# BLOCO 6: MODELO ESTRUTURAL (SEM) COMPLETO E MODELO BASE (H1, H2, H3)
# ------------------------------------------------------------------------------
cat("\n====================================================================\n")
cat(" MODELO ESTRUTURAL COMPLETO (SEM)")
cat("\n====================================================================\n")

modelo_sem_final <- '
  Pressao                 =~ P2 + P4 + P5
  Barreiras               =~ B1 + B3 + B4 + B5
  Praticas                =~ PA1 + PA2 + PA3 + PA5 + PA6 + PA7 + PA8
  Desempenho_Operacional  =~ D2 + D3 + D4 + D5 + D7 + D9 + D10
  Intencao                =~ INT1 + INT2 + INT3 + INT4 

  Intencao                ~ Pressao + Barreiras
  Praticas                ~ Intencao + Barreiras + Pressao
  Desempenho_Operacional  ~ Praticas
'
ajuste_sem <- sem(modelo_sem_final, data = dados, estimator = "WLSMV", ordered = TRUE)
summary(ajuste_sem, fit.measures = TRUE, standardized = TRUE)

parametros <- parameterEstimates(ajuste_sem, standardized = TRUE)
hipoteses <- subset(parametros, op == "~")
hipoteses_limpo <- data.frame(
  Variavel_Dependente = hipoteses$lhs, Seta = "<-", Variavel_Independente = hipoteses$rhs,
  Beta_Padronizado = round(hipoteses$std.all, 3), Erro_Padrao = round(hipoteses$se, 3),
  Estatistica_Z = round(hipoteses$z, 3), P_Valor = round(hipoteses$pvalue, 3)
)
print(hipoteses_limpo)
print(round(lavInspect(ajuste_sem, "rsquare"), 3))

semPaths(ajuste_sem, what = "paths", whatLabels = "std", layout = "tree2", nCharNodes = 0,
         shapeLat = "circle", shapeMan = "rectangle", sizeLat = 10, sizeMan = 5, color = cores_pls,
         edge.color = "black", border.color = "black", label.color = "black", label.cex = 0.9,
         edge.label.cex = 0.7, intercepts = FALSE, residuals = FALSE, mar = c(2, 6, 2, 6), title = FALSE)


# MODELO ESTRUTURAL BASE (H1, H2 e H3)
cat("\n====================================================================\n")
cat(" MODELO ESTRUTURAL BASE (H1, H2 e H3)")
cat("\n====================================================================\n")

modelo_sem_base <- '
  Pressao                 =~ P2 + P4 + P5
  Barreiras               =~ B1 + B3 + B4 + B5
  Praticas                =~ PA1 + PA2 + PA3 + PA5 + PA6 + PA7 + PA8
  Desempenho_Operacional  =~ D2 + D3 + D4 + D5 + D7 + D9 + D10
  Intencao                =~ INT1 + INT2 + INT3 + INT4 

  Praticas                ~ Pressao + Barreiras
  Desempenho_Operacional  ~ Praticas
'
ajuste_sem_base <- sem(modelo_sem_base, data = dados, estimator = "WLSMV", ordered = TRUE)
parametros_base <- parameterEstimates(ajuste_sem_base, standardized = TRUE)
hipoteses_base <- subset(parametros_base, op == "~")
hipoteses_base_limpo <- data.frame(
  Variavel_Dependente = hipoteses_base$lhs, Seta = "<-", Variavel_Independente = hipoteses_base$rhs,
  Beta_Padronizado = round(hipoteses_base$std.all, 3), Erro_Padrao = round(hipoteses_base$se, 3),
  Estatistica_Z = round(hipoteses_base$z, 3), P_Valor = round(hipoteses_base$pvalue, 3)
)
print(hipoteses_base_limpo)

semPaths(ajuste_sem_base, what = "paths", whatLabels = "std", layout = "tree2", rotation = 2, nCharNodes = 0,
         nodeLabels = c("P2","P4","P5","B1","B3","B4","B5","PA1","PA2","PA3","PA5","PA6","PA7","PA8","D2","D3","D4","D5","D7","D9","D10","INT1","INT2","INT3","INT4",
                        "Pressao", "Barreiras", "Praticas", "Desempenho\nOperacional", "Intencao"),
         shapeLat = "circle", shapeMan = "rectangle", sizeLat = 14, sizeMan = 6, label.cex = 1, edge.label.cex = 1.0,
         color = cores_pls, edge.color = "black", border.color = "black", label.color = "black",
         intercepts = FALSE, residuals = FALSE, mar = c(3, 8, 3, 8), title = FALSE)


# ------------------------------------------------------------------------------
# BLOCO 7: BLINDAGEM METODOLÓGICA (HARMAN CMB E MEDIAÇÃO)
# ------------------------------------------------------------------------------
cat("\n====================================================================\n")
cat(" TESTE DO VIÉS DE MÉTODO COMUM (HARMAN)")
cat("\n====================================================================\n")

modelo_harman <- '
  Fator_Geral =~ P2 + P4 + P5 + B1 + B3 + B4 + B5 + PA1 + PA2 + PA3 + PA5 + PA6 + PA7 + PA8 + D2 + D3 + D4 + D5 + D7 + D9 + D10 + INT1 + INT2 + INT3 + INT4
'
ajuste_harman <- cfa(modelo_harman, data = dados, estimator = "WLSMV", ordered = TRUE)
print(round(fitMeasures(ajuste_harman, c("cfi", "tli", "rmsea", "srmr")), 3))


cat("\n====================================================================\n")
cat(" TESTE DE EFEITOS INDIRETOS / MEDIAÇÃO")
cat("\n====================================================================\n")

modelo_mediacao <- '
  Pressao                 =~ P2 + P4 + P5
  Barreiras               =~ B1 + B3 + B4 + B5
  Praticas                =~ PA1 + PA2 + PA3 + PA5 + PA6 + PA7 + PA8
  Desempenho_Operacional  =~ D2 + D3 + D4 + D5 + D7 + D9 + D10

  Praticas                ~ a1*Pressao + a2*Barreiras
  Desempenho_Operacional  ~ b*Praticas

  Indireto_Pressao   := a1 * b
  Indireto_Barreiras := a2 * b
'
ajuste_mediacao <- sem(modelo_mediacao, data = dados, estimator = "WLSMV", ordered = TRUE)
parametros_med <- parameterEstimates(ajuste_mediacao, standardized = TRUE)
efeitos_indiretos <- subset(parametros_med, op == ":=")
tabela_mediacao <- data.frame(
  Trajetoria_Indireta = c("Pressão -> Práticas -> Desempenho", "Barreiras -> Práticas -> Desempenho"),
  Beta_Indireto = round(efeitos_indiretos$std.all, 3), Erro_Padrao = round(efeitos_indiretos$se, 3),
  Z_Valor = round(efeitos_indiretos$z, 3), P_Valor = round(efeitos_indiretos$pvalue, 3)
)
print(tabela_mediacao)


# ------------------------------------------------------------------------------
# BLOCO 8: MODERAÇÃO (H4a E H4b) VIA ESCORES FATORIAIS
# ------------------------------------------------------------------------------
cat("\n====================================================================\n")
cat(" TESTE DE MODERAÇÃO E MODELO ESTRUTURAL COMPLETO (ESCORES)")
cat("\n====================================================================\n")

escores <- as.data.frame(lavPredict(ajuste_afc_final))
escores$Mod_Pressao <- escores$Pressao * escores$Intencao
escores$Mod_Barreiras <- escores$Barreiras * escores$Intencao

modelo_moderacao <- '
  Praticas ~ Pressao + Barreiras + Intencao + Mod_Pressao + Mod_Barreiras
  Desempenho_Operacional ~ Praticas
'
ajuste_moderacao <- sem(modelo_moderacao, data = escores)

parametros_mod <- parameterEstimates(ajuste_moderacao, standardized = TRUE)
hipoteses_mod <- subset(parametros_mod, op == "~")
tabela_final_mod <- data.frame(
  Variavel_Dependente = hipoteses_mod$lhs, Seta = "<-", Variavel_Independente = hipoteses_mod$rhs,
  Beta = round(hipoteses_mod$std.all, 3), P_Valor = round(hipoteses_mod$pvalue, 3)
)
print(tabela_final_mod)

cat("\n=== R-QUADRADO: MODELO BASE ===\n")
print(round(lavInspect(ajuste_sem_base, "rsquare"), 3))

cat("\n=== R-QUADRADO: MODELO COM MODERAÇÃO ===\n")
print(round(lavInspect(ajuste_moderacao, "rsquare"), 3))


# ------------------------------------------------------------------------------
# BLOCO 9: GRÁFICOS FINAIS (SLOPES E LAYOUT MANUAL COM MATRIZ)
# ------------------------------------------------------------------------------
# 9.1 - Gráfico de Interação (H4a)
mod_lm <- lm(Praticas ~ Pressao * Intencao, data = escores)

plot_model(mod_lm, type = "int", mdrt.values = "meansd",
           title = "Efeito Moderador da Intenção do Gestor (H4a)",
           axis.title = c("Pressões Exógenas do Mercado", "Adoção de Práticas Socioambientais"),
           legend.title = "Intenção da Gestão:") +
  scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A"),
                     labels = c("Baixa Intenção (-1 DP)", "Intenção Média", "Alta Intenção (+1 DP)")) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    axis.title.x = element_text(size = 11, face = "bold", margin = margin(t = 10, b = 10)),
    axis.title.y = element_text(size = 11, face = "bold", margin = margin(r = 10)),
    axis.text = element_text(size = 10, color = "black"),
    legend.position = "top",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    plot.margin = margin(t = 15, r = 15, b = 20, l = 15)
  )

# 9.2 - Diagrama Estrutural Completo em Layout Rotação 1 e Matriz Manual
cores_pls_mod <- list(man = "#3399FF")

semPaths(ajuste_moderacao, what = "paths", whatLabels = "std", layout = "tree2", rotation = 1, nCharNodes = 0,
         nodeLabels = c("Práticas", "Desempenho\nOperacional", "Pressão", "Barreiras", 
                        "Intenção\n(Gestão)", "Moderação\n(Pressão)", "Moderação\n(Barreiras)"),
         shapeMan = "rectangle", sizeMan = 12, sizeMan2 = 6, label.cex = 1.0, edge.label.cex = 0.85,
         edge.label.position = 0.50, covariances = FALSE, intercepts = FALSE, residuals = FALSE,
         color = cores_pls_mod, edge.color = "black", border.color = "black", label.color = "black",
         mar = c(6, 4, 6, 4), title = FALSE)

# Matriz de Layout Manual
matriz_layout <- matrix(c(
  0.0,  0.0,  # 1: Práticas
  0.0, -1.0,  # 2: Desempenho Operacional
  -0.8,  1.0,  # 3: Pressão
  -0.4,  1.0,  # 4: Barreiras
  0.0,  1.0,  # 5: Intenção
  0.4,  1.0,  # 6: Moderação Pressão
  0.8,  1.0   # 7: Moderação Barreiras
), ncol = 2, byrow = TRUE)

semPaths(ajuste_moderacao, what = "paths", whatLabels = "std", layout = matriz_layout, nCharNodes = 0,
         nodeLabels = c("Práticas", "Desempenho\nOperacional", "Pressão", "Barreiras", 
                        "Intenção\n(Gestão)", "Moderação\n(Pressão)", "Moderação\n(Barreiras)"),
         shapeMan = "rectangle", sizeMan = 12, sizeMan2 = 6, label.cex = 1.0, edge.label.cex = 0.85,
         edge.label.position = 0.50, color = cores_pls_mod, edge.color = "black", border.color = "black",
         label.color = "black", intercepts = FALSE, residuals = FALSE, mar = c(5, 5, 5, 5), title = FALSE)

cat("\n====================================================================\n")
cat("[✓] SCRIPT MESTRE INTEGRAL EXECUTADO COM SUCESSO!\n")
cat("====================================================================\n")




