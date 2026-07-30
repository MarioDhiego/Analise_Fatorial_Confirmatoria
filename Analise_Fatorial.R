# ==============================================================================
# SCRIPT R: GRÁFICOS LIKERT SEPARADOS POR DIMENSÃO E AFC (AJUSTADO)
# ==============================================================================

# 1. CARREGAMENTO DOS PACOTES
library(readxl)
library(dplyr)
library(likert)
library(ggplot2)
library(patchwork) # agrupar gráficos
library(lavaan)    # adicionado para rodar a AFC

# 2. PREPARAÇÃO DOS DADOS
dados <- read_excel("Análise_Marcelo_Vianna.xlsx", sheet = "Banco1")

itens_cols <- c(paste0("P", 1:6), paste0("B", 1:6), paste0("PA", 1:8),
                paste0("PS", 1:5), paste0("D", 1:10), paste0("INT", 1:4),
                paste0("ATT", 1:4), paste0("NS", 1:5), paste0("CCP", 1:5))

df_itens <- dados %>% select(all_of(itens_cols)) %>% na.omit()

labels_likert <- c("Discordo Totalmente", "Discordo", "Neutro", "Concordo", "Concordo Totalmente")

df_graficos <- df_itens %>%
  mutate(across(everything(), ~ factor(.x, levels = 1:5, labels = labels_likert, ordered = TRUE))) %>%
  as.data.frame()

# Paleta de Cores: Vermelho Vivo (Discordância) -> Cinza (Neutro) -> Verde (Concordância)
minhas_cores <- c("#D7191C", "#F46D43", "#E0E0E0", "#74C476", "#238B45")

# ==============================================================================
# BLOCO 1: DIMENSÃO - PRESSÃO (P1 a P6)
# ==============================================================================
obj_pressao <- likert(df_graficos[, paste0("P", 1:6)])
grafico_pressao <- plot(obj_pressao, colors = minhas_cores) + 
  ggtitle("PRESSÃO") +
  theme_minimal() + 
  theme(legend.position = "bottom", legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.y = element_text(size = 12, face = "bold"))
print(grafico_pressao)

# ==============================================================================
# BLOCO 2: DIMENSÃO - BARREIRAS (B1 a B6)
# ==============================================================================
obj_barreiras <- likert(df_graficos[, paste0("B", 1:6)])
grafico_barreiras <- plot(obj_barreiras, colors = minhas_cores) + 
  ggtitle("BARREIRAS") +
  theme_minimal() + 
  theme(legend.position = "bottom", legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.y = element_text(size = 12, face = "bold"))
print(grafico_barreiras)

# ==============================================================================
# BLOCO 3: DIMENSÃO - PRÁTICAS (PA1 a PA8)
# ==============================================================================
obj_praticas <- likert(df_graficos[, paste0("PA", 1:8)])
grafico_praticas <- plot(obj_praticas, colors = minhas_cores) + 
  ggtitle("PRÁTICAS") +
  theme_minimal() + 
  theme(legend.position = "bottom", legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.y = element_text(size = 12, face = "bold"))
print(grafico_praticas)

# ==============================================================================
# BLOCO 4: DIMENSÃO - PRÁTICAS SOCIAIS (PS1 a PS5)
# ==============================================================================
obj_praticas_sociais <- likert(df_graficos[, paste0("PS", 1:5)])
grafico_praticas_sociais <- plot(obj_praticas_sociais, colors = minhas_cores) + 
  ggtitle("Práticas Sociais") +
  theme_minimal() + 
  theme(legend.position = "bottom", legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.y = element_text(size = 12, face = "bold"))
print(grafico_praticas_sociais)

# ==============================================================================
# BLOCO 5: DIMENSÃO - DESEMPENHO OPERACIONAL (D1 a D10)
# ==============================================================================
obj_desempenho <- likert(df_graficos[, paste0("D", 1:10)])
grafico_desempenho <- plot(obj_desempenho, colors = minhas_cores) + 
  ggtitle("Desempenho Operacional") +
  theme_minimal() + 
  theme(legend.position = "bottom", legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.y = element_text(size = 12, face = "bold"))
print(grafico_desempenho)

# ==============================================================================
# BLOCO 6: DIMENSÃO - INTENÇÃO (INT1 a INT4)
# ==============================================================================
obj_intencao <- likert(df_graficos[, paste0("INT", 1:4)])
grafico_intencao <- plot(obj_intencao, colors = minhas_cores) + 
  ggtitle("Intenção") +
  theme_minimal() + 
  theme(legend.position = "bottom", legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.y = element_text(size = 12, face = "bold"))
print(grafico_intencao)

# ==============================================================================
# BLOCO 7: DIMENSÃO - ATITUDES (ATT1 a ATT4)
# ==============================================================================
obj_atitudes <- likert(df_graficos[, paste0("ATT", 1:4)])
grafico_atitudes <- plot(obj_atitudes, colors = minhas_cores) + 
  ggtitle("Atitudes") +
  theme_minimal() + 
  theme(legend.position = "bottom", legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.y = element_text(size = 12, face = "bold"))
print(grafico_atitudes)

# ==============================================================================
# BLOCO 8: DIMENSÃO - NORMA SUBJETIVA (NS1 a NS5)
# ==============================================================================
obj_norma <- likert(df_graficos[, paste0("NS", 1:5)])
grafico_norma <- plot(obj_norma, colors = minhas_cores) + 
  ggtitle("Norma Subjetiva") +
  theme_minimal() + 
  theme(legend.position = "bottom", legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.y = element_text(size = 12, face = "bold"))
print(grafico_norma)

# ==============================================================================
# BLOCO 9: DIMENSÃO - CONTROLE COMPORTAMENTAL PERCEBIDO (CCP1 a CCP5)
# ==============================================================================
obj_controle <- likert(df_graficos[, paste0("CCP", 1:5)])
grafico_controle <- plot(obj_controle, colors = minhas_cores) + 
  ggtitle("Controle Comportamental Percebido") +
  theme_minimal() + 
  theme(legend.position = "bottom", legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.y = element_text(size = 12, face = "bold"))
print(grafico_controle)

# ==============================================================================
# PAINEL AGRUPADO (PATCHWORK)
# ==============================================================================
# Agora chamamos os gráficos prontos! Adicionei um comando para unificar a legenda.
painel_agrupado <- wrap_plots(grafico_pressao, 
                              grafico_barreiras,
                              grafico_praticas,
                              grafico_praticas_sociais, 
                              grafico_desempenho, 
                              grafico_intencao,
                              grafico_atitudes,
                              grafico_norma,
                              grafico_controle,
                              ncol = 3) +
  plot_layout(guides = 'collect') & 
  theme(legend.position = 'bottom')

print(painel_agrupado)

# ==============================================================================
# 4. ANÁLISE FATORIAL CONFIRMATÓRIA (AFC)
# ==============================================================================
cat("\nRodando Análise Fatorial Confirmatória...\n")

modelo_afc <- '
  Intencao               =~ INT1 + INT2 + INT3 + INT4
  Atitudes               =~ ATT1 + ATT2 + ATT3 + ATT4
  Praticas_Sociais       =~ PS1 + PS2 + PS3 + PS4 + PS5
  Desempenho_Operacional =~ D1 + D2 + D3 + D4 + D5 + D6 + D7 + D8 + D9 + D10
  Praticas               =~ PA1 + PA2 + PA3 + PA4 + PA5 + PA6 + PA7 + PA8
  Norma_Subjetiva        =~ NS1 + NS2 + NS3 + NS4 + NS5
  Controle               =~ CCP1 + CCP2 + CCP3 + CCP4 + CCP5
  Pressao                =~ P1 + P2 + P3 + P4 + P5 + P6
  Barreiras              =~ B1 + B2 + B3 + B4 + B5 + B6
'

ajuste_afc <- cfa(modelo_afc, 
                  data = df_itens, 
                  ordered = names(df_itens), 
                  estimator = "WLSMV")

# ==============================================================================
# 5. RESULTADOS DA AFC (MÉTRICAS GLOBAIS DE AJUSTE)
# ==============================================================================
summary(ajuste_afc, fit.measures = TRUE, standardized = TRUE)








# ==============================================================================
# ANÁLISE FATORIAL CONFIRMATÓRIA (MODELO REFINADO)
# ==============================================================================
library(lavaan)



# Retirado OS itens problemáticos (P6, B2, PA4)

cat("\nRodando AFC com o Modelo Refinado (Exclusão de P6, B2 e PA4)...\n")

# Especificação do Modelo Teórico Refinado
modelo_afc_refinado <- '
  Intencao               =~ INT1 + INT2 + INT3 + INT4
  Atitudes               =~ ATT1 + ATT2 + ATT3 + ATT4
  Praticas_Sociais       =~ PS1 + PS2 + PS3 + PS4 + PS5
  Desempenho_Operacional =~ D1 + D2 + D3 + D4 + D5 + D6 + D7 + D8 + D9 + D10
  # Removido o PA4:
  Praticas               =~ PA1 + PA2 + PA3 + PA5 + PA6 + PA7 + PA8 
  Norma_Subjetiva        =~ NS1 + NS2 + NS3 + NS4 + NS5
  Controle               =~ CCP1 + CCP2 + CCP3 + CCP4 + CCP5
  # Removido o P6:
  Pressao                =~ P1 + P2 + P3 + P4 + P5 
  # Removido o B2:
  Barreiras              =~ B1 + B3 + B4 + B5 + B6 
'

# Ajustando o novo modelo com WLSMV
ajuste_afc_refinado <- cfa(modelo_afc_refinado, 
                           data = df_itens, 
                           ordered = names(df_itens), 
                           estimator = "WLSMV")

# Resultados das Métricas Globais de Ajuste
summary(ajuste_afc_refinado, fit.measures = TRUE, standardized = TRUE)



# ==============================================================================
# ANÁLISE FATORIAL CONFIRMATÓRIA (MODELO CORRIGIDO - SUPER-CONSTRUTO)
# ==============================================================================


cat("\nRodando AFC com Super-Construto e exclusão do item clonado (ATT2)...\n")

# Especificação do Modelo Teórico Corrigido
modelo_afc_super <- '
  # 1. SUPER-CONSTRUTO: Intenção + Atitudes 
  # (Removido o item ATT2 por colinearidade perfeita com ATT1)
  Intencao_Atitude       =~ INT1 + INT2 + INT3 + INT4 + ATT1 + ATT3 + ATT4
  
  # 2. DEMAIS CONSTRUTOS (Com as limpezas anteriores mantidas)
  Praticas_Sociais       =~ PS1 + PS2 + PS3 + PS4 + PS5
  Desempenho_Operacional =~ D1 + D2 + D3 + D4 + D5 + D6 + D7 + D8 + D9 + D10
  Praticas               =~ PA1 + PA2 + PA3 + PA5 + PA6 + PA7 + PA8 
  Norma_Subjetiva        =~ NS1 + NS2 + NS3 + NS4 + NS5
  Controle               =~ CCP1 + CCP2 + CCP3 + CCP4 + CCP5
  Pressao                =~ P1 + P2 + P3 + P4 + P5 
  Barreiras              =~ B1 + B3 + B4 + B5 + B6 
'

# Ajustando o modelo corrigido com WLSMV
ajuste_afc_super <- cfa(modelo_afc_super, 
                        data = df_itens, 
                        ordered = names(df_itens), 
                        estimator = "WLSMV")

# Resultados Finais
summary(ajuste_afc_super, fit.measures = TRUE, standardized = TRUE)



# ==============================================================================
# ANÁLISE FATORIAL CONFIRMATÓRIA (MODELO FINAL - LIVRE DE ERROS)
# ==============================================================================


cat("\nRodando AFC Final (Remoção do item problemático B6)...\n")

# Especificação do Modelo Teórico Final
modelo_afc_final <- '
  # Super-construto fundido
  Intencao_Atitude       =~ INT1 + INT2 + INT3 + INT4 + ATT1 + ATT3 + ATT4
  
  Praticas_Sociais       =~ PS1 + PS2 + PS3 + PS4 + PS5
  Desempenho_Operacional =~ D1 + D2 + D3 + D4 + D5 + D6 + D7 + D8 + D9 + D10
  Praticas               =~ PA1 + PA2 + PA3 + PA5 + PA6 + PA7 + PA8 
  Norma_Subjetiva        =~ NS1 + NS2 + NS3 + NS4 + NS5
  Controle               =~ CCP1 + CCP2 + CCP3 + CCP4 + CCP5
  Pressao                =~ P1 + P2 + P3 + P4 + P5 
  
  # Dimensão Barreiras livre do item B6 que estrangula a matriz
  Barreiras              =~ B1 + B3 + B4 + B5 
'

# Ajustando o modelo
ajuste_afc_final <- cfa(modelo_afc_final, 
                        data = df_itens, 
                        ordered = names(df_itens), 
                        estimator = "WLSMV")

# Resultados
summary(ajuste_afc_final, fit.measures = TRUE, standardized = TRUE)




# ==============================================================================
# ANÁLISE FATORIAL CONFIRMATÓRIA (INTENÇÃO E ATITUDES SEPARADAS)
# ==============================================================================
library(lavaan)

cat("\nRodando AFC com Intenção e Atitudes separadas (Itens limpos)...\n")

# Especificação do Modelo
modelo_separado <- '
  # 1. Dimensões separadas (ATT2 removido por colinearidade perfeita)
  Intencao               =~ INT1 + INT2 + INT3 + INT4
  Atitudes               =~ ATT1 + ATT3 + ATT4
  
  # 2. Demais construtos com as limpezas anteriores (sem PA4, P6, B2, B6)
  Praticas_Sociais       =~ PS1 + PS2 + PS3 + PS4 + PS5
  Desempenho_Operacional =~ D1 + D2 + D3 + D4 + D5 + D6 + D7 + D8 + D9 + D10
  Praticas               =~ PA1 + PA2 + PA3 + PA5 + PA6 + PA7 + PA8 
  Norma_Subjetiva        =~ NS1 + NS2 + NS3 + NS4 + NS5
  Controle               =~ CCP1 + CCP2 + CCP3 + CCP4 + CCP5
  Pressao                =~ P1 + P2 + P3 + P4 + P5 
  Barreiras              =~ B1 + B3 + B4 + B5 
'

# Ajustando o modelo
ajuste_separado <- cfa(modelo_separado, 
                       data = df_itens, 
                       ordered = names(df_itens), 
                       estimator = "WLSMV")

# Resultados
summary(ajuste_separado, fit.measures = TRUE, standardized = TRUE)





# ==============================================================================
# SCRIPT COMPLETO E DEFINITIVO: MEE / SEM PARA A TESE DE DOUTORADO
# ==============================================================================

# 1. CARREGAMENTO DOS PACOTES

library(readxl)
library(dplyr)
library(lavaan)
library(semTools) # Fundamental para AVE, CR e Fornell-Larcker
library(semPlot)  # Para desenhar o diagrama de relações

# 2. PREPARAÇÃO DOS DADOS (Com a base de 416 respondentes)
dados <- read_excel("Análise_Marcelo_Vianna.xlsx", sheet = "Banco1")
itens_cols <- c(paste0("P", 1:6), paste0("B", 1:6), paste0("PA", 1:8),
                paste0("PS", 1:5), paste0("D", 1:10), paste0("INT", 1:4),
                paste0("ATT", 1:4), paste0("NS", 1:5), paste0("CCP", 1:5))
df_itens <- dados %>% select(all_of(itens_cols)) %>% na.omit()

# 3. ESPECIFICAÇÃO DO MODELO ESTRUTURAL COMPLETO
# Aqui combinamos o Modelo de Mensuração (=~) e o Modelo Estrutural (~)
modelo_sem <- '
  # ==========================================================
  # A. MODELO DE MENSURAÇÃO (Definindo as Variáveis Latentes)
  # Aplicando as limpezas de itens problemáticos identificados
  # ==========================================================
  
  # Usaremos o Super-Construto devido à altíssima colinearidade (ATT2 removido)
  Comportamento_Gestao   =~ INT1 + INT2 + INT3 + INT4 + ATT1 + ATT3 + ATT4
  
  Praticas               =~ PA1 + PA2 + PA3 + PA5 + PA6 + PA7 + PA8 
  Desempenho_Operacional =~ D1 + D2 + D3 + D4 + D5 + D6 + D7 + D8 + D9 + D10
  Pressao                =~ P1 + P2 + P3 + P4 + P5 
  Barreiras              =~ B1 + B3 + B4 + B5 
  
  # (As demais variáveis podem ser declaradas aqui se fizerem parte das hipóteses)

  # ==========================================================
  # B. MODELO ESTRUTURAL (Testando as Hipóteses H1, H2 e H3)
  # O til (~) significa "é influenciado por" (Regressão Latente)
  # ==========================================================
  
  # H1 e H2: Práticas são influenciadas por Pressões (+) e Barreiras (-)
  Praticas ~ Pressao + Barreiras
  
  # H3: Desempenho é influenciado pelas Práticas (+)
  Desempenho_Operacional ~ Praticas
'

# 4. AJUSTE DO MODELO ESTRUTURAL (Estimador WLSMV para Likert)
ajuste_sem <- sem(modelo_sem, 
                  data = df_itens, 
                  ordered = names(df_itens), 
                  estimator = "WLSMV")



# ==============================================================================
# SCRIPT COMPLEMENTAR: VISUALIZAÇÃO, VALIDADE DISCRIMINANTE E VIF
# ==============================================================================
# install.packages(c("semPlot", "car"))
library(semPlot)

cat("\n--- 1. VALIDADE DISCRIMINANTE (Matriz de Correlação Latente) ---\n")
# Extrai as correlações estimadas entre os construtos latentes
matriz_cor_latente <- lavInspect(ajuste_sem, "cor.lv")
print(matriz_cor_latente)
# DICA: Para haver validade discriminante, nenhum valor fora da diagonal 
# principal deve ser maior que 0.85 (ou 0.90 em casos flexíveis).

cat("\n--- 2. COLINEARIDADE ESTRUTURAL (VIF) ---\n")
# Em modelos SEM no lavaan, a colinearidade entre os construtos preditores 
# (Pressão e Barreiras) que explicam as Práticas é calculada com base na correlação.
# Fórmula do VIF: 1 / (1 - R²)
cor_pressao_barreiras <- matriz_cor_latente["Pressao", "Barreiras"]
vif_estrutural <- 1 / (1 - cor_pressao_barreiras^2)

cat("Correlação (Pressão e Barreiras):", round(cor_pressao_barreiras, 3), "\n")
cat("VIF Estrutural:", round(vif_estrutural, 3), "\n")
# DICA: O valor ideal do VIF é próximo a 1. Valores acima de 5.0 indicam colinearidade severa.

cat("\n--- 3. GERANDO O DIAGRAMA DO MODELO ESTRUTURAL (Gráfico) ---\n")
# Isso vai desenhar o modelo na aba "Plots" do seu RStudio
semPaths(ajuste_sem, 
         whatLabels = "std",       # Exibe as cargas padronizadas (betas) nas setas
         layout = "tree2",         # CORREÇÃO: Algoritmo em formato de árvore horizontal
         edge.color = "black",     # Cor das setas
         color = list(lat = "blue", man = "white"), # Cor dos nós
         sizeMan = 5,              # Tamanho das caixinhas dos itens (Perguntas)
         sizeLat = 8,              # Tamanho dos círculos dos construtos
         intercepts = FALSE,       # Oculta interceptos 
         residuals = FALSE,        # Oculta setas de erro residual 
         edge.label.cex = 0.9,     # Tamanho da fonte dos números nas setas
         mar = c(3, 3, 3, 3))


