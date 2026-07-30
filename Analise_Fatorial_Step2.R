# ==============================================================================
# MODELO AFC 3 (MODELO DE 6 DIMENSÕES - Todos Itens)
# ==============================================================================

# 1. CARREGAR PACOTES NECESSÁRIOS
library(readxl)   # Leitura do Excel
library(dplyr)    # Manipulação de dados
library(likert)   # Gráficos de escala Likert
library(ggplot2)  # Customização de gráficos
library(patchwork)# Agrupamento de gráficos
library(lavaan)   # Análise Fatorial Confirmatória e Equações Estruturais
library(semTools) # Métricas de validade e confiabilidade (AVE, CR)
library(semPlot)  # Desenho do diagrama de caminhos


# ------------------------------------------------------------------------------
# PASSO 2: LEITURA E PREPARAÇÃO DA BASE DE DADOS
# ------------------------------------------------------------------------------
# Lendo a planilha (certifique-se de que o arquivo está no mesmo diretório do script)
dados <- read_excel("Análise_Marcelo_Vianna.xlsx", sheet = "Banco1")

# Mapeando as colunas das 9 dimensões originais do instrumento
itens_cols <- c(paste0("P", 1:6), paste0("B", 1:6), paste0("PA", 1:8),
                paste0("PS", 1:5), paste0("D", 1:10), paste0("INT", 1:4),
                paste0("ATT", 1:4), paste0("NS", 1:5), paste0("CCP", 1:5))

# Selecionando apenas as colunas dos itens e removendo valores nulos (NA)
df_itens <- dados %>% select(all_of(itens_cols)) %>% na.omit()

cat("\n[✓] Passo 2 concluído: Dados carregados. Amostra N =", nrow(df_itens), "\n")


cat("\n====================================================================\n")
cat(" PASSO 1: ESPECIFICAÇÃO E ESTIMAÇÃO DO MODELO (6 DIMENSÕES)")
cat("\n====================================================================\n")

modelo_afc_6dim <- '
  Pressao                 =~ P1 + P2 + P3 + P4 + P5
  Barreiras               =~ B1 + B3 + B4 + B5
  Praticas                =~ PA1 + PA2 + PA3 + PA5 + PA6 + PA7 + PA8
  Desempenho_Operacional  =~ D1 + D2 + D3 + D4 + D5 + D6 + D7 + D8 + D9 + D10
  Intencao                =~ INT1 + INT2 + INT3 + INT4 
  Praticas_Sociais        =~ PS1 + PS2 + PS3 + PS4
'

ajuste_afc_6dim <- cfa(modelo_afc_6dim, 
                       data = dados, 
                       estimator = "WLSMV", 
                       ordered = TRUE)

cat("\n[✓] Modelo estimado com sucesso!\n")


cat("\n====================================================================\n")
cat(" PASSO 2: ÍNDICES DE AJUSTE GLOBAL E CARGAS FATORIAIS")
cat("\n====================================================================\n")
# Exibe as medidas de ajuste (CFI, TLI, RMSEA) e as estimativas do modelo
summary(ajuste_afc_6dim, standardized = TRUE, fit.measures = TRUE)


cat("\n====================================================================\n")
cat(" PASSO 3: CONFIABILIDADE (ALPHA, CR) E VALIDADE CONVERGENTE (AVE)")
cat("\n====================================================================\n")
# A função reliability() do pacote semTools extrai essas métricas automaticamente
metricas_validade <- reliability(ajuste_afc_6dim)

# Transpor a matriz para facilitar a leitura no console
metricas_formatadas <- t(metricas_validade)
print(round(metricas_formatadas, 3))

cat("\nCOMO LER ESTA TABELA:\n")
cat("- 'alpha': Alfa de Cronbach (deve ser > 0.70)\n")
cat("- 'omega': Confiabilidade Composta / CR (deve ser > 0.70)\n")
cat("- 'ave': Variância Média Extraída (deve ser > 0.50)\n")


cat("\n====================================================================\n")
cat(" PASSO 4: VALIDADE DISCRIMINANTE (FORNELL-LARCKER)")
cat("\n====================================================================\n")
# Extrai a matriz de correlação latente
matriz_correlacao <- lavInspect(ajuste_afc_6dim, "cor.lv")
print(round(matriz_correlacao, 3))

cat("\nCOMO APLICAR O CRITÉRIO (Manualmente):\n")
cat("- Calcule a Raiz Quadrada da AVE de cada dimensão (extraída no Passo 3).\n")
cat("- A raiz quadrada da AVE deve ser MAIOR que todas as correlações daquela \n  dimensão com as demais.\n")


cat("\n====================================================================\n")
cat(" PASSO 5: VALIDADE DISCRIMINANTE AVANÇADA (HTMT)")
cat("\n====================================================================\n")
# Calcula a razão HTMT
matriz_htmt <- htmt(modelo_afc_6dim, data = dados, ordered = TRUE)
print(round(matriz_htmt, 3))

cat("\nCOMO LER O HTMT:\n")
cat("- Todos os valores devem ser estritamente MENORES que 0.90 (ideal < 0.85).\n")
cat("- Valores acima disso indicam forte confusão entre os construtos.\n")


cat("\n====================================================================\n")
cat(" PASSO 6: DIAGRAMA DE CAMINHOS DA AFC")
cat("\n====================================================================\n")
# Plota o diagrama com as cargas padronizadas
semPaths(ajuste_afc_6dim, 
         what = "paths", 
         whatLabels = "std", 
         layout = "tree2", 
         edge.color = "black", 
         color = list(lat = "#EAEAEA", man = "#FFFFFF"),
         sizeMan = 3, 
         sizeLat = 7, 
         intercepts = FALSE, 
         residuals = FALSE, 
         edge.label.cex = 0.8, 
         label.cex = 1.0, 
         label.scale = FALSE,
         mar = c(2, 4, 2, 4),
         title = FALSE)

cat("\n[✓] O Diagrama foi gerado na aba 'Plots' do RStudio!\n")
cat("====================================================================\n")
#-------------------------------------------------------------------------------#



# ==============================================================================
# MODELO AFC (6 DIMENSÕES) - REMOVENDO APENAS O ITEM 'P1'
# ==============================================================================


cat("\n====================================================================\n")
cat(" PASSO 1: SINTAXE DO MODELO (6 DIMENSÕES - SEM P1)")
cat("\n====================================================================\n")

# O item P1 foi removido da Pressão
modelo_afc_6dim_v2 <- '
  Pressao                 =~ P2 + P3 + P4 + P5
  Barreiras               =~ B1 + B3 + B4 + B5
  Praticas                =~ PA1 + PA2 + PA3 + PA5 + PA6 + PA7 + PA8
  Desempenho_Operacional  =~ D1 + D2 + D3 + D4 + D5 + D6 + D7 + D8 + D9 + D10
  Intencao                =~ INT1 + INT2 + INT3 + INT4 
  Praticas_Sociais        =~ PS1 + PS2 + PS3 + PS4
'

ajuste_afc_6dim_v2 <- cfa(modelo_afc_6dim_v2, 
                          data = dados, 
                          estimator = "WLSMV", 
                          ordered = TRUE)

cat("\n[✓] Novo modelo estimado com sucesso!\n")


cat("\n====================================================================\n")
cat(" PASSO 2: ÍNDICES DE AJUSTE GLOBAL E CARGAS FATORIAIS")
cat("\n====================================================================\n")
summary(ajuste_afc_6dim_v2, 
        standardized = TRUE, 
        fit.measures = TRUE)


cat("\n====================================================================\n")
cat(" PASSO 3: CONFIABILIDADE (CR) E VALIDADE CONVERGENTE (AVE)")
cat("\n====================================================================\n")
# Extrai Alpha, CR (omega) e AVE
metricas_validade <- reliability(ajuste_afc_6dim_v2)
print(round(t(metricas_validade), 3))


cat("\n====================================================================\n")
cat(" PASSO 4: VALIDADE DISCRIMINANTE (FORNELL-LARCKER)")
cat("\n====================================================================\n")
matriz_correlacao <- lavInspect(ajuste_afc_6dim_v2, "cor.lv")
print(round(matriz_correlacao, 3))


cat("\n====================================================================\n")
cat(" PASSO 5: VALIDADE DISCRIMINANTE AVANÇADA (HTMT)")
cat("\n====================================================================\n")
matriz_htmt <- htmt(modelo_afc_6dim_v2, data = dados, ordered = TRUE)
print(round(matriz_htmt, 3))


cat("\n====================================================================\n")
cat(" PASSO 6: DIAGRAMA DE CAMINHOS DO NOVO MODELO")
cat("\n====================================================================\n")
semPaths(ajuste_afc_6dim_v2, 
         what = "paths", 
         whatLabels = "std", 
         layout = "tree2", 
         edge.color = "black", 
         color = list(lat = "#EAEAEA", man = "#FFFFFF"),
         sizeMan = 3, 
         sizeLat = 7, 
         intercepts = FALSE, 
         residuals = FALSE, 
         edge.label.cex = 0.8, 
         label.cex = 1.0, 
         label.scale = FALSE,
         mar = c(2, 4, 2, 4),
         title = FALSE)

cat("\n[✓] O Diagrama atualizado foi gerado!\n")
cat("====================================================================\n")
#-------------------------------------------------------------------------------#


# ==============================================================================
# SCRIPT R: MODELO AFC (6 DIMENSÕES) - REMOVENDO P1 E P3
# ==============================================================================



cat("\n====================================================================\n")
cat(" PASSO 1: SINTAXE DO MODELO (6 DIMENSÕES - SEM P1 e SEM P3)")
cat("\n====================================================================\n")

# Os itens P1 e P3 foram removidos da dimensão Pressão
modelo_afc_6dim_v3 <- '
  Pressao                 =~ P2 + P4 + P5
  Barreiras               =~ B1 + B3 + B4 + B5
  Praticas                =~ PA1 + PA2 + PA3 + PA5 + PA6 + PA7 + PA8
  Desempenho_Operacional  =~ D1 + D2 + D3 + D4 + D5 + D6 + D7 + D8 + D9 + D10
  Intencao                =~ INT1 + INT2 + INT3 + INT4 
  Praticas_Sociais        =~ PS1 + PS2 + PS3 + PS4
'

ajuste_afc_6dim_v3 <- cfa(modelo_afc_6dim_v3, 
                          data = dados, 
                          estimator = "WLSMV", 
                          ordered = TRUE)

cat("\n[✓] Novo modelo estimado com sucesso!\n")


cat("\n====================================================================\n")
cat(" PASSO 2: ÍNDICES DE AJUSTE GLOBAL E CARGAS FATORIAIS")
cat("\n====================================================================\n")
summary(ajuste_afc_6dim_v3, 
        standardized = TRUE, 
        fit.measures = TRUE)


cat("\n====================================================================\n")
cat(" PASSO 3: CONFIABILIDADE (CR) E VALIDADE CONVERGENTE (AVE)")
cat("\n====================================================================\n")
metricas_validade <- reliability(ajuste_afc_6dim_v3)
print(round(t(metricas_validade), 3))


cat("\n====================================================================\n")
cat(" PASSO 4: VALIDADE DISCRIMINANTE (FORNELL-LARCKER)")
cat("\n====================================================================\n")
matriz_correlacao <- lavInspect(ajuste_afc_6dim_v3, "cor.lv")
print(round(matriz_correlacao, 3))


cat("\n====================================================================\n")
cat(" PASSO 5: VALIDADE DISCRIMINANTE AVANÇADA (HTMT)")
cat("\n====================================================================\n")
matriz_htmt <- htmt(modelo_afc_6dim_v3, data = dados, ordered = TRUE)
print(round(matriz_htmt, 3))


cat("\n====================================================================\n")
cat(" PASSO 6: DIAGRAMA DE CAMINHOS DO NOVO MODELO")
cat("\n====================================================================\n")
semPaths(ajuste_afc_6dim_v3, 
         what = "paths", 
         whatLabels = "std", 
         layout = "tree2", 
         edge.color = "black", 
         color = list(lat = "#EAEAEA", man = "#FFFFFF"),
         sizeMan = 3, 
         sizeLat = 7, 
         intercepts = FALSE, 
         residuals = FALSE, 
         edge.label.cex = 0.8, 
         label.cex = 1.0, 
         label.scale = FALSE,
         mar = c(2, 4, 2, 4),
         title = FALSE)

cat("\n[✓] O Diagrama da V3 foi gerado!\n")
cat("====================================================================\n")
#-------------------------------------------------------------------------------#


# ==============================================================================
# SCRIPT R: RASTREAMENTO DE ANOMALIAS (ÍNDICES DE MODIFICAÇÃO)
# ==============================================================================


cat("\n====================================================================\n")
cat(" INVESTIGAÇÃO CLÍNICA: ÍNDICES DE MODIFICAÇÃO (CROSS-LOADINGS)")
cat("\n====================================================================\n")

# 1. Extrai todos os índices de modificação do modelo V3
indices_mod <- modindices(ajuste_afc_6dim_v3)

# 2. Filtra APENAS as tentativas de "Cargas Cruzadas" (op == "=~")
cargas_cruzadas <- subset(indices_mod, op == "=~")

# 3. Filtra para mostrar apenas problemas graves (MI > 10) e ordena
cargas_cruzadas_graves <- cargas_cruzadas[cargas_cruzadas$mi > 10, ]
cargas_cruzadas_graves <- cargas_cruzadas_graves[order(-cargas_cruzadas_graves$mi), ]

cat("\n[!] MAIORES CARGAS CRUZADAS (Itens querendo mudar de dimensão):\n")
print(head(cargas_cruzadas_graves, 15))


cat("\n====================================================================\n")
cat(" INVESTIGAÇÃO CLÍNICA: COVARIÂNCIAS DE ERRO RESIDUAL")
cat("\n====================================================================\n")

# 4. Verifica erros altamente correlacionados (op == "~~")
# Corrigido para as siglas corretas do lavaan: 'lhs' e 'rhs'
itens_alvo <- c("PS1", "PS2", "PS3", "PS4", "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10")

erros_correlacionados <- subset(indices_mod, op == "~~" & 
                                  (lhs %in% itens_alvo | rhs %in% itens_alvo))

# Filtra problemas graves nos erros (MI > 15)
erros_graves <- erros_correlacionados[erros_correlacionados$mi > 15, ]
erros_graves <- erros_graves[order(-erros_graves$mi), ]

cat("\n[!] MAIORES CORRELAÇÕES DE ERRO (Perguntas com texto/sentido redundante):\n")
print(head(erros_graves, 15))
cat("====================================================================\n")







# ==============================================================================
# SCRIPT R: MODELO AFC V4 (6 DIMENSÕES) - REMOVENDO P1, P3 E D8
# ==============================================================================


cat("\n====================================================================\n")
cat(" PASSO 1: SINTAXE DO MODELO (6 DIMENSÕES - SEM P1, P3 e D8)")
cat("\n====================================================================\n")

# - P1 e P3 ausentes em Pressao (para manter a AVE > 0.48 / CR > 0.70)
# - D8 removido em Desempenho_Operacional (para curar Cargas Cruzadas e Fornell-Larcker)
modelo_afc_6dim_v4 <- '
  Pressao                 =~ P2 + P4 + P5
  Barreiras               =~ B1 + B3 + B4 + B5
  Praticas                =~ PA1 + PA2 + PA3 + PA5 + PA6 + PA7 + PA8
  Desempenho_Operacional  =~ D1 + D2 + D3 + D4 + D5 + D6 + D7 + D9 + D10
  Intencao                =~ INT1 + INT2 + INT3 + INT4 
  Praticas_Sociais        =~ PS1 + PS2 + PS3 + PS4
'

ajuste_afc_6dim_v4 <- cfa(modelo_afc_6dim_v4, 
                          data = dados, 
                          estimator = "WLSMV", 
                          ordered = TRUE)

cat("\n[✓] Modelo V4 estimado com sucesso!\n")


cat("\n====================================================================\n")
cat(" PASSO 2: ÍNDICES DE AJUSTE GLOBAL E CARGAS FATORIAIS")
cat("\n====================================================================\n")
summary(ajuste_afc_6dim_v4, 
        standardized = TRUE, 
        fit.measures = TRUE)


cat("\n====================================================================\n")
cat(" PASSO 3: CONFIABILIDADE (CR) E VALIDADE CONVERGENTE (AVE)")
cat("\n====================================================================\n")
metricas_validade <- reliability(ajuste_afc_6dim_v4)
print(round(t(metricas_validade), 3))


cat("\n====================================================================\n")
cat(" PASSO 4: VALIDADE DISCRIMINANTE (FORNELL-LARCKER)")
cat("\n====================================================================\n")
matriz_correlacao <- lavInspect(ajuste_afc_6dim_v4, "cor.lv")
print(round(matriz_correlacao, 3))


cat("\n====================================================================\n")
cat(" PASSO 5: VALIDADE DISCRIMINANTE AVANÇADA (HTMT)")
cat("\n====================================================================\n")
matriz_htmt <- htmt(modelo_afc_6dim_v4, data = dados, ordered = TRUE)
print(round(matriz_htmt, 3))


cat("\n====================================================================\n")
cat(" PASSO 6: DIAGRAMA DE CAMINHOS DO MODELO V4")
cat("\n====================================================================\n")
semPaths(ajuste_afc_6dim_v4, 
         what = "paths", 
         whatLabels = "std", 
         layout = "tree2", 
         edge.color = "black", 
         color = list(lat = "#EAEAEA", man = "#FFFFFF"),
         sizeMan = 3, 
         sizeLat = 7, 
         intercepts = FALSE, 
         residuals = FALSE, 
         edge.label.cex = 0.8, 
         label.cex = 1.0, 
         label.scale = FALSE,
         mar = c(2, 4, 2, 4),
         title = FALSE)

cat("\n[✓] O Diagrama da V4 foi gerado!\n")
cat("====================================================================\n")




# ==============================================================================
# SCRIPT R: MODELO AFC V5 (6 DIMENSÕES) - REMOVENDO P1, P3, D8 e D1
# ==============================================================================


cat("\n====================================================================\n")
cat(" PASSO 1: SINTAXE DO MODELO (6 DIMENSÕES - SEM P1, P3, D8 e D1)")
cat("\n====================================================================\n")

# - Pressao: P2, P4, P5
# - Desempenho_Operacional: D2, D3, D4, D5, D6, D7, D9, D10 (D1 removido)
modelo_afc_6dim_v5 <- '
  Pressao                 =~ P2 + P4 + P5
  Barreiras               =~ B1 + B3 + B4 + B5
  Praticas                =~ PA1 + PA2 + PA3 + PA5 + PA6 + PA7 + PA8
  Desempenho_Operacional  =~ D2 + D3 + D4 + D5 + D6 + D7 + D9 + D10
  Intencao                =~ INT1 + INT2 + INT3 + INT4 
  Praticas_Sociais        =~ PS1 + PS2 + PS3 + PS4
'

ajuste_afc_6dim_v5 <- cfa(modelo_afc_6dim_v5, 
                          data = dados, 
                          estimator = "WLSMV", 
                          ordered = TRUE)

cat("\n[✓] Modelo V5 estimado com sucesso!\n")


cat("\n====================================================================\n")
cat(" PASSO 2: ÍNDICES DE AJUSTE GLOBAL E CARGAS FATORIAIS")
cat("\n====================================================================\n")
summary(ajuste_afc_6dim_v5, 
        standardized = TRUE, 
        fit.measures = TRUE)


cat("\n====================================================================\n")
cat(" PASSO 3: CONFIABILIDADE (CR) E VALIDADE CONVERGENTE (AVE)")
cat("\n====================================================================\n")
metricas_validade <- reliability(ajuste_afc_6dim_v5)
print(round(t(metricas_validade), 3))


cat("\n====================================================================\n")
cat(" PASSO 4: VALIDADE DISCRIMINANTE (FORNELL-LARCKER)")
cat("\n====================================================================\n")
matriz_correlacao <- lavInspect(ajuste_afc_6dim_v5, "cor.lv")
print(round(matriz_correlacao, 3))


cat("\n====================================================================\n")
cat(" PASSO 5: VALIDADE DISCRIMINANTE AVANÇADA (HTMT)")
cat("\n====================================================================\n")
matriz_htmt <- htmt(modelo_afc_6dim_v5, data = dados, ordered = TRUE)
print(round(matriz_htmt, 3))


cat("\n====================================================================\n")
cat(" PASSO 6: DIAGRAMA DE CAMINHOS DO MODELO V5")
cat("\n====================================================================\n")
semPaths(ajuste_afc_6dim_v5, 
         what = "paths", 
         whatLabels = "std", 
         layout = "tree2", 
         edge.color = "black", 
         color = list(lat = "#EAEAEA", man = "#FFFFFF"),
         sizeMan = 3, 
         sizeLat = 7, 
         intercepts = FALSE, 
         residuals = FALSE, 
         edge.label.cex = 0.8, 
         label.cex = 1.0, 
         label.scale = FALSE,
         mar = c(2, 4, 2, 4),
         title = FALSE)

cat("\n[✓] O Diagrama da V5 foi gerado!\n")
cat("====================================================================\n")





# ==============================================================================
# SCRIPT R: MODELO AFC V6 (6 DIMENSÕES) - REMOVENDO P1, P3, D8, D1 E D6
# ==============================================================================

library(lavaan)
library(semTools)
library(semPlot)

cat("\n====================================================================\n")
cat(" PASSO 1: SINTAXE DO MODELO (6 DIMENSÕES - SEM P1, P3, D8, D1 e D6)")
cat("\n====================================================================\n")

# - Pressao: P2, P4, P5
# - Desempenho_Operacional: D2, D3, D4, D5, D7, D9, D10 (D1, D6 e D8 removidos)
modelo_afc_6dim_v6 <- '
  Pressao                 =~ P2 + P4 + P5
  Barreiras               =~ B1 + B3 + B4 + B5
  Praticas                =~ PA1 + PA2 + PA3 + PA5 + PA6 + PA7 + PA8
  Desempenho_Operacional  =~ D2 + D3 + D4 + D5 + D7 + D9 + D10
  Intencao                =~ INT1 + INT2 + INT3 + INT4 
  Praticas_Sociais        =~ PS1 + PS2 + PS3 + PS4
'

ajuste_afc_6dim_v6 <- cfa(modelo_afc_6dim_v6, 
                          data = dados, 
                          estimator = "WLSMV", 
                          ordered = TRUE)

cat("\n[✓] Modelo V6 estimado com sucesso!\n")


cat("\n====================================================================\n")
cat(" PASSO 2: ÍNDICES DE AJUSTE GLOBAL E CARGAS FATORIAIS")
cat("\n====================================================================\n")
summary(ajuste_afc_6dim_v6, standardized = TRUE, fit.measures = TRUE)


cat("\n====================================================================\n")
cat(" PASSO 3: CONFIABILIDADE (CR) E VALIDADE CONVERGENTE (AVE)")
cat("\n====================================================================\n")
metricas_validade <- reliability(ajuste_afc_6dim_v6)
print(round(t(metricas_validade), 3))


cat("\n====================================================================\n")
cat(" PASSO 4: VALIDADE DISCRIMINANTE (FORNELL-LARCKER)")
cat("\n====================================================================\n")
matriz_correlacao <- lavInspect(ajuste_afc_6dim_v6, "cor.lv")
print(round(matriz_correlacao, 3))


cat("\n====================================================================\n")
cat(" PASSO 5: VALIDADE DISCRIMINANTE AVANÇADA (HTMT)")
cat("\n====================================================================\n")
matriz_htmt <- htmt(modelo_afc_6dim_v6, data = dados, ordered = TRUE)
print(round(matriz_htmt, 3))


cat("\n====================================================================\n")
cat(" PASSO 6: DIAGRAMA DE CAMINHOS DO MODELO V6")
cat("\n====================================================================\n")
semPaths(ajuste_afc_6dim_v6, 
         what = "paths", 
         whatLabels = "std", 
         layout = "tree2", 
         edge.color = "black", 
         color = list(lat = "#EAEAEA", man = "#FFFFFF"),
         sizeMan = 3, 
         sizeLat = 7, 
         intercepts = FALSE, 
         residuals = FALSE, 
         edge.label.cex = 0.8, 
         label.cex = 1.0, 
         label.scale = FALSE,
         mar = c(2, 4, 2, 4),
         title = FALSE)

cat("\n[✓] O Diagrama da V6 foi gerado!\n")
cat("====================================================================\n")







# ==============================================================================
# SCRIPT R: MODELO AFC FINAL (5 DIMENSÕES) 
# ==============================================================================

cat("\n====================================================================\n")
cat(" PASSO 1: SINTAXE DO MODELO DEFINITIVO (5 DIMENSÕES)")
cat("\n====================================================================\n")

# - Práticas Sociais: Removida inteiramente (Falha no Fornell-Larcker).
# - Pressao: P1 e P3 ausentes (Removidos para curar a AVE).
# - Desempenho_Operacional: D1, D6 e D8 ausentes (Removidos por anomalia na matriz).

modelo_afc_final <- '
  Pressao                 =~ P2 + P4 + P5
  Barreiras               =~ B1 + B3 + B4 + B5
  Praticas                =~ PA1 + PA2 + PA3 + PA5 + PA6 + PA7 + PA8
  Desempenho_Operacional  =~ D2 + D3 + D4 + D5 + D7 + D9 + D10
  Intencao                =~ INT1 + INT2 + INT3 + INT4 
'

# Ajuste do modelo final
ajuste_afc_final <- cfa(modelo_afc_final, 
                        data = dados, 
                        estimator = "WLSMV", 
                        ordered = TRUE)

cat("\n[✓] Modelo FINAL estimado com sucesso!\n")


cat("\n====================================================================\n")
cat(" PASSO 2: ÍNDICES DE AJUSTE GLOBAL E CARGAS FATORIAIS")
cat("\n====================================================================\n")
summary(ajuste_afc_final, standardized = TRUE, fit.measures = TRUE)


cat("\n====================================================================\n")
cat(" PASSO 3: CONFIABILIDADE (CR) E VALIDADE CONVERGENTE (AVE)")
cat("\n====================================================================\n")
metricas_validade <- reliability(ajuste_afc_final)
print(round(t(metricas_validade), 3))


cat("\n====================================================================\n")
cat(" PASSO 4: VALIDADE DISCRIMINANTE (FORNELL-LARCKER)")
cat("\n====================================================================\n")
matriz_correlacao <- lavInspect(ajuste_afc_final, "cor.lv")
print(round(matriz_correlacao, 3))


cat("\n====================================================================\n")
cat(" PASSO 5: VALIDADE DISCRIMINANTE AVANÇADA (HTMT)")
cat("\n====================================================================\n")
matriz_htmt <- htmt(modelo_afc_final, data = dados, ordered = TRUE)
print(round(matriz_htmt, 3))



#----------------------------------------------------------------------------#
# MODELO DE MENSURAÇÃO (AFC)

cat("\n====================================================================\n")
cat(" PASSO 6: DIAGRAMA DE CAMINHOS DO MODELO FINAL")
cat("\n====================================================================\n")
semPaths(ajuste_afc_final, 
         what = "paths", 
         whatLabels = "std", 
         layout = "tree2", 
         edge.color = "black", 
         color = list(lat = "#EAEAEA", man = "#FFFFFF"),
         sizeMan = 3.5, 
         sizeLat = 8, 
         intercepts = FALSE, 
         residuals = FALSE, 
         edge.label.cex = 0.9, 
         label.cex = 1.1, 
         label.scale = FALSE,
         mar = c(3, 5, 3, 5),
         title = FALSE)

cat("\n[✓] O Diagrama Definitivo da Tese foi gerado!\n")
cat("====================================================================\n")


# ==============================================================================
# PASSO 6: DIAGRAMA DE CAMINHOS (ESTÉTICA SMARTPLS)
# ==============================================================================


# 1. Definindo a paleta de cores exata do SmartPLS
cores_pls <- list(
  lat = "#3399FF",  # Azul vibrante para as Dimensões (Círculos)
  man = "#FFFF99"   # Amarelo claro para as Variáveis Observáveis (Retângulos)
)

# 2. Gerando o gráfico com os parâmetros de design ajustados
semPaths(ajuste_afc_final, 
         what = "paths", 
         whatLabels = "std",       # Mostra as cargas fatoriais (como no PLS)
         layout = "tree",       # Organiza dimensões no centro e itens ao redor
         nCharNodes = 0,           # 0 = NUNCA abreviar os nomes dos construtos
         shapeLat = "circle",      # Formato redondo para dimensões
         shapeMan = "rectangle",   # Formato quadrado para itens
         sizeLat = 11,             # Aumenta o tamanho do círculo azul
         sizeMan = 6,              # Ajusta o tamanho do retângulo amarelo
         color = cores_pls,        # Aplica nossa paleta de cores
         edge.color = "black",     # Setas pretas, como no PLS
         border.color = "black",   # Borda preta fina nos nós
         label.color = "black",    # Texto interno em preto
         label.cex = 0.9,          # Tamanho da fonte dos nomes
         edge.label.cex = 0.8,     # Tamanho da fonte dos números nas setas
         intercepts = FALSE,       # Limpa o gráfico de interceptos
         residuals = FALSE,        # Oculta os "erros" dos itens (igual à foto do PLS)
         curvePivot = TRUE,        # Suaviza as curvas de correlação
         mar = c(3, 3, 3, 3),      # Margens para o gráfico não cortar
         title = FALSE)

cat("\n[✓] O Diagrama no estilo SmartPLS foi gerado na aba 'Plots'!\n")
# ==============================================================================









# ==============================================================================
# MODELO ESTRUTURAL DEFINITIVO (SEM) - TESTE DE HIPÓTESES
# ==============================================================================



cat("\n====================================================================\n")
cat(" PASSO 1: SINTAXE DO MODELO ESTRUTURAL (SEM)")
cat("\n====================================================================\n")

modelo_sem_final <- '
  # ----------------------------------------------------------------------
  # 1. MODELO DE MENSURAÇÃO (As 5 dimensões validadas e limpas)
  # ----------------------------------------------------------------------
  Pressao                 =~ P2 + P4 + P5
  Barreiras               =~ B1 + B3 + B4 + B5
  Praticas                =~ PA1 + PA2 + PA3 + PA5 + PA6 + PA7 + PA8
  Desempenho_Operacional  =~ D2 + D3 + D4 + D5 + D7 + D9 + D10
  Intencao                =~ INT1 + INT2 + INT3 + INT4 

  # ----------------------------------------------------------------------
  # 2. MODELO ESTRUTURAL (Suas Hipóteses - Causa e Efeito)
  # ATENÇÃO: Altere estas linhas para refletirem as setas da sua tese!
  # O símbolo "~" deve ser lido como "é causado por" ou "é influenciado por"
  # ----------------------------------------------------------------------
  Intencao                ~ Pressao + Barreiras
  Praticas                ~ Intencao + Barreiras + Pressao
  Desempenho_Operacional  ~ Praticas
'

# Ajustando o modelo estrutural (notar o uso da função sem() em vez de cfa())
ajuste_sem <- sem(modelo_sem_final, 
                  data = dados, 
                  estimator = "WLSMV", 
                  ordered = TRUE)

cat("\n[✓] Modelo Estrutural estimado com sucesso!\n")


cat("\n====================================================================\n")
cat(" PASSO 2: ÍNDICES DE AJUSTE DO MODELO ESTRUTURAL")
cat("\n====================================================================\n")
# Verifica se a imposição das setas direcionais não piorou a matriz
summary(ajuste_sem, 
        fit.measures = TRUE, 
        standardized = TRUE)


cat("\n====================================================================\n")
cat(" PASSO 3: TESTE DE HIPÓTESES (A DECISÃO DA TESE)")
cat("\n====================================================================\n")
# Extrai estritamente os caminhos estruturais (operador ~)
parametros <- parameterEstimates(ajuste_sem, standardized = TRUE)
hipoteses <- subset(parametros, op == "~")

# Formatando a tabela para você colar direto no Word
hipoteses_limpo <- data.frame(
  Variavel_Dependente = hipoteses$lhs,
  Seta = "<-",
  Variavel_Independente = hipoteses$rhs,
  Beta_Padronizado = round(hipoteses$std.all, 3),
  Erro_Padrao = round(hipoteses$se, 3),
  Estatistica_Z = round(hipoteses$z, 3),
  P_Valor = round(hipoteses$pvalue, 3)
)
print(hipoteses_limpo)


cat("\n====================================================================\n")
cat(" PASSO 4: PODER EXPLICATIVO (R-QUADRADO)")
cat("\n====================================================================\n")
# Mostra o % de explicação de cada variável dependente
r_quadrado <- lavInspect(ajuste_sem, "rsquare")
print(round(r_quadrado, 3))


cat("\n====================================================================\n")
cat(" PASSO 5: DIAGRAMA DE CAMINHOS (ESTÉTICA SMARTPLS HIERÁRQUICA)")
cat("\n====================================================================\n")
cores_pls <- list(lat = "#3399FF", man = "#FFFF99")

semPaths(ajuste_sem, 
         what = "paths", 
         whatLabels = "std",       
         layout = "tree2",         # Força o diagrama em árvore (fim da bola!)
         nCharNodes = 0,           
         shapeLat = "circle",      
         shapeMan = "rectangle",   
         sizeLat = 10,             
         sizeMan = 5,              
         color = cores_pls,        
         edge.color = "black",     
         border.color = "black",   
         label.color = "black",    
         label.cex = 0.9,          
         edge.label.cex = 0.7,     
         intercepts = FALSE,       
         residuals = FALSE,        
         mar = c(2, 6, 2, 6),      
         title = FALSE)

cat("\n[✓] Tudo pronto! Verifique o gráfico e a tabela de hipóteses.\n")
cat("====================================================================\n")


# ==============================================================================
# SCRIPT R: MODELO ESTRUTURAL BASE (H1, H2 e H3)
# ==============================================================================

cat("\n====================================================================\n")
cat(" PASSO 1: SINTAXE DO MODELO ESTRUTURAL (EFEITOS DIRETOS)")
cat("\n====================================================================\n")

modelo_sem_base <- '
  # ----------------------------------------------------------------------
  # 1. MODELO DE MENSURAÇÃO (As 5 dimensões limpas e validadas)
  # ----------------------------------------------------------------------
  Pressao                 =~ P2 + P4 + P5
  Barreiras               =~ B1 + B3 + B4 + B5
  Praticas                =~ PA1 + PA2 + PA3 + PA5 + PA6 + PA7 + PA8
  Desempenho_Operacional  =~ D2 + D3 + D4 + D5 + D7 + D9 + D10
  
  # A dimensão Intenção é mantida aqui para ser usada na moderação depois
  Intencao                =~ INT1 + INT2 + INT3 + INT4 

  # ----------------------------------------------------------------------
  # 2. MODELO ESTRUTURAL (Teste de H1, H2 e H3)
  # ----------------------------------------------------------------------
  # H1 e H2: Práticas são impactadas por Pressões (+) e Barreiras (-)
  Praticas                ~ Pressao + Barreiras
  
  # H3: Desempenho Operacional é impactado por Práticas (+)
  Desempenho_Operacional  ~ Praticas
'

# Ajustando o modelo estrutural base
ajuste_sem_base <- sem(modelo_sem_base, 
                       data = dados, 
                       estimator = "WLSMV", 
                       ordered = TRUE)

cat("\n[✓] Modelo Estrutural Base estimado com sucesso!\n")


cat("\n====================================================================\n")
cat(" PASSO 2: TESTE DE HIPÓTESES (H1, H2 e H3)")
cat("\n====================================================================\n")
# Extrai apenas os caminhos estruturais (operador ~)
parametros <- parameterEstimates(ajuste_sem_base, standardized = TRUE)
hipoteses <- subset(parametros, op == "~")

hipoteses_limpo <- data.frame(
  Variavel_Dependente = hipoteses$lhs,
  Seta = "<-",
  Variavel_Independente = hipoteses$rhs,
  Beta_Padronizado = round(hipoteses$std.all, 3),
  Erro_Padrao = round(hipoteses$se, 3),
  Estatistica_Z = round(hipoteses$z, 3),
  P_Valor = round(hipoteses$pvalue, 3)
)
print(hipoteses_limpo)


cat("\n====================================================================\n")
cat(" PASSO 3: DIAGRAMA DE CAMINHOS HIERÁRQUICO")
cat("\n====================================================================\n")

# 1. Cria uma copia temporária do modelo ajustado para alterar os rótulos de exibição
ajuste_plot <- ajuste_sem_base

# 2. Modifica a sintaxe dos nomes latentes substituindo o underline por quebra de linha (\n)
names(ajuste_plot@Model@GLIST$beta)

# Subtituição direta dos nomes das variáveis para exibição visual
layout_labels <- lavNames(ajuste_plot, "lv")
layout_labels[layout_labels == "Desempenho_Operacional"] <- "Desempenho\nOperacional"

cores_pls <- list(lat = "#3399FF", man = "#FFFF99")

# 3. Roda o gráfico com a quebra de linha aplicada
semPaths(ajuste_sem_base, 
         what = "paths", 
         whatLabels = "std",       
         layout = "tree2",         
         rotation = 2,             
         nCharNodes = 0,           
         nodeLabels = c("P2","P4","P5","B1","B3","B4","B5","PA1","PA2","PA3","PA5","PA6","PA7","PA8","D2","D3","D4","D5","D7","D9","D10","INT1","INT2","INT3","INT4",
                        "Pressao", "Barreiras", "Praticas", "Desempenho\nOperacional", "Intencao"),
         shapeLat = "circle",      
         shapeMan = "rectangle",   
         
         # --- TAMANHOS E FONTES ---
         sizeLat = 14,             # Círculos bem proporcionais
         sizeMan = 6,              # Retângulos dos itens
         label.cex = 1,          # Fonte grande e perfeitamente visível
         edge.label.cex = 1.0,     # Números das setas legíveis
         
         color = cores_pls,        
         edge.color = "black",     
         border.color = "black",   
         label.color = "black",    
         intercepts = FALSE,       
         residuals = FALSE,        
         mar = c(3, 8, 3, 8),      
         title = FALSE)

cat("\n[✓] Verifique o gráfico. Ele deve ter um fluxo limpo agora!\n")
cat("====================================================================\n")





















# ==============================================================================
# SCRIPT R: TESTE DE MODERAÇÃO (H4a e H4b) VIA ESCORES FATORIAIS
# ==============================================================================


cat("\n====================================================================\n")
cat(" PASSO 1: EXTRAÇÃO DE ESCORES E CRIAÇÃO DAS MODERAÇÕES")
cat("\n====================================================================\n")

# 1. Extrai as "notas" (escores latentes) do nosso modelo CFA perfeito
escores <- as.data.frame(lavPredict(ajuste_afc_final))

# 2. Cria os Termos de Interação (A mágica matemática da moderação)
escores$Mod_Pressao <- escores$Pressao * escores$Intencao
escores$Mod_Barreiras <- escores$Barreiras * escores$Intencao

cat("\n[✓] Escores extraídos e termos de moderação criados com sucesso!\n")


cat("\n====================================================================\n")
cat(" PASSO 2: SINTAXE DO MODELO ESTRUTURAL COMPLETO")
cat("\n====================================================================\n")

# Agora testamos todas as hipóteses juntas (Path Analysis)
modelo_moderacao <- '
  # H1, H2, H4a e H4b: O que impacta as Práticas?
  # (Incluímos a Intenção sozinha também para não enviesar a moderação)
  Praticas ~ Pressao + Barreiras + Intencao + Mod_Pressao + Mod_Barreiras
  
  # H3: O que impacta o Desempenho?
  Desempenho_Operacional ~ Praticas
'

# Ajusta o modelo (usamos o padrão ML pois os escores agora são números contínuos)
ajuste_moderacao <- sem(modelo_moderacao, data = escores)
cat("\n[✓] Modelo de Moderação estimado com sucesso!\n")


cat("\n====================================================================\n")
cat(" PASSO 3: RESULTADOS DAS HIPÓTESES FINAIS (O LAUDO DA TESE)")
cat("\n====================================================================\n")
parametros_mod <- parameterEstimates(ajuste_moderacao, standardized = TRUE)
hipoteses_mod <- subset(parametros_mod, op == "~")

tabela_final <- data.frame(
  Variavel_Dependente = hipoteses_mod$lhs,
  Seta = "<-",
  Variavel_Independente = hipoteses_mod$rhs,
  Beta = round(hipoteses_mod$std.all, 3),
  P_Valor = round(hipoteses_mod$pvalue, 3)
)
print(tabela_final)


cat("\n====================================================================\n")
cat(" PASSO 4: DIAGRAMA ESTRUTURAL (ESTILO PLS)")
cat("\n====================================================================\n")
# Usaremos o azul vibrante para todas as variáveis agora
cores_pls <- list(man = "#3399FF") 

semPaths(ajuste_moderacao, 
         what = "paths", 
         whatLabels = "std",       
         layout = "tree2",         # Mantém o fluxo da esquerda para direita
         rotation = 2,             
         sizeMan = 12,             # Caixas maiores
         color = cores_pls,        
         edge.color = "black",     
         border.color = "black",   
         label.color = "black",    
         label.cex = 0.8,          
         edge.label.cex = 0.8,     
         intercepts = FALSE,       
         residuals = FALSE,        
         mar = c(3, 8, 3, 8),      
         title = FALSE)

cat("\n[✓] Gráfico final gerado na aba 'Plots'!\n")
cat("====================================================================\n")

# NOTA DE AJUSTE: O CÓDIGO DO R² FOI MOVIDO PARA CÁ (Após a criação dos modelos)
cat("\n=== R-QUADRADO: MODELO ESTRUTURAL BASE ===\n")
print(round(lavInspect(ajuste_sem_base, "rsquare"), 3))

cat("\n=== R-QUADRADO: MODELO COM MODERAÇÃO ===\n")
print(round(lavInspect(ajuste_moderacao, "rsquare"), 3))



# ==============================================================================
# INCLUSÕES ESPECIAIS: TESTES AVANÇADOS PARA BLINDAR A TESE
# ==============================================================================

cat("\n====================================================================\n")
cat(" INCLUSÃO 1: TESTE DO VIÉS DE MÉTODO COMUM (CMB) - HARMAN")
cat("\n====================================================================\n")
# Colocamos TODOS os itens finais do modelo em um único "Fator_Geral"
modelo_harman <- '
  Fator_Geral =~ P2 + P4 + P5 + B1 + B3 + B4 + B5 + PA1 + PA2 + PA3 + PA5 + PA6 + PA7 + PA8 + D2 + D3 + D4 + D5 + D7 + D9 + D10 + INT1 + INT2 + INT3 + INT4
'

# Ajusta o modelo de fator único
ajuste_harman <- cfa(modelo_harman, 
                     data = dados, 
                     estimator = "WLSMV", 
                     ordered = TRUE)

# Extrai os índices de ajuste para comparar com o nosso modelo oficial de 5 dimensões
indices_harman <- fitMeasures(ajuste_harman, c("cfi", "tli", "rmsea", "srmr"))
print(round(indices_harman, 3))

cat("\n[!] INTERPRETAÇÃO (CMB):\n")
cat("- O modelo de fator único deve ter um ajuste muito ruim (CFI < 0.90, RMSEA alto).\n")
cat("- Isso prova que a variância dos seus dados NÃO foi causada por viés de coleta.\n")


cat("\n====================================================================\n")
cat(" INCLUSÃO 2: EFEITOS INDIRETOS (TESTE DE MEDIAÇÃO)")
cat("\n====================================================================\n")
# Nós recriamos o modelo base, mas agora "batizamos" as setas com letras 
# (a1, a2, b) para o R entender quem ele deve multiplicar.
modelo_mediacao <- '
  # 1. MENSURAÇÃO
  Pressao                 =~ P2 + P4 + P5
  Barreiras               =~ B1 + B3 + B4 + B5
  Praticas                =~ PA1 + PA2 + PA3 + PA5 + PA6 + PA7 + PA8
  Desempenho_Operacional  =~ D2 + D3 + D4 + D5 + D7 + D9 + D10

  # 2. EFEITOS DIRETOS (Rótulos)
  Praticas                ~ a1*Pressao + a2*Barreiras
  Desempenho_Operacional  ~ b*Praticas

  # 3. EFEITOS INDIRETOS (Mediação = multiplicar o caminho "a" pelo "b")
  Indireto_Pressao   := a1 * b
  Indireto_Barreiras := a2 * b
'

# Roda a modelagem
ajuste_mediacao <- sem(modelo_mediacao, 
                       data = dados, 
                       estimator = "WLSMV", 
                       ordered = TRUE)

# Extrai exclusivamente as fórmulas criadas (operador :=)
parametros_med <- parameterEstimates(ajuste_mediacao, standardized = TRUE)
efeitos_indiretos <- subset(parametros_med, op == ":=")

# Tabela formatada
tabela_mediacao <- data.frame(
  Trajetoria_Indireta = c("Pressão -> Práticas -> Desempenho", 
                          "Barreiras -> Práticas -> Desempenho"),
  Beta_Indireto = round(efeitos_indiretos$std.all, 3),
  Erro_Padrao = round(efeitos_indiretos$se, 3),
  Z_Valor = round(efeitos_indiretos$z, 3),
  P_Valor = round(efeitos_indiretos$pvalue, 3)
)

print(tabela_mediacao)

cat("\n[!] INTERPRETAÇÃO (MEDIAÇÃO):\n")
cat("- Se o P_Valor for < 0.05, significa que as 'Práticas' funcionam como ponte.\n")
cat("- A Pressão e a Barreira atingem o Desempenho ATRAVÉS das Práticas.\n")
cat("====================================================================\n")







library(sjPlot)
library(ggplot2)

# 1. Ajusta o modelo linear simples com os escores
mod_lm <- lm(Praticas ~ Pressao * Intencao, data = escores)

# 2. Gera o gráfico de interação com margens e posição corrigidas
plot_model(mod_lm, 
           type = "int", 
           mdrt.values = "meansd", 
           title = "Efeito Moderador da Intenção do Gestor (H4a)",
           axis.title = c("Pressões Exógenas do Mercado", "Adoção de Práticas Socioambientais"),
           legend.title = "Intenção da Gestão:") + 
  
  # --- CORES E RÓTULOS ---
  scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A"),
                     labels = c("Baixa Intenção (-1 DP)", "Intenção Média", "Alta Intenção (+1 DP)")) +
  
  theme_bw() + 
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    axis.title.x = element_text(size = 11, face = "bold", margin = margin(t = 10, b = 10)),
    axis.title.y = element_text(size = 11, face = "bold", margin = margin(r = 10)),
    axis.text = element_text(size = 10, color = "black"),
    
    # Coloca a legenda na parte superior para NÃO poluir ou cortar o eixo X
    legend.position = "top", 
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    
    # Dá margem generosa em volta do gráfico para NADA ser cortado nas bordas
    plot.margin = margin(t = 15, r = 15, b = 20, l = 15)
  )






cores_pls_mod <- list(man = "#3399FF") 

semPaths(ajuste_moderacao, 
         what = "paths", 
         whatLabels = "std",       
         layout = "tree2",         # Distribuição em árvore
         rotation = 1,             # ROTACIONA: Fluxo de CIMA para BAIXO (Top-Down)
         nCharNodes = 0,           
         nodeLabels = c("Práticas", "Desempenho\nOperacional", "Pressão", "Barreiras", 
                        "Intenção\n(Gestão)", "Moderação\n(Pressão)", "Moderação\n(Barreiras)"),
         shapeMan = "rectangle",   
         
         # --- TAMANHO DAS CAIXAS ---
         sizeMan = 12,             # Largura do retângulo
         sizeMan2 = 6,             # Altura do retângulo
         
         # --- FONTES E RÓTULOS NAS SETAS ---
         label.cex = 1.0,          # Fonte do texto interno
         edge.label.cex = 0.85,    # Fonte dos coeficientes Beta
         edge.label.position = 0.50, # Centraliza os Betas no meio exato da seta
         
         # --- LIMPEZA E ESPAÇAMENTO ---
         covariances = FALSE,      # Elimina linhas e números sobrepostos
         intercepts = FALSE,       
         residuals = FALSE,        
         
         color = cores_pls_mod,        
         edge.color = "black",     
         border.color = "black",   
         label.color = "black",    
         mar = c(6, 4, 6, 4),      # Margens generosas no topo e na base
         title = FALSE)



library(semPlot)

# 1. Definição da matriz de coordenadas manuais (X, Y)
# Linha 1 (Topo - Y=1): Preditores e Moderações alinhados horizontalmente
# Linha 2 (Meio - Y=0): Práticas
# Linha 3 (Base - Y=-1): Desempenho Operacional

# Ordem dos nós no objeto: 1:Praticas, 2:Desempenho, 3:Pressao, 4:Barreiras, 5:Intencao, 6:Mod_Pressao, 7:Mod_Barreiras
matriz_layout <- matrix(c(
  0.0,  0.0,  # 1: Práticas (Centro)
  0.0, -1.0,  # 2: Desempenho Operacional (Base)
  -0.8,  1.0,  # 3: Pressão (Topo Esquerda)
  -0.4,  1.0,  # 4: Barreiras (Topo Centro-Esquerda)
  0.0,  1.0,  # 5: Intenção (Topo Centro)
  0.4,  1.0,  # 6: Moderação Pressão (Topo Centro-Direita)
  0.8,  1.0   # 7: Moderação Barreiras (Topo Direita)
), ncol = 2, byrow = TRUE)

cores_pls_mod <- list(man = "#3399FF") 

# 2. Renderização limpa e estruturada do diagrama
semPaths(ajuste_moderacao, 
         what = "paths", 
         whatLabels = "std",       
         layout = matriz_layout,   # Aplica o layout hierárquico manual!
         nCharNodes = 0,           
         nodeLabels = c("Práticas", "Desempenho\nOperacional", "Pressão", "Barreiras", 
                        "Intenção\n(Gestão)", "Moderação\n(Pressão)", "Moderação\n(Barreiras)"),
         shapeMan = "rectangle",   
         
         # --- TAMANHO DAS CAIXAS ---
         sizeMan = 12,             # Largura das caixas
         sizeMan2 = 6,             # Altura das caixas
         
         # --- FONTES E POSICIONAMENTO ---
         label.cex = 1.0,          # Fonte interna das caixas
         edge.label.cex = 0.85,    # Fonte dos coeficientes Beta
         edge.label.position = 0.50, # Centraliza os números exatamente no meio da seta
         
         # --- CONFIGURAÇÃO DAS BORDAS E CORES ---
         color = cores_pls_mod,        
         edge.color = "black",     
         border.color = "black",   
         label.color = "black",    
         intercepts = FALSE,       
         residuals = FALSE,        
         mar = c(5, 5, 5, 5),      # Margens equilibradas nas bordas
         title = FALSE)



