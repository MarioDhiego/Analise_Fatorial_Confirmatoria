# ==============================================================================
# SCRIPT COMPLETO E ESTRUTURADO: ANÁLISE ESTATÍSTICA (TESE)
# ==============================================================================

# ------------------------------------------------------------------------------
# PASSO 1: CARREGAMENTO DOS PACOTES
# ------------------------------------------------------------------------------
# Instale os pacotes caso ainda não os tenha:
# install.packages(c("readxl", "dplyr", "likert", "ggplot2", "patchwork", "lavaan", "semTools", "semPlot"))

library(readxl)   # Leitura do Excel
library(dplyr)    # Manipulação de dados
library(likert)   # Gráficos de escala Likert
library(ggplot2)  # Customização de gráficos
library(patchwork)# Agrupamento de gráficos
library(lavaan)   # Análise Fatorial Confirmatória e Equações Estruturais
library(semTools) # Métricas de validade e confiabilidade (AVE, CR)
library(semPlot)  # Desenho do diagrama de caminhos

cat("\n[✓] Passo 1 concluído: Pacotes carregados.\n")

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

# ------------------------------------------------------------------------------
# PASSO 3: GRÁFICOS LIKERT (INDIVIDUAIS E AGRUPADO)
# ------------------------------------------------------------------------------
# Transformando os dados para o formato Likert (Fatores ordenados de 1 a 5)
labels_likert <- c("Discordo Totalmente", "Discordo", "Neutro", "Concordo", "Concordo Totalmente")
df_graficos <- df_itens %>%
  mutate(across(everything(), ~ factor(.x, levels = 1:5, labels = labels_likert, ordered = TRUE))) %>%
  as.data.frame()

# Paleta de Cores do gráfico
minhas_cores <- c("#D7191C", "#F46D43", "#E0E0E0", "#74C476", "#238B45")

# --- GERANDO OS GRÁFICOS INDIVIDUAIS ---
cat("\nGerando Gráficos Likert...\n")

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


# --- GERANDO O PAINEL AGRUPADO ---
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
  plot_layout(guides = 'collect') & theme(legend.position = 'bottom')

print(painel_agrupado)
cat("\n[✓] Passo 3 concluído: Gráficos plotados.\n")
# ==============================================================================




# ------------------------------------------------------------------------------
# PASSO 4: MODELO DE MENSURAÇÃO (Analise Fatorial Confirmatória - MODELO INICIAL COMPLETO)
# ------------------------------------------------------------------------------
cat("\n--- PASSO 4: AFC DO MODELO INICIAL (9 DIMENSÕES) ---\n")

modelo_afc_inicial <- '
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

# Ajustando o modelo inicial
ajuste_afc_inicial <- cfa(modelo_afc_inicial, 
                          data = df_itens, 
                          ordered = names(df_itens), 
                          estimator = "WLSMV")

# Verificando os índices de ajuste do modelo inicial
cat("\n[Resultados do Modelo Inicial - Ajuste Global]\n")
summary(ajuste_afc_inicial, 
        fit.measures = TRUE, 
        standardized = TRUE)

# 2. Extraindo a Matriz de Correlação para provar a falha na Validade Discriminante 
cat("\n[Matriz de Correlação Latente - Modelo Inicial]\n")
matriz_cor_inicial <- lavInspect(ajuste_afc_inicial, "cor.lv")
print(matriz_cor_inicial)
# ==============================================================================





# ==============================================================================
# SCRIPT R: MODELO DE MENSURAÇÃO REFINADO (MODELO 2)
# ==============================================================================



# 3. ESPECIFICAÇÃO DO MODELO REFINADO (MODELO 2)
# Aqui aplicamos a "faxina" psicométrica: 
# - Fusão de Intenção e Atitude em Comportamento_Gestao
# - Exclusão de itens colineares ou com carga baixa: ATT2, PA4, P6, B2, B6
modelo_afc_final <- '
  Comportamento_Gestao   =~ INT1 + INT2 + INT3 + INT4 + ATT1 + ATT3 + ATT4
  Praticas_Sociais       =~ PS1 + PS2 + PS3 + PS4 + PS5
  Desempenho_Operacional =~ D1 + D2 + D3 + D4 + D5 + D6 + D7 + D8 + D9 + D10
  Praticas               =~ PA1 + PA2 + PA3 + PA5 + PA6 + PA7 + PA8 
  Norma_Subjetiva        =~ NS1 + NS2 + NS3 + NS4 + NS5
  Controle               =~ CCP1 + CCP2 + CCP3 + CCP4 + CCP5
  Pressao                =~ P1 + P2 + P3 + P4 + P5 
  Barreiras              =~ B1 + B3 + B4 + B5 
'

# 4. AJUSTE DO MODELO (Estimador WLSMV para dados ordinais)
ajuste_afc_final <- cfa(modelo_afc_final, 
                        data = df_itens, 
                        ordered = names(df_itens %>% select(contains(c("INT","ATT","PS","D","PA","NS","CCP","P","B")))), 
                        estimator = "WLSMV")

# 5. RESULTADOS - MÉTRICAS GLOBAIS DE AJUSTE (Para a Tabela 1 da Tese)
summary(ajuste_afc_final, 
        fit.measures = TRUE, 
        standardized = TRUE)

# 6. MÉTRICAS DE CONFIABILIDADE (Alpha e CR) E VALIDADE CONVERGENTE (AVE)
# (Para a Tabela 2 da Tese)
cat("\n--- MÉTRICAS DE QUALIDADE (CONFIABILIDADE E AVE) ---\n")
confiabilidade <- compRelSEM(ajuste_afc_final, return.total = FALSE)
validade_conv <- AVE(ajuste_afc_final)
metricas_qualidade <- rbind(Alpha_Omega = confiabilidade, AVE = validade_conv)
print(metricas_qualidade)

# 7. VALIDADE DISCRIMINANTE (Para a Tabela 3 da Tese)
cat("\n--- MATRIZ DE CORRELAÇÃO LATENTE ---\n")
matriz_cor_final <- lavInspect(ajuste_afc_final, "cor.lv")
print(matriz_cor_final)

# 8. DIAGRAMA DE CAMINHOS (Para a visualização na Tese)
cat("\n--- GERANDO DIAGRAMA ---\n")
semPaths(ajuste_afc_final, 
         whatLabels = "std", 
         layout = "tree2", 
         edge.color = "black", 
         color = list(lat = "lightblue", man = "white"), 
         sizeMan = 3, 
         sizeLat = 6, 
         edge.label.cex = 0.8)
# ==============================================================================



# ------------------------------------------------------------------------------
# PASSO 6: MODELO DE EQUAÇÕES ESTRUTURAIS (SEM) - TESTE DAS HIPÓTESES
# ------------------------------------------------------------------------------
cat("\n--- PASSO 6: MODELO ESTRUTURAL (TESTE DAS HIPÓTESES H1, H2 e H3) ---\n")

# Declarando o Modelo Estrutural (Apenas as variáveis envolvidas nas hipóteses)
modelo_sem_final <- '
  # Modelo de Mensuração (apenas os construtos relevantes para H1, H2 e H3)
  Praticas               =~ PA1 + PA2 + PA3 + PA5 + PA6 + PA7 + PA8 
  Desempenho_Operacional =~ D1 + D2 + D3 + D4 + D5 + D6 + D7 + D8 + D9 + D10
  Pressao                =~ P1 + P2 + P3 + P4 + P5 
  Barreiras              =~ B1 + B3 + B4 + B5 
  
  # Modelo Estrutural (Testando as relações direcionais ~)
  # H1 e H2:
  Praticas ~ Pressao + Barreiras
  
  # H3:
  Desempenho_Operacional ~ Praticas
'

ajuste_sem_final <- sem(modelo_sem_final, data = df_itens, ordered = names(df_itens), estimator = "WLSMV")

cat("\n[Resultados do Modelo Estrutural - P-values e R-Square]\n")
summary(ajuste_sem_final, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# --- CÁLCULO DA COLINEARIDADE (VIF ESTRUTURAL) ---
cat("\n[Verificação de Colinearidade Estrutural (VIF)]\n")
matriz_cor_sem <- lavInspect(ajuste_sem_final, "cor.lv")
cor_pressao_barreiras <- matriz_cor_sem["Pressao", "Barreiras"]
vif_estrutural <- 1 / (1 - cor_pressao_barreiras^2)
cat("VIF (Pressão vs Barreiras):", round(vif_estrutural, 3), "(Ideal < 5.0)\n")

# ------------------------------------------------------------------------------
# PASSO 7: VISUALIZAÇÃO GRÁFICA DO MODELO ESTRUTURAL (DIAGRAMA)
# ------------------------------------------------------------------------------

library(semPlot)

cat("\n--- PREPARANDO OS RÓTULOS (LABELS) ---\n")

# 1. Extrai a lista de nomes exatos que o R está usando
modelo_base <- semPlotModel(ajuste_sem_final)
nomes_originais <- modelo_base@Vars$name

# 2. Cola o "adesivo" com a quebra de linha apenas no rótulo visual
nomes_corrigidos <- gsub("Desempenho_Operacional", "Desempenho\nOperacional", nomes_originais)

cat("\n--- GERANDO O DIAGRAMA ---\n")

# 3. Gera o gráfico passando a lista de nomes corrigidos
semPaths(ajuste_sem_final, 
         nodeLabels = nomes_corrigidos,  # <--- A mágica visual acontece aqui sem quebrar o modelo
         what = "paths",             
         whatLabels = "std",         
         layout = "tree2",           
         edge.color = "black",       
         color = list(lat = "#EAEAEA", man = "#FFFFFF"), 
         sizeMan = 4,                
         sizeLat = 12,                # Aumentei levemente para a quebra de linha caber bem
         intercepts = FALSE,         
         residuals = FALSE,          
         edge.label.cex = 0.9,       # Tamanho dos números nas setas
         label.cex = 0.9,            # Tamanho da fonte dos nomes
         label.scale = FALSE,        
         mar = c(3, 5, 3, 5),        
         title = FALSE)              

cat("\n[✓] O gráfico corrigido foi gerado com sucesso!\n")

# ==============================================================================





# ==============================================================================
# PASSO FINAL (CORRIGIDO): TESTE DAS HIPÓTESES DE MODERAÇÃO (H4a e H4b)
# ==============================================================================

cat("\n--- PREPARANDO OS DADOS PARA MODERAÇÃO (ESCORES FATORIAIS) ---\n")

# 1. Extraindo os escores
escores_brutos <- lavPredict(ajuste_afc_final)
if(is.list(escores_brutos)) {
  escores <- as.data.frame(escores_brutos[[1]])
} else {
  escores <- as.data.frame(escores_brutos)
}

# 2. Centralizar os escores na média (Mean-Centering)
escores$Pressao_C      <- as.numeric(scale(escores$Pressao, center = TRUE, scale = FALSE))
escores$Barreiras_C    <- as.numeric(scale(escores$Barreiras, center = TRUE, scale = FALSE))

# AQUI ESTÁ A CORREÇÃO: Usando o nome "Intencao_Atitude" que o seu R gerou
escores$CompGestao_C   <- as.numeric(scale(escores$Intencao_Atitude, center = TRUE, scale = FALSE))

# 3. Criar os Termos de Interação (A variável moderadora em ação)
escores$Int_Pressao    <- escores$Pressao_C * escores$CompGestao_C
escores$Int_Barreiras  <- escores$Barreiras_C * escores$CompGestao_C

cat("\n--- RODANDO O MODELO DE MODERAÇÃO ---\n")

# 4. Especificar o modelo de regressão estrutural
modelo_moderacao <- '
  # H4a e H4b: Previsão das Práticas (com os efeitos diretos e as moderações)
  Praticas ~ Pressao_C + Barreiras_C + CompGestao_C + Int_Pressao + Int_Barreiras
  
  # H3: Impacto no Desempenho
  Desempenho_Operacional ~ Praticas
'

# 5. Ajustar o Modelo usando estimador MLR (escores são números contínuos)
ajuste_moderacao <- sem(modelo_moderacao, data = escores, estimator = "MLR")

# 6. Exibir Resultados Finais
cat("\n[RESULTADOS DO TESTE DE MODERAÇÃO (H4a e H4b)]\n")
summary(ajuste_moderacao, standardized = TRUE, rsquare = TRUE)
#-------------------------------------------------------------------------------#









# ==============================================================================
# MODELO AFC 3 (MODELO DE 6 DIMENSÕES - Todos Itens)
# ==============================================================================

# 1. CARREGAR PACOTES NECESSÁRIOS

library(lavaan)
library(semTools)
library(semPlot)

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

library(lavaan)
library(semTools)
library(semPlot)

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

library(lavaan)
library(semTools)
library(semPlot)

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



#####################################################################

# 1) Até o Momento foi Excluído as 
#    Dimensões: Atitudes + Norma Subjetiva + Controle do Comportamento Planejado

# 2) Excluído itens: P1 e P3 (Dimensão Pressão)
#    Com base na Validade Convergente (AVE)

# 3) Excluído itens: D8 (Dimensão Desempenho Operacional)
#    Com base na Validade Discriminante e nos Índices de Modificação

# 4) Excluído itens: D1 (Dimensão Desempenho Operacional)

# 5) Excluído itens: D6 (Dimensão Desempenho Operacional)


#####################################################################



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




#####################################################################

# 1) Excluído as Dimensões: 
#      Atitudes + 
#      Norma Subjetiva + 
#      Controle do Comportamento Planejado

# 2) Excluído itens: P1 e P3 (Dimensão Pressão)
#    Com base na Validade Convergente (AVE)

# 3) Excluído itens: D8 (Dimensão Desempenho Operacional)
# 4) Excluído itens: D1 (Dimensão Desempenho Operacional)
# 5) Excluído itens: D6 (Dimensão Desempenho Operacional)
#    Com base na Validade Discriminante e nos Índices de Modificação

#####################################################################






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


cat("\n=== R-QUADRADO: MODELO ESTRUTURAL BASE ===\n")
print(round(lavInspect(ajuste_sem_base, "rsquare"), 3))

cat("\n=== R-QUADRADO: MODELO COM MODERAÇÃO ===\n")
print(round(lavInspect(ajuste_moderacao, "rsquare"), 3))







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
cores_pls <- list(lat = "#3399FF", man = "#FFFF99")

semPaths(ajuste_sem_base, 
         what = "paths", 
         whatLabels = "std",       
         layout = "tree2",         # Garante o fluxo linear
         rotation = 2,             # Direciona da esquerda para a direita
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






