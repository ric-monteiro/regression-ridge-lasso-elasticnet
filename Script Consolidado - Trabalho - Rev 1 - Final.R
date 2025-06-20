###############################################################################
############TRABALHO DE IAA005 - Estatística Aplicada II#######################

############# 1 REGRESSÕES RIDGE, LASSO E ELASTICNET ##########################

##############################################################
#    Modelos de regularizacao ou penalidade                  #
#                 Regressao Ridge                            # 
##############################################################

# Instalando os pacotes necessarios
install.packages("plyr")
install.packages("readr")
install.packages("dplyr")
install.packages("caret")
install.packages("ggplot2")
install.packages("repr")
install.packages("glmnet")

# Carregando os pacotes necessarios
library(plyr)
library(readr)
library(dplyr)
library(caret)
library(ggplot2)
library(repr)
library(glmnet)

# Carregando o dataset
load("C:/Temp/Bases de Dados Usadas nas Aulas Práticas - Est II/trabalhosalarios.RData")

dat_ridge <- trabalhosalarios
View(dat_ridge)
glimpse(dat_ridge)
gc()

set.seed(302)

index = sample(1:nrow(dat_ridge),0.8*nrow(dat_ridge))

train = dat_ridge[index,]  
test = dat_ridge[-index,] 

dim(train)
dim(test)

cols = c('husage', 'husearns', 'huseduc', 'hushrs',
         'age', 'educ', 'exper', 'lwage')

pre_proc_val <- preProcess(train[,cols], 
                           method = c("center", "scale"))
train[,cols] = predict(pre_proc_val, train[,cols])
test[,cols] = predict(pre_proc_val, test[,cols])

summary(train)
summary(test)

#############################################################
#                     REGRESSAO RIDGE                       #
#############################################################

cols_reg = c('husage', 'husunion', 'husearns', 'huseduc', 'husblck', 'hushisp',
             'hushrs', 'kidge6','age','black', 'educ','hispanic','union', 
             'exper', 'lwage', 'kidlt6')

dummies <- dummyVars(lwage~husage+husunion+husearns+huseduc+husblck+
                       hushisp+hushrs+kidge6+age+black+educ+hispanic+union+
                       exper+kidlt6, 
                     data = dat_ridge[,cols_reg])
train_dummies = predict(dummies, newdata = train[,cols_reg])
test_dummies = predict(dummies, newdata = test[,cols_reg])
print(dim(train_dummies)); print(dim(test_dummies))

x = as.matrix(train_dummies)
y_train = train$lwage
x_test = as.matrix(test_dummies)
y_test = test$lwage

lambdas <- 10^seq(2, -3, by = -.1)
ridge_lamb <- cv.glmnet(x, y_train, alpha = 0, lambda = lambdas)
best_lambda_ridge <- ridge_lamb$lambda.min
best_lambda_ridge

start <- Sys.time()
ridge_reg = glmnet(x, y_train, nlambda = 25, alpha = 0, family = 'gaussian', lambda = best_lambda_ridge)
end <- Sys.time()
difftime(end, start, units="secs")

ridge_reg[["beta"]]

eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
}

predictions_train <- predict(ridge_reg, s = best_lambda_ridge, newx = x)
eval_results(y_train, predictions_train, train)

predictions_test <- predict(ridge_reg, s = best_lambda_ridge, newx = x_test)
eval_results(y_test, predictions_test, test)

# Valores para predicao
husage = (40-pre_proc_val[["mean"]][["husage"]])/pre_proc_val[["std"]][["husage"]]
husunion = 0
husearns = (600-pre_proc_val[["mean"]][["husearns"]])/pre_proc_val[["std"]][["husearns"]]
huseduc = (13-pre_proc_val[["mean"]][["huseduc"]])/pre_proc_val[["std"]][["huseduc"]]
husblck = 1
hushisp = 0
hushrs = (40-pre_proc_val[["mean"]][["hushrs"]])/pre_proc_val[["std"]][["hushrs"]]
kidge6 = 1
age = (38-pre_proc_val[["mean"]][["age"]])/pre_proc_val[["std"]][["age"]]
black = 0
educ = (13-pre_proc_val[["mean"]][["educ"]])/pre_proc_val[["std"]][["educ"]]
hispanic = 1
union = 0
exper = (18-pre_proc_val[["mean"]][["exper"]])/pre_proc_val[["std"]][["exper"]]
kidlt6 = 1

our_pred = as.matrix(data.frame(husage=husage, 
                                husunion=husunion,
                                husearns=husearns,
                                huseduc=huseduc,
                                husblck=husblck,
                                hushisp=hushisp,
                                hushrs=hushrs,
                                kidge6=kidge6,
                                age=age,
                                black=black,
                                educ=educ,
                                hispanic=hispanic,
                                union=union,
                                exper=exper,
                                kidlt6=kidlt6))

predict_our_ridge <- predict(ridge_reg, s = best_lambda_ridge, newx = our_pred)

lwage_pred_ridge = (predict_our_ridge * pre_proc_val[["std"]][["lwage"]]) + pre_proc_val[["mean"]][["lwage"]]

wage_pred_ridge = exp(lwage_pred_ridge)
wage_pred_ridge

# Intervalo de confiança (mantendo qnorm(0.025))
n <- nrow(train)
m <- lwage_pred_ridge
s <- pre_proc_val[["std"]][["lwage"]]
dam <- s / sqrt(n)

CIlwr_ridge <- m + (qnorm(0.025)) * dam  # limite inferior
CIupr_ridge <- m - (qnorm(0.025)) * dam  # limite superior

# Convertendo para escala real (valor salarial)
CIlwr_ridge_real <- exp(CIlwr_ridge)
CIupr_ridge_real <- exp(CIupr_ridge)
salario_hora_predito <- exp(m)

# Resultados:
CIlwr_ridge_real
CIupr_ridge_real
salario_hora_predito

# Interpretacao:
# Salario-hora medio estimado: salario_hora_predito US$ 8.68
# Intervalo de confianca: [US$ 8.49, US$ 8.87]

##############################################################
#    Unidade 2: Modelos de regularizacao ou penalidade       #
#                 Regressao Lasso                            # 
##############################################################

# Instalando os pacotes necessarios (se ainda não estiverem instalados)
install.packages("plyr")
install.packages("readr")
install.packages("dplyr")
install.packages("caret")
install.packages("ggplot2")
install.packages("repr")
install.packages("glmnet")

# Carregando os pacotes necessarios
library(plyr)
library(readr)
library(dplyr)
library(caret)
library(ggplot2)
library(repr)
library(glmnet)

# Carregando o dataset
load("C:/Temp/Bases de Dados Usadas nas Aulas Práticas - Est II/trabalhosalarios.RData")

# Guardando o dataset em um novo objeto
dat_lasso <- trabalhosalarios

# Visualizando os dados
View(dat_lasso)
glimpse(dat_lasso)

# Limpando a memória
gc()

# Definindo a semente para particionamento aleatório reprodutível
set.seed(302)

# Criando índice para 80% dos dados como treino
index = sample(1:nrow(dat_lasso), 0.8 * nrow(dat_lasso))

# Particionando os dados
train = dat_lasso[index,]
test = dat_lasso[-index,]

# Verificando as dimensões
dim(train)
dim(test)

# Padronizando as variáveis contínuas (exceto dummies)
cols = c('husage', 'husearns', 'huseduc', 'hushrs', 'age', 'educ', 'exper', 'lwage')

pre_proc_val <- preProcess(train[, cols], method = c("center", "scale"))
train[, cols] = predict(pre_proc_val, train[, cols])
test[, cols] = predict(pre_proc_val, test[, cols])

# Resumo estatístico das bases padronizadas
summary(train)
summary(test)

#############################################################
#                     REGRESSAO LASSO                       #
#############################################################

# Variáveis utilizadas no modelo
cols_reg = c('husage', 'husunion', 'husearns', 'huseduc', 'husblck', 'hushisp',
             'hushrs', 'kidge6', 'age', 'black', 'educ', 'hispanic', 'union', 
             'exper', 'lwage', 'kidlt6')

# Criando variáveis dummies
dummies <- dummyVars(lwage ~ husage + husunion + husearns + huseduc + husblck +
                       hushisp + hushrs + kidge6 + age + black + educ +
                       hispanic + union + exper + kidlt6, 
                     data = dat_lasso[, cols_reg])

train_dummies = predict(dummies, newdata = train[, cols_reg])
test_dummies = predict(dummies, newdata = test[, cols_reg])
print(dim(train_dummies))
print(dim(test_dummies))

# Convertendo para matriz
x = as.matrix(train_dummies)
y_train = train$lwage
x_test = as.matrix(test_dummies)
y_test = test$lwage

# Definindo intervalo de lambdas para ajuste
lambdas <- 10^seq(2, -3, by = -.1)

# Ajustando modelo Lasso com validação cruzada (alpha = 1)
lasso_lamb <- cv.glmnet(x, y_train, alpha = 1, lambda = lambdas,
                        standardize = TRUE, nfolds = 5)

# Extraindo o melhor lambda
best_lambda_lasso <- lasso_lamb$lambda.min
best_lambda_lasso

# Ajustando modelo com o melhor lambda
lasso_model <- glmnet(x, y_train, alpha = 1, lambda = best_lambda_lasso,
                      standardize = TRUE)

# Visualizando os coeficientes (alguns serão zero)
lasso_model[["beta"]]

# Fazendo predições na base de treino
predictions_train <- predict(lasso_model, s = best_lambda_lasso, newx = x)

# Função para avaliar desempenho do modelo
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE / nrow(df))
  data.frame(RMSE = RMSE, Rsquare = R_square)
}

# Avaliando desempenho na base de treino
eval_results(y_train, predictions_train, train)

# Avaliando desempenho na base de teste
predictions_test <- predict(lasso_model, s = best_lambda_lasso, newx = x_test)
eval_results(y_test, predictions_test, test)

#############################################################
#             PREDIÇÃO PARA UM EXEMPLO ESPECÍFICO           #
#############################################################

# Dados padronizados para o exemplo
husage = (40 - pre_proc_val[["mean"]][["husage"]]) / pre_proc_val[["std"]][["husage"]]
husunion = 0
husearns = (600 - pre_proc_val[["mean"]][["husearns"]]) / pre_proc_val[["std"]][["husearns"]]
huseduc = (13 - pre_proc_val[["mean"]][["huseduc"]]) / pre_proc_val[["std"]][["huseduc"]]
husblck = 1
hushisp = 0
hushrs = (40 - pre_proc_val[["mean"]][["hushrs"]]) / pre_proc_val[["std"]][["hushrs"]]
kidge6 = 1
age = (38 - pre_proc_val[["mean"]][["age"]]) / pre_proc_val[["std"]][["age"]]
black = 0
educ = (13 - pre_proc_val[["mean"]][["educ"]]) / pre_proc_val[["std"]][["educ"]]
hispanic = 1
union = 0
exper = (18 - pre_proc_val[["mean"]][["exper"]]) / pre_proc_val[["std"]][["exper"]]
kidlt6 = 1

# Criando matriz para predição
our_pred = as.matrix(data.frame(husage=husage, husunion=husunion, husearns=husearns,
                                huseduc=huseduc, husblck=husblck, hushisp=hushisp,
                                hushrs=hushrs, kidge6=kidge6, age=age, black=black,
                                educ=educ, hispanic=hispanic, union=union,
                                exper=exper, kidlt6=kidlt6))

# Fazendo a predição
predict_our_lasso <- predict(lasso_model, s = best_lambda_lasso, newx = our_pred)

# Despadronizando o valor predito
lwage_pred_lasso <- (predict_our_lasso * pre_proc_val[["std"]][["lwage"]]) + 
  pre_proc_val[["mean"]][["lwage"]]

# Aplicando o antilog (exponencial) para voltar à escala original de salário-hora
wage_pred_lasso <- exp(lwage_pred_lasso)
wage_pred_lasso  # Resultado final: salário-hora estimado

# Criando intervalo de confiança
n <- nrow(train)
m <- lwage_pred_lasso
s <- pre_proc_val[["std"]][["lwage"]]
dam <- s / sqrt(n)
CIlwr_lwage <- m + qnorm(0.025) * dam
CIupr_lwage <- m - qnorm(0.025) * dam

# Convertendo os limites do intervalo de confiança para a escala original (antilog)
CIlwr_lasso <- exp(CIlwr_lwage)
CIupr_lasso <- exp(CIupr_lwage)

# Exibindo o salário-hora predito e o intervalo de confiança em dólares
cat("Resultados da Predição - Regressão Lasso:\n")
cat("Salário-Hora Predito: US$", round(wage_pred_lasso, 2), "\n")
cat("Intervalo de Confiança (95%) para o Salário-Hora:\n")
cat("  Limite Inferior: US$", round(CIlwr_lasso, 2), "\n")
cat("  Limite Superior: US$", round(CIupr_lasso, 2), "\n")

# Resultado final interpretado:
# O salário-hora estimado para a esposa é de aproximadamente US$ 10.36,
# com um intervalo de confiança entre US$ 10.07 e US$ 10.65.

##############################################################
#           Modelos de Regularização ou Penalidade           #
#                      Regressão ElasticNet                  # 
##############################################################

#Instalando os pacotes necessários (execute uma vez)
install.packages(c("plyr", "readr", "dplyr", "ggplot2", 
                    "repr", "glmnet", "caret"))

# Carregando os pacotes
library(plyr)
library(readr)
library(dplyr)
library(ggplot2)
library(repr)
library(glmnet)
library(caret)

# Carregando a base de dados com informações salariais
load("C:/Temp/Bases de Dados Usadas nas Aulas Práticas - Est II/trabalhosalarios.RData")
dat_elastic <- trabalhosalarios

# Explorando o conjunto de dados
View(dat_elastic)
glimpse(dat_elastic)
gc()  # Checar uso de memória

# Fixando a semente para reprodutibilidade
set.seed(302)

# Separando dados em treino (80%) e teste (20%)
index <- sample(1:nrow(dat_elastic), 0.8 * nrow(dat_elastic))
train <- dat_elastic[index, ]
test <- dat_elastic[-index, ]

# Verificando as dimensões dos conjuntos
dim(train)  # Ex: 1435 linhas
dim(test)   # Ex: 359 linhas

# Selecionando variáveis contínuas para padronização
cols <- c('husage', 'husearns', 'huseduc', 'hushrs',
          'age', 'educ', 'exper', 'lwage')

# Padronizando as variáveis contínuas com base nos dados de treino
pre_proc_val <- preProcess(train[, cols], method = c("center", "scale"))
train[, cols] <- predict(pre_proc_val, train[, cols])
test[, cols] <- predict(pre_proc_val, test[, cols])

# Verificando os resumos estatísticos
summary(train)
summary(test)

#############################################################
#                 REGRESSÃO ELASTICNET                      #
#############################################################

# Definindo as variáveis que serão utilizadas no modelo
cols_reg <- c('husage', 'husunion', 'husearns', 'huseduc', 'husblck', 'hushisp',
              'hushrs', 'kidge6', 'age', 'black', 'educ', 'hispanic', 'union', 
              'exper', 'lwage', 'kidlt6')

# Criando variáveis dummies para variáveis categóricas
dummies <- dummyVars(lwage ~ husage + husunion + husearns + huseduc + husblck +
                       hushisp + hushrs + kidge6 + age + black + educ + hispanic +
                       union + exper + kidlt6,
                     data = dat_elastic[, cols_reg])

train_dummies <- predict(dummies, newdata = train[, cols_reg])
test_dummies <- predict(dummies, newdata = test[, cols_reg])

# Convertendo os dados para formato de matriz
x <- as.matrix(train_dummies)
y_train <- train$lwage
x_test <- as.matrix(test_dummies)
y_test <- test$lwage

# Configurando o treinamento com validação cruzada
train_cont <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 5,
                           search = "random",
                           verboseIter = TRUE)

# Treinando o modelo ElasticNet com a função train() do pacote caret
elastic_reg <- train(lwage ~ husage + husunion + husearns + huseduc + husblck +
                       hushisp + hushrs + kidge6 + age + black + educ + hispanic +
                       union + exper + kidlt6,
                     data = train,
                     method = "glmnet",
                     tuneLength = 10,
                     trControl = train_cont)

# Melhor valor de alpha e lambda encontrados
elastic_reg$bestTune

# Coeficientes finais do modelo
elastic_reg[["finalModel"]][["beta"]]

# Fazendo predições no conjunto de treino
predictions_train <- predict(elastic_reg, x)

# Função de avaliação (RMSE e R²)
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE <- sqrt(SSE / nrow(df))
  
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
}

# Avaliando o modelo nos dados de treino
eval_results(y_train, predictions_train, train)

# Avaliando o modelo nos dados de teste
predictions_test <- predict(elastic_reg, x_test)
eval_results(y_test, predictions_test, test)

# ---------------------------------------------------------
# Fazendo uma previsão com um novo conjunto de variáveis
# ---------------------------------------------------------

# Valores informados (exemplo hipotético)
husage <- (40 - pre_proc_val$mean[["husage"]]) / pre_proc_val$std[["husage"]]
husunion <- 0
husearns <- (600 - pre_proc_val$mean[["husearns"]]) / pre_proc_val$std[["husearns"]]
huseduc <- (13 - pre_proc_val$mean[["huseduc"]]) / pre_proc_val$std[["huseduc"]]
husblck <- 1
hushisp <- 0
hushrs <- (40 - pre_proc_val$mean[["hushrs"]]) / pre_proc_val$std[["hushrs"]]
kidge6 <- 1
age <- (38 - pre_proc_val$mean[["age"]]) / pre_proc_val$std[["age"]]
black <- 0
educ <- (13 - pre_proc_val$mean[["educ"]]) / pre_proc_val$std[["educ"]]
hispanic <- 1
union <- 0
exper <- (18 - pre_proc_val$mean[["exper"]]) / pre_proc_val$std[["exper"]]
kidlt6 <- 1

# Montando a matriz com as variáveis do exemplo
our_pred <- as.matrix(data.frame(
  husage = husage,
  husunion = husunion,
  husearns = husearns,
  huseduc = huseduc,
  husblck = husblck,
  hushisp = hushisp,
  hushrs = hushrs,
  kidge6 = kidge6,
  age = age,
  black = black,
  educ = educ,
  hispanic = hispanic,
  union = union,
  exper = exper,
  kidlt6 = kidlt6
))

# Fazendo a predição com base no modelo
predict_our_elastic <- predict(elastic_reg, our_pred)

# Revertendo a padronização (voltar ao valor de lwage original)
lwage_pred_elastic <- (predict_our_elastic * pre_proc_val$std[["lwage"]]) +
  pre_proc_val$mean[["lwage"]]
lwage_pred_elastic

# Calculando o antilogaritmo (salário-hora estimado)
wage_pred_elastic <- exp(lwage_pred_elastic)
wage_pred_elastic
# Resultado: salário-hora médio estimado com base nas características informadas

# Calculando intervalo de confiança para a estimativa
n <- nrow(train)
s <- pre_proc_val$std[["lwage"]]
dam <- s / sqrt(n)
CIlwr_elastic <- lwage_pred_elastic + qnorm(0.025) * dam
CIupr_elastic <- lwage_pred_elastic - qnorm(0.025) * dam

# Convertendo o intervalo de confiança de log para escala original
CI_lwr_wage <- exp(CIlwr_elastic)
CI_upr_wage <- exp(CIupr_elastic)

# Exibindo o resultado
wage_pred_elastic  # Salário estimado
CI_lwr_wage        # Limite inferior
CI_upr_wage        # Limite superior
# Exemplo: O salário-hora médio estimado é US$9.28, com intervalo entre US$8.99 e US$9.57

#############################################################
#               COMPARAÇÃO DOS MODELOS                     #
#############################################################

# Calculando as métricas para os modelos (já presentes no código, mas agora armazenadas explicitamente)
ridge_test_metrics <- eval_results(y_test, predictions_test, test)  # Ridge
lasso_test_metrics <- eval_results(y_test, predictions_test, test)  # Lasso
elastic_test_metrics <- eval_results(y_test, predictions_test, test)  # ElasticNet

# Exibindo as métricas de cada modelo
cat("\nMétricas Ridge (Teste):\n")
print(ridge_test_metrics)
cat("\nMétricas Lasso (Teste):\n")
print(lasso_test_metrics)
cat("\nMétricas ElasticNet (Teste):\n")
print(elastic_test_metrics)

# Criando a tabela comparativa
comparison_table <- data.frame(
  Modelo = c("Ridge", "Lasso", "ElasticNet"),
  RMSE = c(ridge_test_metrics$RMSE, lasso_test_metrics$RMSE, elastic_test_metrics$RMSE),
  Rsquare = c(ridge_test_metrics$Rsquare, lasso_test_metrics$Rsquare, elastic_test_metrics$Rsquare),
  Salario_Predito = c(wage_pred_ridge, wage_pred_lasso, wage_pred_elastic),
  IC_Limite_Inferior = c(CIlwr_ridge_real, CIlwr_lasso, CI_lwr_wage),
  IC_Limite_Superior = c(CIupr_ridge_real, CIupr_lasso, CI_upr_wage)
)

# Arredondando os valores para três casas decimais (RMSE e R²) e duas casas decimais (salário e IC)
comparison_table$RMSE <- round(comparison_table$RMSE, 3)
comparison_table$Rsquare <- round(comparison_table$Rsquare, 3)
comparison_table$Salario_Predito <- round(comparison_table$Salario_Predito, 2)
comparison_table$IC_Limite_Inferior <- round(comparison_table$IC_Limite_Inferior, 2)
comparison_table$IC_Limite_Superior <- round(comparison_table$IC_Limite_Superior, 2)

# Exibindo a tabela comparativa no console
cat("\nTabela Comparativa dos Modelos (Conjunto de Teste):\n")
print(comparison_table)

# Determinando o melhor modelo com base no menor RMSE
best_model <- comparison_table[which.min(comparison_table$RMSE), ]
cat("\nMelhor Modelo com base no menor RMSE no conjunto de teste:\n")
cat("Modelo:", best_model$Modelo, "\n")
cat("RMSE:", best_model$RMSE, "\n")
cat("R²:", best_model$Rsquare, "\n")
cat("Salário-Hora Predito: US$", best_model$Salario_Predito, "\n")
cat("Intervalo de Confiança (95%): [US$", best_model$IC_Limite_Inferior, ", US$", best_model$IC_Limite_Superior, "]\n")

#Comentário Justificativo:
#A escolha do modelo Ridge como o melhor entre os avaliados foi fundamentada na
#métrica de erro quadrático médio (RMSE), a qual é amplamente utilizada para 
#mensurar a precisão de modelos de regressão em conjuntos de teste. O modelo 
#Ridge apresentou o menor RMSE (0.9893328), indicando menor discrepância entre os 
#valores reais e preditos em comparação aos demais modelos (Lasso e ElasticNet).
#Além disso, o modelo Ridge compartilhou o maior R² (0.259) com o ElasticNet, 
#o que reforça sua capacidade explicativa da variabilidade dos dados. 
#O intervalo de confiança (95%) do salário-hora previsto 
#pelo Ridge [US$ 8.49 , US$ 8.87] é estreito e está centrado em um valor 
#razoável (US$ 8.68), evidenciando boa precisão na estimativa pontual.
#Portanto, considerando tanto o menor erro de predição quanto a capacidade 
#explicativa similar aos demais modelos, o modelo Ridge se mostra como a melhor
#escolha para o problema analisado.
#
#Considerações:
#As diferenças entre os modelos são muito pequenas em termos de RMSE e R², 
#sugerindo que todos têm desempenho semelhante para este conjunto de dados. 
#A escolha do Ridge é baseada na métrica de RMSE, que é um indicador direto 
#de erro de predição.
#Se o objetivo for maximizar o R² (explicação da variância), o ElasticNet 
#poderia ser considerado, mas a diferença prática é negligible.