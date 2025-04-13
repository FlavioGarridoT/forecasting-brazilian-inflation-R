
library("readxl")
library("lmtest")
library("forecast")
library("tseries")
library("writexl") 
library("Metrics") 
library(caret)


dados <- read_excel("D:/Mestrado/Materias/DISSERTACAO/parte empirica/projeto-github/forecasting-brazilian-inflation-R/data/DADOS.xlsx")
ipca_2018e2019 <- read_excel("D:/Mestrado/Materias/DISSERTACAO/parte empirica/projeto-github/forecasting-brazilian-inflation-R/data/IPCA_2018e2019.xlsx")


#Transformando os dados em serie de tempo
ipca <- ts(dados$ipca, start = c(2006,1), end = c(2017,12), frequency = 12)
ipca_alimentos <- ts(dados$ipca_alimentos, start = c(2006,1), end = c(2017,12), frequency = 12)
ipca_habitacao <- ts(dados$ipca_habitacao, start = c(2006,1), end = c(2017,12), frequency = 12)
ipca_transporte <- ts(dados$ipca_transporte, start = c(2006,1), end = c(2017,12), frequency = 12)
ipca_saude <- ts(dados$`ipca_saude e cuidados`, start = c(2006,1), end = c(2017,12), frequency = 12)
ipca_educacao <- ts(dados$ipca_educacao, start = c(2006,1), end = c(2017,12), frequency = 12) 
ipca_servicos <- ts(dados$ipca_servi?os, start = c(2006,1), end = c(2017,12), frequency = 12) 
ipca_15 <- ts(dados$ipca_15, start = c(2006,1), end = c(2017,12), frequency = 12)
ipca_ex0 <- ts(dados$ipca_ex0, start = c(2006,1), end = c(2017,12), frequency = 12)
ipca_ex1 <- ts(dados$ipca_ex1, start = c(2006,1), end = c(2017,12), frequency = 12)
ipc_fipe <- ts(dados$ipc_fipe, start = c(2006,1), end = c(2017,12), frequency = 12)
inpc <- ts(dados$inpc, start = c(2006,1), end = c(2017,12), frequency = 12)
igpm <- ts(dados$`igp-m`, start = c(2006,1), end = c(2017,12), frequency = 12)
igpdi <- ts(dados$`igp-di`, start = c(2006,1), end = c(2017,12), frequency = 12)
incc <- ts(dados$incc, start = c(2006,1), end = c(2017,12), frequency = 12) 
igp10 <- ts(dados$`igp-10`, start = c(2006,1), end = c(2017,12), frequency = 12)
baseMonetaria <- ts(dados$`base monetaria`, start = c(2006,1), end = c(2017,12), frequency = 12)
M1 <- ts(dados$M1  , start = c(2006,1), end = c(2017,12), frequency = 12)
M2 <- ts(dados$M2, start = c(2006,1), end = c(2017,12), frequency = 12)
M3 <- ts(dados$M3 , start = c(2006,1), end = c(2017,12), frequency = 12)
M4 <- ts(dados$M4, start = c(2006,1), end = c(2017,12), frequency = 12)
compulsorio <- ts(dados$compulsorio, start = c(2006,1), end = c(2017,12), frequency = 12)
reservasInternacionais <- ts(dados$`reservas internacionais`, start = c(2006,1), end = c(2017,12), frequency = 12)
selicMeta <- ts(dados$`selic meta`, start = c(2006,1), end = c(2017,12), frequency = 12)
tjlp <- ts(dados$tjlp, start = c(2006,1), end = c(2017,12), frequency = 12) 
pib <- ts(dados$pib, start = c(2006,1), end = c(2017,12), frequency = 12)
ibcBr <- ts(dados$`ibc-br`, start = c(2006,1), end = c(2017,12), frequency = 12)
variacaoMensalVarejo <- ts(dados$`variacao mensal varejo - pmc`, start = c(2006,1), end = c(2017,12), frequency = 12)
cambio <- ts(dados$cambio, start = c(2006,1), end = c(2017,12), frequency = 12)
embi <- ts(dados$`embi+`, start = c(2006,1), end = c(2017,12), frequency = 12)
crb <- ts(dados$crb, start = c(2006,1), end = c(2017,12), frequency = 12)
petroleoBrent <- ts(dados$`petroleo brent`, start = c(2006,1), end = c(2017,12), frequency = 12)
exportacoes <- ts(dados$exportacoes, start = c(2006,1), end = c(2017,12), frequency = 12)
importacoes <- ts(dados$importacoes, start = c(2006,1), end = c(2017,12), frequency = 12)
indiceConfianca <- ts(dados$`indice confianca consumidor`, start = c(2006,1), end = c(2017,12), frequency = 12)
ibovespa <- ts(dados$ibovespa, start = c(2006,1), end = c(2017,12), frequency = 12)
prodVeiculos <- ts(dados$`prod veiculos`, start = c(2006,1), end = c(2017,12), frequency = 12)
DLSP <- ts(dados$DLSP, start = c(2006,1), end = c(2017,12), frequency = 12)
DLSP_PIB <- ts(dados$DLSPPIB, start = c(2006,1), end = c(2017,12), frequency = 12)
res_primario <- ts(dados$ResPrimario, start = c(2006,1), end = c(2017,12), frequency = 12)
res_primario_PIB <- ts(dados$ResPrimarioPIB, start = c(2006,1), end = c(2017,12), frequency = 12)
  

# Teste de raiz unitaria para saber se a serie eh estacionaria ou nao
# ADF
# H0: tem raiz unitaria (nao estacionaria)
# H1: nao tem raiz unitaria (estacionaria) 
# Comparar p-valor com 0.05 (se for maior, nao rejeita)
# Um p-valor alto eh compativel com H0. Ou seja, nao rejeito. Eh nao estacionaria

# ESTACIONARIAS

adf.test(ipca) #warning message: p-value smaller than printed p-value  --> estacionaria
adf.test(ipca_alimentos) # p-valor: 0.01  -> estacionaria
adf.test(ipca_habitacao) #warning message: p-value smaller than printed p-value  --> estacionaria
adf.test(ipca_transporte)#warning message: p-value smaller than printed p-value  --> estacionaria
adf.test(ipca_saude)#warning message
adf.test(ipca_educacao)#warning message: p valor menor que 0.01
adf.test(ipca_servicos)#warning message: p valor menor que 0.01
adf.test(ipca_15) #warning message: p valor menor que 0.01
adf.test(ipca_ex0)#warning message: p valor menor que 0.01
adf.test(ipca_ex1) #warning message: p valor menor que 0.01
adf.test(ipc_fipe) #warning message: p valor menor que 0.01
adf.test(inpc) #warning message: p valor menor que 0.01
adf.test(igpm) #p-valor: 0.02676
adf.test(igpdi) #pvalor: 0.01638
adf.test(incc) #warning message: p valor menor que 0.01
adf.test(igp10) #pvalor: 0.0286
adf.test(variacaoMensalVarejo) #warning message: p valor menor que 0.01


# NAO ESTACIONARIAS

adf.test(embi) #pvalor: 0.10
adf.test(exportacoes) #pvalor:
adf.test(prodVeiculos) #pvalor: 0.21

adf.test(baseMonetaria) # pvalor: 
adf.test(M1) #NAO ESTACIONARIA, pvalor: 0.31
adf.test(M2) #NAO ESTACIONARIA, pvalor: 0.45
adf.test(M3) #NAO ESTACIONARIA, pvalor: 0.41
adf.test(M4) #NAO ESTACIONARIA, pvalor: 0.46
adf.test(compulsorio) #NAO ESTACIONARIA, pvalor: 0.56
adf.test(reservasInternacionais) #NAO ESTACIONARIA, pvalor: 0.86
adf.test(selicMeta) #NAO ESTACIONARIA, pvalor: 0.30
adf.test(tjlp) #NAO ESTACIONARIA, pvalor: 0.41
adf.test(pib) #NAO ESTACIONARIA, pvalor: 0.19
adf.test(ibcBr) #NAO ESTACIONARIA, pvalor: 0.29
adf.test(cambio) #NAO ESTACIONARIA, pvalor: 
adf.test(crb) #NAO ESTACIONARIA, pvalor: 
adf.test(petroleoBrent) #NAO ESTACIONARIA, pvalor: 
adf.test(importacoes) #NAO ESTACIONARIA, pvalor: 
adf.test(indiceConfianca) #NAO ESTACIONARIA, pvalor: 
adf.test(ibovespa) #NAO ESTACIONARIA, pvalor: 
adf.test(DLSP) #0.99
adf.test(DLSP_PIB) #0.99
adf.test(res_primario) #0.071
adf.test(res_primario_PIB) 


#
# Primeira diferenca -> selic, tjlp, compulsorio, indice confianca, res_primario, res_primario_pib
# Segunda diferenca -> DLSP, DLSP_PIB
# ln(x) ->
# delta(ln(x)) -> base monetaria, embi, prodVeiculos, IBC-BR, Importacoes, exportacoes, crb, petroleo brent, M1, ibovespa, cambio
# delta²(ln(x)) -> PIB, reservas internacionais, M2, M3, M4

# transformando em estacionarias
ndiffs(baseMonetaria)
baseMonetaria_estacionaria <- diff(log(baseMonetaria))
adf.test(baseMonetaria_estacionaria) #pvalor: 0.01
plot(baseMonetaria_estacionaria)


ndiffs(M1)
M1_estacionaria <- diff(log(M1))
adf.test(M1_estacionaria) #pvalor: 0.01
plot(M1_estacionaria)


ndiffs(M2) #diz que eh preciso diferenciar duas vezes para tornar estacionario
M2_estacionaria <- diff(log(M2), differences = 2)   
adf.test(M2_estacionaria) #pvalor: 0.01
plot(M2_estacionaria)


ndiffs(M3)
M3_estacionaria <- diff(log(M3), differences = 2)
adf.test(M3_estacionaria) #pvalor: 0.01
plot(M3_estacionaria)

ndiffs(M4)
M4_estacionaria <- diff(log(M4), differences = 2)
adf.test(M4_estacionaria) #pvalor: 0.01
plot(M4_estacionaria, type = "l")

ndiffs(compulsorio)
compulsorio_estacionario <- diff(compulsorio)
adf.test(compulsorio_estacionario)#p-valor: 0.01
plot(compulsorio_estacionario, type = "l")

ndiffs(reservasInternacionais)
reservas_estacionaria <- diff(log(reservasInternacionais), differences = 2)
adf.test(reservas_estacionaria)#pvalor: 0.01
plot(reservas_estacionaria, type = "l")  

ndiffs(selicMeta)
selicMeta_estacionaria <- diff(selicMeta)
adf.test(selicMeta_estacionaria)
plot(selicMeta_estacionaria, type = "l")

ndiffs(tjlp)
tjlp_estacionaria <- diff(tjlp, differences = 2)
adf.test(tjlp_estacionaria)
plot(tjlp_estacionaria, type = "l")   # estacionario??

ndiffs(pib)
pib_estacionario <- diff(log(pib), differences = 2)
adf.test(pib_estacionario)
plot(pib_estacionario, type = "l")

ndiffs(ibcBr)
ibcBr_estacionario <- diff(log(ibcBr))
adf.test(ibcBr_estacionario)
plot(ibcBr_estacionario)

#cambio , crb, petroleo, importacoes, indiceConfianca, ibovespa
ndiffs(cambio)
cambio_estacionario <- diff(log(cambio))
adf.test(cambio_estacionario) #pvalor 0.01
plot(cambio_estacionario)

ndiffs(crb)                        ##########################################################
crb_estacioniario <- diff(log(crb))
adf.test(crb_estacioniario)
plot(crb_estacioniario, type = "l")

ndiffs(petroleoBrent)
petroleo_estacionario <- diff(log(petroleoBrent))
adf.test(petroleo_estacionario)
plot(petroleo_estacionario)

ndiffs(importacoes)
importacoes_estacionario <- diff(log(importacoes))
adf.test(importacoes_estacionario)
plot

ndiffs(indiceConfianca)
indiceConfianca_estacionario <- diff(indiceConfianca)
adf.test(indiceConfianca_estacionario)
plot(indiceConfianca_estacionario)

ndiffs(ibovespa)
ibovespa_estacionario <- diff(log(ibovespa))
adf.test(ibovespa_estacionario)
plot(ibovespa_estacionario)

ndiffs(embi)
embi_estacionario <- diff(log(embi))
adf.test(embi_estacionaro)
plot(embi_estacionaro)
#embi, exportacoes, prodVeiculos

ndiffs(exportacoes)
exportacoes_estacionario <- diff(log(exportacoes))
adf.test(exportacoes_estacionario)
plot(exportacoes_estacionario)

ndiffs(prodVeiculos)
prodVeiculos_estacionario <- diff(log(prodVeiculos))
adf.test(prodVeiculos_estacionario)
plot(prodVeiculos_estacionario)

ndiffs(DLSP)
DLSP_estacionario <- diff(DLSP, differences = 2)
adf.test(DLSP_estacionario)
plot(DLSP_estacionario)

ndiffs(DLSP_PIB)
DLSP_PIB_estacionario <- diff(DLSP_PIB, differences = 2)
adf.test(DLSP_PIB_estacionario)
plot(DLSP_PIB_estacionario)

ndiffs(res_primario)
res_primario_estacionario <- diff(res_primario)
adf.test(res_primario_estacionario)
plot(res_primario_estacionario)

ndiffs(res_primario_PIB)
res_primario_PIB_estacionario <- diff(res_primario_PIB, differences = 2)
adf.test(res_primario_PIB_estacionario)
plot(res_primario_PIB_estacionario)


# pegando o IPCA com 3 defasagens por conta da autocorrelacao ######  Igual na maioria dos artigos que usam o lag do indice para fazer previsao
inflacao_3lag <- diff(ipca, 3)
plot(inflacao_3lag, type = "l")
nrow(inflacao_3lag)

##### Tratando os dados. Ou seja, arrumando os dados estacionarios e nao estacionarios para que eles fiquem
##### em uma mesma planilha. 


df1 <- data.frame(dados[1:17],variacaoMensalVarejo) #variaveis que ja sao estacionarias 

df2 <- data.frame(baseMonetaria_estacionaria, M1_estacionaria, compulsorio_estacionario,
                  ibcBr_estacionario, cambio_estacionario, 
                  petroleo_estacionario, importacoes_estacionario, indiceConfianca_estacionario,
                  ibovespa_estacionario, selicMeta_estacionaria,  embi_estacionario, exportacoes_estacionario, 
                  prodVeiculos_estacionario, crb_estacioniario, res_primario_estacionario)  # variaveis transformadas em estacionarias com 167 observacoes

df3 <- data.frame(M2_estacionaria, M3_estacionaria, M4_estacionaria, reservas_estacionaria,
                  tjlp_estacionaria, pib_estacionario, DLSP_estacionario, DLSP_PIB_estacionario, res_primario_PIB_estacionario) # variaveis transformadas em estacionarias com 166 observacoes
  
df4 <- data.frame(inflacao_3lag)   ## ipca com 3 lags colocada em um data frame
                

# Irei pegar os dois data frames que foram transformados em estacionarios e transformar em planilha do excel
# Eh mais facil tratar os dados direto do excel do que por aqui no R
# Entao eu junto tudo em uma planilha do excel e depois importo de novo pro R

write_xlsx(df2, "D:/Mestrado/Materias/DISSERTACAO/parte empirica/projeto-github/forecasting-brazilian-inflation-R/data/dataFrame5.xlsx") #exporto uma planilha
write_xlsx(df3, "D:/Mestrado/Materias/DISSERTACAO/parte empirica/projeto-github/forecasting-brazilian-inflation-R/data/dataFrame6.xlsx") #exporto a outra
write_xlsx(df4, "D:/Mestrado/Materias/DISSERTACAO/parte empirica/projeto-github/forecasting-brazilian-inflation-R/data/dataFrame7.xlsx") 

### Agora irei importar uma planilha que ja tenha todas as variaveis estacionarias

dados_transformados <- read_excel("D:/Mestrado/Materias/DISSERTACAO/parte empirica/projeto-github/forecasting-brazilian-inflation-R/data/DADOS_tratamento_novo.xlsx")


n_total <- nrow(dados_transformados)
n_treino <- floor(0.7 * n_total)
dados_treino <- dados_transformados[1:n_treino,]
dados_teste <- dados_transformados[(n_treino + 1):n_total,]

inflacao_AR <- ts(dados_transformados$ipca, start = c(2006,1), end = c(2017,12), frequency = 12)


par(mfrow=c(1,2))
acf(inflacao_AR)
pacf(inflacao_AR) ## VER ORDEM DO AR POR AQUI
par(mfrow=c(1,1))
tsdisplay(inflacao_AR)

decomposicao <- decompose(inflacao_AR)
plot(decomposicao)


############################################# MODELO AUTOREGRESSIVO #######################################

AR1 <- Arima(inflacao_AR, order = c(1,0,0), seasonal = c(1,0,0))
AR2 <- Arima(inflacao_AR, order = c(2,0,0), seasonal = c(2,2,0))
AR3 <- Arima(inflacao_AR, order = c(3,0,0), seasonal = c(1,1,1))  

# Fazendo um ARMA (4,3) igual o BACEN fez 
#BC_ARMA <- Arima(inflacao_AR, order = c(4,0,3), seasonal = c(1,1,1))
BC_ARMA <- Arima(inflacao_AR, order = c(4,0,3), seasonal = c(2,2,2))

auto.arima(inflacao_AR)


#AR1
for (i in 1:24) {
  prev_ar1 <- print(forecast(AR1, h=i)) #faco a previsao 
  previsto_ar1 <- c(prev_ar1$`Point Forecast`) #pego os valores
}

#AR2
for (i in 1:24) {
  prev_ar2 <- print(forecast(AR2, h=i))
  previsto_ar2 <- c(prev_ar2$`Point Forecast`)
}

#AR3
for (i in 1:24) {
  prev_ar3 <- print(forecast(AR3, h=i))
  previsto_ar3 <- c(prev_ar3$`Point Forecast`)
}

#ARMA(4,3)
for (i in 1:24) {
  prev_arma <- print(forecast(BC_ARMA, h=i))
  previsto_arma <- c(prev_arma$`Point Forecast`)
}

#AR1
#3meses
previsto_3meses_AR1 <- previsto_ar1[1:3]
#6meses
previsto_6meses_AR1 <- previsto_ar1[1:6]
#9meses
previsto_9meses_AR1 <- previsto_ar1[1:9]
#12 meses
previsto_12meses_AR1 <- previsto_ar1[1:12]
#24 meses
previsto_24meses_AR1 <- previsto_ar1[1:24]


#AR2
#3meses
previsto_3meses_AR2 <- previsto_ar2[1:3]
#6meses
previsto_6meses_AR2 <- previsto_ar2[1:6]
#9meses
previsto_9meses_AR2 <- previsto_ar2[1:9]
#12 meses
previsto_12meses_AR2 <- previsto_ar2[1:12]
#24 meses
previsto_24meses_AR2 <- previsto_ar2[1:24]


#AR3
#3meses
previsto_3meses_AR3 <- previsto_ar3[1:3]
#6meses
previsto_6meses_AR3 <- previsto_ar3[1:6]
#9meses
previsto_9meses_AR3 <- previsto_ar3[1:9]
#12 meses
previsto_12meses_AR3 <- previsto_ar3[1:12]
#24 meses
previsto_24meses_AR3 <- previsto_ar3[1:24]


#ARMA(4,3)
#6meses
previsto_6meses_ARMA <- previsto_arma[1:6]
#9meses
previsto_9meses_ARMA <- previsto_arma[1:9]
#12 meses
previsto_12meses_ARMA <- previsto_arma[1:12]
#24 meses
previsto_24meses_ARMA <- previsto_arma[1:24]



### RESIDUOS DOS MODELOS

# 6meses
residuo_AR1_6meses <- ipca_2018e2019$IPCA[1:6] - previsto_6meses_AR1
residuo_AR2_6meses <- ipca_2018e2019$IPCA[1:6] - previsto_6meses_AR2
residuo_AR3_6meses <- ipca_2018e2019$IPCA[1:6] - previsto_6meses_AR3
residuo_ARMA_6meses <- ipca_2018e2019$IPCA[1:6] - previsto_6meses_ARMA

# 9meses
residuo_AR1_9meses <- ipca_2018e2019$IPCA[1:9] - previsto_9meses_AR1
residuo_AR2_9meses <- ipca_2018e2019$IPCA[1:9] - previsto_9meses_AR2
residuo_AR3_9meses <- ipca_2018e2019$IPCA[1:9] - previsto_9meses_AR3
residuo_ARMA_9meses <- ipca_2018e2019$IPCA[1:9] - previsto_9meses_ARMA

# 12 meses
residuo_AR1_12meses <- ipca_2018e2019$IPCA[1:12] - previsto_12meses_AR1
residuo_AR2_12meses <- ipca_2018e2019$IPCA[1:12] - previsto_12meses_AR2
residuo_AR3_12meses <- ipca_2018e2019$IPCA[1:12] - previsto_12meses_AR3
residuo_ARMA_12meses <- ipca_2018e2019$IPCA[1:12] - previsto_12meses_ARMA

# 24 meses 
residuo_AR1_24meses <- ipca_2018e2019$IPCA[1:24] - previsto_24meses_AR1
residuo_AR2_24meses <- ipca_2018e2019$IPCA[1:24] - previsto_24meses_AR2
residuo_AR3_24meses <- ipca_2018e2019$IPCA[1:24] - previsto_24meses_AR3
residuo_ARMA_24meses <- ipca_2018e2019$IPCA[1:24] - previsto_24meses_ARMA


# GR?FICOS - MODELO AUTORREGRESSIVO

par(mfrow=c(2,2))

# 6 MESES
plot(previsto_6meses_AR1, type="l", col = "black", ylim = c(-0.5,1.7),
     xlab = "Meses",  ylab = "Taxa de infla??o", main = "AR x IPCA, h = 6", xlim = range(c(1:6)))
lines(previsto_6meses_AR2, col = "purple")
lines(previsto_6meses_AR3, col = "green")
lines(ipca_2018e2019$IPCA[1:6], col = "red")  
legend("topleft", legend = c("AR1", "AR2", "AR3", "IPCA"), col = c("black","purple", "green", "red"), lwd = 1, cex = 0.65) # cex eh pra diminuir o tamanho da legenda

# 9 MESES
plot(previsto_9meses_AR1, type="l", col = "black", ylim = c(-0.5,1.7),
     xlab = "Meses",  ylab = "Taxa de infla??o", main = "AR x IPCA, h = 9", xlim = range(c(1:9)))
lines(previsto_9meses_AR2, col = "purple")
lines(previsto_9meses_AR3, col = "green")
lines(ipca_2018e2019$IPCA[1:9], col = "red")  
legend("topleft", legend = c("AR1", "AR2", "AR3", "IPCA"), col = c("black","purple", "green", "red"), lwd = 1, cex = 0.65) 

# 12 MESES
plot(previsto_12meses_AR1, type="l", col = "black", ylim = c(-0.5,1.7),
     xlab = "Meses",  ylab = "Taxa de infla??o", main = "AR x IPCA, h = 12", xlim = range(c(1:12)))
lines(previsto_12meses_AR2, col = "purple")
lines(previsto_12meses_AR3, col = "green")
lines(ipca_2018e2019$IPCA[1:12], col = "red")  
legend("topleft", legend = c("AR1", "AR2", "AR3", "IPCA"), col = c("black","purple", "green", "red"), lwd = 1, cex = 0.65) 

# 24 MESES
plot(previsto_24meses_AR1, type="l", col = "black", ylim = c(-0.5,1.7),
     xlab = "Meses",  ylab = "Taxa de infla??o", main = "AR x IPCA, h = 24", xlim = range(c(1:24)))
lines(previsto_24meses_AR2, col = "purple")
lines(previsto_24meses_AR3, col = "green")
lines(ipca_2018e2019$IPCA[1:24], col = "red")  
legend("topleft", legend = c("AR1", "AR2", "AR3", "IPCA"), col = c("black","purple", "green", "red"), lwd = 1, cex = 0.65) 

par(mfrow=c(1,1))


# ERRO DE PREVISAO  - MSE E RMSE

#AR1 
#6 meses
mse(ipca_2018e2019$IPCA[1:6],previsto_6meses_AR1) #
Metrics::rmse(ipca_2018e2019$IPCA[1:6],previsto_6meses_AR1) #                (0.3630308)
#9 meses
mse(ipca_2018e2019$IPCA[1:9],previsto_9meses_AR1) #                         antigo0.1153827
Metrics::rmse(ipca_2018e2019$IPCA[1:9],previsto_9meses_AR1) #                 antigo0.3396803
#12 meses
mse(ipca_2018e2019$IPCA[1:12],previsto_12meses_AR1) #                           antigo0.1012665
Metrics::rmse(ipca_2018e2019$IPCA[1:12],previsto_12meses_AR1) #              antigo0.3182239
#24 meses
mse(ipca_2018e2019$IPCA[1:24],previsto_24meses_AR1) #                                  antigo0.1061136
Metrics::rmse(ipca_2018e2019$IPCA[1:24],previsto_24meses_AR1) #                     antigo0.3257509


#AR2
#6 meses
mse(ipca_2018e2019$IPCA[1:6],previsto_6meses_AR2) #0.2375065                           antigo0.1560399
Metrics::rmse(ipca_2018e2019$IPCA[1:6],previsto_6meses_AR2) # 0.4873464               antigo0.3950189
#9 meses
mse(ipca_2018e2019$IPCA[1:9],previsto_9meses_AR2) #0.1739101                           antigo0.1154635
Metrics::rmse(ipca_2018e2019$IPCA[1:9],previsto_9meses_AR2) #0.4170253                 antigo0.3397992
#12 meses
mse(ipca_2018e2019$IPCA[1:12],previsto_12meses_AR2) #0.1866701                           antigo0.1013382
Metrics::rmse(ipca_2018e2019$IPCA[1:12],previsto_12meses_AR2) #0.4320533                antigo0.3183367
#24 meses
mse(ipca_2018e2019$IPCA[1:24],previsto_24meses_AR2) # 0.1356683                          antigo0.1062221
Metrics::rmse(ipca_2018e2019$IPCA[1:24],previsto_24meses_AR2) #0.3683318               antigo0.3259173


#AR3
#6 meses
mse(ipca_2018e2019$IPCA[1:6],previsto_6meses_AR3) # 0.2402218                            antigo0.1617577
Metrics::rmse(ipca_2018e2019$IPCA[1:6],previsto_6meses_AR3) # 0.4901243                antigo0.4021911
#9 meses
mse(ipca_2018e2019$IPCA[1:9],previsto_9meses_AR3) #0.1754369                            antigo0.1192943
Metrics::rmse(ipca_2018e2019$IPCA[1:9],previsto_9meses_AR3) #0.4188519                 antigo0.3453901
#12 meses
mse(ipca_2018e2019$IPCA[1:12],previsto_12meses_AR3) #0.1863155                         antigo0.1041352
Metrics::rmse(ipca_2018e2019$IPCA[1:12],previsto_12meses_AR3) #0.4316427             antigo0.3226998
#24 meses
mse(ipca_2018e2019$IPCA[1:24],previsto_24meses_AR3) #0.134974                        antigo0.1068062
Metrics::rmse(ipca_2018e2019$IPCA[1:24],previsto_24meses_AR3) #0.3673881              antigo0.3268121


#ARMA(4,3)
#6 meses
mse(ipca_2018e2019$IPCA[1:6],previsto_6meses_ARMA) # 
Metrics::rmse(ipca_2018e2019$IPCA[1:6],previsto_6meses_ARMA) # 
#9 meses
mse(ipca_2018e2019$IPCA[1:9],previsto_9meses_ARMA) #
Metrics::rmse(ipca_2018e2019$IPCA[1:9],previsto_9meses_ARMA) #
#12 meses
mse(ipca_2018e2019$IPCA[1:12],previsto_12meses_ARMA) #
Metrics::rmse(ipca_2018e2019$IPCA[1:12],previsto_12meses_ARMA) # 
#24 meses
mse(ipca_2018e2019$IPCA[1:24],previsto_24meses_ARMA) #
Metrics::rmse(ipca_2018e2019$IPCA[1:24],previsto_24meses_ARMA) #



###################################### TESTE DE DIEBOLD-MARIANO

# se p-valor for menor que 0.05, rejeito H0 de que os erros sao iguais. Ele sao diferentes estatisticamente.
# se p-valor for maior que 0.05, os erros sao iguais estatisticamente
# OU
# Para 5% (2,5% para cada lado), rejeitar? se a estat?stica DM for maior que 1,96 em modulo

# NAO SAO SIGNIFICATIVOS PARA NENHUM HORIZONTE

# 6 meses
dm.test(residuo_AR1_6meses, residuo_AR2_6meses) # p-valor 0.6858                       antigo0.6145
dm.test(residuo_AR1_6meses, residuo_AR3_6meses) # p-valor 0.9887                      antigo0.1251
dm.test(residuo_AR2_6meses, residuo_AR3_6meses) # p-valor 0.6899                     antigo0.128

# 9 meses
dm.test(residuo_AR1_9meses, residuo_AR2_9meses) # p-valor 0.6327                    antigo0.5995
dm.test(residuo_AR1_9meses, residuo_AR3_9meses) # p-valor 0.961                      antigo 0.123
dm.test(residuo_AR2_9meses, residuo_AR3_9meses) # p-valor 0.7337                     antigo0.1261

# 12 meses
dm.test(residuo_AR1_12meses, residuo_AR2_12meses) # p-valor 0.4459                     antigo0.5254
dm.test(residuo_AR1_12meses, residuo_AR3_12meses) # p-valor 0.0.6762                     antigo0.132
dm.test(residuo_AR2_12meses, residuo_AR3_12meses) # p-valor 0.9196                    antigo0.1369

# 24 meses
dm.test(residuo_AR1_24meses, residuo_AR2_24meses) # p-valor 0.3467                     antigo0.08187
dm.test(residuo_AR1_24meses, residuo_AR3_24meses) # p-valor 0.5025                      antigo0.49
dm.test(residuo_AR2_24meses, residuo_AR3_24meses) # p-valor 0.6867                      antigo0.5583


# GR?FICO - MODELO ARMA
par(mfrow=c(2,2))

# 6 MESES
plot(previsto_6meses_ARMA, type="l", col = "darkorchid2", ylim = c(-0.5,1.7),
     xlab = "Meses",  ylab = "Taxa de infla??o", main = "ARMA(4,3) x IPCA, h = 6", xlim = range(c(1:6)))
lines(ipca_2018e2019$IPCA[1:6], col = "red")  
legend("topleft", legend = c("ARMA(4,3)", "IPCA"), col = c("darkorchid2", "red"), lwd = 1, cex = 0.65) # cex eh pra diminuir o tamanho da legenda

# 9 MESES
plot(previsto_9meses_ARMA, type="l", col = "darkorchid2", ylim = c(-0.5,1.7),
     xlab = "Meses",  ylab = "Taxa de infla??o", main = "ARMA(4,3) x IPCA, h = 9", xlim = range(c(1:9)))
lines(ipca_2018e2019$IPCA[1:9], col = "red")  
legend("topleft", legend = c("ARMA(4,3)", "IPCA"), col = c("darkorchid2", "red"), lwd = 1, cex = 0.65) 

# 12 MESES
plot(previsto_12meses_ARMA, type="l", col = "darkorchid2", ylim = c(-0.5,1.7),
     xlab = "Meses",  ylab = "Taxa de infla??o", main = "ARMA(4,3) x IPCA, h = 12", xlim = range(c(1:12)))
lines(ipca_2018e2019$IPCA[1:12], col = "red")  
legend("topleft", legend = c("ARMA(4,3)", "IPCA"), col = c("darkorchid2", "red"), lwd = 1, cex = 0.65) 

# 24 MESES
plot(previsto_24meses_ARMA, type="l", col = "darkorchid", ylim = c(-0.5,1.7),
     xlab = "Meses",  ylab = "Taxa de infla??o", main = "ARMA x IPCA, h = 24", xlim = range(c(1:24)))
lines(ipca_2018e2019$IPCA[1:24], col = "red")  
legend("topleft", legend = c("ARMA (4,3)" , "IPCA"), col = c("darkorchid2", "red"), lwd = 1, cex = 0.65) 

par(mfrow=c(1,1))



####################################################### MODELO KNN ###################################


# Dividindo a base de dados entre treino e teste
??createDataPartition    # da um split nos dados. Para dividir entre treino e teste
indiceDivisao <- createDataPartition(y = dados_transformados$ipca, p = 0.7, list = FALSE)
training <- dados_transformados[indiceDivisao,]   #70% dos dados fica para treino. 30% para teste
testing <- dados_transformados[-indiceDivisao,]   #o -indiceDivisao eh o q sobrou
#training <- dados_treino
#testing <- dados_teste


#Vendo a distribuicao da base de dados original e dos dados de treino
# serve pra ver se meus dados de treino representam bem a base de dados original
prop.table(table(training$ipca)) * 100
prop.table(table(dados_transformados$ipca)) * 100


#Normalizando os dados
trainX <- training[,names(training) != "ipca"]  #nos dados de treino, pego tudo menos o ipca. Ou seja, todas as variaveis preditoras
pre_processamento <- preProcess(x = trainX, method = c("center", "scale"))  #normalizara os dados pelo metodo de escala. Subtrai a media (center) e divide pelo desvio padrao(scale)
pre_processamento


#Criando o modelo
set.seed(555)
??trainControl   #arquivo de controle. Serve para criar um objeto com alguns parametros, e depois usa-se esse objeto em outra funcao
?train  #essa funcao constroi o modelo
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)#fara repetidas iteracoes tentnado buscar a melhor combinacao possivel. Nesse caso, 3 repeticoes
# a proxima linha cria o modelo
knnFit <- train(ipca ~ ., data = training, method = "knn", metric ='RMSE', trControl = ctrl, preProcess = c("center", "scale"), tuneLength = 20, na.action = na.omit)

#Modelo KNN
knnFit


#grafico do numero de vizinhos K
plot(knnFit)  


# Fazendo previsao
previsao_KNN <- predict(knnFit, newdata = testing)  
previsao_KNN

knn_3meses <- previsao_KNN[0:3]
knn_6meses <- previsao_KNN[0:6]
knn_9meses <- previsao_KNN[0:9]
knn_12meses <- previsao_KNN[0:12]
knn_24meses <- previsao_KNN[0:24]


# KNN x IPCA

par(mfrow=c(2,2))

# 6 MESES
plot(knn_6meses, type="l", col = "blue", ylim = c(-0.5,1.7),
     xlab = "Meses",  ylab = "Taxa de infla??o", main = "KNN x IPCA, h = 6", xlim = range(c(1:6)))
lines(ipca_2018e2019$IPCA[1:6], col = "red")  
legend("topleft", legend = c("KNN", "IPCA"), col = c("blue", "red"), lwd = 1, cex = 0.65) # cex eh pra diminuir o tamanho da legenda

# 9 MESES
plot(knn_9meses, type="l", col = "blue", ylim = c(-0.5,1.7),
     xlab = "Meses",  ylab = "Taxa de infla??o", main = "KNN x IPCA, h = 9", xlim = range(c(1:9)))
lines(ipca_2018e2019$IPCA[1:9], col = "red")  
legend("topleft", legend = c("KNN", "IPCA"), col = c("blue", "red"), lwd = 1, cex = 0.65) 

# 12 MESES
plot(knn_12meses, type="l", col = "blue", ylim = c(-0.5,1.7),
     xlab = "Meses",  ylab = "Taxa de infla??o", main = "KNN x IPCA, h = 12", xlim = range(c(1:12)))
lines(ipca_2018e2019$IPCA[1:12], col = "red")  
legend("topleft", legend = c("KNN", "IPCA"), col = c("blue", "red"), lwd = 1, cex = 0.65) 

# 24 MESES
plot(knn_24meses, type="l", col = "blue", ylim = c(-0.5,1.7),
     xlab = "Meses",  ylab = "Taxa de infla??o", main = "KNN x IPCA, h = 24", xlim = range(c(1:24)))
lines(ipca_2018e2019$IPCA[1:24], col = "red")  
legend("topleft", legend = c("KNN" , "IPCA"), col = c("blue", "red"), lwd = 1, cex = 0.65) 

par(mfrow=c(1,1))



#KNN - erros de previsao
#6 meses
mse(ipca_2018e2019$IPCA[1:6],knn_6meses) #0.140732                      antigo0.08426133
Metrics::rmse(ipca_2018e2019$IPCA[1:6],knn_6meses) #0.3751426           antigo0.290278
#9 meses
mse(ipca_2018e2019$IPCA[1:9],knn_9meses) #0.1037209                     antigo0.06067778
Metrics::rmse(ipca_2018e2019$IPCA[1:9],knn_9meses) #0.3220573           antigo0.2463286
#12 meses
mse(ipca_2018e2019$IPCA[1:12],knn_12meses) #0.09789367                  antigo0.048061
Metrics::rmse(ipca_2018e2019$IPCA[1:12],knn_12meses) #0.3128796         antigo0.2192282
#24 meses
mse(ipca_2018e2019$IPCA[1:24],knn_24meses) #0.1147353                   antigo0.03824683
Metrics::rmse(ipca_2018e2019$IPCA[1:24],knn_24meses) #0.338726         antigo0.195568


# RESIDUOS
residuo_KNN_6meses <- ipca_2018e2019$IPCA[1:6] - knn_6meses
residuo_KNN_9meses <- ipca_2018e2019$IPCA[1:9] - knn_9meses
residuo_KNN_12meses <- ipca_2018e2019$IPCA[1:12] - knn_12meses
residuo_KNN_24meses <- ipca_2018e2019$IPCA[1:24] - knn_24meses




######################################################### MACHINE LEARNING ##########################################################


library(tidymodels)
library(tidyverse)
library(kknn)
library(ISLR)


treino_novo <- training[,-1]

row.has.na <- apply(training, 1, function(x){any(is.na(x))}) # vendo quais  colunas tem NA nos dados de treino
predictors_no_NA <- training[!row.has.na, ]

linha_com_na <- apply(testing, 1, function(x){any(is.na(x))}) # vendo quais  colunas tem NA nos dados de teste
base_teste <- testing[!linha_com_na, ]



##############################################################  SVM

library(e1071)

# SVM 1
set.seed(875)
modelo_svm <- svm(ipca ~ . , data = predictors_no_NA, type = 'eps-regression')
summary(modelo_svm) #70 vetores de suporte. Aqui esta usando kernel RADIAL

# SVM 2
# fazendo outro modelo com melhoria dos parametros
# tunando os hiperparametros (no caso, cost e gamma)
set.seed(123)
tune_results <- tune(svm, ipca ~ . , data = predictors_no_NA, 
                     ranges = list(cost = 10^(-1:2), gamma = c(.5,1,2)))

summary(tune_results)

modelo_svm_tunado <- svm(ipca ~ . , data = predictors_no_NA, type = 'eps-regression', 
                         kernel = "linear", cost = 0.1, gamma = 0.5)
summary(modelo_svm_tunado) #47 vetores de suporte. Aqui esta usando kernel LINEAR



# SVM COM CARET
library(kernlab)
set.seed(456)
control_svm <- trainControl(method = "repeatedcv", number = 10, repeats = 2)
svm_caret <- train(ipca ~ . , data = predictors_no_NA, method = "svmRadial", trControl = control_svm)#com kernel radial (rbf)
svm_caret$finalModel  # TAMBEM COM KERNEL RADIAL (RBF) e com 67 vetores de suporte
plot(svm_caret)



# prever os 3 SVM's e depois plotar contra a base de teste. E depois contra o IPCA

# SVM 1 - sem tunar
previsao_svm1 <- predict(modelo_svm, newdata = base_teste)
previsao_svm1_3meses <- previsao_svm1[1:3]
previsao_svm1_6meses <- previsao_svm1[1:6]
previsao_svm1_9meses <- previsao_svm1[1:9]
previsao_svm1_12meses <- previsao_svm1[1:12]
previsao_svm1_24meses <- previsao_svm1[1:24]

# SVM 2 - tunado
previsao_svm2 <- predict(modelo_svm_tunado, newdata = base_teste)
previsao_svm2_3meses <- previsao_svm2[1:3]
previsao_svm2_6meses <- previsao_svm2[1:6]
previsao_svm2_9meses <- previsao_svm2[1:9]
previsao_svm2_12meses <- previsao_svm2[1:12]
previsao_svm2_24meses <- previsao_svm2[1:24]

previsao_svm2

# SVM 3 - caret
previsao_svm_caret <- predict(svm_caret, newdata = base_teste)
previsao_svm_caret_3meses <- previsao_svm_caret[1:3]
previsao_svm_caret_6meses <- previsao_svm_caret[1:6]
previsao_svm_caret_9meses <- previsao_svm_caret[1:9]
previsao_svm_caret_12meses <- previsao_svm_caret[1:12]
previsao_svm_caret_24meses <- previsao_svm_caret[1:24]


# GR?FICO - Support Vector Machines

par(mfrow=c(2,2))

# 6 MESES
plot(previsao_svm1_6meses, type="l", col = "blue", ylim = c(-0.5,1.7),
     xlab = "Meses",  ylab = "Taxa de infla??o", main = "SVM x IPCA, h = 6", xlim = range(c(1:6)))
lines(previsao_svm2_6meses, col = "green")
lines(previsao_svm_caret_6meses, col = "darkgoldenrod2")
lines(ipca_2018e2019$IPCA[1:6], col = "red")  
legend("topleft", legend = c("SVM 1", "SVM 2", "SVM 3", "IPCA"), col = c("blue","green", "darkgoldenrod2", "red"), lwd = 1, cex = 0.65) # cex eh pra diminuir o tamanho da legenda

# 9 MESES
plot(previsao_svm1_9meses, type="l", col = "blue", ylim = c(-0.5,1.7),
     xlab = "Meses",  ylab = "Taxa de infla??o", main = "SVM x IPCA, h = 9", xlim = range(c(1:9)))
lines(previsao_svm2_9meses, col = "green")
lines(previsao_svm_caret_9meses, col = "darkgoldenrod2")
lines(ipca_2018e2019$IPCA[1:9], col = "red")  
legend("topleft", legend = c("SVM 1", "SVM 2", "SVM 3", "IPCA"), col = c("blue","green", "darkgoldenrod2", "red"), lwd = 1, cex = 0.65) 

# 12 MESES
plot(previsao_svm1_12meses, type="l", col = "blue", ylim = c(-0.5,1.7),
     xlab = "Meses",  ylab = "Taxa de infla??o", main = "SVM x IPCA, h = 12", xlim = range(c(1:12)))
lines(previsao_svm2_12meses, col = "green")
lines(previsao_svm_caret_12meses, col = "darkgoldenrod2")
lines(ipca_2018e2019$IPCA[1:12], col = "red")  
legend("topleft", legend = c("SVM 1", "SVM 2", "SVM 3", "IPCA"), col = c("blue","green", "darkgoldenrod2", "red"), lwd = 1, cex = 0.65) 

# 24 MESES
plot(previsao_svm1_24meses, type="l", col = "blue", ylim = c(-0.5,1.7),
     xlab = "Meses",  ylab = "Taxa de infla??o", main = "SVM x IPCA, h = 24", xlim = range(c(1:24)))
lines(previsao_svm2_24meses, col = "green")
lines(previsao_svm_caret_24meses, col = "darkgoldenrod2")
lines(ipca_2018e2019$IPCA[1:24], col = "red")  
legend("topleft", legend = c("SVM 1", "SVM 2", "SVM 3", "IPCA"), col = c("blue","green", "darkgoldenrod2", "red"), lwd = 1, cex = 0.65) 

par(mfrow=c(1,1))


#ERROS DE PREVISAO - SVM
# SVM1
#6meses
mse(ipca_2018e2019$IPCA[1:6],previsao_svm1_6meses) # 0.1602109                   
Metrics::rmse(ipca_2018e2019$IPCA[1:6],previsao_svm1_6meses) #0.4002636        
#9 meses
mse(ipca_2018e2019$IPCA[1:9],previsao_svm1_9meses) #0.1363856                    
Metrics::rmse(ipca_2018e2019$IPCA[1:9],previsao_svm1_9meses) # 0.3693042         
#12 meses
mse(ipca_2018e2019$IPCA[1:12],previsao_svm1_12meses) #0.1537079                   
Metrics::rmse(ipca_2018e2019$IPCA[1:12],previsao_svm1_12meses) #0.3920559          
#24 meses
mse(ipca_2018e2019$IPCA[1:24],previsao_svm1_24meses) #0.1599787                    
Metrics::rmse(ipca_2018e2019$IPCA[1:24],previsao_svm1_24meses) #0.3999734        

# SVM2
#6meses
mse(ipca_2018e2019$IPCA[1:6],previsao_svm2_6meses) # 0.2449443                    
Metrics::rmse(ipca_2018e2019$IPCA[1:6],previsao_svm2_6meses) #0.4949185             
#9 meses
mse(ipca_2018e2019$IPCA[1:9],previsao_svm2_9meses) #0.1842655                       
Metrics::rmse(ipca_2018e2019$IPCA[1:9],previsao_svm2_9meses) #0.4292616             
#12 meses
mse(ipca_2018e2019$IPCA[1:12],previsao_svm2_12meses) #0.1981388                     
Metrics::rmse(ipca_2018e2019$IPCA[1:12],previsao_svm2_12meses) #0.4451278          
#24 meses
mse(ipca_2018e2019$IPCA[1:24],previsao_svm2_24meses) # 0.1938691                     
Metrics::rmse(ipca_2018e2019$IPCA[1:24],previsao_svm2_24meses) #  0.4403057         


# SVM3
#6 meses
mse(ipca_2018e2019$IPCA[1:6],previsao_svm_caret_6meses) #0.1682024                            
Metrics::rmse(ipca_2018e2019$IPCA[1:6],previsao_svm_caret_6meses) # 0.4101249                 
#9 meses
mse(ipca_2018e2019$IPCA[1:9],previsao_svm_caret_9meses) # 0.1386297                         
Metrics::rmse(ipca_2018e2019$IPCA[1:9],previsao_svm_caret_9meses) # 0.3723301                
#12 meses
mse(ipca_2018e2019$IPCA[1:12],previsao_svm_caret_12meses) #0.1576075                          
Metrics::rmse(ipca_2018e2019$IPCA[1:12],previsao_svm_caret_12meses) #0.3969981                
#24 meses
mse(ipca_2018e2019$IPCA[1:24],previsao_svm_caret_24meses) # 0.1653519                         
Metrics::rmse(ipca_2018e2019$IPCA[1:24],previsao_svm_caret_24meses) # 0.4066348               



# Residuos

# 6 meses
residuo_SVM1_6meses <- ipca_2018e2019$IPCA[1:6] - previsao_svm1_6meses
residuo_SVM2_6meses <- ipca_2018e2019$IPCA[1:6] - previsao_svm2_6meses
residuo_SVM3_6meses <- ipca_2018e2019$IPCA[1:6] - previsao_svm_caret_6meses

# 9 meses
residuo_SVM1_9meses <- ipca_2018e2019$IPCA[1:9] - previsao_svm1_9meses
residuo_SVM2_9meses <- ipca_2018e2019$IPCA[1:9] - previsao_svm2_9meses
residuo_SVM3_9meses <- ipca_2018e2019$IPCA[1:9] - previsao_svm_caret_9meses

# 12 meses
residuo_SVM1_12meses <- ipca_2018e2019$IPCA[1:12] - previsao_svm1_12meses
residuo_SVM2_12meses <- ipca_2018e2019$IPCA[1:12] - previsao_svm2_12meses
residuo_SVM3_12meses <- ipca_2018e2019$IPCA[1:12] - previsao_svm_caret_12meses

# 24 meses
residuo_SVM1_24meses <- ipca_2018e2019$IPCA[1:24] - previsao_svm1_24meses
residuo_SVM2_24meses <- ipca_2018e2019$IPCA[1:24] - previsao_svm2_24meses
residuo_SVM3_24meses <- ipca_2018e2019$IPCA[1:24] - previsao_svm_caret_24meses



####################################### TESTE DE DIEBOLD-MARIANO

# se p-valor for menor que 0.05, rejeito H0 de que os erros sao iguais. Ele sao diferentes estatisticamente.
# se p-valor for maior que 0.05, os erros sao iguais estatisticamente
# OU
# Para 5% (2,5% para cada lado), rejeitar? se a estat?stica DM for maior que 1,96 em modulo

#NAO SAO SIGNIFICATIVOS PARA NENHUM HORIZONTe

# 6 meses
dm.test(residuo_SVM1_6meses, residuo_SVM2_6meses) #p-valor 0.2045                    antes0.1997
dm.test(residuo_SVM1_6meses, residuo_SVM3_6meses) #p-valor 0.3565                    antes0.05653  # diferente
dm.test(residuo_SVM2_6meses, residuo_SVM3_6meses) #p-valor 0.2023                    antes0.2623 

# 9 meses
dm.test(residuo_SVM1_9meses, residuo_SVM2_9meses) #p-valor 0.2914                      antes0.1751
dm.test(residuo_SVM1_9meses, residuo_SVM3_9meses) #p-valor 0.729                     antes0.0409 diferente
dm.test(residuo_SVM2_9meses, residuo_SVM3_9meses) #p-valor 0.2584                          antes0.2415

# 12 meses
dm.test(residuo_SVM1_12meses, residuo_SVM2_12meses) #p-valor  0.1868                      antes0.1708
dm.test(residuo_SVM1_12meses, residuo_SVM3_12meses) #p-valor  0.431                      antes0.04206 diferente
dm.test(residuo_SVM2_12meses, residuo_SVM3_12meses) #p-valor  0.1751                  antes0.236  

# 24 meses
dm.test(residuo_SVM1_24meses, residuo_SVM2_24meses) #p-valor 0.06411               antes0.02757 diferente
dm.test(residuo_SVM1_24meses, residuo_SVM3_24meses) #p-valor 0.07307                   antes0.00148  diferente
dm.test(residuo_SVM2_24meses, residuo_SVM3_24meses) #p-valor 0.0766            antes0.05968  um pouco diferente pq o DM ? |1,98| > |1,96|


#########################################################################



#### outro xgboost

library(xgboost)
library(Ckmeans.1d.dp)
library(DiagrammeR)
library(ggplot2)
library(corrplot)
library(Rtsne)
library(knitr)
library(dplyr)
library(magrittr)
library(Matrix)


############################################################# LIGHT GBM

library(lightgbm)
library(bonsai)

# boosted tree = decision trees where each tree depends on the results of previous trees

??boost_tree
# a funcao boost_tree define um modelo que cria varias arvores de decisao, sendo que cada arvore ? uma boosted tree
# todas as arvores sao combinadas para gerar uma predicao final

lgb_recipe <- recipe( ipca ~ . , data = predictors_no_NA[]) %>%
  step_nzv(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors())

lgbm_spec <- boost_tree(
  trees = tune(), 
  tree_depth = tune(), #max_depth = o numero de splits
  min_n = tune(), #numero minimo de data points em um n? para que esse n? seja splitado
  loss_reduction = tune(), #a reducao requerida na funcao de perda para gerar mais splits
  mtry = tune(), #proporcao de preditores selecionados aleatoriamente em cada split para criar os tree models
  learn_rate = tune() #shrinkage parameter; taxa pela qual o algoritmo de boosting se adapta a cada iteracao
) %>%
  set_engine("lightgbm") %>%
  set_mode("regression")
lgbm_spec


train_sparse <- Matrix(as.matrix(predictors_no_NA[,-1:-2]), sparse = T)
#test_sparse <- Matrix(as.matrix(predictors_no_NA[,2]), sparse = T)   

y_train <- predictors_no_NA$ipca

dtrain <- lgb.Dataset(train_sparse, label = y_train)


param <- list(objective = "regression",
              learning_rate = 0.03,
              num_threads  = 2
              )



model_cv <- lgb.cv(
  params = param
  , data = dtrain
  , nrounds = 5
  , nfold = 5
)
model_cv$best_score

best.iter = 5

lgb_model <- lgb.train(params = param, data = dtrain, nrounds = 5)
lgb_model

predict(lgb_model, newdata = base_teste)


##### LIGHT GBM - MODELO 1
set.seed(1)
lgbm_mod <- 
  boost_tree() %>%
  set_engine(engine = "lightgbm") %>%
  set_mode(mode = "regression") %>%
  fit(formula = ipca ~ ., data = predictors_no_NA[,-1])

lgbm_mod

LGBM1_prev <- predict(lgbm_mod, new_data = base_teste[,-1])


#### LIGHT GBM - MODELO 2
set.seed(159)
lgbm_model <- boost_tree(
  mtry = 7, #colsample_bytree
  trees = 200, #nrounds
  min_n = 3,  #min_child_weight
  tree_depth = 2, #max_depth
  learn_rate = 0.1, #eta
  loss_reduction = 0, #gamma
) %>%
  set_mode("regression") %>%
  set_engine("lightgbm", objective = "regression") %>%
  fit(ipca ~ . ,data = predictors_no_NA[,-1])

lgbm_model


LGBM2_prev <- predict(lgbm_model, new_data = base_teste[,-1])
LGBM2_prev


prev_lgbm1_24meses <- LGBM1_prev$.pred[1:24]
prev_lgbm2_24meses <- LGBM2_prev$.pred[1:24]



# GR?FICO - LGBM x IPCA
par(mfrow=c(2,2))

# 6 MESES
plot(LGBM1_prev$.pred[1:6], type="l", col = "blue", ylim = c(-0.5,1.7),
     xlab = "Meses",  ylab = "Taxa de infla??o", main = "LGBM x IPCA, h = 6", xlim = range(c(1:6)))
lines(LGBM2_prev$.pred[1:6], col = "chartreuse3")
lines(ipca_2018e2019$IPCA[1:6], col = "red")  
legend("topleft", legend = c("LGBM 1", "LGBM 2", "IPCA"), col = c("blue","chartreuse3", "red"), lwd = 1, cex = 0.65) # cex eh pra diminuir o tamanho da legenda

# 9 MESES
plot(LGBM1_prev$.pred[1:9], type="l", col = "blue", ylim = c(-0.5,1.7),
     xlab = "Meses",  ylab = "Taxa de infla??o", main = "LGBM x IPCA, h = 9", xlim = range(c(1:9)))
lines(LGBM2_prev$.pred[1:9], col = "chartreuse3")
lines(ipca_2018e2019$IPCA[1:9], col = "red")  
legend("topleft", legend = c("LGBM 1", "LGBM 2", "IPCA"), col = c("blue","chartreuse3", "red"), lwd = 1, cex = 0.65) 

# 12 MESES
plot(LGBM1_prev$.pred[1:12], type="l", col = "blue", ylim = c(-0.5,1.7),
     xlab = "Meses",  ylab = "Taxa de infla??o", main = "LGBM x IPCA, h = 12", xlim = range(c(1:12)))
lines(LGBM2_prev$.pred[1:12], col = "chartreuse3")
lines(ipca_2018e2019$IPCA[1:12], col = "red")  
legend("topleft", legend = c("LGBM 1", "LGBM 2", "IPCA"), col = c("blue","chartreuse3", "red"), lwd = 1, cex = 0.65) 

# 24 MESES
plot(prev_lgbm1_24meses, type="l", col = "blue", ylim = c(-0.5,1.7),
     xlab = "Meses",  ylab = "Taxa de infla??o", main = "LGBM x IPCA, h = 24", xlim = range(c(1:24)))
lines(prev_lgbm2_24meses, col = "chartreuse3")
lines(ipca_2018e2019$IPCA[1:24], col = "red")  
legend("topleft", legend = c("LGBM 1", "LGBM 2", "IPCA"), col = c("blue","chartreuse3", "red"), lwd = 1, cex = 0.65) 

par(mfrow=c(1,1))


#Erros de previsao 
#Light GBM 1
#6 meses
mse(ipca_2018e2019$IPCA[1:6],LGBM1_prev$.pred[1:6]) # 
Metrics::rmse(ipca_2018e2019$IPCA[1:6],LGBM1_prev$.pred[1:6]) #
#9 meses
mse(ipca_2018e2019$IPCA[1:9],LGBM1_prev$.pred[1:9]) # 
Metrics::rmse(ipca_2018e2019$IPCA[1:9],LGBM1_prev$.pred[1:9]) #
#12 meses  
mse(ipca_2018e2019$IPCA[1:12],LGBM1_prev$.pred[1:12]) # 
Metrics::rmse(ipca_2018e2019$IPCA[1:12],LGBM1_prev$.pred[1:12]) # 
#24 meses
mse(ipca_2018e2019$IPCA[1:24],LGBM1_prev$.pred[1:24]) #  
Metrics::rmse(ipca_2018e2019$IPCA[1:24],LGBM1_prev$.pred[1:24]) # 

#Light GBM 2
#6 meses
mse(ipca_2018e2019$IPCA[1:6],LGBM2_prev$.pred[1:6]) # 
Metrics::rmse(ipca_2018e2019$IPCA[1:6],LGBM2_prev$.pred[1:6]) # 
#9 meses
mse(ipca_2018e2019$IPCA[1:9],LGBM2_prev$.pred[1:9]) #  
Metrics::rmse(ipca_2018e2019$IPCA[1:9],LGBM2_prev$.pred[1:9]) # 
#12 meses
mse(ipca_2018e2019$IPCA[1:12],LGBM2_prev$.pred[1:12]) # 
Metrics::rmse(ipca_2018e2019$IPCA[1:12],LGBM2_prev$.pred[1:12]) # 
#24 meses
mse(ipca_2018e2019$IPCA[1:24],LGBM2_prev$.pred[1:24]) #
Metrics::rmse(ipca_2018e2019$IPCA[1:24],LGBM2_prev$.pred[1:24]) # 


# RESIDUOS

# 6 meses
residuo_LGBM1_6meses <- ipca_2018e2019$IPCA[1:6] - LGBM1_prev$.pred[1:6]
residuo_LGBM2_6meses <- ipca_2018e2019$IPCA[1:6] - LGBM2_prev$.pred[1:6]

# 9 meses
residuo_LGBM1_9meses <- ipca_2018e2019$IPCA[1:9] - LGBM1_prev$.pred[1:9]
residuo_LGBM2_9meses <- ipca_2018e2019$IPCA[1:9] - LGBM2_prev$.pred[1:9]

# 12 meses
residuo_LGBM1_12meses <- ipca_2018e2019$IPCA[1:12] - LGBM1_prev$.pred[1:12]
residuo_LGBM2_12meses <- ipca_2018e2019$IPCA[1:12] - LGBM2_prev$.pred[1:12]

# 24 meses
residuo_LGBM1_24meses <- ipca_2018e2019$IPCA[1:24] - LGBM1_prev$.pred[1:24]
residuo_LGBM2_24meses <- ipca_2018e2019$IPCA[1:24] - LGBM2_prev$.pred[1:24]



####################################### TESTE DE DIEBOLD-MARIANO

# se p-valor for menor que 0.05, rejeito H0 de que os erros sao iguais. Ele sao diferentes estatisticamente.
# se p-valor for maior que 0.05, os erros sao iguais estatisticamente
# OU
# Para 5% (2,5% para cada lado), rejeitar? se a estat?stica DM for maior que 1,96 em modulo

# 6 meses
dm.test(residuo_LGBM1_6meses, residuo_LGBM2_6meses) #p-valor 0.2637                   antes 0.09609 ... DM = 2.04

# 9 meses
dm.test(residuo_LGBM1_9meses, residuo_LGBM2_9meses) #p-valor 0.2407                   antes 0.1675  ... DM = 1.51

# 12 meses 
dm.test(residuo_LGBM1_12meses, residuo_LGBM2_12meses) #p-valor  0.1526                antes 0.2301  ... DM = 1.2705

# 24 meses
dm.test(residuo_LGBM1_24meses, residuo_LGBM2_24meses) #p-valor  0.9504                antes 0.2138  .... DM = 1.2786




### DIEBOLD-MARIANO PARA TODOS OS MODELOS --- AR1, ARMA, MIDAS, KNN, SVM 2, LGBM 2


# AR1 x ARMA 
# 6 meses
dm.test(residuo_AR1_6meses, residuo_ARMA_6meses) #p-valor 0.1704              antes0.1923
# 9 meses
dm.test(residuo_AR1_9meses, residuo_ARMA_9meses) #p-valor 0.2395              antes0.146
# 12 meses 
dm.test(residuo_AR1_12meses, residuo_ARMA_12meses) #p-valor 0.1149            antes0.2048
# 24 meses
dm.test(residuo_AR1_24meses, residuo_ARMA_24meses) #p-valor 0.352             antes0.4693

# AR1 x KNN
dm.test(residuo_AR1_6meses, residuo_KNN_6meses) #p-valor 0.4365                antes0.05691
# 9 meses
dm.test(residuo_AR1_9meses, residuo_KNN_9meses) #p-valor 0.8902             antes0.03041
# 12 meses 
dm.test(residuo_AR1_12meses, residuo_KNN_12meses) #p-valor 0.9426             antes0.02107
# 24 meses
dm.test(residuo_AR1_24meses, residuo_KNN_24meses) #p-valor 0.07522             antes0.001072

# AR1 x SVM 1
dm.test(residuo_AR1_6meses, residuo_SVM1_6meses) #p-valor 0.2168                  antes0.01043
# 9 meses
dm.test(residuo_AR1_9meses, residuo_SVM1_9meses) #p-valor 0.3045                    antes0.007751
# 12 meses 
dm.test(residuo_AR1_12meses, residuo_SVM1_12meses) #p-valor 0.9651                 antes0.004141
# 24 meses
dm.test(residuo_AR1_24meses, residuo_SVM1_24meses) #p-valor 0.2705                 antes0.0002312

#AR 1 x LGBM1
dm.test(residuo_AR1_6meses, residuo_LGBM1_6meses) #p-valor 0.6683
# 9 meses
dm.test(residuo_AR1_9meses, residuo_LGBM1_9meses) #p-valor 0.3901
# 12 meses 
dm.test(residuo_AR1_12meses, residuo_LGBM1_12meses) #p-valor 0.9079
# 24 meses
dm.test(residuo_AR1_24meses, residuo_LGBM1_24meses) #p-valor 0.1654



# ARMA x KNN 
dm.test(residuo_ARMA_6meses, residuo_KNN_6meses) #p-valor  0.2341                   antes0.5787
# 9 meses
dm.test(residuo_ARMA_9meses,residuo_KNN_9meses) #p-valor  0.1845                    antes0.5035
# 12 meses 
dm.test(residuo_ARMA_12meses, residuo_KNN_12meses) #p-valor 0.07313                  antes0.2513
# 24 meses
dm.test(residuo_ARMA_24meses, residuo_KNN_24meses) #p-valor 0.5538                  antes0.002429

# ARMA x SVM 1 
dm.test(residuo_ARMA_6meses, residuo_SVM1_6meses) #p-valor  0.173                   antes0.01262
# 9 meses
dm.test(residuo_ARMA_9meses,residuo_SVM1_9meses) #p-valor  0.2452                   antes0.01052
# 12 meses 
dm.test(residuo_ARMA_12meses, residuo_SVM1_12meses) #p-valor 0.2223                 antes0.00234
# 24 meses
dm.test(residuo_ARMA_24meses, residuo_SVM1_24meses) #p-valor  0.8732                antes6.774e-06

# ARMA x LGBM 1
dm.test(residuo_ARMA_6meses, residuo_LGBM1_6meses) #p-valor 0.2117
# 9 meses
dm.test(residuo_ARMA_9meses,residuo_LGBM1_9meses) #p-valor 0.2016
# 12 meses 
dm.test(residuo_ARMA_12meses, residuo_LGBM1_12meses) #p-valor 0.2073
# 24 meses
dm.test(residuo_ARMA_24meses, residuo_LGBM1_24meses) #p-valor 0.9461




# KNN x SVM 1
dm.test(residuo_KNN_6meses, residuo_SVM1_6meses) #p-valor 0.2542                  antes0.1333
# 9 meses
dm.test(residuo_KNN_9meses, residuo_SVM1_9meses) #p-valor 0.1567                   antes0.1003
# 12 meses 
dm.test(residuo_KNN_12meses, residuo_SVM1_12meses) #p-valor 0.8713                 antes0.08121
# 24 meses
dm.test(residuo_KNN_24meses, residuo_SVM1_24meses) #p-valor 0.3932                 antes0.01035

# KNN x LGBM 1
dm.test(residuo_KNN_6meses, residuo_LGBM1_6meses) #p-valor 0.5944        
# 9 meses
dm.test(residuo_KNN_9meses, residuo_LGBM1_9meses) #p-valor 0.5068                
# 12 meses 
dm.test(residuo_KNN_12meses, residuo_LGBM1_12meses) #p-valor  0.9358              
# 24 meses
dm.test(residuo_KNN_24meses, residuo_LGBM1_24meses) #p-valor  0.8729              



# SVM 1 x LGBM 1
dm.test(residuo_SVM1_6meses, residuo_LGBM1_6meses) #p-valor 0.2103                  
# 9 meses
dm.test(residuo_SVM1_9meses, residuo_LGBM1_9meses) #p-valor  0.9868                  
# 12 meses 
dm.test(residuo_SVM1_12meses, residuo_LGBM1_12meses) #p-valor  0.7988               
# 24 meses
dm.test(residuo_SVM1_24meses, residuo_LGBM1_24meses) #p-valor   0.4391             





## GRAFICO FINAL

par(mfrow=c(2,2))

# 6 MESES
plot(previsto_6meses_AR1, type="l", col = "black", ylim = c(-0.5,1.7),
     xlab = "Meses",  ylab = "Taxa de infla??o", main = "M?todos x IPCA, h = 6", xlim = range(c(1:6)))
lines(previsto_6meses_ARMA, col = "darkorchid2")
lines(knn_6meses, col = "darkgoldenrod1")
lines(previsao_svm1_6meses, col = "blue")
lines(LGBM1_prev$.pred[1:6], col = "green")
lines(ipca_2018e2019$IPCA[1:6], col = "red")  
legend("topleft", legend = c("AR1", "ARMA (4,3)", "KNN","SVM","LGBM" ,"IPCA"), col = c("black","darkorchid2","darkgoldenrod1","blue","green", "red"), lwd = 1, cex = 0.55) 

# 9 MESES
plot(previsto_9meses_AR1, type="l", col = "black", ylim = c(-0.5,1.7),
     xlab = "Meses",  ylab = "Taxa de infla??o", main = "M?todos x IPCA, h = 9", xlim = range(c(1:9)))
lines(previsto_9meses_ARMA, col = "darkorchid2")
lines(knn_9meses, col = "darkgoldenrod1")
lines(previsao_svm1_9meses, col = "blue")
lines(LGBM1_prev$.pred[1:9], col = "green")
lines(ipca_2018e2019$IPCA[1:9], col = "red")  
legend("topleft", legend = c("AR1", "ARMA (4,3)", "KNN","SVM","LGBM" ,"IPCA"), col = c("black","darkorchid2","darkgoldenrod1","blue","green", "red"), lwd = 1, cex = 0.55) 

# 12 MESES
plot(previsto_12meses_AR1, type="l", col = "black", ylim = c(-0.5,1.7),
     xlab = "Meses",  ylab = "Taxa de infla??o", main = "M?todos x IPCA, h = 12", xlim = range(c(1:12)))
lines(previsto_12meses_ARMA, col = "darkorchid2")
lines(knn_12meses, col = "darkgoldenrod1")
lines(previsao_svm1_12meses, col = "blue")
lines(LGBM1_prev$.pred[1:12], col = "green")
lines(ipca_2018e2019$IPCA[1:12], col = "red")  
legend("topleft", legend = c("AR1", "ARMA (4,3)", "KNN","SVM","LGBM" ,"IPCA"), col = c("black","darkorchid2","darkgoldenrod1","blue","green", "red"), lwd = 1, cex = 0.55) 

# 24 MESES
plot(previsto_24meses_AR1, type="l", col = "black", ylim = c(-0.5,1.7),
     xlab = "Meses",  ylab = "Taxa de infla??o", main = "M?todos x IPCA, h = 24", xlim = range(c(1:24)))
lines(previsto_24meses_ARMA, col = "darkorchid2")
lines(knn_24meses, col = "darkgoldenrod1")
lines(previsao_svm1_24meses, col = "blue")
lines(prev_lgbm1_24meses, col = "green")
lines(ipca_2018e2019$IPCA[1:24], col = "red")  
legend("topleft", legend = c("AR1", "ARMA (4,3)", "KNN","SVM","LGBM" ,"IPCA"), col = c("black","darkorchid2","darkgoldenrod1","blue","green", "red"), lwd = 1, cex = 0.55) 

par(mfrow=c(1,1))



