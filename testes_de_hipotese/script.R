# -------------------------------
# Testes de Normalidade
# -------------------------------

# Limpar console
cat("\014")

# Mudar o diretório do trabalho
setwd("C:/Users/feeli/Documents/GitHub/inferencia_estatistica/testes_de_hipotese")

# Gerar Números Aleatórios
# Funções estatísticas para dados normais
#   dnorm -> Densidade da distribuição normal (P[X=x])
#   pnorm -> Função distribuição de probabilidade (P[X<x])
#   qnorm -> Inserir uma probabilidade (0,1) retoma o valor que a geraria
#   rnorm -> Retoma um vetor de dados normais

N10 <- rnorm(10, mean=30, sd=5)
N50 <- rnorm(50, mean=30, sd=5)
N100 <- rnorm(100, mean=30, sd=5)
N500 <- rnorm(500, mean=30, sd=5)
N50000 <- rnorm(50000, mean=30, sd=5)

# Gerar histograma
hist(N10)
hist(N50)
hist(N100)
hist(N500)
hist(N50000)

# Estatísticas descritivas
summary(N10)
summary(N50)
summary(N100)
summary(N500)
summary(N50000)

# Biblioteca fBasics
library(fBasics)
basicStats(N10)
basicStats(N50)
basicStats(N100)
basicStats(N500)


# Teste de normalidade 
# Usar o pacote normtest
# install.packages("normtest")
library(normtest)

# Função do teste K-s ks.test(varX, "pnorm")
# Função do teste de Shapiro-Wilk - shapiro.test(VarX)
# Função do teste de Jarque Bera - jb.norm.test(VarX)

ks.test(N10, "pnorm")
ks.test(N50, "pnorm")
ks.test(N100, "pnorm")
ks.test(N500, "pnorm")
ks.test(N50000, "pnorm")

shapiro.test(N10)
shapiro.test(N50)
shapiro.test(N100)
shapiro.test(N500)
# shapiro.test(N50000) -- Teste shapiro não permite mais que 5000 casos

jb.norm.test(N10)
jb.norm.test(N50)
jb.norm.test(N100)
jb.norm.test(N500)
jb.norm.test(N50000)