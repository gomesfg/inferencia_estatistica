#-----------------------------------------------------
#           Exemplos Teste de Média
#               20/06/2020
#-----------------------------------------------------

# Redefinir o Diretório de Trabalho
setwd("C:/Users/Usuario/OneDrive - FURB/FURB - Especializações/Data Science/Inferência Estatística e Modelos de Previsão/Aula 1")

#-----------------------------------------------------
# Teste t de uma amostra

# Carregando o Banco de Dados
dados1 <- read.csv("Dados Exemplos Teste t uma amostra.csv", header = T, sep = ";", dec = ",")

# Estatísticas Descritivas
summary(dados1$horas.Semanais)
sd(dados1$horas.Semanais)
library(fBasics)
skewness(dados1$horas.Semanais)
kurtosis(dados1$horas.Semanais)

# Teste Normalidade
  #H0 - a distribuição é normal
  #H1 - a distribuição não é normal
library(normtest)
shapiro.test(dados1$horas.Semanais)
  #Conclusão - Como p-value=0.5973>0.05= alpha, não rejeitamos a normalidade

# Teste t de uma amostra
  #H0 - a média de horas semanais trabalhadas é igual a 44h.
  #H1 - a média de horas semanais trabalhadas NÃO é igual a 44h.
  #alpha = 0,05

t.test(dados1$horas.Semanais, mu=44)
  #Conclusão: como o pvalue = 0.923 > 0.05=alpha, não rejeitamos H0.
  #           logo, podemos assumir como verdadeira a ideia de que os executivos trabalham em média 44h semanais.

#--------------------------------------------------------
# Teste t de Duas Amostras Pareadas

# Carregar o Banco de Dados
dados2 <- read.csv("Dados Exemplos Teste t par.csv", header = T, sep = ";", dec = ",")

dif <- dados2$Depois-dados2$Antes

# Estatística Descritiva
summary(dados2$Antes) # Peso Antes da Dieta
summary(dados2$Depois) # Peso Depois da Dieta
sd(dados2$Antes)
sd(dados2$Antes)/mean(dados2$Antes) #Coeficiente de Variação
sd(dados2$Depois)
sd(dados2$Depois)/mean(dados2$Depois) #Coeficiente de Variação
skewness(dados2$Antes)
skewness(dados2$Depois)
kurtosis(dados2$Antes)
kurtosis(dados2$Depois)

# teste de Normalidade
  #H0 - a distribuição é normal
  #H1 - a distribuição não é normal
shapiro.test(dados2$Antes)
shapiro.test(dados2$Depois)

shapiro.test(dif)

# Teste t de Amostra Pareadas
  # H0 - A média Antes é igual a média Depois
  # H1 - A média Antes NÃO é igual a média Depois
t.test(dados2$Antes, dados2$Depois, paired = T)  # Teste Bi-caudal
  # Conclusão: Como p-value = 0.07774 > 0.05 = alpha, não rejeitamos H0.
  #            Logo, O peso antes da dieta é igual em média ao peso depois.

  # Teste Uni-caudal
  # H0 - A média antes é igual a média depois
  # H1 - A média antes é maior que a média depois
t.test(dados2$Antes, dados2$Depois, alternative = "greater", paired = T) # Uni-caudal
  # Conclusão: Como o p-value = 0.03887 < 0.05 = alpha
  #            Rejeitamos H0 e assumimos H1, logo o pelo ante da dieta é maior que o peso depois.

#--------------------------------------------------------
# Teste t  de duas amostras independentes

# carregando o Banco de Dados
dados3 <- read.csv("Dados Exemplos Teste t ind.csv", header = T, sep = ";", dec = ",")

# Estatísticas Descritivas
summary(dados3$peso_comida) # Peso Total
summary(dados3$peso_comida[dados3$sexo==0]) #Peso para Homens
summary(dados3$peso_comida[dados3$sexo==1]) #Peso para Mulheres

sd(dados3$peso_comida) # Peso Total
sd(dados3$peso_comida[dados3$sexo==0]) #Peso para Homens
sd(dados3$peso_comida[dados3$sexo==1]) #Peso para Mulheres

sd(dados3$peso_comida)/mean(dados3$peso_comida) # Coeficiente de Variação Peso Total
sd(dados3$peso_comida[dados3$sexo==0])/mean(dados3$peso_comida[dados3$sexo==0]) # Coeficiente de variação Peso para Homens
sd(dados3$peso_comida[dados3$sexo==1])/mean(dados3$peso_comida[dados3$sexo==1]) # Coeficiente de variação Peso para Mulheres

skewness(dados3$peso_comida) # Peso Total
skewness(dados3$peso_comida[dados3$sexo==0]) #Peso para Homens
skewness(dados3$peso_comida[dados3$sexo==1]) #Peso para Mulheres

kurtosis(dados3$peso_comida) # Peso Total
kurtosis(dados3$peso_comida[dados3$sexo==0]) #Peso para Homens
kurtosis(dados3$peso_comida[dados3$sexo==1]) #Peso para Mulheres

# Teste de Normalida
  # H0 - A distribuição de dados é normal
  # H1 - A distribuição de dados não é normal
shapiro.test(dados3$peso_comida)
shapiro.test(dados3$peso_comida[dados3$sexo==0]) #Peso para Homens
shapiro.test(dados3$peso_comida[dados3$sexo==1]) #Peso para Mulheres

# teste de Levene - Igualdade das Variância
  # H0 - As variâncias são iguais
  # H1 - As variâncias são diferentes
library(car)
leveneTest(dados3$peso_comida, dados3$sexo)

# Análise Gráfica
plot(dados3$peso_comida, dados3$sexo,
     xlab="Peso da Comida (g)",
     ylab="Sexo")
    abline(h=c(0,1),v=c(mean(dados3$peso_comida[dados3$sexo==0]),mean(dados3$peso_comida[dados3$sexo==1])), col="red")

# Teste t de Amostras Independentes
    # H0 - O peso da comida das mulheres é igual ao peso da comida dos homens
    # (Bi-caudal) H1 - O peso da comida das mulheres NÃO é igual ao peso da comida dos homens

t.test(dados3$peso_comida ~ dados3$sexo)
t.test(dados3$peso_comida ~ dados3$sexo, var.equal = T)
  #Conclusão: Como o p-value = 0.0075 < 0.05 = alpha, Rejeitamos H0 e Aceitamos H1.
  #           Logo, temos que homens consomem peso média de comida diferente do que as mulheres.

# H0 - O peso da comida das mulheres é igual ao peso da comida dos homens
# (Uni-caudal) H1 - O peso da comida dos homens é maior que o peso da comida das Mulheres
t.test(dados3$peso_comida ~ dados3$sexo, alternative = "greater")
#Conclusão: Como o p-value = 0.0037 < 0.05 = alpha, Rejeitamos H0 e Aceitamos H1.
#           Logo, temos que homens consomem peso média de comida maior do que as mulheres.

#---------------------------------------------------------------
# Teste ANOVA

dados4 <- read.csv("Dados Exemplos ANOVA.csv", header = T, sep = ";", dec = ".")

# Estatísticas Descritivas
summary(dados4$tx_Hemo)
summary(dados4$tx_Hemo[dados4$Grupo==1]) # Grupo 1
summary(dados4$tx_Hemo[dados4$Grupo==2]) # Grupo 2
summary(dados4$tx_Hemo[dados4$Grupo==3]) # Grupo 3

sd(dados4$tx_Hemo)
sd(dados4$tx_Hemo[dados4$Grupo==1]) # Grupo 1
sd(dados4$tx_Hemo[dados4$Grupo==2]) # Grupo 2
sd(dados4$tx_Hemo[dados4$Grupo==3]) # Grupo 3

sd(dados4$tx_Hemo)/mean(dados4$tx_Hemo) # Coeficientes de Variação Geral
sd(dados4$tx_Hemo[dados4$Grupo==1])/mean(dados4$tx_Hemo[dados4$Grupo==1]) # Coeficientes de Variação Grupo 1
sd(dados4$tx_Hemo[dados4$Grupo==2])/mean(dados4$tx_Hemo[dados4$Grupo==2]) # Coeficientes de Variação Grupo 2
sd(dados4$tx_Hemo[dados4$Grupo==3])/mean(dados4$tx_Hemo[dados4$Grupo==3]) # Coeficientes de Variação Grupo 3

skewness(dados4$tx_Hemo)
skewness(dados4$tx_Hemo[dados4$Grupo==1]) # Grupo 1
skewness(dados4$tx_Hemo[dados4$Grupo==2]) # Grupo 2
skewness(dados4$tx_Hemo[dados4$Grupo==3]) # Grupo 3

kurtosis(dados4$tx_Hemo)
kurtosis(dados4$tx_Hemo[dados4$Grupo==1]) # Grupo 1
kurtosis(dados4$tx_Hemo[dados4$Grupo==2]) # Grupo 2
kurtosis(dados4$tx_Hemo[dados4$Grupo==3]) # Grupo 3

# Teste Normalidade
  #H0 - A distribuição é Normal
  #H1 - A distribuição não é Normal
shapiro.test(dados4$tx_Hemo)
shapiro.test(dados4$tx_Hemo[dados4$Grupo==1]) # Grupo 1
shapiro.test(dados4$tx_Hemo[dados4$Grupo==2]) # Grupo 2
shapiro.test(dados4$tx_Hemo[dados4$Grupo==3]) # Grupo 3
  #Conclusão: O teste Anova precisa ser analisado com um pouco de cuidada devido a não normalidade dos dados do Grupo 2.

# teste Levene
  #H0 - Os grupos possuem a mesma variância entre si.
  #H1 - Os grupos possuem variâncias distintas entre si.
leveneTest(dados4$tx_Hemo, dados4$Grupo)
  #Conclusão: Como p-value (Pr(>F))= 0.1087 > 0.05 = alpha, não rejeitamos H0. Os grupos possuem variâncias semelhantes entre si.

# Análise Gráfica
plot(dados4$tx_Hemo, dados4$Grupo,
     xlab="Taxa de Hemoglobina",
     ylab="Grupos de Tratamento")
abline(v=mean(dados4$tx_Hemo[dados4$Grupo==1]), col="red")
abline(v=mean(dados4$tx_Hemo[dados4$Grupo==2]), col="green")
abline(v=mean(dados4$tx_Hemo[dados4$Grupo==3]), col="blue")

# Teste ANOVA
  #H0 - Os grupos possuem médias iguais.
  #H1 - Existe, pelo menos 2 grupos com médias distintas.
ANOVA <- aov(dados4$tx_Hemo ~ dados4$Grupo)
summary(ANOVA)
  # Conclusão: Como o p-value = 0.00194 < 0.05 = alpha, rejeitamos H0 e Aceitamos H1.

# Testes 2 a 2
t.test(dados4$tx_Hemo[dados4$Grupo!=3] ~ dados4$Grupo[dados4$Grupo!=3])
t.test(dados4$tx_Hemo[dados4$Grupo!=2] ~ dados4$Grupo[dados4$Grupo!=2])
t.test(dados4$tx_Hemo[dados4$Grupo!=1] ~ dados4$Grupo[dados4$Grupo!=1])

#------------------------------------------------------------
# Testes Paramétricos

# Teste de Wilcoxon - Alternativa ao Teste t de 2 amostras pareadas
  # H0 - Peso Antes é igual ao Peso Depois
  # H1 - Peso Antes é diferente ao Peso Depois
wilcox.test(dados2$Antes, dados2$Depois, paired = T)
  #Conclusão: p-value = 0.09485 > 0.05 = alpha, não rejeitamos H0.

# Teste de Mann-Whitney - Alternativa ao Teste t de 2 amostras independentes
  # H0 - Peso da comida dos homens é igual ao peso da comida das mulheres
  # H1 - Peso da comida dos homens é diferente do peso da comida das mulheres
wilcox.test(dados3$peso_comida ~ dados3$sexo)
  # Conclusão: p-value = 0.0122 < 0.05 = alpha, logo rejeitamos H0 e aceitamos H1.

# Teste de Kruskal-Wallis - Alternativa ao Teste ANOVA
  # H0 - Os grupos possuem o mesmo nível médio de Hemoglobina
  # H1 - Existe, pelo menos, dois grupos que possuem níveis de hemoglobina diferentes.
kruskal.test(dados4$tx_Hemo ~ dados4$Grupo)
  # Conclusão: p-value = 0.0005319 < 0.05 = alpha, logo: rejeitamos H0 e Aceitamos H1.

#-------------------------------------------------------------
#Teste Qui-Quadrado (Chi-Square)
  #H0 - Não há discrepância entre o esperado e o realizado
  #H1 - Há discrepância entre o esperado e o realizado.

# Carregar os Dados
dados5 <- read.csv("Dados Exemplos Chi-Square.csv", header = T, sep = ";", dec=",")

chisq.test(dados5$Tratamento, dados5$Dor_Abdomen)
