#-----------------------------------------------------
#           Exemplos Teste de M�dia
#               20/06/2020
#-----------------------------------------------------

# Redefinir o Diret�rio de Trabalho
setwd("C:/Users/Usuario/OneDrive - FURB/FURB - Especializa��es/Data Science/Infer�ncia Estat�stica e Modelos de Previs�o/Aula 1")

#-----------------------------------------------------
# Teste t de uma amostra

# Carregando o Banco de Dados
dados1 <- read.csv("Dados Exemplos Teste t uma amostra.csv", header = T, sep = ";", dec = ",")

# Estat�sticas Descritivas
summary(dados1$horas.Semanais)
sd(dados1$horas.Semanais)
library(fBasics)
skewness(dados1$horas.Semanais)
kurtosis(dados1$horas.Semanais)

# Teste Normalidade
  #H0 - a distribui��o � normal
  #H1 - a distribui��o n�o � normal
library(normtest)
shapiro.test(dados1$horas.Semanais)
  #Conclus�o - Como p-value=0.5973>0.05= alpha, n�o rejeitamos a normalidade

# Teste t de uma amostra
  #H0 - a m�dia de horas semanais trabalhadas � igual a 44h.
  #H1 - a m�dia de horas semanais trabalhadas N�O � igual a 44h.
  #alpha = 0,05

t.test(dados1$horas.Semanais, mu=44)
  #Conclus�o: como o pvalue = 0.923 > 0.05=alpha, n�o rejeitamos H0.
  #           logo, podemos assumir como verdadeira a ideia de que os executivos trabalham em m�dia 44h semanais.

#--------------------------------------------------------
# Teste t de Duas Amostras Pareadas

# Carregar o Banco de Dados
dados2 <- read.csv("Dados Exemplos Teste t par.csv", header = T, sep = ";", dec = ",")

dif <- dados2$Depois-dados2$Antes

# Estat�stica Descritiva
summary(dados2$Antes) # Peso Antes da Dieta
summary(dados2$Depois) # Peso Depois da Dieta
sd(dados2$Antes)
sd(dados2$Antes)/mean(dados2$Antes) #Coeficiente de Varia��o
sd(dados2$Depois)
sd(dados2$Depois)/mean(dados2$Depois) #Coeficiente de Varia��o
skewness(dados2$Antes)
skewness(dados2$Depois)
kurtosis(dados2$Antes)
kurtosis(dados2$Depois)

# teste de Normalidade
  #H0 - a distribui��o � normal
  #H1 - a distribui��o n�o � normal
shapiro.test(dados2$Antes)
shapiro.test(dados2$Depois)

shapiro.test(dif)

# Teste t de Amostra Pareadas
  # H0 - A m�dia Antes � igual a m�dia Depois
  # H1 - A m�dia Antes N�O � igual a m�dia Depois
t.test(dados2$Antes, dados2$Depois, paired = T)  # Teste Bi-caudal
  # Conclus�o: Como p-value = 0.07774 > 0.05 = alpha, n�o rejeitamos H0.
  #            Logo, O peso antes da dieta � igual em m�dia ao peso depois.

  # Teste Uni-caudal
  # H0 - A m�dia antes � igual a m�dia depois
  # H1 - A m�dia antes � maior que a m�dia depois
t.test(dados2$Antes, dados2$Depois, alternative = "greater", paired = T) # Uni-caudal
  # Conclus�o: Como o p-value = 0.03887 < 0.05 = alpha
  #            Rejeitamos H0 e assumimos H1, logo o pelo ante da dieta � maior que o peso depois.

#--------------------------------------------------------
# Teste t  de duas amostras independentes

# carregando o Banco de Dados
dados3 <- read.csv("Dados Exemplos Teste t ind.csv", header = T, sep = ";", dec = ",")

# Estat�sticas Descritivas
summary(dados3$peso_comida) # Peso Total
summary(dados3$peso_comida[dados3$sexo==0]) #Peso para Homens
summary(dados3$peso_comida[dados3$sexo==1]) #Peso para Mulheres

sd(dados3$peso_comida) # Peso Total
sd(dados3$peso_comida[dados3$sexo==0]) #Peso para Homens
sd(dados3$peso_comida[dados3$sexo==1]) #Peso para Mulheres

sd(dados3$peso_comida)/mean(dados3$peso_comida) # Coeficiente de Varia��o Peso Total
sd(dados3$peso_comida[dados3$sexo==0])/mean(dados3$peso_comida[dados3$sexo==0]) # Coeficiente de varia��o Peso para Homens
sd(dados3$peso_comida[dados3$sexo==1])/mean(dados3$peso_comida[dados3$sexo==1]) # Coeficiente de varia��o Peso para Mulheres

skewness(dados3$peso_comida) # Peso Total
skewness(dados3$peso_comida[dados3$sexo==0]) #Peso para Homens
skewness(dados3$peso_comida[dados3$sexo==1]) #Peso para Mulheres

kurtosis(dados3$peso_comida) # Peso Total
kurtosis(dados3$peso_comida[dados3$sexo==0]) #Peso para Homens
kurtosis(dados3$peso_comida[dados3$sexo==1]) #Peso para Mulheres

# Teste de Normalida
  # H0 - A distribui��o de dados � normal
  # H1 - A distribui��o de dados n�o � normal
shapiro.test(dados3$peso_comida)
shapiro.test(dados3$peso_comida[dados3$sexo==0]) #Peso para Homens
shapiro.test(dados3$peso_comida[dados3$sexo==1]) #Peso para Mulheres

# teste de Levene - Igualdade das Vari�ncia
  # H0 - As vari�ncias s�o iguais
  # H1 - As vari�ncias s�o diferentes
library(car)
leveneTest(dados3$peso_comida, dados3$sexo)

# An�lise Gr�fica
plot(dados3$peso_comida, dados3$sexo,
     xlab="Peso da Comida (g)",
     ylab="Sexo")
    abline(h=c(0,1),v=c(mean(dados3$peso_comida[dados3$sexo==0]),mean(dados3$peso_comida[dados3$sexo==1])), col="red")

# Teste t de Amostras Independentes
    # H0 - O peso da comida das mulheres � igual ao peso da comida dos homens
    # (Bi-caudal) H1 - O peso da comida das mulheres N�O � igual ao peso da comida dos homens

t.test(dados3$peso_comida ~ dados3$sexo)
t.test(dados3$peso_comida ~ dados3$sexo, var.equal = T)
  #Conclus�o: Como o p-value = 0.0075 < 0.05 = alpha, Rejeitamos H0 e Aceitamos H1.
  #           Logo, temos que homens consomem peso m�dia de comida diferente do que as mulheres.

# H0 - O peso da comida das mulheres � igual ao peso da comida dos homens
# (Uni-caudal) H1 - O peso da comida dos homens � maior que o peso da comida das Mulheres
t.test(dados3$peso_comida ~ dados3$sexo, alternative = "greater")
#Conclus�o: Como o p-value = 0.0037 < 0.05 = alpha, Rejeitamos H0 e Aceitamos H1.
#           Logo, temos que homens consomem peso m�dia de comida maior do que as mulheres.

#---------------------------------------------------------------
# Teste ANOVA

dados4 <- read.csv("Dados Exemplos ANOVA.csv", header = T, sep = ";", dec = ".")

# Estat�sticas Descritivas
summary(dados4$tx_Hemo)
summary(dados4$tx_Hemo[dados4$Grupo==1]) # Grupo 1
summary(dados4$tx_Hemo[dados4$Grupo==2]) # Grupo 2
summary(dados4$tx_Hemo[dados4$Grupo==3]) # Grupo 3

sd(dados4$tx_Hemo)
sd(dados4$tx_Hemo[dados4$Grupo==1]) # Grupo 1
sd(dados4$tx_Hemo[dados4$Grupo==2]) # Grupo 2
sd(dados4$tx_Hemo[dados4$Grupo==3]) # Grupo 3

sd(dados4$tx_Hemo)/mean(dados4$tx_Hemo) # Coeficientes de Varia��o Geral
sd(dados4$tx_Hemo[dados4$Grupo==1])/mean(dados4$tx_Hemo[dados4$Grupo==1]) # Coeficientes de Varia��o Grupo 1
sd(dados4$tx_Hemo[dados4$Grupo==2])/mean(dados4$tx_Hemo[dados4$Grupo==2]) # Coeficientes de Varia��o Grupo 2
sd(dados4$tx_Hemo[dados4$Grupo==3])/mean(dados4$tx_Hemo[dados4$Grupo==3]) # Coeficientes de Varia��o Grupo 3

skewness(dados4$tx_Hemo)
skewness(dados4$tx_Hemo[dados4$Grupo==1]) # Grupo 1
skewness(dados4$tx_Hemo[dados4$Grupo==2]) # Grupo 2
skewness(dados4$tx_Hemo[dados4$Grupo==3]) # Grupo 3

kurtosis(dados4$tx_Hemo)
kurtosis(dados4$tx_Hemo[dados4$Grupo==1]) # Grupo 1
kurtosis(dados4$tx_Hemo[dados4$Grupo==2]) # Grupo 2
kurtosis(dados4$tx_Hemo[dados4$Grupo==3]) # Grupo 3

# Teste Normalidade
  #H0 - A distribui��o � Normal
  #H1 - A distribui��o n�o � Normal
shapiro.test(dados4$tx_Hemo)
shapiro.test(dados4$tx_Hemo[dados4$Grupo==1]) # Grupo 1
shapiro.test(dados4$tx_Hemo[dados4$Grupo==2]) # Grupo 2
shapiro.test(dados4$tx_Hemo[dados4$Grupo==3]) # Grupo 3
  #Conclus�o: O teste Anova precisa ser analisado com um pouco de cuidada devido a n�o normalidade dos dados do Grupo 2.

# teste Levene
  #H0 - Os grupos possuem a mesma vari�ncia entre si.
  #H1 - Os grupos possuem vari�ncias distintas entre si.
leveneTest(dados4$tx_Hemo, dados4$Grupo)
  #Conclus�o: Como p-value (Pr(>F))= 0.1087 > 0.05 = alpha, n�o rejeitamos H0. Os grupos possuem vari�ncias semelhantes entre si.

# An�lise Gr�fica
plot(dados4$tx_Hemo, dados4$Grupo,
     xlab="Taxa de Hemoglobina",
     ylab="Grupos de Tratamento")
abline(v=mean(dados4$tx_Hemo[dados4$Grupo==1]), col="red")
abline(v=mean(dados4$tx_Hemo[dados4$Grupo==2]), col="green")
abline(v=mean(dados4$tx_Hemo[dados4$Grupo==3]), col="blue")

# Teste ANOVA
  #H0 - Os grupos possuem m�dias iguais.
  #H1 - Existe, pelo menos 2 grupos com m�dias distintas.
ANOVA <- aov(dados4$tx_Hemo ~ dados4$Grupo)
summary(ANOVA)
  # Conclus�o: Como o p-value = 0.00194 < 0.05 = alpha, rejeitamos H0 e Aceitamos H1.

# Testes 2 a 2
t.test(dados4$tx_Hemo[dados4$Grupo!=3] ~ dados4$Grupo[dados4$Grupo!=3])
t.test(dados4$tx_Hemo[dados4$Grupo!=2] ~ dados4$Grupo[dados4$Grupo!=2])
t.test(dados4$tx_Hemo[dados4$Grupo!=1] ~ dados4$Grupo[dados4$Grupo!=1])

#------------------------------------------------------------
# Testes Param�tricos

# Teste de Wilcoxon - Alternativa ao Teste t de 2 amostras pareadas
  # H0 - Peso Antes � igual ao Peso Depois
  # H1 - Peso Antes � diferente ao Peso Depois
wilcox.test(dados2$Antes, dados2$Depois, paired = T)
  #Conclus�o: p-value = 0.09485 > 0.05 = alpha, n�o rejeitamos H0.

# Teste de Mann-Whitney - Alternativa ao Teste t de 2 amostras independentes
  # H0 - Peso da comida dos homens � igual ao peso da comida das mulheres
  # H1 - Peso da comida dos homens � diferente do peso da comida das mulheres
wilcox.test(dados3$peso_comida ~ dados3$sexo)
  # Conclus�o: p-value = 0.0122 < 0.05 = alpha, logo rejeitamos H0 e aceitamos H1.

# Teste de Kruskal-Wallis - Alternativa ao Teste ANOVA
  # H0 - Os grupos possuem o mesmo n�vel m�dio de Hemoglobina
  # H1 - Existe, pelo menos, dois grupos que possuem n�veis de hemoglobina diferentes.
kruskal.test(dados4$tx_Hemo ~ dados4$Grupo)
  # Conclus�o: p-value = 0.0005319 < 0.05 = alpha, logo: rejeitamos H0 e Aceitamos H1.

#-------------------------------------------------------------
#Teste Qui-Quadrado (Chi-Square)
  #H0 - N�o h� discrep�ncia entre o esperado e o realizado
  #H1 - H� discrep�ncia entre o esperado e o realizado.

# Carregar os Dados
dados5 <- read.csv("Dados Exemplos Chi-Square.csv", header = T, sep = ";", dec=",")

chisq.test(dados5$Tratamento, dados5$Dor_Abdomen)
