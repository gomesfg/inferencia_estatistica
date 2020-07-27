#----------------------------------------------
# Trabalho Final
# Aluno: Felipe Eduardo Gomes
# Data Science - Turma 2
#----------------------------------------------

# Limpar a sujeira do console
cat("\014")

# Mudar o diretório do trabalho
setwd("C:/Users/feeli/Documents/GitHub/inferencia_estatistica/trabalho_final")

# Pacotes
library(fBasics) # Estatísticas básicas
library(car) # Teste de Levene
library(normtest) # Teste de normalidade

# Carregar a Base de Dados
dados <- read.csv("./seed/Dados.csv", header = T, sep = ";", dec = ",")
attach(dados)

dados_grupo <- read.csv("./seed/Dados_Grupo.csv", header = T, sep = ";", dec = ",")
attach(dados_grupo)


# 1) Considerando os dados apresentados acima, construa os testes de hipótese dadas as questões 
# apresentadas. Elaborar: 
# (i) A hipótese nula e a alternativa a ser testada; 
# (ii) Destaque o teste que será realizado; 
# (iii) Descreva os pressupostos necessários para cada teste; 
# (iv) Efetue o teste de hipótese pretendido, e 
# (v) Conclua sobre quais os indícios trazidos pelos dados.

# a) O Salário médio dos homens se difere do salário médio das mulheres?
summary(dados$salario) # Salário
summary(dados$salario[dados$sexo == 1]) # Salário Mulheres
summary(dados$salario[dados$sexo == 0]) # Salário Homens

basicStats(dados$salario) # Salário
basicStats(dados$salario[dados$sexo == 1]) # Salário Mulheres
basicStats(dados$salario[dados$sexo == 0]) # Salário Homens

# Pressupostos
#  Normalidade
#    H0 - A distribuição dos dados é normal
#    H1 - A distribuição dos dados não é normal
shapiro.test(dados$salario[dados$sexo == 1]) # Salário Mulheres = p-value < 2.2e-16
shapiro.test(dados$salario[dados$sexo == 0]) # Salário Homens = p-value < 2.2e-16
# Conclusão: Como todos os teste rejeitaram a normalidade, vamos fazer um teste Não-Paramétrico.

# Homodedasticidade - Levene
#   H0 - As variâncias são iguais
#   H1 - As variâncias são diferentes
leveneTest(dados$salario, dados$sexo) # Pr(>F) 5.737e-06
# Conclusão: As variâncias dos grupos são diferentes.

# Teste t - 2 amostras Independentes
#   H0 - As médias dos grupos são iguais.
#   H1 - As médias dos grupos são diferentes.
t.test(dados$salario~dados$sexo, var.equal=F) # p-value = 4.784e-16
#   Conclusão: Como p-value - 0,0000 < 0,05 = alpha, Rejeitamos H0 e Aceitamos H1.
#   Logo, há diferença significativa no salário de homens(Média=14.11889) e mulheres(Média=10.59367)
#   Os homens, em média, possuem um salário maior do que as mulheres.

# Teste Não-Paramétrico - Teste de Mann-Whitney
wilcox.test(dados$salario~dados$sexo, var.equal=F) # p-value < 2.2e-16
# Conclusão: Como o p-value = 0.0000 < 0.05 = alpha, Rejeitamos H0 e Aceitamos H1. Confirmando o teste t.



# b) O Salário médio das pessoas não brancas se difere das pessoas brancas?
summary(dados$salario) # Salário
summary(dados$salario[dados$cor == 1]) # Salário Não brancos
summary(dados$salario[dados$cor == 0]) # Salário Brancos

basicStats(dados$salario) # Salário
basicStats(dados$salario[dados$cor == 1]) # Salário Não brancos
basicStats(dados$salario[dados$cor == 0]) # Salário brancos

# Pressupostos
#  Normalidade
#    H0 - A distribuição dos dados é normal
#    H1 - A distribuição dos dados não é normal
shapiro.test(dados$salario[dados$cor == 1]) # Salário Não brancos = p-value = 8.177e-13
shapiro.test(dados$salario[dados$cor == 0]) # Salário brancos = p-value < 2.2e-16
# Conclusão: Como todos os teste rejeitaram a normalidade, vamos fazer um teste Não-Paramétrico.

# Homodedasticidade - Levene
#   H0 - As variâncias são iguais
#   H1 - As variâncias são diferentes
leveneTest(dados$salario, dados$cor) # Pr(>F) 0.0004716
# Conclusão: As variâncias dos grupos são diferentes.

# Teste t - 2 amostras Independentes
#   H0 - As médias dos grupos são iguais.
#   H1 - As médias dos grupos são diferentes.
t.test(dados$salario~dados$cor, var.equal=F) # p-value = 1.501e-08
#   Conclusão: Como p-value - 0,0000 < 0,05 = alpha, Rejeitamos H0 e Aceitamos H1.
#   Logo, há diferença significativa no salário de brancos (Média=12.794423) e não-brancos (Média=9.990203)
#   Os brancos, em média, possuem um salário maior do que os não-brancos.

# Teste Não-Paramétrico - Teste de Mann-Whitney
wilcox.test(dados$salario~dados$cor, var.equal=F) # p-value = 5.492e-07
# Conclusão: Como o p-value = 0.0000 < 0.05 = alpha, Rejeitamos H0 e Aceitamos H1. Confirmando o teste t.


# c) O Salário médio das pessoas casadas se difere das pessoas solteiras?
summary(dados$salario) # Salário
summary(dados$salario[dados$est_civil == 1]) # Salário Casados
summary(dados$salario[dados$est_civil == 0]) # Salário Solteiros

basicStats(dados$salario) # Salário
basicStats(dados$salario[dados$est_civil == 1]) # Salário Casados
basicStats(dados$salario[dados$est_civil == 0]) # Salário Solteiros

# Pressupostos
#  Normalidade
#    H0 - A distribuição dos dados é normal
#    H1 - A distribuição dos dados não é normal
shapiro.test(dados$salario[dados$est_civil == 1]) # Salário Casados = p-value = 2.294e-05
shapiro.test(dados$salario[dados$est_civil == 0]) # Salário Solteiros = p-value < 2.2e-16
# Conclusão: Como todos os teste rejeitaram a normalidade, vamos fazer um teste Não-Paramétrico.

# Homodedasticidade - Levene
#   H0 - As variâncias são iguais
#   H1 - As variâncias são diferentes
leveneTest(dados$salario, dados$est_civil) # Pr(>F) 0.03557
# Conclusão: As variâncias dos grupos são diferentes.

# Teste t - 2 amostras Independentes
#   H0 - As médias dos grupos são iguais.
#   H1 - As médias dos grupos são diferentes.
t.test(dados$salario~dados$est_civil, var.equal=F) # p-value = 6.184e-06
#   Conclusão: Como p-value - 0,0000 < 0,05 = alpha, Rejeitamos H0 e Aceitamos H1.
#   Logo, há diferença significativa no salário de casados (Média=14.22171) e solteiros (Média=12.01488)
#   Os casados, em média, possuem um salário maior do que os solteiros.

# Teste Não-Paramétrico - Teste de Mann-Whitney
wilcox.test(dados$salario~dados$est_civil, var.equal=F) # p-value = 4.229e-12
# Conclusão: Como o p-value = 0.0000 < 0.05 = alpha, Rejeitamos H0 e Aceitamos H1. Confirmando o teste t.


# d) Considerando o sexo das pessoas e seu estado civil conjuntamente, é possível afirmar que
# algumas dos grupos formados possui média salarial diferente dos demais?
summary(dados_grupo$salario) # Salário
summary(dados_grupo$salario[dados_grupo$grupo == 1]) # Salário Grupo 1 - Homens Solteiros
summary(dados_grupo$salario[dados_grupo$grupo == 2]) # Salário Grupo 2 - Homens Casados
summary(dados_grupo$salario[dados_grupo$grupo == 3]) # Salário Grupo 3 - Mulheres Solteiras
summary(dados_grupo$salario[dados_grupo$grupo == 4]) # Salário Grupo 4 - Mulheres Casadas


basicStats(dados_grupo$salario) # Salário
basicStats(dados_grupo$salario[dados_grupo$grupo == 1]) # Salário Grupo 1 - Homens Solteiros
basicStats(dados_grupo$salario[dados_grupo$grupo == 2]) # Salário Grupo 2 - Homens Casados
basicStats(dados_grupo$salario[dados_grupo$grupo == 3]) # Salário Grupo 3 - Mulheres Solteiras
basicStats(dados_grupo$salario[dados_grupo$grupo == 4]) # Salário Grupo 4 - Mulheres Casadas

# Pressupostos
#  Normalidade
#    H0 - A distribuição dos dados é normal
#    H1 - A distribuição dos dados não é normal
shapiro.test(dados_grupo$salario[dados_grupo$grupo == 1]) # Salário Grupo 1 - Homens Solteiros = p-value < 2.2e-16
shapiro.test(dados_grupo$salario[dados_grupo$grupo == 2]) # Salário Grupo 2 - Homens Casados = p-value = 0.0003714
shapiro.test(dados_grupo$salario[dados_grupo$grupo == 3]) # Salário Grupo 3 - Mulheres Solteiras = p-value < 2.2e-16
shapiro.test(dados_grupo$salario[dados_grupo$grupo == 4]) # Salário Grupo 4 - Mulheres Casadas = p-value = 0.002512
# Conclusão: Como todos os teste rejeitaram a normalidade, vamos fazer um teste Não-Paramétrico.

# Homodedasticidade - Levene
#   H0 - As variâncias são iguais
#   H1 - As variâncias são diferentes
leveneTest(dados_grupo$salario, dados_grupo$grupo) # Pr(>F) 6.892e-07
# Conclusão: As variâncias dos grupos são diferentes.


#Teste Kruskal-Wallis - Devido a distribuição dos grupos ser diferentes não foi usado ANOVA
#   H0 - Todas as média são iguais
#   H1 - Há pelo menos um grupo com médias diferente.
kruskal.test(dados_grupo$salario~dados_grupo$grupo)
#   Conclusão: Como o p-value < 2.2e-16 < 0,05, Rejeitamos H0 e Aceitamos H1.
#   Logo, há, pelo menos, uma média de salário diferente da média das demais.



# e) O tempo de experiência médio é diferente para homens e mulheres?
summary(dados$experiencia) # Experiência
summary(dados$experiencia[dados$sexo == 1]) # Experiência Mulheres
summary(dados$experiencia[dados$sexo == 0]) # Experiência Homens

basicStats(dados$experiencia) # Experiência
basicStats(dados$experiencia[dados$sexo == 1]) # Experiência Mulheres
basicStats(dados$experiencia[dados$sexo == 0]) # Experiência Homens

# Pressupostos
#  Normalidade
#    H0 - A distribuição dos dados é normal
#    H1 - A distribuição dos dados não é normal
shapiro.test(dados$experiencia[dados$sexo == 1]) # Experiência Mulheres = p-value = 3.313e-11
shapiro.test(dados$experiencia[dados$sexo == 0]) # Experiência Homens = p-value = 3.173e-10
# Conclusão: Como todos os teste rejeitaram a normalidade, vamos fazer um teste Não-Paramétrico.

# Homodedasticidade - Levene
#   H0 - As variâncias são iguais
#   H1 - As variâncias são diferentes
leveneTest(dados$experiencia, dados$sexo) # Pr(>F) 0.514
# Conclusão: As variâncias dos grupos são iguais.

# Teste t - 2 amostras Independentes
#   H0 - As médias dos grupos são iguais.
#   H1 - As médias dos grupos são diferentes.
t.test(dados$experiencia~dados$sexo, var.equal=T) # p-value = 0.4164
#   Conclusão: Como p-value - 0.4164 > 0,05 = alpha, não rejeitamos H0.
#   Logo, há pequena diferença na experiência, quase iguais de homens(Média=19.05247) e mulheres(Média=18.52418)
#   Os homens, em média, possuem uma experiência pouco maior, quase igual, do que as mulheres.

# Teste Não-Paramétrico - Teste de Mann-Whitney
wilcox.test(dados$experiencia~dados$sexo, var.equal=T) # p-value < 0.3127
# Conclusão: Como o p-value = 0.3127 > 0.05 = alpha, não rejeitamos H0. 


# f) A idade média dos casados é diferente da idade média dos solteiros?
summary(dados$idade) # Idade
summary(dados$idade[dados$est_civil == 1]) # Idade Casados
summary(dados$idade[dados$est_civil == 0]) # Idade Solteiros

basicStats(dados$idade) # Idade
basicStats(dados$idade[dados$est_civil == 1]) # Idade Casados
basicStats(dados$idade[dados$est_civil == 0]) # Idade Solteiros

# Pressupostos
#  Normalidade
#    H0 - A distribuição dos dados é normal
#    H1 - A distribuição dos dados não é normal
shapiro.test(dados$idade[dados$est_civil == 1]) # Idade Casados = p-value = 0.07607
shapiro.test(dados$idade[dados$est_civil == 0]) # Idade Solteiros = p-value = 8.32e-14
# Conclusão: Como o teste dos casados rejeitou a normalidade, vamos fazer um teste Não-Paramétrico.

# Homodedasticidade - Levene
#   H0 - As variâncias são iguais
#   H1 - As variâncias são diferentes
leveneTest(dados$idade, dados$est_civil) # Pr(>F) 0.17
# Conclusão: As variâncias dos grupos são iguais

# Teste t - 2 amostras Independentes
#   H0 - As médias dos grupos são iguais.
#   H1 - As médias dos grupos são diferentes.
t.test(dados$idade~dados$est_civil, var.equal=T) # p-value = 1.296e-08
#   Conclusão: Como p-value - 0,0000 < 0,05 = alpha, Rejeitamos H0 e Aceitamos H1.
#   Logo, há diferença significativa na idade de casados (Média=42.09756) e solteiros (Média=37.14760)
#   Os casados, em média, possuem idade maior do que os solteiros.

# Teste Não-Paramétrico - Teste de Mann-Whitney
wilcox.test(dados$idade~dados$est_civil, var.equal=T) # p-value = 8.259e-09
# Conclusão: Como o p-value = 0.0000 < 0.05 = alpha, Rejeitamos H0 e Aceitamos H1. Confirmando o teste t.



# 2) Analisando o banco de dados apresentado, é possível afirmar que haveria alguma
# das relações destacadas a seguir? Destaque o grau de associação para cada uma das relações 
# apresentadas e verifique se seria significativo.

# g) Há relação entre Salário e Experiência?
# Correlação de Pearson
#   H0 - r=0 (Não há uma associação)
#   H1 - r!=0 (Há uma associação)
cor.test(dados$salario,dados$experiencia, method = "pearson")
# Conclusão: O coeficiente de correlação foi de 0.1731733 (desprezível) com p-value = 3.882e-10 < 0.05 = alpha, Rejeitamos H0.
# Logo, há associação desprezível entre salario e experiencia. 

# Correlação de Spearman
#   H0 - r=0 (Não há uma associação)
#   H1 - r!=0 (Há uma associação)
cor.test(dados$salario,dados$experiencia, method = "spearman")
# Conclusão: O coeficiente de correlação foi de 0.2414725 (desprezível) com p-value < 2.2e-16 < 0.05 = alpha, Rejeitamos H0.
# Logo, há associação desprezível entre salario e experiencia. 

# Correlação de Kendall
#   H0 - r=0 (Não há uma associação)
#   H1 - r!=0 (Há uma associação)
cor.test(dados$salario,dados$experiencia, method = "kendall")
# Conclusão: O coeficiente de correlação foi de 0.1665496 (desprezível) com p-value < 2.2e-16 < 0.05 = alpha, Rejeitamos H0.
# Logo, há associação desprezível entre salario e experiencia. 

# h) Há relação entre Salário e tempo de Instrução?
# Correlação de Pearson
#   H0 - r=0 (Não há uma associação)
#   H1 - r!=0 (Há uma associação)
cor.test(dados$salario,dados$instrucao, method = "pearson")
# Conclusão: O coeficiente de correlação foi de 0.456518 (fraca) com p-value < 2.2e-16 < 0.05 = alpha, Rejeitamos H0.
# Logo, há associação fraca entre salario e Instrução 

# Correlação de Spearman
#   H0 - r=0 (Não há uma associação)
#   H1 - r!=0 (Há uma associação)
cor.test(dados$salario,dados$instrucao, method = "spearman")
# Conclusão: O coeficiente de correlação foi de 0.4574217 (fraca) com p-value < 2.2e-16 < 0.05 = alpha, Rejeitamos H0.
# Logo, há associação fraca entre salario e Instrução 

# Correlação de Kendall
#   H0 - r=0 (Não há uma associação)
#   H1 - r!=0 (Há uma associação)
cor.test(dados$salario,dados$instrucao, method = "kendall")
# Conclusão: O coeficiente de correlação foi de 0.3562081 (fraca) com p-value < 2.2e-16 < 0.05 = alpha, Rejeitamos H0.
# Logo, há associação fraca entre salario e Instrução 


# i) Há relação entre Salário e Idade dos indivíduos investigados?
# Correlação de Pearson
#   H0 - r=0 (Não há uma associação)
#   H1 - r!=0 (Há uma associação)
cor.test(dados$salario,dados$idade, method = "pearson")
# Conclusão: O coeficiente de correlação foi de 0.2874694 (desprezível) com p-value < 2.2e-16 < 0.05 = alpha, Rejeitamos H0.
# Logo, há associação desprezível entre salario e Idade 

# Correlação de Spearman
#   H0 - r=0 (Não há uma associação)
#   H1 - r!=0 (Há uma associação)
cor.test(dados$salario,dados$idade, method = "spearman")
# Conclusão: O coeficiente de correlação foi de 0.3409942 (fraca) com p-value < 2.2e-16 < 0.05 = alpha, Rejeitamos H0.
# Logo, há associação fraca entre salario e Idade 

# Correlação de Kendall
#   H0 - r=0 (Não há uma associação)
#   H1 - r!=0 (Há uma associação)
cor.test(dados$salario,dados$idade, method = "kendall")
# Conclusão: O coeficiente de correlação foi de 0.2393206 (desprezível) com p-value < 2.2e-16 < 0.05 = alpha, Rejeitamos H0.
# Logo, há associação desprezível entre salario e Idade 


# j) Há relação entre a Experiência e a Idade dos Indivíduos?
# Correlação de Pearson
#   H0 - r=0 (Não há uma associação)
#   H1 - r!=0 (Há uma associação)
cor.test(dados$experiencia,dados$idade, method = "pearson")
# Conclusão: O coeficiente de correlação foi de 0.970575 (forte) com p-value < 2.2e-16 < 0.05 = alpha, Rejeitamos H0.
# Logo, há associação significativa entre Experiência e Idade 

# Correlação de Spearman
#   H0 - r=0 (Não há uma associação)
#   H1 - r!=0 (Há uma associação)
cor.test(dados$experiencia,dados$idade, method = "spearman")
# Conclusão: O coeficiente de correlação foi de 0.971742 (forte) com p-value < 2.2e-16 < 0.05 = alpha, Rejeitamos H0.
# Logo, há associação significativa entre Experiência e Idade 

# Correlação de Kendall
#   H0 - r=0 (Não há uma associação)
#   H1 - r!=0 (Há uma associação)
cor.test(dados$experiencia,dados$idade, method = "kendall")
# Conclusão: O coeficiente de correlação foi de 0.8823579 (forte) com p-value < 2.2e-16 < 0.05 = alpha, Rejeitamos H0.
# Logo, há associação significativa entre Experiência e Idade 


# 3) O banco de dados descrito acima, foi utilizado para investigar o poder preditivo
# dos fatores destacados na base sobre o salário. Desta forma apresente o modelo de regressão que ajude a
# entender a forma como cada uma destas variáveis explicativas explica o salário das pessoas investigadas.
# Considere o seguinte modelo e responda as questões que seguem:
# 𝑠𝑎𝑙𝑎𝑟𝑖𝑜𝑖 = 𝛽0 + 𝛽1𝑠𝑒𝑥𝑜𝑖 + 𝛽2𝑐𝑜𝑟𝑖 + 𝛽3𝑒𝑠𝑡𝑐𝑖𝑣𝑖𝑙𝑖 + 𝛽4𝑖𝑛𝑠𝑡𝑟𝑢𝑐𝑎𝑜𝑖 + 𝛽5𝑒𝑥𝑝𝑒𝑟𝑖𝑒𝑛𝑐𝑖𝑎𝑖 + 𝛽6𝑖𝑑𝑎𝑑𝑒𝑖 + 𝜀𝑖

modelo_regressao <- lm(dados$salario~dados$sexo+dados$cor+dados$est_civil+dados$instrucao+dados$experiencia+dados$idade)
summary(modelo_regressao)

# k) Sabendo que as variáveis 𝛽1, 𝛽2 e 𝛽3 são todas variáveis dummies(dicotômicas), qual deveria ser
# a interpretação dada destes coeficientes sobre o salário?
# Conclusões: b0 - é significativamente diferente de 0 (p-value (Pr(>|t|)=2.51e-12 < 0.05)
#             b1 - é significativo à alpha = 0,05 (p-value (Pr(>|t|) < 2e-16 < 0.05), logo, existe efeito do sexo sobre o salário da pessoa.
#             b2 - é significativo à alpha = 0,05 (p-value (Pr(>|t|) = 0.00216 < 0.05), logo, existe efeito da cor sobre o salário da pessoa.
#             b3 - é significativo à alpha = 0,05 (p-value (Pr(>|t|) = 0.03052 < 0.05), logo, existe efeito do estado civil sobre o salário da pessoa.
#             b4 - é significativo à alpha = 0,05 (p-value (Pr(>|t|) < 2e-16 < 0.05), logo, existe efeito da instrução sobre o salário da pessoa.
#             b5 - é significativo à alpha = 0,05 (p-value (Pr(>|t|) < 2e-16 < 0.05), logo, existe efeito da experiência sobre o salário da pessoa.
#             b6 - Não definido devido a singularidade

# l) Qual é o poder explicativo do modelo (percentual de variância explicada)?
# Conclusão: R² = 0.3233 (As variáveis explicam 32% da variância do salário)

# m) De modelo geral, o modelo foi significativo?
#   Teste ANOVA - Significância do modelo como um todo.
#     Conclusão: como o p-value= 2.2e-16(0.0000)< 0.05 = alpha, Rejeitamos H0 (modelo não significativo) e Aceitamos H1 (modelo significativo)
#               Logo, existe uma coerencia nas variáveis explicativas frente a variável dependente.

# n) É possível afirmar que o grau de instrução do indivíduo afeta significativamente no seu salário?
# A que nível de significância?
# Conclusão: b4 - é significativo à alpha = 0,05 (p-value (Pr(>|t|) < 2e-16 < 0.05), logo, existe efeito da instrução sobre o salário da pessoa.

# o) É possível afirmar que o estado civil do indivíduo afeta significativamente no seu salário? A que
# nível de significância?
# Conclusão: b3 - é significativo à alpha = 0,05 (p-value (Pr(>|t|) = 0.03052 < 0.05), logo, existe efeito do estado civil sobre o salário da pessoa.

# p) É possível afirmar que a experiência do indivíduo afeta significativamente no seu salário? A que
# nível de significância?
# Conclusão: b5 - é significativo à alpha = 0,05 (p-value (Pr(>|t|) < 2e-16 < 0.05), logo, existe efeito da experiência sobre o salário da pessoa.

# q) É possível afirmar que o sexo do indivíduo afeta significativamente no seu salário? A que nível de
# significância?
# Conclusão: b1 - é significativo à alpha = 0,05 (p-value (Pr(>|t|) < 2e-16 < 0.05), logo, existe efeito do sexo sobre o salário da pessoa.

# r) Considere o modelo de modo geral, principalmente os sinais de cada um dos coeficientes do
# modelo de regressão, os resultados fazem sentido conceitualmente para você? Justifique.
# Conclusão: Podemos concluir através dos testes realizados (ANOVA) que o modelo possui coeficientes que ajudam a explicar a variável dependente.
# É importante salientar que o modelo explica apenas 32% do valor do salário, visto que existem outros fatores que impactam nesta variação.
# Todos os coeficientes são significativos a alpha, logo podemos concluir que existe efeito sobre o salário.