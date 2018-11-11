library(dplyr)


# A) Mulher
# B) Língua mãe não é o inglês
# C) GMAT 680-700
# D) Aluna de 1o quartil
# E) 8 anos de experiência
# F) 29 anos

# 1) Importar .csv
mba <- read.csv("data/mba.csv",
                 stringsAsFactors = FALSE) # para arquivos pequenos

# ou
# require(data.table) # para arquivos maiores, f_read do 
# fread("mba.csv")
View(mba)

# 2) nomes das colunas
names(mba)

# 3) média: 
# Se média e media não são próximas, é mais dificil prever alguma coisa
# dados normais, média e mediana são próximas
mean(mba$salary)

# 4) mediana:
median(mba$salary)

# média muito diferente da mediana => dados não normais
# 5) histograma
hist(mba$salary)

# típico de base de dados incompleto, erros
# 999 - não informou
# 998 não respondeu

# 6) limpeza de dados
mba2 <- filter(mba, salary != 998 & salary != 999)

# 7) repete
mean(mba2$salary)
median(mba2$salary)
hist(mba2$salary)

length(mba2$salary)
sum(mba2$salary == 0)

# 8) retira 0
mba3 <- filter(mba2, salary != 0)
mean(mba3$salary)
median(mba3$salary)
hist(mba3$salary)

# A) Mulher
# teste de normalidade (pesquisar)
boxplot(formula = mba3$salary ~ mba3$sex)

# oneway.test - ANOVA (para dois grupos é praticamente um teste de média)
# H_A: salário é influenciada pelo gênero
# H_0: salário não é influenciada pelo gênero
oneway.test(mba3, formula = salary ~ sex)

# retirar o outlier feminino
mba4 <- filter(mba3, salary != max(mba3$salary))

boxplot(formula = mba4$salary ~mba4$sex)
oneway.test(mba4, formula = salary ~ sex)

# p-value = 0; concluímos que o salário é influenciado pelo gênero

# B) Língua materna
unique(mba4$frstlang)
# 1 - ingês | 2 - outros
sum(mba4$frstlang == 1)
sum(mba4$frstlang == 2)
# não vamos utilizar frstlang, pois a quantidade de 
# frstlang == 2 é muito baixa (n = 6)

# C) Quartil
boxplot(mba4$salary ~ mba4$quarter)

# Quartil, variável categórica ordinal, portanto é possível fazer uma
# regressão linear
modelo1 <- lm(mba4, formula = salary ~ quarter)

# teste de hipóste:
# H_0: quartil não influencia o salário
# H_A: quartil influencia o salário
modelo1
summary(modelo1)
# p-value = 0.00179, quartil influencia no salário

# D) Anos de experiência
# H_0: anos de experiência não influencia no salário
# H_A: anos de experiência influencia no salário
modelo2 <- lm(mba4, formula = salary ~ work_yrs)
summary(modelo2)
# 96687.8 + 1456.1*work_yrs
# p-valor = 0.0022, portanto anos de experiência influencia no salário

# E) Idade
# H_0: idade não influencia no salário
# H_A: idade influencia no salário
modelo3 <- lm(mba4, formula = salary ~ age)
summary(modelo3)
# 60613.5 + 1548.8*age
# p-value = 0,0004, portanto idade influencia no salário

#####
# salâry = b_0 + b_1*work_yrs + b_2*age
modelo4 <- lm(mba4, formula = salary ~ work_yrs + age)
summary(modelo4)
# p-value: 0.00209 é o p-valor do resíduo (resíduos estão normais)
# problema de multicolinearidade (as variáveis não são independetes)
# p-value work_yrs: 0.90
# p-value age     : 0.08
# usar work_yrs pois age pode ser discriminatório

str(modelo4)

# GMAT - fazer

modelo5 <- lm(mba4, formula = salary ~ sex + work_yrs + quarter)
summary(modelo5)
# modelo final
# salâry = 119309.8 - 9973.4*sex + 912.2*work_yrs - 3462.8*quarter

