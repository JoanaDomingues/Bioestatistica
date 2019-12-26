# Abre a base de dados  
setwd("D:\\BIOEST\\Trabalho final")
data <- read.csv2("student-mat.csv", header = TRUE, sep = ";")

# Temos as notas de mat. do 1º, 2º e 3º períodos
# Queremos apenas as do 3º período (porque são o balanço de todo o ano escolar)
# A função NULL permite-nos remover as colunas indesejadas 
data$G1 <- NULL
data$G2 <- NULL

# Vamos chamar a variável G3 por grade (só para que fique mais claro)
# E acrescentar a nova variável à tabela
grade <- data$G3
data$G3 <- NULL
data <- cbind(data, grade)

# A variável grade é quantitativa
# Queremos transformá-la numa variável nominal dicotómica: positiva (10 a 20) e negativa (0 a 9)
# E atribuir o valor 1 a positiva e o valor 0 a negativa
# A variável assim definida será a nossa variável dependente
nominal_var <- c(1:395)
dependent_var <- c(1:395)
for (i in 1:395){
  if (grade[i] >= 10){ 
    nominal_var[i] <- "positive"
    dependent_var[i] <- 1} else{
      nominal_var[i] <- "negative"
      dependent_var[i] <- 0
    }
}

# Vamos acrescentar à tabela as variáveis nominal_var e dependent_var
data <- cbind(data, nominal_var, dependent_var)
View(data)

# Vamos passar à seleção das variáveis independentes mais significativas (pontos 1 a 3)

# 1) Fazer gráficos de extremos e quartis para excluir algumas variáveis quantitativas
#    O caso ideal é ter um gráfico com o 1º, 2º (mediana) e 3º quartis, pois denota a variabilidade da amostra
#    Também são admissíveis alguns (poucos) outliers
boxplot(data$age~data$dependent_var, xlab = "dependent var", ylab = "age") 
# Exclui-se a variável age, porque o 1º quartil e a mediana são coincidentes 
boxplot(data$failures~data$dependent_var, xlab = "dependent var", ylab = "failures")
# Exclui-se a variável failures, porque não existem valores inferiores à mediana, e quando a nota é positiva (1) os alunos não tiveram qualquer reprovação, à exceção de 3 (os outliers)
boxplot(data$absences~data$dependent_var, xlab = "dependent var", ylab = "absences")
# Exclui-se a variável absences, devida à presença de muitos outliers quando a nota é positiva (1)
# O principal problema associado aos outliers advém do facto de estes introduzirem um viés na média
# Rejeitam-se, assim, todas as variáveis independentes

# Por curiosidade, vamos fazer testes de hipóteses para as variáveis failures (número de reprovações) e absences (número de faltas), para perceber se elas estão ou não associadas
# Podemos considerar que as amostras são emparelhadas, uma vez que ambas medem um número inteiro adimensional e as medidas são repetidas para os mesmos sujeitos
# Comecemos por averiguar se as distribuições das variáveis seguem a distribuição normal
# Para tal, utilizamos o teste de Shapiro-Wilk, cuja hipótese nula é que a distribuição em estudo segue a distribuição normal
shapiro.test(data$failures)
shapiro.test(data$absences)
# Como em ambos os casos o valor p é muito menor do que o nível de significância considerado (alpha = 0.05), a hipótese nula deve ser rejeitada
# Assim, o teste mais potente é um teste não paramétrico, que neste caso é o teste de Wilcoxon Signed-Rank, cuja hipótese nula é que as distribuições das variáveis são idênticas
wilcox.test(data$failures, data$absences, paired = TRUE)
# Uma vez que p << 0.05, rejeita-se a hipótese nula, reconhecendo-se que existem diferenças estatisticamente significativas entre as variáveis
# Apesar das diferenças, não vamos utilizar estas variáveis, pela razão já indicada

# 2) As restantes variáveis são qualitativas, tanto ordinais como nominais, mas vamos tratá-las da mesma forma
#    Numa primeira análise, vamos descrever estas variáveis em termos de frequências (vamos preferir frequências relativas) e elaborar gráficos de barras
#    Devemos, então, excluir as variáveis cujos valores pertençam a um número relativamente pequeno de classes, dado que isso demonstra a falta de variabilidade na amostra
school_abs_freq <- table(data$school)
school_rel_freq <- prop.table(school_abs_freq)*100
barplot(school_rel_freq, xlab = "school", ylab = "% of students")
# Exclui-se a variável school, pois claramente existem mais alunos a frequentar a escola Gabriel Pereira (GP) do que a escola Mousinho da Silveira (MS)
sex_abs_freq <- table(data$sex)
sex_rel_freq <- prop.table(sex_abs_freq)*100
barplot(sex_rel_freq, xlab = "sex", ylab = "% of students")
# Ficamos com a variável sex, pois a proporção de alunos (M) e alunas (F) é semelhante
address_abs_freq <- table(data$address)
address_rel_freq <- prop.table(address_abs_freq)*100
barplot(address_rel_freq, xlab = "address", ylab = "% of students")
# Exclui-se a variável address, pois claramente existem mais alunos a viver em meio urbano (U) do que em meio rural (R)
famsize_abs_freq <- table(data$famsize)
famsize_rel_freq <- prop.table(famsize_abs_freq)*100
barplot(famsize_rel_freq, xlab = "famsize", ylab = "% of students")
# Exclui-se a variável famsize, pois claramente existem mais alunos cujo agregado familiar é composto por mais de 3 elementos (GT3) do que quando é composto por menos de três (LE3)
Pstatus_abs_freq <- table(data$Pstatus)
Pstatus_rel_freq <- prop.table(Pstatus_abs_freq)*100
barplot(Pstatus_rel_freq, xlab = "Pstatus", ylab = "% of students")
# Exclui-se a variável Pstatus, pois claramente existem mais alunos cujos pais estão juntos (T) do que quando os pais estão separados (A)
Medu_abs_freq <- table(data$Medu)
Medu_rel_freq <- prop.table(Medu_abs_freq)*100
barplot(Medu_rel_freq, xlab = "Medu", ylab = "% of students")
# Ficamos com a variável Medu, pois apesar de existir uma classe que quase não apresenta representação, as restantes estão todas bem representadas 
Fedu_abs_freq <- table(data$Fedu)
Fedu_rel_freq <- prop.table(Fedu_abs_freq)*100
barplot(Fedu_rel_freq, xlab = "Fedu", ylab = "% of students")
# Ficamos com a variável Fedu, pelo motivo anterior
Mjob_abs_freq <- table(data$Mjob)
Mjob_rel_freq <- prop.table(Mjob_abs_freq)*100
barplot(Mjob_rel_freq, xlab = "Mjob", ylab = "% of students")
# Ficamos com a variável Mjob, pois não existe nenhuma classe que se realce muito além das outras
Fjob_abs_freq <- table(data$Fjob)
Fjob_rel_freq <- prop.table(Fjob_abs_freq)*100
barplot(Mjob_rel_freq, xlab = "Fjob", ylab = "% of students")
# Ficamos com a variável Fjob, pelo motivo anterior
reason_abs_freq <- table(data$reason)
reason_rel_freq <- prop.table(reason_abs_freq)*100
barplot(reason_rel_freq, xlab = "reason", ylab = "% of students")
# Ficamos com a variável reason, pelo motivo anterior
guardian_abs_freq <- table(data$guardian)
guardian_rel_freq <- prop.table(guardian_abs_freq)*100
barplot(guardian_rel_freq, xlab = "guardian", ylab = "% of students")
# Rejeitamos a variável guardian, pois claramente existem mais alunos cujo encarregado de educação é a mãe (mother), do que quando é o pai (father) ou outra pessoa (other)
traveltime_abs_freq <- table(data$traveltime)
traveltime_rel_freq <- prop.table(traveltime_abs_freq)*100
barplot(traveltime_rel_freq, xlab = "traveltime", ylab = "% of students")
# Rejeitamos a variável traveltime, pois claramente existem mais alunos que demoram menos de 15 minutos (1) no percurso escola-casa (e vice-versa)
studytime_rel_freq <- prop.table(studytime_abs_freq)*100
barplot(studytime_rel_freq, xlab = "studytime", ylab = "% of students")
# Rejeitamos a variável studytime, pois claramente existem mais alunos que estudam 2 a 5 horas por dia (2)
schoolsup_abs_freq <- table(data$schoolsup)
schoolsup_rel_freq <- prop.table(schoolsup_abs_freq)*100
barplot(schoolsup_rel_freq, xlab = "schoolsup", ylab = "% of students")
# Rejeitamos a variável schoolsup, pois claramente existem mais alunos que não frequentam o apoio ao estudo na escola (no) do que alunos que o frequentam (yes)
famsup_abs_freq <- table(data$famsup)
famsup_rel_freq <- prop.table(famsup_abs_freq)*100
barplot(famsup_rel_freq, xlab = "famsup", ylab = "% of students")
# Rejeitamos a variável famsup, pois claramente existem mais alunos que recebem apoio ao estudo pelos pais (yes) do que os que não recebem (no)
paid_abs_freq <- table(data$paid)
paid_rel_freq <- prop.table(paid_abs_freq)*100
barplot(paid_rel_freq, xlab = "paid", ylab = "% of students")
# Aceitamos a variável paid, pois a proporção de alunos que têm (yes) e que não têm (no) explicações pagas é semelhantes 
activities_abs_freq <- table(data$activities)
activities_rel_freq <- prop.table(activities_abs_freq)*100
barplot(activities_rel_freq, xlab = "activities", ylab = "% of students")
# Aceitamos a variável activities, pois a proporção de alunos que tem (yes) ou não tem (no) uma atividade extracurricular é semelhante
nursery_abs_freq <- table(data$nursery)
nursery_rel_freq <- prop.table(nursery_abs_freq)*100
barplot(nursery_rel_freq, xlab = "nursery", ylab = "% of students")
# Rejeitamos a variável nursery, pois claramente existem mais alunos que frequentaram o infantário (yes) do que os que não frequentaram (no)
higher_abs_freq <- table(data$higher)
higher_rel_freq <- prop.table(higher_abs_freq)*100
barplot(higher_rel_freq, xlab = "higher", ylab = "% of students")
# Rejeitamos a variável higher, pois claramente existem mais alunos que querem frequentar o ensino superior (yes) do que os que não querem (no)
internet_abs_freq <- table(data$internet)
internet_rel_freq <- prop.table(internet_abs_freq)*100
barplot(internet_rel_freq, xlab = "internet", ylab = "% of students")
# Rejeitamos a variável internet, pois claramente existem mais alunos com acesso à internet em casa (yes) do que os que não têm esse acesso (no)
romantic_abs_freq <- table(data$romantic)
romantic_rel_freq <- prop.table(romantic_abs_freq)*100
barplot(romantic_rel_freq, xlab = "romantic", ylab = "% of students")
# Rejeitamos a variável romantic, pois claramente existem mais alunos que não estão num relacionamento amoroso (no), do que os que estão (yes)
famrel_abs_freq <- table(data$famrel)
famrel_rel_freq <- prop.table(famrel_abs_freq)*100
barplot(famrel_rel_freq, xlab = "famrel", ylab = "% of students")
# Rejeitamos a variável famrel, pois existem mais alunos cuja relação com a família é boa (4)
freetime_abs_freq <- table(data$freetime)
freetime_rel_freq <- prop.table(freetime_abs_freq)*100
barplot(freetime_rel_freq, xlab = "freetime", ylab = "% of students")
# Aceitamos a variável freetime, pois não existe nenhuma classe que se realce muito além das outras 
goout_abs_freq <- table(data$goout)
goout_rel_freq <- prop.table(goout_abs_freq)*100
barplot(goout_rel_freq, xlab = "goout", ylab = "% of students")
# Aceitamos a variável goout, pelo motivo anterior
Dalc_abs_freq <- table(data$Dalc)
Dalc_rel_freq <- prop.table(Dalc_abs_freq)*100
barplot(Dalc_rel_freq, xlab = "Dalc", ylab = "% of students")
# Rejeitamos a variável Dalc, pois claramente existem mais alunos que consomem muito pouco álcool durante a semana (1)
Walc_abs_freq <- table(data$Walc)
Walc_rel_freq <- prop.table(Walc_abs_freq)*100
barplot(Walc_rel_freq, xlab = "Walc", ylab = "% of students")
# Aceitamos a variável Walc, pois não existe nenhuma classe que se realce muito além das outras 
health_abs_freq <- table(data$health)
health_rel_freq <- prop.table(health_abs_freq)*100
barplot(health_rel_freq, xlab = "health", ylab = "% of students")
# Aceitamos a variável health, pois não existe nenhuma classe que se realce muito além das outras 
# Ficámos com 12 variáveis independentes:sex, Medu, Fedu, Mjob, Fjob, reason, paid, activities, freetime, goout, Walc e health 
# É de salientar que se o número de classes aumenta, é de esperar que os alunos se encontrem mais distribuídos pelas várias classes

# 3) Vamos agora recorrer ao Teste Exato de Fisher para extrairmos as variáveis independentes mais significativas
#    Este teste tem como hipótese nula que as variáveis estão associadas 
#    Como é bastante árduo analisar a relação entre todas as variáveis vamos "a olho nu" identificar as variáveis que julgamos estarem mais fortemente associadas
#    Elas são: Medu e Mjob; Fedu e Fjob; freetime e goout
#    Para realizarmos o Teste Exato de Fisher, temos primeiro que elaborar as tabelas de contingência dessas variáveis
library(knitr)
table_1 <- table(data$Medu, data$Mjob)
kable(table_1)
fisher.test(table_1, simulate.p.value = TRUE) 
table_2 <- table(data$Fedu, data$Fjob)
kable(table_2)
fisher.test(table_2, simulate.p.value = TRUE)
table_3 <- table(data$freetime, data$goout)
kable(table_3)
fisher.test(table_3, simulate.p.value = TRUE)
# Para os 3 casos, conclui-se que a hipótese nula deve ser rejeitada (p << 0.05), ou seja, não existe associação estatisticamente significativa entre as variáveis
# Assim, vamos utilizar as 12 variáveis independentes na regressão logística

# 5) Vamos, agora, avaliar a significância de cada uma das variáveis independentes selecionadas por meio da regressão logística
grade_LR <- glm(dependent_var ~ sex + Medu + Fedu + Mjob + Fjob + reason + paid + activities + freetime + goout + Walc + health, data = data, family = binomial)
summary(grade_LR) 
confint(grade_LR)
# Observa-se em todos os casos, exceto para a variável goout, que o valor p é superior ao nível de significância e que o intervalo de confiança contém o zero 
# A hipótese nula conjetura que a distribuição da variável dependente segue as distribuições das variáveis independentes
# Assim, aceitamos a hipótese nula em todos os casos, exceto para a variável goout, que vamos excluir

# Por fim, procedemos à avaliação da regressão logística, que pode ser feita por meio de diferentes metodologias
# 1) Teste de Hosmer-Lemeshow 
# 2) Pseudo R^2, Cox-Snell e Nagelkerke
# 3) Matriz confusão

# 1)
library(ResourceSelection)
HL_test <- hoslem.test(data$dependent_var, fitted(grade_LR), g = 10)
HL_test
# A hipótese nula do teste de Hosmer-Lemeshow é que os valores previstos são iguais aos valores observados
# Tem-se p > 0.05, pelo que se aceita a hipótese nula
# Podemos assim considerar que modelo logístico está bem ajustado

# 2)
grade_LR_null <- glm(dependent_var ~ 1,  data = data, family = binomial)
LL_null <- logLik(grade_LR_null)
LL_k    <- logLik(grade_LR)
R_Cox <- 1 - (exp(LL_null[1])/exp(LL_k[1]))^(2/length(data$dependent_var))
R_Nag <- R_Cox/(1-(exp(LL_null[1]))^(2/length(data$dependent_var)))
print(sprintf('R2 Cox = %s',R_Cox))
print(sprintf('R2 Naguelkerke = %s',R_Nag))
# Tenha-se em conta a preferência pelo valor do Naguelkerke. Nesse caso, R^2 = 0.255
# Esse valor indica que o modelo logístico explica 25,5% da variável dependente

# 3) 
prob <- predict(grade_LR, type = c('response'), data)
confusion_matrix <- table(prob > 0.5, data$dependent_var)
kable(confusion_matrix)
# Para podermos tirar conclusões, vamos calcular as seguintes métricas: exatidão (Ex), especificidade (E), sensibilidade (S), valor preditivo positivo (VP+), valor preditivo negativo (VP-)
print(sprintf('Ex = %s',(241+50)/(50+24+80+241)*100))
print(sprintf('E = %s',50/(50+80)*100))
print(sprintf('S = %s',241/(241+24)*100))
print(sprintf('VP+ = %s',241/(241+80)*100))
print(sprintf('VP- = %s',50/(50+24)*100))
# Ex = 73,7%, pelo que a exatidão, i.e, a proximidade dos valores ao valor verdadeiro, é boa, tal como gostaríamos 
# E = 38,5%, pelo que a probabilidade de obtermos um falso negativo é baixa, tal como gostaríamos 
# S = 90,9%, pelo que a probabilidade de obtermos um verdadeiro positivo é bastante elevada. Um valor assim tão elevado não é bom, pois indica que o teste é demasiado otimista
# VP+ = 75,1%, pelo que a probabilidade de se estar a classificar bem os verdadeiros positivos é elevada, tal como gostaríamos 
# VP- = 67,6%, pelo que a probabilidade de se estar a classificar bem os verdadeiros negativos é elevada, tal como gostaríamos
