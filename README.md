# wine-quality-modeling
### Cria um modelo com script feito no R para estimar a qualidade do vinho
### - Murillo Marco Carvalho Cunha
### - Teste de Modelagem
### - Objetivo: criar um modelo para estimar a qualidade do vinho
### - 11/06/2018

#pacotes necessários
install.packages("MASS")   # polr{MASS}
install.packages("rpart")  # rpart{rpart}
install.packages("randomForest")  # randomForest{randomForest}
library(MASS)
library(rpart)
library(randomForest)

#carregando base de dados
setwd("C:\\Users\\Murillo Marco\\Desktop\\Cognitivo.AI")

wqdata = read.csv("winequality.csv", sep = ";", h = T, stringsAsFactors = F)

#visualizar a base de dados que acabei de carregar
View(head(wqdata))

#nomes das colunas existentes
names(wqdata)

#visualizar a dimensão da minha tabela
dim(wqdata)

#visualizar a estrutura dos campos da tabela
str(wqdata)

################################################
####### 1.ANALISE EXPLORATÓRIA DOS DADOS #######
################################################

#OBS.: a variável alcohol entrou como string
head(wqdata)

# verificando possíveis inconsistências
length.alcohol = nchar(wqdata$alcohol)

ftable(length.alcohol) #foram encontrados 40 valores com 19 caracteres, possivelmente por erro de input

#insere a qtd de caracteres da variável "alcohol"
wqdata.1 = cbind(wqdata,length.alcohol)

check.values=head(wqdata.1[order(-length.alcohol),c("alcohol","length.alcohol","quality")],40)
unique(check.values$quality)

# separando a base onde os valores de "alcohol" foi inconsistente das demais
wqdata.1.part1 = wqdata.1[which(wqdata.1$length.alcohol<=5),]
wqdata.1.part2 = wqdata.1[which(wqdata.1$length.alcohol>5),]

#retirando a variável "length.alcohol" de wqdata.1.part1
wqdata.1.part1 = subset(wqdata.1.part1,select = -length.alcohol)
names(wqdata.1.part1)

#retirando a variável "length.alcohol" e "alcohol" de wqdata.1.part2
wqdata.1.part2 = subset(wqdata.1.part2,select = -c(length.alcohol,alcohol))
names(wqdata.1.part2)

#convertendo "alcohol" para numérica
wqdata.1.part1$alcohol = as.numeric(wqdata.1.part1$alcohol)
str(wqdata.1.part1)

#calculando a média dos valores de "alcohol" agrupado por "quality" e "type"
join = aggregate(alcohol~quality+type,data=wqdata.1.part1,mean)

#substituindo os valores inconsistentes de "alcohol" pelas médias
wqdata.1.part2=merge(wqdata.1.part2,join,by = c("quality","type"),all.x = T)
str(wqdata.1.part2)
str(wqdata.1.part1)

#unindo as bases novamente
wqdata.2 = rbind(wqdata.1.part1, wqdata.1.part2)
dim(wqdata.2)
str(wqdata.2)
names(wqdata.2)

##############################################################
#ainda verificando inconsistências

layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
boxplot(fixed.acidity~quality, data = wqdata.2, main = "Boxplot by quality")
boxplot(wqdata.2$fixed.acidity, main = "Boxplot")
hist(wqdata.2$fixed.acidity, main = "Histogram")
max(wqdata.2$fixed.acidity)
sort(boxplot.stats(wqdata.2$fixed.acidity)$out) #outliers

#atenção para outlier
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
boxplot(volatile.acidity~quality, data = wqdata.2, main = "Boxplot by quality")
boxplot(wqdata.2$volatile.acidity, main = "Boxplot")
hist(wqdata.2$volatile.acidity, main = "Histogram")
sort(boxplot.stats(wqdata.2$volatile.acidity)$out) #outliers
length(sort(boxplot.stats(wqdata.2$volatile.acidity)$out))
q1=quantile(wqdata.2$volatile.acidity,1/4)
q3=quantile(wqdata.2$volatile.acidity,3/4)
min(wqdata.2$volatile.acidity)
max(wqdata.2$volatile.acidity)

#atenção para outlier
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
boxplot(citric.acid~quality, data = wqdata.2, main = "Boxplot by quality")
boxplot(wqdata.2$citric.acid, main = "Boxplot")
hist(wqdata.2$citric.acid, main = "Histogram")
sort(boxplot.stats(wqdata.2$citric.acid)$out) #outliers
length(sort(boxplot.stats(wqdata.2$citric.acid)$out))

layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
boxplot(residual.sugar~quality, data = wqdata.2, main = "Boxplot by quality")
boxplot(wqdata.2$residual.sugar, main = "Boxplot")
hist(wqdata.2$residual.sugar, main = "Histogram")
sort(boxplot.stats(wqdata.2$residual.sugar)$out) #outliers
q1=quantile(wqdata.2$residual.sugar,1/4)
q3=quantile(wqdata.2$residual.sugar,3/4)
min(wqdata.2$residual.sugar)
q1-1.5*(q3-q1)
q3+1.5*(q3-q1)
max(wqdata.2$residual.sugar)

layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
boxplot(chlorides~quality, data = wqdata.2, main = "Boxplot by quality")
boxplot(wqdata.2$chlorides, main = "Boxplot")
hist(wqdata.2$chlorides, main = "Histogram")
sort(boxplot.stats(wqdata.2$chlorides)$out) #outliers

layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
boxplot(free.sulfur.dioxide~quality, data = wqdata.2, main = "Boxplot by quality")
boxplot(wqdata.2$free.sulfur.dioxide, main = "Boxplot")
hist(wqdata.2$free.sulfur.dioxide, main = "Histogram")
sort(boxplot.stats(wqdata.2$free.sulfur.dioxide)$out)

layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
boxplot(total.sulfur.dioxide~quality, data = wqdata.2, main = "Boxplot by quality")
boxplot(wqdata.2$total.sulfur.dioxide, main = "Boxplot")
hist(wqdata.2$total.sulfur.dioxide, main = "Histogram")
sort(boxplot.stats(wqdata.2$total.sulfur.dioxide)$out)

#grande evidência de inconsitência em "density" - tratar
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
boxplot(density~quality, data = wqdata.2, main = "Boxplot by quality")
boxplot(wqdata.2$density, main = "Boxplot")
hist(wqdata.2$density, main = "Histogram")
sort(boxplot.stats(wqdata.2$density)$out) #outliers
q1=quantile(wqdata.2$density,1/4)
q3=quantile(wqdata.2$density,3/4)
min(wqdata.2$density)
q1-1.5*(q3-q1)
q3+121.5*(q3-q1)
max(wqdata.2$density)

layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
boxplot(pH~quality, data = wqdata.2, main = "Boxplot by quality")
boxplot(wqdata.2$pH, main = "Boxplot")
hist(wqdata.2$pH, main = "Histogram")
sort(boxplot.stats(wqdata.2$pH)$out)

layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
boxplot(sulphates~quality, data = wqdata.2, main = "Boxplot by quality")
boxplot(wqdata.2$sulphates, main = "Boxplot")
hist(wqdata.2$sulphates, main = "Histogram")
sort(boxplot.stats(wqdata.2$sulphates)$out) #outliers

#alcohol evidentemente impacta a qualidade do vinho
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
boxplot(alcohol~quality, data = wqdata.2, main = "Boxplot by quality")
boxplot(wqdata.2$alcohol, main = "Boxplot")
hist(wqdata.2$alcohol, main = "Histogram")
sort(boxplot.stats(wqdata.2$alcohol)$out) #outliers

###########################################################
#verificou-se que há presença de outliers em todas as variáveis algumas 
#muito discrepantes (no caso de "density" será feito tratamento devido alta discrepância).
###########################################################

############## tratamento density #####
# separando a base onde os valores de "density" foi inconsistente das demais
wqdata.2.part1 = wqdata.2[which(wqdata.2$density < 10.001),]
wqdata.2.part2 = wqdata.2[which(wqdata.2$density >= 10.001),]

names(wqdata.2.part1)
dim(wqdata.2.part1)
wqdata.2.part2 = subset(wqdata.2.part2,select = -density)
names(wqdata.2.part2)
dim(wqdata.2.part2)

#calculando a média dos valores de "density" agrupado por "quality" e "type"
join = aggregate(density~quality+type,data=wqdata.2.part1,mean)

#substituindo os valores inconsistentes de "density" pelas médias
wqdata.2.part2=merge(wqdata.2.part2,join,by = c("quality","type"),all.x = T)
str(wqdata.2.part2)
str(wqdata.2.part1)

#unindo as bases novamente
wqdata.3 = rbind(wqdata.2.part1, wqdata.2.part2)
dim(wqdata.3)
str(wqdata.3)
names(wqdata.3)

dev.off()
boxplot(fixed.acidity~quality, data = wqdata.3, main = "Boxplot by quality")
boxplot(volatile.acidity~quality, data = wqdata.3, main = "Boxplot by quality")
boxplot(citric.acid~quality, data = wqdata.3, main = "Boxplot by quality")
boxplot(residual.sugar~quality, data = wqdata.3, main = "Boxplot by quality")
boxplot(chlorides~quality, data = wqdata.3, main = "Boxplot by quality")
boxplot(free.sulfur.dioxide~quality, data = wqdata.3, main = "Boxplot by quality")
boxplot(total.sulfur.dioxide~quality, data = wqdata.3, main = "Boxplot by quality")
boxplot(density~quality, data = wqdata.3, main = "Boxplot by quality")
boxplot(pH~quality, data = wqdata.3, main = "Boxplot by quality")
boxplot(sulphates~quality, data = wqdata.3, main = "Boxplot by quality")
boxplot(alcohol~quality, data = wqdata.3, main = "Boxplot by quality")

spearman.cor=round(c(cor(wqdata.3$fixed.acidity,wqdata.3$quality, method = "spearman")
,cor(wqdata.3$volatile.acidity,wqdata.3$quality, method = "spearman")
,cor(wqdata.3$citric.acid,wqdata.3$quality, method = "spearman")
,cor(wqdata.3$residual.sugar,wqdata.3$quality, method = "spearman")
,cor(wqdata.3$chlorides,wqdata.3$quality, method = "spearman")
,cor(wqdata.3$free.sulfur.dioxide,wqdata.3$quality, method = "spearman")
,cor(wqdata.3$total.sulfur.dioxide,wqdata.3$quality, method = "spearman")
,cor(wqdata.3$density,wqdata.3$quality, method = "spearman")
,cor(wqdata.3$pH,wqdata.3$quality, method = "spearman")
,cor(wqdata.3$sulphates,wqdata.3$quality, method = "spearman")
,cor(wqdata.3$alcohol,wqdata.3$quality, method = "spearman")
),4)
var.names=c("fixed.acidity","volatile.acidity","citric.acid","residual.sugar","chlorides","free.sulfur.dioxide","total.sulfur.dioxide","density","pH","sulphates","alcohol")
spear.cor=as.data.frame(cbind(var.names,spearman.cor))
spear.cor[order(-spearman.cor),]

# As variáveis "alcohol", "volatile.acidity","chlorides" e "density" são
# que possívelmente impactam mais na qualidade do vinho devido maior correlação
# com a variável resposta, sendo que "alcohol" possue correlação positiva
# as demais negativa.
# Além disso, o teste Wilcoxon-Mann-Whitney (p-valor = 2.2e-16) mostrou que a distribuição
# dos escores de qualidade entre o vinho tinto e o branco diferem. Observando o gráfico,
# pode-se ver que há um deslocamento dos escores 5 e 6, sendo que a proporção de 6 é maior
# no vinho branco. Ainda, seus escores 7 e 8 são maiores e uma pequena quantidade chega ao 9.

# independent 2-group Mann-Whitney U Test 
wilcox.test(wqdata.3$quality~factor(wqdata.3$type,ordered = T)) 

data.red = wqdata.3[which(wqdata.3$type == "Red"),]
data.white = wqdata.3[which(wqdata.3$type == "White"),]

layout(matrix(c(1,2), 1, 2, byrow = TRUE))
barplot(table(data.red$quality)/nrow(data.red), #divide the frequency counts by the total
        main = "Red", #Give your chart a title
        xlab = "Quality", #Label the x axis
        ylab = "Relative Frequency", #Label the y axis 
        col = "red")

barplot(table(data.white$quality)/nrow(data.white), #divide the frequency counts by the total
        main = "White", #Give your chart a title
        xlab = "Quality", #Label the x axis
        ylab = "Relative Frequency", #Label the y axis 
        col = "grey")
dev.off()

###############################################
# Quanto aos outliers:
###############################################
names(wqdata.3)
#wqdata.3$type.out = 0
wqdata.3$fixed.acidity.out = 0
wqdata.3$volatile.acidity.out = 0
wqdata.3$citric.acid.out = 0
wqdata.3$residual.sugar.out = 0
wqdata.3$chlorides.out = 0
wqdata.3$free.sulfur.dioxide.out = 0
wqdata.3$total.sulfur.dioxide.out = 0
wqdata.3$density.out = 0
wqdata.3$pH.out = 0
wqdata.3$sulphates.out = 0
wqdata.3$alcohol.out = 0
#wqdata.3$quality.out = 0
wqdata.3$total.out = 0

#marcando outliers

n=length(wqdata.3$quality)

for(j in 14:24){
  k=j-12
  min.highers = quantile(wqdata.3[,k], 3/4) + 1.75*IQR(wqdata.3[,k])
  for (i in 1:n) {
    if (wqdata.3[i,k] >= min.highers) {
      wqdata.3[i,j] = 1
      } 
    else {
      wqdata.3[i,j] = 0  
    }
  }
}
head(wqdata.3)

#marcando flag de registros com pelo menos um outlier
for(i in 1:n){
  if(sum(wqdata.3[i,14:24]) > 0) {
    wqdata.3[i,25] = 1 } else {
      wqdata.3[i,25] = 0
    }
  }
head(wqdata.3)

round((table(wqdata.3$total.out)/sum(table(wqdata.3$total.out)))*100,2)
#15,45% de outliers

table(wqdata.3$total.out,wqdata.3$quality)
round((table(wqdata.3$total.out,wqdata.3$quality)/sum(table(wqdata.3$total.out,wqdata.3$quality)))*100,2)

freq.noout = table(wqdata.3$total.out,wqdata.3$quality)[1,]
freq.out = table(wqdata.3$total.out,wqdata.3$quality)[2,]
dist.noout = round((freq.noout/sum(freq.noout))*100,2)
dist.out = round((freq.out/sum(freq.out))*100,2)
tbl=rbind(dist.noout,dist.out)
barplot(tbl, main="Car Distribution by Gears and VS",
        xlab="Number of Gears", col=c("darkblue","red"),legend = c("Não outliers","Outliers"), beside=TRUE)

# Pode-se ver que os outliers tendem a ter pior qualidade de vinho.
# Eles tem maior proporção nos scores 3 a 5, enquanto não outilers tem maiores
# proporções nos scores de 6 a 9.

#wqdata.4: base sem outliers
wqdata.4=wqdata.3[which(wqdata.3$total.out==0),]
wqdata.4 = subset(wqdata.4,select = c(1:13))
dim(wqdata.4)
hist(wqdata.4$fixed.acidity)
hist(wqdata.4$volatile.acidity)
hist(wqdata.4$citric.acid)
hist(wqdata.4$residual.sugar)
hist(wqdata.4$chlorides)
hist(wqdata.4$free.sulfur.dioxide)
hist(wqdata.4$total.sulfur.dioxide)
hist(wqdata.4$density)
hist(wqdata.4$pH)
hist(wqdata.4$sulphates)
hist(wqdata.4$alcohol)

################################################
###########       2.MODELAGEM       ############
################################################

#######################################################
###### A. ESTRATÉGIA DE MODELAGEM
# COMO TEMOS VARIÁVEL RESPOSTA CATEGÓRICA ORDINAL, IREI UTIZAR DUAS ABORDAGENS INICIAIS:
# (1) REGRESSÃO LOGÍSTICA POLITÔMICA ORDINAL E (2) ÁRVORE DE DECISÃO.
# UMA ÚLTIMA ABORDAGEM SERÁ POR MACHINE LEARNING UTILIZANDO RANDOM FOREST PARA CHECAR QUAL
# MODELO POSSUI MELHOR PERFORMANCE NA CLASSIFICAÇÃO.

#######################################################
###### B. DEFINIÇÃO DA FUNÇÃO DE CUSTO
# COMO O OBJETIVO É ESTIMAR A QUALIDADE DO VINHO, BUSCAMOS O MODELO QUE GERa O MENOR
# ERRO POSSÍVEL DE CLASSIFICAÇÃO. CADA MODELO TEM UM ALGORITMO QUE BUSCA O MELHOR AJUSTE.
# PARA MODELOS DE CLASSIFICAÇÃO NO GERAL PODEMOS USAR CROSS-ENTROPY, QUE É UMA MEDIDA QUE VARIA DE 0 A 1, EM QUE QUANTO MAIS
# A PROBABILIDADE ESTIMADA DIVERGIR DO VALOR REAL OBSERVADO MAIS PRÓXIMO DE 1 SERÁ SEU VALOR.
# UM MODELO PERFEITO TERIA O VALOR ZERO.

#######################################################
###### C. CRITÉRIO UTILIZADO NA SELEÇÃO DO MODELO FINAL
# QUANDO AVALIEI OS MODELOS DE REGRESSÃO LOGÍSTICA, UTILIZEI O "RESIDUAL DEVIANCE" PARA
# SELECIONAR ENTRE ELES O FINAL. NO ENTANTO, PARA SELEÇÃO DO MODELO FINAL DAS 3 ABORDAGENS
# SELECIONEI O MODELO COM A MELHOR ACURÁCIA.

#######################################################
###### D. CRITÉRIO UTILIZADO NA VALIDAÇÃO DO MODELO. POR QUE?
# FOI UTILIZADO TRADICIONAL MÉTODO "DATA SPLIT" EM QUE 70% DA BASE DE DADOS FOI UTILIZADA
# PARA TREINAR/DESENVOLVER O MODELO. OS 30% RESTANTES FORAM O USADOS PARA TESTAR O MODELO
# E VERIFICAR SUA ACURÁRIO NA BASE DE TESTE.
# ESTE MÉTODO FOI UTILIZADO PORQUE ELE É CERCA DE 10X O MAIS BARATO COMPUTACIONALMENTE FALANDO,
# COMPARADO COM OUTROS MÉTODOS QUE TAMBÉM PODERIAM SER UTILIZADOS COMO CROSS-VALIDATION E BOOTSTRAP.

#######################################################
###### E. EVIDENCIAS DE QUE O MODELO É SUFICIENTEMENTE BOM
# O MODELO FINAL CONSEGUIU PREVER 71,43% DOS VERDADEIROS VALORES OBSERVADOS

# Prepara dados de treino e validação
wqdata.model = subset(wqdata.3,select = c(1:13))
set.seed(1991)
trainingRows = sample(1:nrow(wqdata.model), 0.7 * nrow(wqdata.model))
trainingData = wqdata.model[trainingRows, ]
testData = wqdata.model[-trainingRows, ]
dim(trainingData)
dim(testData)

####### Validando amostra:
training.type=table(trainingData$type)
test.type=table(testData$type)

#teste qui-quadrado para variável type
chisq.test(rbind(training.type,test.type))

#testes Wilcoxon para demais variáveis
wilcox.test(trainingData$quality,testData$quality) 
wilcox.test(trainingData$fixed.acidity,testData$fixed.acidity)
wilcox.test(trainingData$volatile.acidity,testData$volatile.acidity)
wilcox.test(trainingData$citric.acid,testData$citric.acid)
wilcox.test(trainingData$residual.sugar,testData$residual.sugar)
wilcox.test(trainingData$chlorides,testData$chlorides)
wilcox.test(trainingData$free.sulfur.dioxide,testData$free.sulfur.dioxide)
wilcox.test(trainingData$total.sulfur.dioxide,testData$total.sulfur.dioxide)
wilcox.test(trainingData$density,testData$density)
wilcox.test(trainingData$pH,testData$pH)
wilcox.test(trainingData$sulphates,testData$sulphates)
wilcox.test(trainingData$alcohol,testData$alcohol)

######## todos resultados tem p-valor > 5%, assim pode-se concluir que não há evidência contra a consistência da amostra.

# Cria modelo de regressão logística ordinal
trainingData$quality = factor(trainingData$quality,ordered = T)
str(trainingData)
options(contrasts = c("contr.treatment", "contr.poly"))

View(cor(trainingData[,c(2:12)]))
# Antes de rodar o modelo, percebe-se alta correlação entre as variáveis "free.sulfur.dioxide" e 
# "total.sulfur.dioxide" (0.71), o que nos leva a escolher apenas uma para evitar multicolineariedade.

polrMod = polr(quality ~ 
type + fixed.acidity
+ volatile.acidity
+ citric.acid
+ residual.sugar
+ chlorides
#+ free.sulfur.dioxide
+ total.sulfur.dioxide
+ density
+ pH
+ sulphates
+ alcohol
, data = trainingData, Hess=TRUE)
summary(polrMod)
#Residual Deviance: 9836.962

polrMod = polr(quality ~ 
                 type + fixed.acidity
               + volatile.acidity
               + citric.acid
               + residual.sugar
               + chlorides
               + free.sulfur.dioxide
               #+ total.sulfur.dioxide
               + density
               + pH
               + sulphates
               + alcohol
               , data = trainingData, Hess=TRUE)
summary(polrMod)
#Residual Deviance: 9814.43

# O modelo sem "total.sulfur.dioxide" apresentou menor "Residual Deviance" (9814 contra 9836 do modelo anterior).
# Percebe-se também um alto valor no coeficiente de "density" que por ser negativo
# vai gerar um odds ratio próximo de zero que dificulda a interpretação. Outro ponto,
# é que seu coef. de variação é muito próximo de zero (0,28%) que apesar de ser um ótimo
# indicator de estabilidade, dificulta ranquear o score.
sd(trainingData$density)/mean(trainingData$density)

polrMod = polr(quality ~ 
                 type + fixed.acidity
               + volatile.acidity
               + citric.acid
               + residual.sugar
               + chlorides
               + free.sulfur.dioxide
               #+ total.sulfur.dioxide
               #+ density
               + pH
               + sulphates
               + alcohol
               , data = trainingData, Hess=TRUE)
summary(polrMod)
#Residual Deviance: 9835.28

(ctable = coef(summary(polrMod))) # tabela com coeficientes
p = pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # tabela com p-valores
(ctable <- cbind(ctable, "p value" = p)) # tabela final
(ci = confint(polrMod,na.rm = T)) # intervalo de confiança
exp(coef(polrMod)) # odds ratios
exp(cbind(OR = coef(polrMod), ci)) # OR e intervalos de confiança
#"fixed.acidity" e "pH" também saem do modelo por terem p-valor de 0.73 e 0.11 (>5%)
# respectivamente, e por terem o valor 1 no intervalo de confiança do odds ratio

polrMod = polr(quality ~ 
                 type
               #+ fixed.acidity
               + volatile.acidity
               + citric.acid
               + residual.sugar
               + chlorides
               + free.sulfur.dioxide
               #+ total.sulfur.dioxide
               #+ density
               #+ pH
               + sulphates
               + alcohol
               , data = trainingData, Hess=TRUE)
summary(polrMod)
#Residual Deviance: 9837.83 

(ctable = coef(summary(polrMod))) # tabela com coeficientes
p = pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # tabela com p-valores
(ctable <- cbind(ctable, "p value" = p)) # tabela final
(ci = confint(polrMod,na.rm = T)) # intervalo de confiança
exp(coef(polrMod)) # odds ratios
exp(cbind(OR = coef(polrMod), ci)) # OR e intervalos de confiança

predictedQuality = predict(polrMod, testData)  # estima escore de qualidade
head(predictedQuality)

predictedScores = predict(polrMod, testData, type="p")  # probabilidades estimadas
head(predictedScores)

#pred.score=0
#for(i in 1:length(predictedScores[,1])){
#  pred.score[i] = max(predictedScores[i,1],predictedScores[i,2],predictedScores[i,3],predictedScores[i,4],predictedScores[i,5],predictedScores[i,6],predictedScores[i,7])
#}
#log.scores = -log(pred.score)
#plot(pred.score,log.scores,type = "p")

# Matriz de confusão e erro de classificação
conf.matrix = table(testData$quality, predictedQuality)  # confusion matrix
acuracia = sum(diag(conf.matrix))/sum(conf.matrix)
acuracia
#[1] 0.5394872

#############################################################################
##Criação da árvore - modelo com todas as variáveis
arvore = rpart(quality~.,data=trainingData, method = "class")
arvore

#plotar a visualização da árvore
plot(arvore, uniform=T, branch=0.8, compress=T, margin=0.1)
text(arvore, digits=3, cex=0.65, pretty=0, all=T, use.n=T, fancy=T)

## Aplica o modelo obtido para fazer previsoes 
trainingData.previsao = predict(arvore,trainingData, type= "class")

# Mostra as previsões
head(trainingData)

# estima probabilidades na base de teste
treepredictedScores = predict(arvore, testData, type="p") 
head(treepredictedScores)

# estima as classes diretamente na base de teste
treepredictedQuality = predict(arvore, testData, type = "class")
head(treepredictedQuality)

# Matriz de confusao e erro
conf.matrix = table(testData$quality, treepredictedQuality)  # confusion matrix
acuracia = sum(diag(conf.matrix))/sum(conf.matrix)
acuracia
#[1] 0.514359

#############################################################################
#random forest
#Fit Random Forest Model
trainingData$type = factor(trainingData$type)
rf = randomForest(quality ~ .,  
                  ntree = 100,
                  data = trainingData)
plot(rf)
rf$confusion

testData$type = factor(testData$type)

rfpredictedQuality = predict(rf, testData)

# Matriz de confusao e erro
conf.matrix = table(testData$quality, rfpredictedQuality)  # confusion matrix
acuracia = sum(diag(conf.matrix))/sum(conf.matrix)
acuracia
#[1] 0.6651282
# dentre os três modelos aplicados o random forest apresentou o melhor resultado
# com acurácia de 66,51%.

##################################################
########################### MODELAGEM SEM OUTLIERS
##################################################
# Prepara dados de treino e validação
wqdata.model = subset(wqdata.4,select = c(1:13))
set.seed(1991)
trainingRows = sample(1:nrow(wqdata.model), 0.7 * nrow(wqdata.model))
trainingData = wqdata.model[trainingRows, ]
testData = wqdata.model[-trainingRows, ]
dim(trainingData)
dim(testData)

####### Validando amostra:
training.type=table(trainingData$type)
test.type=table(testData$type)

#teste qui-quadrado para variável type
chisq.test(rbind(training.type,test.type))

#testes Wilcoxon para demais variáveis
wilcox.test(trainingData$quality,testData$quality) 
wilcox.test(trainingData$fixed.acidity,testData$fixed.acidity)
wilcox.test(trainingData$volatile.acidity,testData$volatile.acidity)
wilcox.test(trainingData$citric.acid,testData$citric.acid)
wilcox.test(trainingData$residual.sugar,testData$residual.sugar)
wilcox.test(trainingData$chlorides,testData$chlorides)
wilcox.test(trainingData$free.sulfur.dioxide,testData$free.sulfur.dioxide)
wilcox.test(trainingData$total.sulfur.dioxide,testData$total.sulfur.dioxide)
wilcox.test(trainingData$density,testData$density)
wilcox.test(trainingData$pH,testData$pH)
wilcox.test(trainingData$sulphates,testData$sulphates)
wilcox.test(trainingData$alcohol,testData$alcohol)

######## todos resultados tem p-valor > 5%, assim pode-se concluir que não há evidência contra a consistência da amostra.

# Cria modelo de regressão logística ordinal
trainingData$quality = factor(trainingData$quality,ordered = T)
str(trainingData)
options(contrasts = c("contr.treatment", "contr.poly"))

View(cor(trainingData[,c(2:12)]))
# Antes de rodar o modelo, ainda percebe-se alta correlação entre as variáveis "free.sulfur.dioxide" e 
# "total.sulfur.dioxide" (0.71), o que nos leva a escolher apenas uma para evitar multicolineariedade.

polrMod = polr(quality ~ 
                 type + fixed.acidity
               + volatile.acidity
               + citric.acid
               + residual.sugar
               + chlorides
               #+ free.sulfur.dioxide
               + total.sulfur.dioxide
               + density
               + pH
               + sulphates
               + alcohol
               , data = trainingData, Hess=TRUE)
summary(polrMod)
#Residual Deviance: 8355.58

polrMod = polr(quality ~ 
                 type + fixed.acidity
               + volatile.acidity
               + citric.acid
               + residual.sugar
               + chlorides
               + free.sulfur.dioxide
               #+ total.sulfur.dioxide
               + density
               + pH
               + sulphates
               + alcohol
               , data = trainingData, Hess=TRUE)
summary(polrMod)
#Residual Deviance: 8335.531 
# O modelo sem "total.sulfur.dioxide" apresentou menor Residual Deviance (8666 contra 8694 do modelo anterior).
# Percebe-se também resultado semelhante ao modelo anterior na variável "density".

polrMod = polr(quality ~ 
                 type + fixed.acidity
               + volatile.acidity
               + citric.acid
               + residual.sugar
               + chlorides
               + free.sulfur.dioxide
               #+ total.sulfur.dioxide
               #+ density
               + pH
               + sulphates
               + alcohol
               , data = trainingData, Hess=TRUE)
summary(polrMod)
#Residual Deviance: 8347.375

# estatísticas do modelo
summary(polrMod)

(ctable = coef(summary(polrMod))) # tabela com coeficientes
p = pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # tabela com p-valores
(ctable <- cbind(ctable, "p value" = p)) # tabela final
(ci = confint(polrMod,na.rm = T)) # intervalo de confiança
exp(coef(polrMod)) # odds ratios
exp(cbind(OR = coef(polrMod), ci)) # OR e intervalos de confiança
#"fixed.acidity" e "citric.acid" também saem do modelo por terem p-valor de 0.30 e 0.059 (>5%)
# respectivamente, e por terem o valor 1 no intervalo de confiança do odds ratio

polrMod = polr(quality ~ 
                 type
               #+ fixed.acidity
               + volatile.acidity
               #+ citric.acid
               + residual.sugar
               + chlorides
               + free.sulfur.dioxide
               #+ total.sulfur.dioxide
               #+ density
               + pH
               + sulphates
               + alcohol
               , data = trainingData, Hess=TRUE)
summary(polrMod)
#Residual Deviance: 8352.797 

(ctable = coef(summary(polrMod))) # tabela com coeficientes
p = pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # tabela com p-valores
(ctable <- cbind(ctable, "p value" = p)) # tabela final
(ci = confint(polrMod,na.rm = T)) # intervalo de confiança
exp(coef(polrMod)) # odds ratios
exp(cbind(OR = coef(polrMod), ci)) # OR e intervalos de confiança

# estima escore de qualidade na base de teste
predictedQuality = predict(polrMod, testData)
head(predictedQuality)

predictedScores = predict(polrMod, testData, type="p")  # probabilidades estimadas
head(predictedScores)

# Matriz de confusão e erro de classificação
conf.matrix = table(testData$quality, predictedQuality)  # confusion matrix
acuracia = sum(diag(conf.matrix))/sum(conf.matrix)
acuracia
#[1] 0.5339806

#############################################################################
##Criação da árvore - modelo com todas as variáveis
arvore = rpart(quality~.,data=trainingData, method = "class")
arvore

#plotar a visualização da árvore
plot(arvore, uniform=T, branch=0.8, compress=T, margin=0.1)
text(arvore, digits=3, cex=0.65, pretty=0, all=T, use.n=T, fancy=T)

## Aplica o modelo obtido para fazer previsoes 
trainingData.previsao = predict(arvore,trainingData, type= "class")

# Mostra as previsões
head(trainingData)

# estima probabilidades na base de teste
treepredictedScores = predict(arvore, testData, type="p") 
head(treepredictedScores)

# estima as classes diretamente na base de teste
treepredictedQuality = predict(arvore, testData, type = "class")
head(treepredictedQuality)

# Matriz de confusao e erro
conf.matrix = table(testData$quality, treepredictedQuality)  # confusion matrix
acuracia = sum(diag(conf.matrix))/sum(conf.matrix)
acuracia
#[1] 0.5345874

#############################################################################
#random forest
#Fit Random Forest Model
trainingData$type = factor(trainingData$type)
rf = randomForest(quality ~ .,  
                  ntree = 100,
                  data = trainingData)
plot(rf)
rf$confusion

testData$type = factor(testData$type)

# aplica modelo na base de teste
rfpredictedQuality = predict(rf, testData)

# Matriz de confusao e erro
conf.matrix = table(testData$quality, rfpredictedQuality)  # confusion matrix
acuracia = sum(diag(conf.matrix))/sum(conf.matrix)
acuracia
# [1] 0.660801

# Os resultados mostram que a presença dos outliers não impactaram nas
# estimativas da modelagem porque a acurácia não sofreu grande alteração.

##############################################################################
# Uma última abordagem seria reclassificar a variável resposta, visto que os scores
# 3 e 4 são eventos raros, assim como 8 e 9.
wqdata.new = wqdata.3[,c(1:13)]
head(wqdata.new)
round((table(wqdata.new$quality)/sum(table(wqdata.new$quality)))*100,2)
# O que torna difícil para o modelo capturar.
# Dessa forma, podemos fazer:
# score anterior  novo score
# 3,4,5         = 5           (vinho ruim/médio)
# 6             = 6           (vinho bom)
# 7,8,9         = 7           (vinho muito bom)

wqdata.new$quality2=wqdata.new$quality

wqdata.new$quality=0

for(i in 1:length(wqdata.new$quality)){
  if (wqdata.new[i,14] <= 5){
    wqdata.new[i,13] = 5 } 
  else if (wqdata.new[i,14] <= 6) {
        wqdata.new[i,13] = 6} else {
          wqdata.new[i,13] = 7
        }
}
wqdata.new = wqdata.new[,c(1:13)]
round((table(wqdata.new$quality)/sum(table(wqdata.new$quality)))*100,2)

#random forest
#Fit Random Forest Model
set.seed(1991)
trainingRows = sample(1:nrow(wqdata.new), 0.7 * nrow(wqdata.new))
trainingData = wqdata.new[trainingRows, ]
testData = wqdata.new[-trainingRows, ]
dim(trainingData)
dim(testData)

trainingData$type = factor(trainingData$type)
trainingData$quality = factor(trainingData$quality,ordered = T)
rf = randomForest(quality ~ .,  
                  ntree = 100,
                  data = trainingData)
plot(rf)
rf$confusion

testData$type = factor(testData$type)

# aplica modelo na base de teste
rfpredictedQuality = predict(rf, testData)

# Matriz de confusao e erro
conf.matrix = table(testData$quality, rfpredictedQuality)  # confusion matrix
acuracia = sum(diag(conf.matrix))/sum(conf.matrix)
acuracia
# [1] 0.714359
# Obtemos assim uma acurácia de 71,43%
