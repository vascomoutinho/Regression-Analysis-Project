#Ler base de dados
setwd("C:/Users/utilizador/Desktop")
homesales <- read.csv("BatonRouge_HomeSales_data.csv",header=TRUE, sep=",",dec=".")


##### Ver se as variaveis estão a ser bem lidas:
str(homesales)
head(homesales)
summary(homesales)

##### Ver se existem NA
is.na(homesales)
sum(is.na(homesales))

#alterar colunas:
install.packages("dplyr")
library(dplyr)

##estilo:
homesales$style <- as.numeric(as.character(homesales$style))
style <- data.frame(
  style = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
  style_name = c("Traditional", "Townhouse", "Ranch", "New Orleans", "Mobile Home",
                 "Garden", "French", "Cottage", "Contemporary", "Colonial", "Acadian")
)

homesales <- homesales %>%
  left_join(style, by = "style") %>%
  mutate(style = ifelse(is.na(style_name), "Unknown", style_name))

#Piscina
homesales$pool <- ifelse(homesales$pool == 1, "yes", "no")

#Lareira
homesales$fireplace <- ifelse(homesales$fireplace == 1, "yes", "no")

#Beira-mar
homesales$waterfront <- ifelse(homesales$waterfront == 1, "yes", "no")

#Ocupação
# Convert occupancy column to numeric
homesales$occupancy <- as.numeric(as.character(homesales$occupancy))

occupancy <- data.frame(
  occupancy = c(1, 2, 3),
  occupancy_label = c("Owner", "Vacant", "Tenant")
)

homesales <- homesales %>%
  left_join(occupancy, by = "occupancy") %>%
  mutate(occupancy = ifelse(is.na(occupancy_label), "Unknown", occupancy_label))

#Retirar colunas repetidas
library(dplyr)

homesales <- select(homesales, -c(12, 13))

#correção das variáveis:
homesales$occupancy<-as.factor(homesales$occupancy)
homesales$pool<-as.factor(homesales$pool)
homesales$style<-as.factor(homesales$style)
homesales$fireplace<-as.factor(homesales$fireplace)
homesales$waterfront<-as.factor(homesales$waterfront)

str(homesales)
summary(homesales)

#ordenar por "integer" e "factor"
homesales<-homesales[,c(1,2,3,4,5,11,6,7,8,9,10)]
colnames(homesales)
str(homesales)

#Análise Descritiva

#Médias e Desvio Padrão
mean <- unlist(lapply(homesales[,c(1:6)], mean))
sd <- unlist(lapply(homesales[,c(1:6)], sd))
median <- unlist(lapply(homesales[,c(1:6)], median))
mean
sd
median

#Minimos e Máximos
max <- unlist(lapply(homesales[,c(1:6)], min))
min <- unlist(lapply(homesales[,c(1:6)], max))
max
min

#amplitude interquartis:
quantile(homesales$price,0.25)
quantile(homesales$price,0.75)
quantile(homesales$price,0.75) - quantile(homesales$price,0.25)

apply(homesales[,c(1:6)],2, quantile)

IQR = function(x)
{
  quantile(x,0.75)-quantile(x,0.25)
}
apply(homesales[,c(1:6)],2, IQR)
cor(homesales[,c(1:6)])

#coeficiente de variação:
CV <- function(x){sd(x)/mean(x)}
apply(homesales[,c(1:6)],2,CV)

#Plots para cada variável
plot(homesales$price,main="Plot do Preço",ylab="Preço")
plot(homesales$sqft,main="Plot de Sqft",ylab="SQFT")
plot(homesales$bedrooms,main="Plot do Número de Quartos",ylab="Número de Quartos")
plot(homesales$baths,main="Plot do Número de WC",ylab="Número de WC")
plot(homesales$age, main = "Plot de Idade da Casa",ylab="Idade da Casa")
plot(homesales$dom,main = "Plot de Dias no Mercado", ylab="Dias no Mercado")
plot(homesales$occupancy,main="Plot de Ocupação",ylab="Ocupação")
plot(homesales$pool,main="Plot de Piscina",ylab="Piscina")
plot(homesales$style, main="Plot de Estilo",ylab="Estilo")
plot(homesales$fireplace, main="Plot de Lareira",ylab="Lareira")
plot(homesales$waterfront,main="Plot Beira-Mar",ylab="Beira-Mar")

#Hsitograma para o preço
hist(homesales$price,xlab="Preço",ylab="Frequencia",main="Histograma do Preço")

#Boxplot
boxplot(homesales$price,main="BoxPlot Preço")
boxplot(homesales$sqft,main="BoxPlot SQFT")
boxplot(homesales$bedrooms,main="BoxPlot Número de Quartos")
boxplot(homesales$age,main="BoxPlot Idade")
boxplot(homesales$dom,main="BoxPlot Dias no Mercado")

#Boxplot Relacional
boxplot(homesales$price ~ homesales$style, main ='BoxPlot de Relação entre Preço e Estilo', col = "steelblue",xlab ="Estilo",ylab="Preço")
boxplot(homesales$price ~ homesales$bedrooms, main ='BoxPlot de Relação entre Preço e Quartos', col = "steelblue",xlab ="Quartos",ylab="Preço")
boxplot(homesales$price ~ homesales$baths, main ='BoxPlot de Relação entre Preço e WC', col = "steelblue",xlab ="WC",ylab="Preço")
boxplot(homesales$price ~ homesales$age, main ='BoxPlot de Relação entre Preço e Idade', col = "steelblue",xlab ="Idade",ylab="Preço")
boxplot(homesales$price ~ homesales$pool, main ='BoxPlot de Relação entre Preço e Piscina', col = "steelblue",xlab ="Piscina",ylab="Preço")
boxplot(homesales$price ~ homesales$fireplace, main ='BoxPlot de Relação entre Preço e Lareira', col = "steelblue",xlab ="Lareira",ylab="Preço")
boxplot(homesales$price ~ homesales$waterfront, main ='BoxPlot de Relação entre Preço e Beira-Mar', col = "steelblue",xlab ="Beira-Mar",ylab="Preço")


#Pairs
pairs(homesales[, c("price", "sqft", "bedrooms", "baths", "age", "dom")])

# Encontrar os outliers
boxplot.stats(homesales$price)$out
Outliers <- sapply(homesales$price,is.element,set=boxplot.stats(homesales$price)$out)
summary(homesales[!Outliers,])
#uma vez que não são assim tão significativos, não existe necessidade de os eliminar

#Plot para analisar cada observação numérica
num_cols<-unlist(lapply(homesales,is.numeric))
plot(homesales[,num_cols])

#ScatterPlotMatrix
install.packages("car")
library(car)
scatterplotMatrix(homesales[,c(1:6)])
scatterplotMatrix(homesales[,c(1:6)],diagonal=list(method="histogram"),smooth = FALSE)
scatterplotMatrix(homesales[,c(1:6)],diagonal=list(method="boxplot"),smooth = FALSE)


##Correlações:
cor(homesales$price,homesales$sqft)
cor(homesales$price,homesales$bedrooms)
cor(homesales$price,homesales$baths)
cor(homesales$price,homesales$age)
cor(homesales$price,homesales$dom)

library(corrplot)
corrplot(cor(homesales[,c(1:6)]),order="AOE")

#Tabela de frequências para a variável dependente
table(homesales$price)

#Qplot
library(ggplot2)
g1<-qplot(homesales$price,homesales$sqft,data=homesales,main="Preço vs SQFT", xlab="Preço",ylab="SQFT")
g1 + theme(plot.title=element_text(hjust=0.5))+stat_smooth(method=lm)

g2<-qplot(homesales$price,homesales$bedrooms,data=homesales,main="Preço vs Número de Quartos",xlab = "Preço",ylab="Quartos")
g2 + theme(plot.title=element_text(hjust=0.5))+stat_smooth(method=lm)

g3<-qplot(homesales$price,homesales$baths,data=homesales,main="Preço vs Número de WC",xlab = "Preço",ylab="WC")
g3 + theme(plot.title=element_text(hjust=0.5))+stat_smooth(method=lm)

g4<-qplot(homesales$price,homesales$age,data=homesales,main="Preço vs Idade",xlab = "Preço",ylab="Idade")
g4 + theme(plot.title=element_text(hjust=0.5))+stat_smooth(method=lm)

g5<-qplot(homesales$price,homesales$pool,data=homesales,main="Preço vs Piscina",xlab = "Preço",ylab="Piscina")
g5 + theme(plot.title=element_text(hjust=0.5))

g6<-qplot(homesales$price,homesales$style,data=homesales,main="Preço vs Estilo",xlab = "Preço",ylab="Estilo")
g6 + theme(plot.title=element_text(hjust=0.5))

g7<-qplot(homesales$price,homesales$fireplace,data=homesales,main="Preço vs Lareira",xlab = "Preço",ylab="Lareira")
g7 + theme(plot.title=element_text(hjust=0.5))

g8<-qplot(homesales$price,homesales$waterfront,data=homesales,main="Preço vs Beira-Mar",xlab = "Preço",ylab="Beira-Mar")
g8 + theme(plot.title=element_text(hjust=0.5))

g9<-qplot(homesales$price,homesales$occupancy,data=homesales,main="Preço vs Ocupação",xlab = "Preço",ylab="Ocupação")
g9 + theme(plot.title=element_text(hjust=0.5))


######################Selecao do modelo de regressao####################
#MODELO 1
Mod1res <- lm(homesales$price ~ .,data=homesales)
summary(Mod1res)
#Multiple R-squared:  0.658,	Adjusted R-squared:  0.6518 
#65,8% da variância da variável dependente (vendas$preço) é explicada pelas variáveis independentes incluídas no modelo
# valor de R quadrado ajustado: responsável pelo número de variáveis independentes incluídas no modelo. É ajustado para penalizar a inclusão de variáveis desnecessárias no modelo, que podem levar ao overfitting.

Mod2res <- update(Mod1res, ~. - occupancy)
summary(Mod2res)
#Multiple R-squared:  0.6567,	Adjusted R-squared:  0.6512

Mod3res<-step(Mod2res)
summary(Mod3res)
#Multiple R-squared:  0.6564,	Adjusted R-squared:  0.6515 

Mod4res<-update(Mod3res, ~. - dom)
summary(Mod4res)
#Multiple R-squared:  0.6554,	Adjusted R-squared:  0.6509 


#assim, todas as variaveis que temos no modelo passam a ser significativas

AIC(Mod1res)
#27260.77
AIC(Mod2res)
#27260.8
AIC(Mod3res)
#27257.86
AIC(Mod4res)
#27258.74


#como o melhor modelo é o que apresenta menor valor AIC, vamos optar pelo modelo 3
#um valor de AIC mais baixo indica um melhor ajuste, pois significa que o modelo tem um melhor equilíbrio entre a qualidade do ajuste e o número de parâmetros usados.

###################Para a normalidade#############################
###vamos realizar um teste de hipotese
#H0: modelo reduzido satisfatorio face ao mais completo
#H1: modelo reduzido nao satisfatorio face ao mais completo

anova(Mod3res, Mod1res)

#pela analise, verificamos que a direita de 1.251 temos probabilidade de 0.2876
#concluimos, assim, que o modelo 3 e satisfatorio, pela probabilidade ser maior que o nivel de significancia (por defeito 5%)
#Continuamos, assim, com análise com Mod3res



############Multicolinearidade###############
#Analise de Correlacao entre variaveis (tendo em conta as variaveis do modelo 3):

cor(homesales[,c(2,3,4,5,6)])

library(GGally)
GGally::ggpairs(homesales[,c(2,3,4,5,6)])
pairs(homesales[,c(2,3,4,5,6)])

library(car)
vif(Mod3res)

##########################Autocorrelacao############################
#visto nao se tratar de uma base de dados de serie temporal, nao faz sentido analisar a autocorrelacao

##########################heteroelastecidade############################
library(car)
residualPlots(Mod3res)

#Breush-Pagan Test
#H0: modelo homocedastico
#H1: modelo heterocedastico
library(lmtest)
bptest(Mod3res)
#p value, aproximadamente, zero, pelo que o teste e inconclusivo (H0 e rejeitada)
sum(!complete.cases(homesales$style))
#Como se rejeita H0 de BP teste, entao existe heterocedasticidade, pelo que a temos de corrigir

#vamos manter OLS estimantivas, mas substituir erros padrao classicos por erros padrao robustos
library(sandwich)
attach(homesales)
EBOLres<-lm(price ~ sqft + bedrooms + baths + age + dom + style + waterfront, data = homesales)
EBOLres$robVCOV<-vcovHC(Mod3res)

#vou querer ter erros padroes robustos
library(lmtest)
robust_model <- coeftest(Mod3res,Mod3res$robVCOV)
robust_model
summary(robust_model)

################ analise de influencia##########################
#graficos de influencia e medidas de influencia
install.packages("car")
library(car)

influenceIndexPlot(Mod1res)
#pelos grafico podemos perceber que existem alguns ouliers
InfMreg1<-influence.measures(Mod1res)
print(InfMreg1)
#aqui podemos analisar a inflencia

influenceIndexPlot(Mod3res)
InfMreg3<-influence.measures(Mod3res)
print(InfMreg3)

outlierTest(Mod1res)
outlierTest(Mod3res)
#temos alguns outliers

qqPlot(Mod3res)
#compara os residuos esperados com os obtidos

#####estimacao do modelo de regressao#################
ysquare <- fitted(EBOLres)
ysquare
summary(ysquare)

plot(homesales$price,ysquare)

#prever o modelo com base no intervalo de confianca
Predict <- predict(EBOLres,interval = "confidence")

Mod.prediction <- predict(EBOLres,interval = "prediction")

head(Predict)
head(Mod.prediction)


#------------------------------------------------------------------------------#
#Gráficos que podem ser utilizados
#
ggplot(homesales, aes(x = style, y = price, fill = cut(age, breaks = c(0, 10, 20, 30, Inf)))) +
  geom_boxplot() +
  labs(title = "Price by Style and Age",
       x = "Style",
       y = "Price",
       fill = "Age") +
  scale_fill_discrete(name = "Age Group",
                      labels = c("0-10", "10-20", "20-30", "30+"))

ggplot(homesales, aes(x = age, y = price, color = style)) +
  geom_point() +
  labs(title = "Price by Style and Age",
       x = "Age",
       y = "Price",
       color = "Style")

ggplot(homesales, aes(x = dom, y = price)) +
  geom_point() +
  labs(title = "Price vs Days on Market",
       x = "Days on Market",
       y = "Price")

ggplot(homesales, aes(x = dom, y = price, color = style)) +
  geom_point() +
  labs(title = "Price vs Days on Market by Style",
       x = "Days on Market",
       y = "Price",
       color = "Style")

ggplot(homesales, aes(x = dom, color = style)) +
  geom_density() +
  labs(title = "Days on Market by Style",
       x = "Days on Market",
       color = "Style")
