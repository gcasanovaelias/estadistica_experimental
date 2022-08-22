#Packages----
library(nlme)
library(lsmeans)
library(multcompView)
library(multcomp)
library(car)
library(carData)
#Pregunta 1----
#Datos----
Control=c(2.43,2.63,2.56,2.76,2.70,2.54)
A=c(3.00,3.02,2.87,2.96,2.77,2.75)
E=c(2.74,2.88,2.42,2.73,2.83,2.66)
F.=c(2.74,2.88,2.85,3.02,2.85,2.66)
I=c(3.03,2.81,3.06,3.11,2.94,3.06)

estiercol=c(Control,A,E,F.,I);estiercol

Trat=c(rep('Control',6),
rep('A',6),
rep('E',6),
rep('F',6),
rep('I',6))

datos=data.frame(Trat,estiercol)

#Estadística descriptiva----
summary.data.frame(datos)
Control=datos$estiercol[datos$Trat=='Control'];Control
A=datos$estiercol[datos$Trat=='A'];A
E=datos$estiercol[datos$Trat=='E'];E

summary(Control)
summary(A)

#Modelo de clasificación----
cm=gls(estiercol~1+Trat,method = 'REML',na.action = na.omit,data=datos)
summary(cm)
anova(cm) #Pruebas de hipótesis marginales (SC tipo III)
#*IMPORTANTE: debe rechazarse la H0 en este test anova para que podamos aplicar PCA (pruebas de comparaciones múltiples)
cm$sigma
lsmeans(cm,~Trat) #para calcular las medias

#Comparaciones múltiples entre las medias----
lsmeans(cm,~Trat) %>% cld()
  #Se realiza un test de Tukey y no uno de Fisher
  #Esta sí sirve para modelos complejos (heterocedasticidad), existen paquetes con funciones que permiten calcular LSD Fisher pero que no funcionan bien con modelos complejos
#Supuestos del modelo----
#Normalidad de los residuos
#Forma gráfica
qqnorm(cm$residuals)#cm$res ó residuals(cm)
qqline(cm$res)
qqPlot(cm,line='none',quadratic=F)

#Forma test estadístico
shapiro.test(cm$residuals) ## HO: distribución normal

#Homocedasticidad de las varianzas
#Forma gráfica
plot(cm$fit,cm$res)
abline(h=0)
plot(cm)

boxplot(cm$residuals~Trat,data=datos)

#Pregunta 2----
a=c(11.2,11.6,12,16.5,16.8,16.1,18.3,18.7,19);a
b=c(14.1,13.8,14.2,19,18.5,18.2,11.9,12.4,12);b
c=c(15.3,15.9,16,19.5,20.1,19.3,16.5,17.2,19.9);c
d=c(7.3,7.8,7,8.9,9.4,9.3,11.2,10.9,10.5);d

estanque=rep(c(1:12),3) %>% sort()

dieta=rep(c('a','b','c','d'),9) %>% sort()

datos2=data.frame(dieta,estanque,grasa=c(a,b,c,d))
#Modelo de clasificación----
cm2=gls(grasa~1+dieta,method = 'REML',na.action = na.omit,data=datos2)
summary(cm)
anova(cm2) #Pruebas de hipótesis marginales (SC tipo III)
  #*IMPORTANTE: debe rechazarse la H0 en este test anova para que podamos aplicar PCA (pruebas de comparaciones múltiples)
cm2$sigma
lsmeans(cm2,~dieta)#para calcular las medias
#Comparaciones múltiples entre las medias----
lsmeans(cm2,~dieta) %>% cld()

#Supuestos del modelo----
par(mfrow=c(2,2))
#Normalidad de residuos
qqnorm(cm2$res);qqline(cm2$res)

#Homocedasticidad de varianza
plot(cm2$fit,cm2$res);abline(h=0)
boxplot(cm2$res~dieta,data=datos2)
plot(cm2)

#Alternativa al análisis de estos datos----
grasa2=c(datos2$grasa[datos2$estanque==1] %>% mean() %>% round(2),
datos2$grasa[datos2$estanque==2] %>% mean() %>% round(2),
datos2$grasa[datos2$estanque==3] %>% mean() %>% round(2),
datos2$grasa[datos2$estanque==4] %>% mean() %>% round(2),
datos2$grasa[datos2$estanque==5] %>% mean() %>% round(2),
datos2$grasa[datos2$estanque==6] %>% mean() %>% round(2),
datos2$grasa[datos2$estanque==7] %>% mean() %>% round(2),
datos2$grasa[datos2$estanque==8] %>% mean() %>% round(2),
datos2$grasa[datos2$estanque==9] %>% mean() %>% round(2),
datos2$grasa[datos2$estanque==10] %>% mean() %>% round(2),
datos2$grasa[datos2$estanque==11] %>% mean() %>% round(2),
datos2$grasa[datos2$estanque==12] %>% mean() %>% round(2))

dieta2=rep(c('a','b','c','d'),3) %>% sort()

datos2.1=data.frame(dieta2,grasa2)

#Modelo de clasificación
cm2.1=gls(grasa2~1+dieta2,method = 'REML',na.action = na.omit,data=datos2.1)
summary(cm2.1)
anova(cm2.1)
lsmeans(cm2.1,~dieta2) %>% cld()

#Supuestos
#Normalidad de residuos
qqnorm(cm2.1$res);qqline(cm2.1$res)

#Homocedasticidad de varianza
plot(cm2.1$fit,cm2.1$res);abline(h=0)
boxplot(cm2.1$res~dieta2,data=datos2.1)
plot(cm2.1)

#Ajuste heterocedástico según estrategia de agrupamieto----
datos2.1
grupo=rep(c('a','b'),6) %>% sort()
  #grupo=rep(c('a'),6) %>% c(rep('c',3)) %>% c(rep('d',3)) %>% sort();grupo

datos2.2=bind_cols(datos2.1,'grupo'=grupo)

mhet=gls(grasa2~1,
         weights = varComb(varIdent(form = ~1|grupo)),#describe la estructura heterocedástica del grupo
         method = 'REML',
         na.action = na.omit,
         data = datos2.2)

boxplot(mhet$res~dieta2,data=datos2.2)

#Prueba de Verosimilitud Restringida (AIC y BIC)----
summary(cm2.1)
summary(mhet)
  #Hubo un incremento de AIC y BIC por lo que el modelo ajustado es poco parsimonioso y no genera mejores predicciones a pesar de estimar más parámetros
