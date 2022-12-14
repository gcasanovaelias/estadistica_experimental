###----CORRELACI?N---####
cor.test(data$gs,data$pn)
cor(data$gs,data$pn)

###----REGRESI?N LINEAL SIMPLE---####
#Importaci?n de datos
data=read.delim("clipboard");data

##Gr?ficos de dispersi?n
pairs(data)#primera aproximaci?n, vd y vi no definidas
plot(lm)#varios gr?ficos generales
plot(data$pn~data$gs)

##Modelo de Regresi?n Lineal Simple
lm=lm(data$pn~data$gs);lm
summary(lm)
  #Valores importantes
coef(lm)
fitted.values(lm)

##Residuos
lm$residuals
residuals(lm)
#Residuos en el Gr?fico de dispersi?n
segments(data$gs,data$pn,data$gs,fitted(lm), lty =3)
plot(data$gs,lm$residuals)
abline(h=0)

##ANOVA
aov=aov(data$pn~data$gs);aov
anova(lm)

##Relaci?n funcional graficada
curve(1.28909+0.06728*x, 
      add=T,
      col='red')
abline(lm,col='blue')

##Intervalos de confianza
  ##Valores de IC
confint(lm,level = 0.99)
ci=confint.lm(lm,level = 0.99);ci
  #Graficar intervalos de confianza
ic=predict(lm,interval='confidence',level = 0.99);ic
lines(data$gs, ic[, 2], lty = 2,col='lightblue')
lines(data$gs,ic[,3],lty=2,col='lightblue')
lines(nuevas.edades$edad, ic[, 3], lty = 2)

##Intervalos de predicci?n
  #Graficar intervalos de prediccion
ip=predict(lm, interval = 'prediction');ip
lines(data$gs, ip[, 2], lty = 2, col = 'red')
lines(data$gs, ip[, 3], lty = 2, col = 'red')

##Valores outliers
  #Influencia en los par?metros
dcook=cooks.distance(lm);dcook
plot(dcook~seq(1,93))#n (case): n?mero de la observaci?n, de 1 a 93.

  #Influencia en los predichos
leverages=hatvalues(lm)
plot(leverages~seq(1,93))
h=2*2/93
abline(h=h,col='red')

####-----SUPUESTOS-----#####
##Normalidad de los residuos
#Forma gr?fica
qqnorm(lm$residuals) #lm$res ? residuals(lm)
qqline(lm$residuals)
#Forma test estad?stico
shapiro.test(lm$residuals) ## HO: distribuci?n normal

##Homocedasticidad de las varianzas
#Forma gr?fica
plot(lm$fitted.values,lm$residuals) #fitted values: valores predichos por el modelo. (lm$fit,lm$res)
abline(h=2)
