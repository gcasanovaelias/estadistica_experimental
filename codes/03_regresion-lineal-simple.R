#Packages----
library(car)
library(carData)
#Regresión Lineal Múltiple----
data=read.delim("clipboard");data
plot(data)
rlm=lm(Y~Tiempo+Temp+Nut,data=data);rlm;summary(rlm)
anova(rlm)

#Residuos Parciales----
library(car)
crPlots(rlm,line=F,smooth=F)
  #*Component+Residual (partial residuals) plots

#Correcciones----
rlm2=lm(Y~Tiempo+Temp+I(Temp^2)+Nut,data=data);rlm2;summary(rlm2)
crPlots(rlm2,line=F,smooth=F)
anova(rlm2)

#Funcion----
Cbact=function(Tiempo,Temp,Nut)(-1087.2012 + 2.8855*Tiempo + 56.9262*Temp -0.7736*Temp^2+96.6759*Nut)
Cbact(33,28.8,0.9)

Cbact(0,45,0)
#Supuestos del modelo----
#Normalidad de los residuos
  #Forma gráfica
qqnorm(rlm2$residuals)#lm$res ó residuals(lm)
qqline(rlm2$residuals)
qqPlot(rlm2,line="none",quadratic=F)

  #Forma test estadístico
shapiro.test(rlm2$residuals) ## HO: distribución normal

#Homocedasticidad de las varianzas
  #Forma gráfica
plot(rlm2$fitted.values,rlm2$residuals)#fitted values: valores predichos por el modelo. (lm$fit,lm$res)
abline(h=2)

pairs(data)
plot(rlm2,which = c(1,2,3))

residualPlot(rlm2,linear=F,quadratic=F,grid=F)
residualPlots(rlm2,linear=F,quadratic=F)

#Valores outliers----
#Influencia en los parámetros----
dcook=cooks.distance(rlm2);dcook
plot(dcook~seq(1,33))#n (case): número de la observación, de 1 a 93.

#Influencia en los predichos----
leverages=hatvalues(rlm2);leverages
plot(leverages~seq(1,33))
h=2*6/33;h #6 (b0,b1,b2,b3,b4 y b5)
abline(h=h,col='red')

#Relación SCE----
15559+23980+46392-(22612+63319) #0
  #* la SCT se mantiene igual para el modelo 1 y 2

a=-1087.2012+2.8855+56.9262+96.6759
b=0.7736*2
a/b

56.93/0.77
