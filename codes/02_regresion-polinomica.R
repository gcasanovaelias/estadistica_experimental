#EJERCICIO 1----
#Creación de datos----
N=c(0,20,40,60,80,120,160)
R=c(24.5,41.5,52.1,61.4,73.5,86,92.8)
length(N);length(R)
datos=data.frame(N,R);datos

#Gráfico de dispersión----
plot(datos$R~datos$N)
plot(datos)

#Modelo Regresión Polinómica----
mrp=lm(R~N,datos);mrp
residualPlot(mrp,line=F,quadratic=F)
plot(mrp$fitted.values,mrp$residuals)
summary(mrp)
anova(mrp)

mrp2=lm(R~N+I(N^2),datos);mrp2
residualPlot(mrp2,line=F,quadratic=F)
summary(mrp2)
anova(mrp2)

#Supuestos del modelo----
#Normalidad de los residuos
  #Forma gráfica
qqPlot(mrp2,simulate=F,envelope=F,id=F)
qqnorm(mrp2$residuals)
qqline(mrp2$residuals)
  #Forma test estadístico
shapiro.test(mrp2$residuals)

#Homocedasticidad de la varianza
  #Forma gráfica
plot(mrp2$fitted.values,mrp2$residuals)
residualPlot(mrp2,linear=F,quadratic=F)

#Función----
Rend=function(x)(25.363958+0.756350*x-0.002092*x*x)
Rend(75)
Rend(160)

#Gráfico----
plot(datos)
curve(25.363958+0.756350*x-0.002092*x^2,
      col='red',
      add=T)

#Comparación de anova's----
anova(mrp)
anova(mrp2)

#EJERCICIO 2----
#Gráficos de dispersión----
cars
plot(cars)
head(cars)
#Modelo de regresión ¿rectilinea o curvilinea?----
rlp_cars=lm(dist~speed,cars);rlp_cars;summary(rlp_cars)
residualPlot(rlp_cars,lin=F,quadratic=F)
plot(rlp_cars$fitted.values,rlp_cars$residuals)

rlp_cars2=lm(dist~speed+I(speed^2),cars);rlp_cars2;summary(rlp_cars2)
plot(cars)
curve(2.47014+0.91329*x+0.09996*x*x,
      add=T,
      col="red")
  #*El modelo no es polinomico
