#Packages----
library(corrplot)
#Pregunta 1----
#Datos----
datos_Me=read.delim("clipboard");datos_Me
#Tipo de datos----
typeof(datos_Me$Lago)
#Correlación----
cor(datos_Me[,-1]) #no incluir la variable "Lagos", no es cuantitativa
  #*var() y cov() son funciones que tambien entrgan los resultados en forma matricial
#Correlación como test estadistico----
cor.test(datos_Me$Alcalinidad,datos_Me$Mercurio)
cor.test(datos_Me$pH,datos_Me$Mercurio)
cor.test(datos_Me$Alcalinidad,datos_Me$Calcio)
#Correlograma----
install.packages("corrplot")
library(corrplot)

corrplot(cor(datos_Me[,-1]),
         diag=F,
         type="upper",
         tl.col="black",
         tl.srt=45,
         addCoef.col="white")

install.packages("Hmisc")
library("Hmisc")
mydata.rcorr=rcorr(as.matrix(datos_Me[,-1]));mydata.rcorr

#Pregunta 2----
datos_agri=read.delim("clipboard");datos_agri
typeof(datos_agri$Country)
View(datos_agri)

names(datos_agri)=c("Country","NAO","Popu","A.Lands","Con.ratio","Pro.Livest","Work.stock","Fert.consum","N°tractors")
cor(datos_agri[,-1])

#Matriz de p-values (sthda.com)----
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(datos_agri[,-1]);p.mat
#Correlograma con valores de significancia----
corrplot(cor(datos_agri[,-1]),
         method="color", #tambien queda bien con "number"
         type="upper",
         diag=F,
         p.mat = p.mat,
         sig.level = 0.05,
         tl.col="black",
         tl.srt=45,
         addCoef.col="white",
         insig="blank") #"pch" es el parametro por default y coloca cruces, con pch.color podemos cambiar el color de las cruces
