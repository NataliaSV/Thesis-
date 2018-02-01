#A small p-value (typically = 0.05) --> reject the null hypothesis.
#A large p-value (> 0.05) --> you fail to reject the null hypothesis.
rm(list=ls())
set.seed(50)


#setwd("C:/Users/F15141/Documents/Natalia/Tesis/ultimo/Imagenes")
setwd("C:/Users/Natalia/Documents/CodigosTesis/Ultimo/")

#importaremos los datos
library(zoo)

# mexchem$Date <- as.Date(as.character(mexchem$Date))
# data1<-data.frame(mexchem$Date,mexchem$Close)
# d1<-read.zoo(data1)

# FunciÃ³n para extraer sÃ³lo lo que nos interesa del csv que da yahoo
# yahoo.read <- function(url){
#   dat <- read.table(url,header=TRUE,sep=",")
#   df <- dat[,c(1,5)]
#   df$Date <- as.Date(as.character(df$Date))
#   return(df)}
# 
# # URL's 
# s1_url <- "http://chart.finance.yahoo.com/table.csv?s=BIMBOA.MX&a=0&b=1&c=2012&d=11&e=31&f=2015&g=d&ignore=.csv"
# s2_url <- "http://chart.finance.yahoo.com/table.csv?s=MEXCHEM.MX&a=0&b=1&c=2012&d=11&e=31&f=2015&g=d&ignore=.csv"

s1<-read.table("C:/Users/Natalia/Documents/bimbo.csv", header=TRUE, sep=",")
s2<-read.table("C:/Users/Natalia/Documents/mexchem.csv", header=TRUE, sep=",")

s1<-s1[,c(1,5)]
s1<-as.zoo(s1)
s1[,1] <- as.Date(s1[,1],"%d/%m/%Y")

s2<-s2[,c(1,5)]
s2<-as.zoo(s2)
# Para quedarnos con las fechas en las que ambos tienen datos
datos<-merge.zoo(s1,s2)
ss1<-coredata(datos$s1)
ss2<-coredata(datos$s2)
datos_aux<-data.frame(ss1,ss2)

# Funciona con GFNORTEO.MX
# s1<-read.zoo(yahoo.read(s1_url))
# s2<-read.zoo(yahoo.read(s2_url))

# # Para quedarnos con las fechas en las que ambos tienen datos
# datos<-merge.zoo(s1,s2)
# ss1<-coredata(datos$s1)
# ss2<-coredata(datos$s2)
# datos_aux<-data.frame(ss1,ss2)

# Hacemos una grÃ¡fica de las series con diversos colores
png("series_originales.png")
par(mfrow=c(1,2),oma = c(0, 0, 2, 0))
plot(x = datos, xlab="Fechas",ylab = "Precios", main = "BIMBO vs MEXCHEM",
     col = c("green","blue"), screens = 1)
legend(x = "topright", legend = c("BIMBO", "MEXCHEM"), 
       lty = 1,col = c("green","blue"),cex=0.5)
plot(datos_aux$ss1, datos_aux$ss2,xlab="BIMBO",ylab="MEXCHEM", main="Estructura de dependencia")
mtext("Descripción de las series", outer = TRUE, cex = 1)
dev.off()

png("Precios_estructurada.png")
plot(x = datos, xlab="Fechas",ylab = "Precios", main = "BIMBO vs MEXCHEM",
     col = c("green","blue"), screens = 1)
abline(v=as.Date("2012-12-31"))
abline(v=as.Date("2013-08-10"))
abline(v=as.Date("2014-03-10"))
abline(v=as.Date("2014-09-10"))
abline(v=as.Date("2015-03-20"))
abline(v=as.Date("2015-08-20"))
abline(v=as.Date("2015-1-20"))
dev.off()

#Fragmentamos estructura de dependencia
png("Dependencia_estructurada.png")
par(mfrow=c(2,4),oma = c(0, 0, 2, 0))
plot(datos_aux$ss1[1:261], datos_aux$ss2[1:261],xlab="BIMBO",ylab="MEXCHEM", main="De 02/01/2012 a 31/12/2012")
plot(datos_aux$ss1[262:421], datos_aux$ss2[262:421],xlab="BIMBO",ylab="MEXCHEM", main="De 01/01/2013 a 12/08/2013")
plot(datos_aux$ss1[422:571], datos_aux$ss2[422:571],xlab="BIMBO",ylab="MEXCHEM", main="De 13/08/2013 a 10/03/2014")
plot(datos_aux$ss1[572:703], datos_aux$ss2[572:703],xlab="BIMBO",ylab="MEXCHEM", main="De 11/03/2014 a 10/09/2014")
plot(datos_aux$ss1[704:797], datos_aux$ss2[704:797],xlab="BIMBO",ylab="MEXCHEM", main="De 11/09/2014 a 20/01/2015")
plot(datos_aux$ss1[798:840], datos_aux$ss2[798:840],xlab="BIMBO",ylab="MEXCHEM", main="De 21/01/2015 a 20/03/2015")
plot(datos_aux$ss1[841:949], datos_aux$ss2[841:949],xlab="BIMBO",ylab="MEXCHEM", main="De 21/03/2015 a 20/08/2015")
plot(datos_aux$ss1[950:1044], datos_aux$ss2[950:1044],xlab="BIMBO",ylab="MEXCHEM", main="De 21/08/2015 a 31/12/2015")
mtext("Estructura de dependencia segmentada por fechas", outer = TRUE, cex = 1)
dev.off()

#install.packages("psych")
library(psych) #Para el pairs.panels
c<-c(0,261,421,571,703,797,840,949,1044)

corr = matrix(1,ncol=3,nrow=8)
n<-length(c)-1
for(i in 1:n){
  corr[i,1]=cor(datos_aux$ss1[c[i]:c[i+1]], datos_aux$ss2[c[i]:c[i+1]], method = c("pearson"))
  corr[i,2]=cor(datos_aux$ss1[c[i]:c[i+1]], datos_aux$ss2[c[i]:c[i+1]], method = c("kendall"))
  corr[i,3]=cor(datos_aux$ss1[c[i]:c[i+1]], datos_aux$ss2[c[i]:c[i+1]], method = c("spearman"))
}

png("Medidas_asociacion.png")
par(mfrow=c(1,1))
plot(corr[,1], type="o", col="blue", ylim=c(-1,1),ylab="Correlación",xlab="Período del tiempo")
lines(corr[,2], type="o", pch=22, lty=2, col="red")
lines(corr[,3], type="o", pch=23, lty=3, col="green")
legend(x = "topright", legend = c("Pearson","Kendall","Spearman"), 
       col=c("blue","red","green"), pch=21:23, lty=1:3);
dev.off()

# Hacemos una grÃ¡fica de las series con diversos colores
png("graf1.png")
par(mfrow=c(2,2),oma = c(0, 0, 2, 0))
plot(x = datos$s1, xlab="Fechas",ylab = "Precios", main = "Precios",col = c("black"), screens = 1)
plot(x = log(datos$s1), xlab="Fechas",ylab = "Log Precios", main = "Precios con transformación logarítmica",col = c("black"), screens = 1)
plot(x = diff(datos$s1), xlab="Fechas",ylab = "Diferencias", main = "Diferencia de los precios",col = c("black"), screens = 1)
plot(x = diff(log(datos$s1)), xlab="Fechas",ylab = "Rendimientos", main = "Log-rendimientos",col = c("black"), screens = 1)
mtext("BIMBOA.MX", outer = TRUE, cex = 1)
dev.off()

png("graf2.png")
par(mfrow=c(2,2),oma = c(0, 0, 2, 0))
plot(x = datos$s2, xlab="Fechas",ylab = "Precios", main = "Precios",col = c("black"), screens = 1)
plot(x = log(datos$s2), xlab="Fechas",ylab = "Log Precios", main = "Precios con transformación logarítmica",col = c("black"), screens = 1)
plot(x = diff(datos$s2), xlab="Fechas",ylab = "Diferencias", main = "Diferencia de los precios",col = c("black"), screens = 1)
plot(x = diff(log(datos$s2)), xlab="Fechas",ylab = "Rendimientos", main = "Log-rendimientos",col = c("black"), screens = 1)
mtext("MEXCHEM.MX", outer = TRUE, cex = 1)
dev.off()

# install.packages("forecast")
library("forecast")
library(tseries)

ls1<-diff(log(ss1))
auto.arima(ls1)

png("acf&pacf_s1.png")
par(mfrow=c(2,2),oma = c(0, 0, 2, 0))
Acf(log(ss1),main="ACF Log Bimbo")
Acf(ls1*ls1,main="ACF Log-rendimientos")
pacf(log(ss1),main="PACF Log Bimbo")
pacf(ls1,main="PACF Log-rendimientos")
mtext("BIMBOA.MX", outer = TRUE, cex = 1)
dev.off()

ls2<-diff(log(ss2))
auto.arima(ls2)

png("acf&pacf_s2.png")
par(mfrow=c(2,2),oma = c(0, 0, 2, 0))
Acf(log(ss2),main="ACF Log MEXCHEM")
Acf(ls1*ls2,main="ACF Log-rendimientos")
pacf(log(ss2),main="PACF Log MEXCHEM")
pacf(ls2,main="PACF Log-rendimientos")
mtext("MEXCHEM.MX", outer = TRUE, cex = 1)
dev.off()

adf.test(ls1)
adf.test(ls2)

par(mfrow=c(1,1))

# LÃ¡tex
para_stas <- data.frame(ss1,c(0,ls1),ss2,c(0,ls2))

library(xtable)
print(xtable(summary(para_stas)),include.rownames=FALSE)

library(moments)

skewness(ss1)
skewness(ls1)
kurtosis(ss1)
kurtosis(ls1)
var(ss1)
var(ls1)

skewness(ss2)
skewness(ls2)
kurtosis(ss2)
kurtosis(ls2)
var(ss2)
var(ls2)

cor(ss1, ss2)
cor(ls1, ls2)

# Ajuste de varios modelos

library(astsa)

# a<-c(0,0,0,1,0,0,1,1,0,0,2,1,0,1,0,1,0,1,1,1,0,1,2,1,0,2,0,1,0,2,1,1,0,2,2,1,1,0,0,1,1,0,1,1,1,0,2,1,1,1,0,1,1,1,1,1,1,1,2,1,1,2,0,1,1,2,1,1,1,2,2,1,2,0,0,1,2,0,1,1,2,0,2,1,2,1,0,1,2,1,1,1,2,1,2,1,2,2,0,1,2,2,1,1,2,2,2,1)
# 
# mejor_m <- function(serie){
#   
#   mat_aux<-matrix(a,nrow=27,ncol=4,byrow=TRUE)
#   
#   for (p in 1:27){
#     mat_aux[p,4]<-sarima(serie,mat_aux[p,1], mat_aux[p,2], mat_aux[p,3])$AIC
#   }
#   
#   mat_aux2<-mat_aux[2:27,]
#   d<-mat_aux2[mat_aux2[,4]==min(mat_aux2[,4])]
#   
#   return(list(u1=d,u2=mat_aux))}
# 
# rs1<-mejor_m(ls1)


# xtable(round(rs1$u2,8))
# rs1$u2
# 
# rs2<-mejor_m(ls2)
# xtable(round(rs2$u2,8))
# rs2$u2
# 
mejor<-function(ftrt){
  ft <- as.numeric(ftrt)
  ft <- ft[!is.na(ft)]
  
  ftfinal.aic <- Inf
  ftfinal.order <- c(0,0,0)
  for (p in 1:2) for (d in 0:2) for (q in 0:2) {
    ftcurrent.aic <- AIC(arima(ft, order=c(p, d, q)))
    if (ftcurrent.aic < ftfinal.aic) {
      ftfinal.aic <- ftcurrent.aic
      ftfinal.order <- c(p, d, q)
      ftfinal.arima <- arima(ft, order=ftfinal.order)
    }
  }
  return(ftfinal.order)}

mejor_s1<-mejor(ls1)
mejor_s2<-mejor(ls2)
# 

#install.packages("lmtest")
library(lmtest)

library(astsa)
library(fGarch)
#Te interesa que todos los pvalues sean mayores que 0.5

arma_s1<-sarima(ls1,1,0,1,no.constant=TRUE)
garch_s1 <- garchFit(formula~garch(1,1), data=resid(arma_s1$fit), cond.dist="std",include.delta=FALSE,include.skew=FALSE,include.shape=FALSE,include.mean=FALSE)
ssres_s1 <- garch_s1@residuals/sqrt(garch_s1@h.t)

arma_s2<-sarima(ls2,1,0,1,no.constant=TRUE)
garch_s2 <- garchFit(formula~garch(1,1), data=resid(arma_s2$fit), cond.dist="std",include.delta=FALSE,include.skew=FALSE,include.shape=FALSE,include.mean=FALSE)
ssres_s2 <- garch_s2@residuals/sqrt(garch_s2@h.t)

auto.arima(ls1)
arma_s1$fit$coef
coeftest(arima(ls1,order=c(1,0,1)))

auto.arima(ls2)
arma_s2$fit$coef
coeftest(arima(ls2,order=c(1,0,1)))
#Ojo al parecer, no se necesita ajuste ARIMA

png("acf&pacf_s1_aj.png")
par(mfrow=c(1,2),oma = c(0, 0, 2, 0))
Acf(resid(arma_s1$fit),main="ACF residuales")
pacf(resid(arma_s1$fit),main="PACF residuales")
mtext("BIMBOA.MX", outer = TRUE, cex = 1)
dev.off()

png("acf&pacf_s2_aj.png")
par(mfrow=c(1,2),oma = c(0, 0, 2, 0))
Acf(resid(arma_s2$fit),main="ACF residuales")
pacf(resid(arma_s2$fit),main="PACF residuales")
mtext("MEXCHEM.MX", outer = TRUE, cex = 1)
dev.off()

png("res_arma.png")
par(mfrow=c(1,2),oma = c(0, 0, 2, 0))
hist(resid(arma_s1$fit),prob=TRUE,ylim=c(0,40),main="Log-rendimientos BIMBO",xlab="Log-rendimientos",ylab="Densidad")
lines(density(resid(arma_s1$fit)))
hist(resid(arma_s2$fit),prob=TRUE,ylim=c(0,40),main="Log-rendimientos MEXCHEM",xlab="Log-rendimientos",ylab="Densidad")
lines(density(resid(arma_s2$fit)))
mtext("Distribución de los rendimientos", outer = TRUE, cex = 1)
dev.off()

png("res_arma_sqr_s1.png")
par(mfrow=c(1,3),oma = c(0, 0, 2, 0))
plot((resid(arma_s1$fit))^2, main="Residuales al cuadrado")
Acf((resid(arma_s1$fit))^2,main="ACF residuales al cuadrado")
pacf((resid(arma_s1$fit))^2,main="PACF residuales al cuadrado")
mtext("BIMBOA.MX", outer = TRUE, cex = 1)
dev.off()

png("res_arma_sqr_s2.png")
par(mfrow=c(1,3),oma = c(0, 0, 2, 0))
plot((resid(arma_s2$fit))^2, main="Residuales al cuadrado")
Acf((resid(arma_s2$fit))^2,main="ACF residuales al cuadrado")
pacf((resid(arma_s2$fit))^2,main="PACF residuales al cuadrado")
mtext("MEXCHEM.MX", outer = TRUE, cex = 1)
dev.off()

png("ajuste_garch_s1.png")
par(mfrow=c(1,2),oma = c(0, 0, 2, 0))
plot(x = log(datos$s1), xlab="Fechas",ylab = "Log Precios", main = "Precios con transformación logarítmica",col = c("black"), screens = 1)
plot(garch_s1@h.t,type="l",main="Varianzas condicionales")
mtext("BIMBOA.MX", outer = TRUE, cex = 1)
dev.off()

png("ajuste_garch_s2.png")
par(mfrow=c(1,2),oma = c(0, 0, 2, 0))
plot(x = log(datos$s2), xlab="Fechas",ylab = "Log Precios", main = "Precios con transformación logarítmica",col = c("black"), screens = 1)
plot(garch_s2@h.t,type="l", main= "Varianzas condicionales")
mtext("MEXCHEM.MX", outer = TRUE, cex = 1)
dev.off()

library("stats")

png("ajuste_t_s1.png")
par(mfrow=c(1,2),oma = c(0, 0, 2, 0))
qqnorm(resid(arma_s1$fit))
qqline(resid(arma_s1$fit))
qqnorm(ssres_s1)
qqline(ssres_s1)
mtext("BIMBOA.MX", outer = TRUE, cex = 1)
dev.off()

png("ajuste_t_s2.png")
par(mfrow=c(1,2),oma = c(0, 0, 2, 0))
qqnorm(resid(arma_s2$fit))
qqline(resid(arma_s2$fit))
qqnorm(ssres_s2)
qqline(ssres_s2)
mtext("MEXCHEM.MX", outer = TRUE, cex = 1)
dev.off()

png("dependencia_res.png")
plot(resid(arma_s1$fit),resid(arma_s2$fit),type="p")
abline(lm(resid(arma_s1$fit)~resid(arma_s2$fit)),col='red',lwd=1)
dev.off()

pairs.panels(cbind(resid(arma_s1$fit),resid(arma_s2$fit)))

cor(resid(arma_s1$fit),resid(arma_s2$fit), method = c("pearson"))
cor(resid(arma_s1$fit),resid(arma_s2$fit), method = c("kendall"))
cor(resid(arma_s1$fit),resid(arma_s2$fit), method = c("spearman"))

png("serie1.png")
arma_s1<-sarima(ls1,1,0,1,no.constant=FALSE)
dev.off()

png("serie2.png")
arma_s2<-sarima(ls2,1,0,1,no.constant=FALSE)
dev.off()

#Ajuste del modelo ARMA por serie

library(spd)

fit_s1<-spdfit(ssres_s1, upper = 0.9, lower = 0.1, tailfit="GPD", type = c("mle"),information = c("observed"))
fit_s2<-spdfit(ssres_s2, upper = 0.9, lower = 0.1, tailfit="GPD", type = c("mle"),information = c("observed"))

png("dist_orig_s1.png")
plot(ecdf(ssres_s1))
dev.off()

png("dist_orig_s2.png")
plot(ecdf(ssres_s2))
dev.off()

png("GPD_s1.png")
plot(fit_s1)
dev.off()

png("GPD_s2.png")
plot(fit_s2)
dev.off()

## Ajuste de la c??pula

library(copula)
library(copBasic)

u<-pspd(ssres_s1, fit_s1)
v<-pspd(ssres_s2, fit_s2)

png("u_v_dep.png")
plot(u,v)
dev.off()

png("u_v_dep2.png")
pairs.panels(cbind(u,v))
dev.off()

cor(u,v, method = c("pearson"))
cor(u,v, method = c("kendall"))
cor(u,v, method = c("spearman"))

# Realizar aquí análisis de dependencia


uv<-data.frame(U=u,V=v)

# Ajuste de la cópula empírica
#  Pruebas los valores que te interesan. Puedes tomar como referencia el valor que ajusta R con fitCopula
empcop <- EMPIRcopdf(para=uv)
emp<-empcop$empcop
data<-cbind(uv$U,uv$V)


x=.8
t.cop<-tCopula(x,dim=2,df=5,df.fixed=TRUE)
cop<-pCopula(data,t.cop)

min_sqr<-function(cop_,emp_){
  lim = 0
  
  for (i in 1:length(emp_)){
    lim = lim + (abs(emp_[i]-cop_[i]))^2
  }
  return(lim)
}

min_sqr(cop,emp)

# 0.8004137, 0.3424068 0.07962807, 0.01758788, 0.02333812 0.1926576

a<-c(.1,.2,.3,.37,.4,.5, 0.8004137, 0.3424068, 0.07962807, 0.01758788, 0.02333812, 0.1926576)
matrix(a,ncol=2,nrow=6)

### Te da el valor que deber??a tener la correlaci??n

min_sqr<-function(emp_,dat){
  x=0
  lim = 40000
  while(lim > 0.1){
    t.cop<-tCopula(x,dim=2,df=5,df.fixed=TRUE)
    cop<-pCopula(dat,t.cop)
    
    lim = 0
    
    for (i in 1:length(emp_)){
      lim = lim + (abs(emp_[i]-cop[i]))^2
    }
    x = x + .001
  }
  return(x)
}

min_sqr(emp,data)
###

## Ajuste de la funcion de cola

t.tail<-function(u,v,rho,df){
  cte = -sqrt((df+1)/(1-rho^2)) 
  res <- u * pt((cte*(((u/v)^(1/df))-rho)),df+1) + v * pt((cte*(((v/u)^(1/df))-rho)),df+1)
  return(res)
}

#Primero modificamos a u y v
k = 100*log(1043)/1043
u_modif <- u*k
v_modif <- v*k

uv_modif<-data.frame(U=u_modif,V=v_modif)

empcop <- EMPIRcopdf(para=uv_modif)
Gamma<-k*empcop$empco

#Gamma<-EMPIRcop(u_modif, v_modif)
ajuste<-function(vector,u1,u2,y){
  lim = 0
  for (i in 1:1043){
    lim = lim + abs((vector[i] - t.tail(u1[i],u2[i],y,5)))^2
  }
  return(lim)
}

ajuste(Gamma,u_modif,v_modif,.1)
ajuste(Gamma,u_modif,v_modif,.2)
ajuste(Gamma,u_modif,v_modif,.8)
ajuste(Gamma,u_modif,v_modif,.9)
ajuste(Gamma,u_modif,v_modif,1)


#####################

t.cop<-tCopula(dim=2,df=5)
fit <- fitCopula(t.cop,uv,method="ml")
coef(fit)

rho<-.37
r#df<-coef(fit)[2]
df<-5

library(CDVine)

BiCopPar2TailDep(2,rho,df)

png("cop_fit.png")
persp(tCopula(dim=2,rho,df=df),dCopula)
dev.off()

install.packages("rgl")
library(rgl)
plot3d(u,v,pch=20,col='navyblue')


precios_s1<-matrix(nrow=20,ncol=100)
precios_s2<-matrix(nrow=20,ncol=100)

rendi_s1<-matrix(nrow=21,ncol=100)
rendi_s2<-matrix(nrow=21,ncol=100)

simulacion<-function(arma_serie,garch_serie,resi_sim_serie,lserie,ss_serie){
  
  #   arma_serie <-arma_s1
  #   garch_serie<-garch_s1
  #   resi_sim_serie<-resi_sim_s1
  #   lserie<-ls1
  #   ss_serie<-ss1
  #   
  #   
  ar<-as.matrix(arma_serie$fit$coef)[1]
  ma<-as.matrix(arma_serie$fit$coef)[2]
  #mean<-as.matrix(arma_serie$fit$coef)[3]
  mean<-0
  omega<-as.matrix(garch_serie@fit$param$params)[2]
  alpha1<-as.matrix(garch_serie@fit$param$params)[3]
  gamma1<-as.matrix(garch_serie@fit$param$params)[4]
  
  todos_res<-append(residuals(arma_serie$fit),1:21)
  #todos_var<-append(garch_serie@h.t,1:21)
  todos_var<-append(garch_serie@h.t,(predict(garch_serie,21)[1:21,2])^2)
  aux_x = append(lserie,1:21)
  
  i = 1
  for (t in (1044 : 1064)){
    #todos_var[t] = omega +  alpha1* todos_var[t-1] + gamma1*todos_res[t-1]
    todos_res[t] = resi_sim_serie[i] *  sqrt(todos_var[t])
    aux_x[t] = todos_res[t] + mean + ar *aux_x[t-1] + ma*todos_res[t-1]
    i = i + 1
  }
  
  precios<-append(ss_serie,1:20)
  
  for (r in (1045 : 1064)){
    precios[r]=exp(aux_x[r])*precios[r-1]
  }
  
  return(list(v1=precios[1045:1064],v2=aux_x[1044:1064]))
}

n = 21

for(h in 1:100){
  
  simuladas<-BiCopSim(n,2,rho,par2=df)
  
  resi_sim_s1<-c(1:n)
  resi_sim_s2<-c(1:n)
  
  for(l in 1:n){
    resi_sim_s1[l]<-qspd(simuladas[l,1],fit_s1,TRUE)
    resi_sim_s2[l]<-qspd(simuladas[l,2],fit_s2,TRUE)
  }
  
  resultados_s1<-simulacion(arma_s1,garch_s1,resi_sim_s1,ls1,ss1)
  resultados_s2<-simulacion(arma_s2,garch_s2,resi_sim_s2,ls2,ss2)
  
  precios_s1[,h]<-round(resultados_s1$v1,2)
  precios_s2[,h]<-round(resultados_s2$v1,2)
  
  rendi_s1[,h]<-resultados_s1$v2
  rendi_s2[,h]<-resultados_s2$v2
  
}
# 
# 
# library(ggplot2)
# autoplot(ts(cbind(precios_s1[,1],precios_s1[,2],precios_s1[,3],precios_s1[,4],precios_s1[,5],precios_s1[,6],precios_s1[,7],precios_s1[,8],precios_s1[,9],precios_s1[,10],precios_s1[,11],precios_s1[,12],precios_s1[,13],precios_s1[,14],precios_s1[,15],precios_s1[,16],precios_s1[,17],precios_s1[,18],precios_s1[,19],precios_s1[,20],precios_s1[,21],precios_s1[,22],precios_s1[,23],precios_s1[,24],precios_s1[,25],precios_s1[,26],precios_s1[,27],precios_s1[,28],precios_s1[,29],precios_s1[,30],precios_s1[,31],precios_s1[,32],precios_s1[,33],precios_s1[,34],precios_s1[,35],precios_s1[,36],precios_s1[,37],precios_s1[,38],precios_s1[,39],precios_s1[,40],precios_s1[,41],precios_s1[,42],precios_s1[,43],precios_s1[,44],precios_s1[,45],precios_s1[,46],precios_s1[,47],precios_s1[,48],precios_s1[,49],precios_s1[,50],precios_s1[,51],precios_s1[,52],precios_s1[,53],precios_s1[,54],precios_s1[,55],precios_s1[,56],precios_s1[,57],precios_s1[,58],precios_s1[,59],precios_s1[,60],precios_s1[,61],precios_s1[,62],precios_s1[,63],precios_s1[,64],precios_s1[,65],precios_s1[,66],precios_s1[,67],precios_s1[,68],precios_s1[,69],precios_s1[,70],precios_s1[,71],precios_s1[,72],precios_s1[,73],precios_s1[,74],precios_s1[,75],precios_s1[,76],precios_s1[,77],precios_s1[,78],precios_s1[,79],precios_s1[,80],precios_s1[,81],precios_s1[,82],precios_s1[,83],precios_s1[,84],precios_s1[,85],precios_s1[,86],precios_s1[,87],precios_s1[,88],precios_s1[,89],precios_s1[,90],precios_s1[,91],precios_s1[,92],precios_s1[,93],precios_s1[,94],precios_s1[,95],precios_s1[,96],precios_s1[,97],precios_s1[,98],precios_s1[,99],precios_s1[,100])))
# autoplot(ts(cbind(precios_s2[,1],precios_s2[,2],precios_s2[,3],precios_s2[,4],precios_s2[,5],precios_s2[,6],precios_s2[,7],precios_s2[,8],precios_s2[,9],precios_s2[,10],precios_s2[,11],precios_s2[,12],precios_s2[,13],precios_s2[,14],precios_s2[,15],precios_s2[,16],precios_s2[,17],precios_s2[,18],precios_s2[,19],precios_s2[,20],precios_s2[,21],precios_s2[,22],precios_s2[,23],precios_s2[,24],precios_s2[,25],precios_s2[,26],precios_s2[,27],precios_s2[,28],precios_s2[,29],precios_s2[,30],precios_s2[,31],precios_s2[,32],precios_s2[,33],precios_s2[,34],precios_s2[,35],precios_s2[,36],precios_s2[,37],precios_s2[,38],precios_s2[,39],precios_s2[,40],precios_s2[,41],precios_s2[,42],precios_s2[,43],precios_s2[,44],precios_s2[,45],precios_s2[,46],precios_s2[,47],precios_s2[,48],precios_s2[,49],precios_s2[,50],precios_s2[,51],precios_s2[,52],precios_s2[,53],precios_s2[,54],precios_s2[,55],precios_s2[,56],precios_s2[,57],precios_s2[,58],precios_s2[,59],precios_s2[,60],precios_s2[,61],precios_s2[,62],precios_s2[,63],precios_s2[,64],precios_s2[,65],precios_s2[,66],precios_s2[,67],precios_s2[,68],precios_s2[,69],precios_s2[,70],precios_s2[,71],precios_s2[,72],precios_s2[,73],precios_s2[,74],precios_s2[,75],precios_s2[,76],precios_s2[,77],precios_s2[,78],precios_s2[,79],precios_s2[,80],precios_s2[,81],precios_s2[,82],precios_s2[,83],precios_s2[,84],precios_s2[,85],precios_s2[,86],precios_s2[,87],precios_s2[,88],precios_s2[,89],precios_s2[,90],precios_s2[,91],precios_s2[,92],precios_s2[,93],precios_s2[,94],precios_s2[,95],precios_s2[,96],precios_s2[,97],precios_s2[,98],precios_s2[,99],precios_s2[,100])))

#Precios originales

os1_url <- "http://chart.finance.yahoo.com/table.csv?s=BIMBOA.MX&a=0&b=1&c=2016&d=11&e=31&f=2016&g=d&ignore=.csv"
os2_url <- "http://chart.finance.yahoo.com/table.csv?s=MEXCHEM.MX&a=0&b=1&c=2016&d=11&e=31&f=2016&g=d&ignore=.csv"

os1<-read.zoo(yahoo.read(os1_url))
os2<-read.zoo(yahoo.read(os2_url))

# Para quedarnos con las fechas en las que ambos tienen datos
datos2<-merge.zoo(os1,os2)
names(datos2)

oss1<-coredata(datos2$os1)
oss2<-coredata(datos2$os2)

da <- matrix(ss1[1044],ncol=100,nrow=1)
sims<-rbind(da,precios_s1)
# Datos originales vs simulaciones 
png("simulaciones_s1.png")
matplot(sims[,1:100], type='l', xlab='Días', ylab='Precios',
        main='Trayectorias de los precios')
matlines(oss1[1:20],type="p",lty=2,lwd=10)
dev.off()

da2 <- matrix(ss2[1044],ncol=100,nrow=1)
sims2<-rbind(da2,precios_s2)

png("simulaciones_s2.png")
matplot(sims2[,1:100], type='l', xlab='Dí­as', ylab='Precios',
        main='Trayectorias de los precios')
matlines(oss2[1:20],type="p")
dev.off()

# El VaR se calcula de manera directa sobre los log rendimientos

# Rendimientos acumulados logaritmicos de la cartera
# consideraremos pesos iguales para nuestros 2 instrumentos, se calcula para el horizonte de tiempo simulado

rend_acum = c(1:100)

for (f in 1:100){
  nada = 0 
  for  (g in 1:21){
    nada = log(1 + ((1/2)*(exp(rendi_s1[g,f])-1))+((1/2)*(exp(rendi_s2[g,f])-1))) + nada
  }
  rend_acum[f] = nada
}

# VaR simulado al 90%, 95% y 99% (estÃ¡n en procentaje)
VaR = 100 * quantile(rend_acum, c(0.90, 0.95, 0.91))

# MÃ¡xima pÃ©rdida simulada (estÃ¡n en procentaje)
-100*min(rend_acum)

# MÃ¡xima ganancia simulada (estÃ¡n en procentaje)
100*max(rend_acum)

VaR <- as.table(VaR)

print(xtable(VaR),include.rownames=FALSE)




