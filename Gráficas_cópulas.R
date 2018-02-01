rm(list=ls())
setwd("C:/Users/F15141/Documents/Natalia/Tesis/ultimo/Imagenes")

library(copula)
library(CDVine)
library(scatterplot3d)
library(rgl)

# Gráfica página 18: copulas fundamentales

n.grid <- 26
u <- seq(0, 1, length.out = n.grid)
grid <- expand.grid("u" = u, "v" = u)

# Cota inferior
W <- function(u) pmax(0, rowSums(u)-1) # lower bound W
x.W <- cbind(grid, "W.C" = W(grid)) # evaluate W on 'grid'
b<-matrix(data=x.W$W.C,ncol=26,nrow=26,byrow=TRUE)

# Cota superior
M <- function(u) apply(u, 1, min) # upper bound M
x.M <- cbind(grid, "M.C" = M(grid)) # evaluate M on 'grid'
a<-matrix(data=x.M$M.C,ncol=26,nrow=26,byrow=TRUE)

# Independencia
t.C<- grid$u*grid$v
x.T <- cbind(grid, t.C) # evaluate M on 'grid'
c<-matrix(data=t.C,ncol=26,nrow=26,byrow=TRUE)

outer = FALSE
adj  = 0.025

png("CopulasFundamentales.jpg")
par(mfrow=c(2,3))
scatterplot3d(grid$u, grid$v, x.W$W.C, color="black",xlab ="u", ylab="v", zlab="C(u,v)",angle=70)
title(outer=outer,adj=adj,main="(a)",cex.main=3,col="black",line=-1)
scatterplot3d(grid$u, grid$v, x.T$t.C, color="black", xlab ="u", ylab="v", zlab="C(u,v)",angle=70)
title(outer=outer,adj=adj,main="(b)",cex.main=3,col="black",line=-1)
scatterplot3d(grid$u, grid$v, x.M$M.C, color="black", xlab ="u", ylab="v", zlab="C(u,v)",angle=70)
title(outer=outer,adj=adj,main="(c)",cex.main=3,col="black",line=-1)

contour(sort(u), sort(u), b)
title(outer=outer,adj=adj,main="(d)",cex.main=3,col="black",line=-1)
contour(sort(u), sort(u), c)
title(outer=outer,adj=adj,main="(e)",cex.main=3,col="black",line=-1)
contour(sort(u), sort(u), a)
title(outer=outer,adj=adj,main="(f)",cex.main=3,col="black",line=-1)
dev.off()
          

# Cópula normal
norm.cop = normalCopula(param=0.3, dim=2)

png("Gaussiana.jpg")
par(mfrow=c(1,1))
persp(norm.cop, dcopula, xlab="u", ylab="v", zlab="c(u,v)")
dev.off()

# t cópula
t.cop = tCopula(param=0.3, dim=2, df=2)

png("Student.jpg")
par(mfrow=c(1,1))
persp(t.cop, dcopula, xlab="u", ylab="v", zlab="c(u,v)")
dev.off()

# Cópulas arquimedeanas
clayton<- claytonCopula(2, dim = 2)
frank<-frankCopula(5, dim = 2)
gumbel<-gumbelCopula(2, dim = 2)

png("Arquimedeanas.jpg")
par(mfrow=c(1,3))
persp(gumbel, dcopula, main="Gumbel", xlab="u", ylab="v", zlab="C(u,v)")
persp(clayton, dcopula, main="Clayton",xlab="u", ylab="v", zlab="C(u,v)")
persp(frank, dcopula, main="Frank",xlab="u", ylab="v", zlab="C(u,v)")
dev.off()

# Marshall Olkin
n.grid <- 26
u <- seq(0.1, 1, length.out = n.grid)
grid <- expand.grid("u" = u, "v" = u)

ans_<-c(1:length(grid$u))
alpha=.25
beta=.75

for(i in 1:length(grid$u)){
  ans_[i] = min((grid$u[i])^(-alpha), (grid$v[i])^(-beta))
}

par(mfrow=c(1,1))
png("MO.jpg")
scatterplot3d(ans_, grid$v, grid$u,angle=100,xlab="u", ylab="v", zlab="c(u,v)")
dev.off()

plot3d(grid$u, grid$v,ans_)
scatterplot3d(grid$u, grid$v, ans_,angle=70)


#Gráfica pág 35
#Gumbel y clayton param = 2

png("ClaytonGumbel.jpg")
par(mfrow=c(1,2))
persp(gumbel, dcopula, main="Gumbel", xlab="u", ylab="v", zlab="C(u,v)")
persp(clayton, dcopula, main="Clayton",xlab="u", ylab="v", zlab="C(u,v)")
dev.off()

#Gráfica pág 36
#Simulaciones Gauss, Gumbel, Clayton y t
a <- rCopula(2000, norm.cop)
b <- rCopula(2000, gumbel)
c <- rCopula(2000, clayton)
d <- rCopula(2000, t.cop)

png("DepTail.jpg")
par(mfrow=c(2,2))
plot(a,xlab="U1",ylab="U2")
title(outer=outer,adj=adj,main="(a)",cex.main=1,col="black",line=-1)
plot(b,xlab="U1",ylab="U2")
title(outer=outer,adj=adj,main="(b)",cex.main=1,col="black",line=-1)
plot(c,xlab="U1",ylab="U2")
title(outer=outer,adj=adj,main="(c)",cex.main=1,col="black",line=-1)
plot(d,xlab="U1",ylab="U2")
title(outer=outer,adj=adj,main="(d)",cex.main=1,col="black",line=-1)
dev.off()
