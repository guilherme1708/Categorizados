# lista de categorizados - MAE0560

library(VGAM)
library(tidyr)
library(ggplot2)
library(gridExtra)

dados <- read.table("idosos.txt",header = T)

# item a
par(mfrow=c(1,2))
data <- cbind(S = as.numeric(dados[1,c(3,4,5,6)]/sum(dados[1,c(3,4,5,6)])), N = as.numeric(dados[2,c(3,4,5,6)]/sum(dados[2,c(3,4,5,6)])))
bp<- barplot(height = data, beside = TRUE,
             col = c("black","darkgray","lightgray","white"), ylim=range(c(0,1)),
             names.arg = c("Sim", "Não"), xlab="Uso de tabaco: Sim", ylab="Proporções amostrais",
             legend.text = c("Excelente", "Bom", "Moderado", "Ruim"),
             args.legend = list(x = "topleft", bty="n", cex=1.4))
abline(h=0)
text(bp, c(0.11,0.313,0.415,0.16,0.19,0.496,0.246,0.068), round(data,2), cex=1.4, pos=3) 

data<-cbind(S = as.numeric(dados[3,c(3,4,5,6)]/sum(dados[3,c(3,4,5,6)])),
            N = as.numeric(dados[4,c(3,4,5,6)]/sum(dados[4,c(3,4,5,6)])))
bp<- barplot(height = data, beside = TRUE,
             col = c("black","darkgray","lightgray","white"), ylim=range(c(0,1)),
             names.arg = c("Sim", "Não"), xlab="Uso de tabaco: Não", ylab="Proporções amostrais",
             legend.text = c("Excelente", "Bom", "Moderado", "Ruim"),
             args.legend = list(x = "topleft", bty="n", cex=1.4))
abline(h=0)
text(bp, c(0.079,0.388,0.423,0.109,0.215,0.495,0.246,0.044), 
     round(data,2), cex=1.4, pos=3) 

# item b

# para todas as variáveis
mcp <- vglm(cbind(exc,bom,mod,ruim)~factor(tabaco)+factor(pcard),
            cumulative(parallel=T, reverse=F),data=dados)

mlc <- vglm(cbind(exc,bom,mod,ruim)~factor(tabaco)+factor(pcard),
            cumulative(parallel=F, reverse=F),data=dados)

TRV <- 2*(logLik(mlc)-logLik(mcp))
gl <- length(coef(mlc))-length(coef(mcp))
p <- 1-pchisq(TRV,gl)
cbind(TRV, gl, p)

# para variável X1

mcp1 <- vglm(cbind(exc,bom,mod,ruim)~factor(tabaco),
             cumulative(parallel=T, reverse=F),data=dados)

mlc1 <- vglm(cbind(exc,bom,mod,ruim)~factor(tabaco),
             cumulative(parallel=F, reverse=F),data=dados)

TRV1 <- 2*(logLik(mlc1)-logLik(mcp1))
gl1 <- length(coef(mlc1))-length(coef(mcp1))
p1 <- 1-pchisq(TRV1,gl1)
cbind(TRV1, gl1, p1)

# para variável X2

mcp2 <- vglm(cbind(exc,bom,mod,ruim)~factor(pcard),
             cumulative(parallel=T, reverse=F),data=dados)

mlc2 <- vglm(cbind(exc,bom,mod,ruim)~factor(pcard),
             cumulative(parallel=F, reverse=F),data=dados)

TRV2 <- 2*(logLik(mlc2)-logLik(mcp2))
gl2 <- length(coef(mlc2))-length(coef(mcp2))
p2 <- 1-pchisq(TRV2,gl2)
cbind(TRV2, gl2, p2)

# tabela deviances
mlcr0<-vglm(cbind(exc,bom,mod,ruim)~1,cumulative(parallel=F~factor(tabaco), reverse=F), dados)
mlcr0
mlcr1<-vglm(cbind(exc,bom,mod,ruim)~factor(tabaco), cumulative(parallel=F~factor(tabaco), reverse=F), dados)
mlcr1
mlcr2<-vglm(cbind(exc,bom,mod,ruim)~factor(tabaco)+factor(pcard), cumulative(parallel=F~factor(tabaco), reverse=F), dados)
mlcr2
mlcr3<-vglm(cbind(exc,bom,mod,ruim)~factor(tabaco)+factor(pcard)+factor(tabaco)*factor(pcard),
            cumulative(parallel=F~factor(tabaco), reverse=F), dados)
mlcr3

# graus de liberdade
gl1 <- df.residual(mlcr0)-df.residual(mlcr1)
gl2 <- df.residual(mlcr1)-df.residual(mlcr2)
gl3 <- df.residual(mlcr2)-df.residual(mlcr3)

# dif deviances
dev1 <- deviance(mlcr0)-deviance(mlcr1)
dev2 <- deviance(mlcr1)-deviance(mlcr2)
dev3 <- deviance(mlcr2)-deviance(mlcr3)

# p-values
p1 <- 1-pchisq(dev1,gl1)
p2 <- 1-pchisq(dev2,gl2)
p3 <- 1-pchisq(dev3,gl3)

# AIC dos modelos
AIC(mlcr0)
AIC(mlcr1)
AIC(mlcr2)
AIC(mlcr3)

# análise resíduos
rp <- resid(mlcr2, type = "pearson")

d1 <- data.frame(x=1:4,y=rp[,1])
d2 <- data.frame(x=1:4,y=rp[,2])
d3 <- data.frame(x=1:4,y=rp[,3])

plot1 <- d1 %>% ggplot(aes(y=rp[,1],x=1:4)) +
  geom_point() +
  scale_y_continuous(limits = c(-2,2)) +
  geom_hline(yintercept = 0,linetype = "dashed") +
  labs(x="Índices",
       y="Resíduos de Pearson",
       title="Gráfico 1: Logito 1")
plot2 <- d2 %>% ggplot(aes(y=rp[,2],x=1:4)) +
  geom_point() +
  scale_y_continuous(limits = c(-2,2)) +
  geom_hline(yintercept = 0,linetype = "dashed") +
  labs(x="Índices",
       y="Resíduos de Pearson",
       title="Gráfico 2: Logito 2") 
plot3 <- d3 %>% ggplot(aes(y=rp[,3],x=1:4)) +
  geom_point() +
  scale_y_continuous(limits = c(-2,2)) +
  geom_hline(yintercept = 0,linetype = "dashed") +
  labs(x="Índices",
       y="Resíduos de Pearson",
       title="Gráfico 3: Logito 3") 
grid.arrange(plot1, plot2,plot3, ncol=3)

# teste de qualida de ajuste

Qp <- sum(rp[,1]^2) + sum(rp[,2]^2)+ sum(rp[,3]^2)
p.value <- 1-pchisq(Qp,5)
cbind(Qp,p.value)

QL <- deviance(mlcr2)
p.value <- 1-pchisq(QL,5)
cbind(QL,p.value)

# item c

# coeficientes e probabilidades preditas
coef(mlcr2,matrix = TRUE)
fitted(mlcr2)

# Exercício 6

# item a

# para todas as variáveis
mca1 <- vglm(cbind(exc,bom,mod,ruim)~ factor(tabaco)+factor(pcard),
           family=acat(reverse=T,parallel=T),dados)

mca2 <- vglm(cbind(exc,bom,mod,ruim)~ factor(tabaco)+factor(pcard),
             family=acat(reverse=T,parallel=F),dados)

TRV<- deviance(mca1)-deviance(mca2)
gl <- df.residual(mca1)-df.residual(mca2)
p.value <- 1-pchisq(TRV,gl)
cbind(TRV, gl, p.value)

# para variável X1
mca11 <- vglm(cbind(exc,bom,mod,ruim)~ factor(tabaco),
              family=acat(reverse=T,parallel=T),dados)

mca21 <- vglm(cbind(exc,bom,mod,ruim)~ factor(tabaco),
              family=acat(reverse=T,parallel=F),dados)

TRV1<- deviance(mca11)-deviance(mca21)
gl1 <- df.residual(mca11)-df.residual(mca21)
p.value <- 1-pchisq(TRV1,gl1)
cbind(TRV1, gl1, p.value)

# para variável X2
mca12 <- vglm(cbind(exc,bom,mod,ruim)~ factor(pcard),
              family=acat(reverse=T,parallel=T),dados)

mca22 <- vglm(cbind(exc,bom,mod,ruim)~ factor(pcard),
              family=acat(reverse=T,parallel=F),dados)

TRV2<- deviance(mca12)-deviance(mca22)
gl2 <- df.residual(mca12)-df.residual(mca22)
p.value <- 1-pchisq(TRV2,gl2)
cbind(TRV2, gl2, p.value)

# tabela deviances
mca0 <- vglm(cbind(exc,bom,mod,ruim)~1, acat(reverse=T,parallel=F), dados)
mca0
mca1 <- vglm(cbind(exc,bom,mod,ruim)~factor(tabaco), acat(reverse=T,parallel=F), dados)
mca1
mca2 <- vglm(cbind(exc,bom,mod,ruim)~factor(tabaco)+factor(pcard), acat(reverse=T,parallel=F), dados)
mca2
mca3 <- vglm(cbind(exc,bom,mod,ruim)~factor(tabaco)+factor(pcard)+factor(tabaco)*factor(pcard),
             acat(reverse=T,parallel=F), dados)
mca3

# graus de liberdade
gl1 <- df.residual(mca0)-df.residual(mca1)
gl2 <- df.residual(mca1)-df.residual(mca2)
gl3 <- df.residual(mca2)-df.residual(mca3)

# dif deviances
dev1 <- deviance(mca0)-deviance(mca1)
dev2 <- deviance(mca1)-deviance(mca2)
dev3 <- deviance(mca2)-deviance(mca3)

# p-values
p1 <- 1-pchisq(dev1,gl1)
p2 <- 1-pchisq(dev2,gl2)
p3 <- 1-pchisq(dev3,gl3)

# AIC dos modelos
AIC(mca0)
AIC(mca1)
AIC(mca2)
AIC(mca3)

# análise resíduos
rp <- resid(mca2, type = "pearson")

d1 <- data.frame(x=1:4,y=rp[,1])
d2 <- data.frame(x=1:4,y=rp[,2])
d3 <- data.frame(x=1:4,y=rp[,3])

plot1 <- d1 %>% ggplot(aes(y=rp[,1],x=1:4)) +
  geom_point() +
  scale_y_continuous(limits = c(-2.5,2.5)) +
  geom_hline(yintercept = 0,linetype = "dashed") +
  labs(x="Índices",
       y="Resíduos de Pearson",
       title="Gráfico 6: Logito 1")
plot2 <- d2 %>% ggplot(aes(y=rp[,2],x=1:4)) +
  geom_point() +
  scale_y_continuous(limits = c(-2.5,2.5)) +
  geom_hline(yintercept = 0,linetype = "dashed") +
  labs(x="Índices",
       y="Resíduos de Pearson",
       title="Gráfico 7: Logito 2") 
plot3 <- d3 %>% ggplot(aes(y=rp[,3],x=1:4)) +
  geom_point() +
  scale_y_continuous(limits = c(-2.5,2.5)) +
  geom_hline(yintercept = 0,linetype = "dashed") +
  labs(x="Índices",
       y="Resíduos de Pearson",
       title="Gráfico 8: Logito 3") 
grid.arrange(plot1, plot2,plot3, ncol=3)

# teste de qualida de ajuste

Qp <- sum(rp[,1]^2) + sum(rp[,2]^2)+ sum(rp[,3]^2)
p.value <- 1-pchisq(Qp,5)
cbind(Qp,p.value)

QL <- deviance(mca2)
p.value <- 1-pchisq(QL,5)
cbind(QL,p.value)

# coeficientes e probabilidades preditas
coef(mca2,matrix = TRUE)
fitted(mca2)

# item b

# para todas as variáveis
mlrc1 <- vglm(cbind(exc,bom,mod,ruim)~ factor(tabaco)+factor(pcard),
              family=cratio(reverse=T,parallel=T),dados)

mlrc2 <- vglm(cbind(exc,bom,mod,ruim)~ factor(tabaco)+factor(pcard),
              family=cratio(reverse=T,parallel=F),dados)

TRV<- deviance(mlrc1)-deviance(mlrc2)
gl <- df.residual(mlrc1)-df.residual(mlrc2)
p.value <- 1-pchisq(TRV,gl)
cbind(TRV, gl, p.value)

# para variável X1
mlrc11 <- vglm(cbind(exc,bom,mod,ruim)~ factor(tabaco),
              family=cratio(reverse=T,parallel=T),dados)

mlrc21 <- vglm(cbind(exc,bom,mod,ruim)~ factor(tabaco),
              family=cratio(reverse=T,parallel=F),dados)

TRV1 <- deviance(mlrc11)-deviance(mlrc21)
gl1 <- df.residual(mlrc11)-df.residual(mlrc21)
p.value <- 1-pchisq(TRV1,gl1)
cbind(TRV1, gl1, p.value)

# para variável X2
mlrc12 <- vglm(cbind(exc,bom,mod,ruim)~ factor(pcard),
               family=cratio(reverse=T,parallel=T),dados)

mlca22 <- vglm(cbind(exc,bom,mod,ruim)~ factor(pcard),
               family=cratio(reverse=T,parallel=F),dados)

TRV2<- deviance(mlrc12)-deviance(mlca22)
gl2 <- df.residual(mlrc12)-df.residual(mlca22)
p.value <- 1-pchisq(TRV2,gl2)
cbind(TRV2, gl2, p.value)

# tabela deviances
mrc0 <- vglm(cbind(exc,bom,mod,ruim)~1, family=cratio(reverse=T,parallel=F), dados)
mrc0
mrc1 <- vglm(cbind(exc,bom,mod,ruim)~factor(tabaco), family=cratio(reverse=T,parallel=F), dados)
mrc1
mrc2 <- vglm(cbind(exc,bom,mod,ruim)~factor(tabaco)+factor(pcard), family=cratio(reverse=T,parallel=F), dados)
mrc2
mrc3 <- vglm(cbind(exc,bom,mod,ruim)~factor(tabaco)+factor(pcard)+factor(tabaco)*factor(pcard),
             family=cratio(reverse=T,parallel=F), dados)
mrc3

# graus de liberdade
gl1 <- df.residual(mrc0)-df.residual(mrc1)
gl2 <- df.residual(mrc1)-df.residual(mrc2)
gl3 <- df.residual(mrc2)-df.residual(mrc3)

# dif deviances
dev1 <- deviance(mrc0)-deviance(mrc1)
dev2 <- deviance(mrc1)-deviance(mrc2)
dev3 <- deviance(mrc2)-deviance(mrc3)

# p-values
p1 <- 1-pchisq(dev1,gl1)
p2 <- 1-pchisq(dev2,gl2)
p3 <- 1-pchisq(dev3,gl3)

# AIC dos modelos
AIC(mrc0)
AIC(mrc1)
AIC(mrc2)
AIC(mrc3)

# análise resíduos
rp <- resid(mrc2, type = "pearson")

d1 <- data.frame(x=1:4,y=rp[,1])
d2 <- data.frame(x=1:4,y=rp[,2])
d3 <- data.frame(x=1:4,y=rp[,3])

plot1 <- d1 %>% ggplot(aes(y=rp[,1],x=1:4)) +
  geom_point() +
  scale_y_continuous(limits = c(-2.5,2.5)) +
  geom_hline(yintercept = 0,linetype = "dashed") +
  labs(x="Índices",
       y="Resíduos de Pearson",
       title="Gráfico 9: Logito 1")
plot2 <- d2 %>% ggplot(aes(y=rp[,2],x=1:4)) +
  geom_point() +
  scale_y_continuous(limits = c(-2.5,2.5)) +
  geom_hline(yintercept = 0,linetype = "dashed") +
  labs(x="Índices",
       y="Resíduos de Pearson",
       title="Gráfico 10: Logito 2") 
plot3 <- d3 %>% ggplot(aes(y=rp[,3],x=1:4)) +
  geom_point() +
  scale_y_continuous(limits = c(-2.5,2.5)) +
  geom_hline(yintercept = 0,linetype = "dashed") +
  labs(x="Índices",
       y="Resíduos de Pearson",
       title="Gráfico 11: Logito 3") 
grid.arrange(plot1, plot2,plot3, ncol=3)

# teste de qualida de ajuste

Qp <- sum(rp[,1]^2) + sum(rp[,2]^2)+ sum(rp[,3]^2)
p.value <- 1-pchisq(Qp,5)
cbind(Qp,p.value)

QL <- deviance(mrc2)
p.value <- 1-pchisq(QL,5)
cbind(QL,p.value)

# coeficientes e probabilidades preditas
coef(mrc2,matrix = TRUE)
fitted(mrc2)
