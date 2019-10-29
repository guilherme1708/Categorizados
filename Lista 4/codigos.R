# Lista 4 - categorizados

library(ggplot2)
library(gridExtra)
library(Epi)
library(tidyr)

# dados
data <- read.table("ex1cap7.txt",header = T)

# preparação dos dados para curva ROC
a <- matrix(rep(c(1,0,0,0),rep(17,4)),17,4);b <- matrix(rep(c(0,0,0,0),rep(257,4)),257,4)
c <- matrix(rep(c(1,0,1,0),rep(15,4)),15,4);d <- matrix(rep(c(0,0,1,0),rep(107,4)),107,4)
e <- matrix(rep(c(1,0,0,1),rep(7,4)),7,4);f <- matrix(rep(c(0,0,0,1),rep(52,4)),52,4)
g <- matrix(rep(c(1,0,1,1),rep(5,4)),5,4);h <- matrix(rep(c(0,0,1,1),rep(27,4)),27,4)
i <- matrix(rep(c(1,1,0,0),rep(1,4)),1,4);j <- matrix(rep(c(0,1,0,0),rep(7,4)),7,4)
k <- matrix(rep(c(1,1,1,0),rep(9,4)),9,4);l <- matrix(rep(c(0,1,1,0),rep(30,4)),30,4)
m <- matrix(rep(c(1,1,0,1),rep(3,4)),3,4);n <- matrix(rep(c(0,1,0,1),rep(14,4)),14,4)
o <- matrix(rep(c(1,1,1,1),rep(14,4)),14,4);p <- matrix(rep(c(0,1,1,1),rep(44,4)),44,4)

dados <- as.data.frame(rbind(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p))
names(dados) <- c("Y","cat","idade","ecg")

attach(data)

# item a

# Modelo saturado
ajust <- glm(as.matrix(data[,c(1,2)]) ~ cat+idade+ecg+cat*idade+cat*ecg+idade*ecg, family=binomial(link="logit"),data=data)
round(anova(ajust, test="Chisq"),3)

# AIC
glm(as.matrix(data[,c(1,2)]) ~1, family=binomial(link="logit"),data=data)$aic
glm(as.matrix(data[,c(1,2)]) ~ cat, family=binomial(link="logit"),data=data)$aic
glm(as.matrix(data[,c(1,2)]) ~ cat+idade, family=binomial(link="logit"),data=data)$aic
glm(as.matrix(data[,c(1,2)]) ~ cat+idade+ecg, family=binomial(link="logit"),data=data)$aic
glm(as.matrix(data[,c(1,2)]) ~ cat+idade+ecg+cat*idade, family=binomial(link="logit"),data=data)$aic
glm(as.matrix(data[,c(1,2)]) ~ cat+idade+ecg+cat*idade+cat*ecg, family=binomial(link="logit"),data=data)$aic
glm(as.matrix(data[,c(1,2)]) ~ cat+idade+ecg+cat*idade+cat*ecg+idade*ecg, family=binomial(link="logit"),data=data)$aic

# Modelo final
mod.final <- glm(as.matrix(data[,c(1,2)]) ~ cat+idade, family=binomial(link="logit"),data=data)
summary(mod.final)

# Análise de Resíduos
dev <- residuals(mod.final,type='deviance')
QL <- sum(dev^2)
p1 <- (1-pchisq(QL,5))
round(cbind(QL,p1),3)

# Pearson
rpears <- residuals(mod.final,type='pearson')
QP <- sum(rpears^2)
p2 <- (1-pchisq(QP,5))
round(cbind(QP,p2),3)

d1 <- data.frame(x=1:8,y=rpears)
d2 <- data.frame(x=1:8,y=dev)

plot1 <- d1 %>% ggplot(aes(y=rpears,x=1:8)) +
  geom_point() +
  scale_y_continuous(limits = c(-2,2)) +
  geom_hline(yintercept = 0,linetype = "dashed") +
  labs(x="Índices",
       y="Resíduos de Pearson",
       title="Gráfico 2:Resíduos de Pearson x índices")

plot2 <- d2 %>% ggplot(aes(y=dev,x=1:8)) +
  geom_point() +
  scale_y_continuous(limits = c(-2,2)) +
  geom_hline(yintercept = 0,linetype = "dashed") +
  labs(x="Índices",
       y="Resíduos deviance",
       title="Gráfico 3:Resíduos deviance x índices") 

grid.arrange(plot1, plot2, ncol=2)

# Envelope

envelope <- function(mod.final,link="logit"){
  X <- model.matrix(mod.final)
  k <- nrow(X)
  e <- matrix(0,k,100)
  tot <- numeric(k)
  w <- mod.final$weights
  W <- diag(w)
  H <- solve(t(X)%*%W%*%X)
  H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
  h <- diag(H)
  td <- sort(resid(mod.final, type="deviance")/sqrt(1-h))

  for(i in 1:100){
    for(j in 1:k){
      dif <- runif(ntot[j]) - fitted(mod.final)[j]
      dif[dif>=0] <- 0
      dif[dif<0]  <- 1
      tot[j] <- sum(dif)}
    xmat <- cbind(tot,ntot-tot)
    fit <- glm(xmat ~ X, family=binomial(link=link))
    w <- fit$weights
    W <- diag(w)
    H <- solve(t(X)%*%W%*%X)
    H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
    h <- diag(H)
    e[,i] <- sort(resid(fit, type="deviance")/sqrt(1-h))}

  e1 <- numeric(k)
  e2 <- numeric(k)

  for(i in 1:k){
    eo <- sort(e[i,])
    e1[i] <- (eo[2]+eo[3])/2
    e2[i] <- (eo[97]+eo[98])/2}

  med <- apply(e,1,mean)
  faixa <- range(td,e1,e2)
  
  qqn <- function(x){
    xo <- sort(x)                 # ordena a amostra
    n <- length(x)                # número de elementos
    i <- seq_along(x)             # índices posicionais
    pteo <- (i-0.5)/n             # probabilidades teóricas
    qnorm(pteo)                   # quantis teóricos sob a normal padrão
  }

  q.norm <- qqn(td)
  return(data.frame(td,e1,e2,med,q.norm))
}

ntot <- c(274,122,59,32,8,39,17,58) # replicas

d3 <- envelope(mod.final)

d3 %>% ggplot() +
  geom_ribbon(data = d3, aes(ymin=e1,ymax=e2,x=q.norm), fill="grey", alpha="0.5") +
  geom_point(aes(y = td,x=q.norm)) +
  geom_line(data = d3, aes(x = q.norm, y = med),linetype = "dashed") +
  geom_line(data = d3, aes(x = q.norm, y = e1)) +
  geom_line(data = d3, aes(x = q.norm, y = e2)) +
  labs(x = "Percentil da N(0,1)", 
       y = "Componente do Desvio",
       title = "Gráfico 3: QQ-Plot")

# Curva ROC
ROC(form=Y~cat+idade,plot="ROC",data=dados,main="Gráfico 4: Curva ROC")

# item b

# Modelos Saturados

# Probito
ajust2.tot <- glm(as.matrix(data[,c(1,2)]) ~ cat+idade+ecg+cat*idade+cat*ecg+idade*ecg, family=binomial(link="probit"),data=data)
round(anova(ajust2.tot, test="Chisq"),3)

# AIC
glm(as.matrix(data[,c(1,2)]) ~1, family=binomial(link="probit"),data=data)$aic
glm(as.matrix(data[,c(1,2)]) ~ cat, family=binomial(link="probit"),data=data)$aic
glm(as.matrix(data[,c(1,2)]) ~ cat+idade, family=binomial(link="probit"),data=data)$aic
glm(as.matrix(data[,c(1,2)]) ~ cat+idade+ecg, family=binomial(link="probit"),data=data)$aic
glm(as.matrix(data[,c(1,2)]) ~ cat+idade+ecg+cat*idade, family=binomial(link="probit"),data=data)$aic
glm(as.matrix(data[,c(1,2)]) ~ cat+idade+ecg+cat*idade+cat*ecg, family=binomial(link="probit"),data=data)$aic
glm(as.matrix(data[,c(1,2)]) ~ cat+idade+ecg+cat*idade+cat*ecg+idade*ecg, family=binomial(link="probit"),data=data)$aic

# Cauchy
ajust3.tot <- glm(as.matrix(data[,c(1,2)]) ~ cat+idade+ecg+cat*idade+cat*ecg+idade*ecg, family=binomial(link="cauchit"),data=data)
round(anova(ajust3.tot, test="Chisq"),3)

# AIC
glm(as.matrix(data[,c(1,2)]) ~1, family=binomial(link="cauchit"),data=data)$aic
glm(as.matrix(data[,c(1,2)]) ~ cat, family=binomial(link="cauchit"),data=data)$aic
glm(as.matrix(data[,c(1,2)]) ~ cat+idade, family=binomial(link="cauchit"),data=data)$aic
glm(as.matrix(data[,c(1,2)]) ~ cat+idade+ecg, family=binomial(link="cauchit"),data=data)$aic
glm(as.matrix(data[,c(1,2)]) ~ cat+idade+ecg+cat*idade, family=binomial(link="cauchit"),data=data)$aic
glm(as.matrix(data[,c(1,2)]) ~ cat+idade+ecg+cat*idade+cat*ecg, family=binomial(link="cauchit"),data=data)$aic
glm(as.matrix(data[,c(1,2)]) ~ cat+idade+ecg+cat*idade+cat*ecg+idade*ecg, family=binomial(link="cauchit"),data=data)$aic

# Clog-log
ajust4.tot <- glm(as.matrix(data[,c(1,2)]) ~ cat+idade+ecg+cat*idade+cat*ecg+idade*ecg, family=binomial(link="cloglog"),data=data)
round(anova(ajust4.tot, test="Chisq"),3)

# AIC
glm(as.matrix(data[,c(1,2)]) ~1, family=binomial(link="cloglog"),data=data)$aic
glm(as.matrix(data[,c(1,2)]) ~ cat, family=binomial(link="cloglog"),data=data)$aic
glm(as.matrix(data[,c(1,2)]) ~ cat+idade, family=binomial(link="cloglog"),data=data)$aic
glm(as.matrix(data[,c(1,2)]) ~ cat+idade+ecg, family=binomial(link="cloglog"),data=data)$aic
glm(as.matrix(data[,c(1,2)]) ~ cat+idade+ecg+cat*idade, family=binomial(link="cloglog"),data=data)$aic
glm(as.matrix(data[,c(1,2)]) ~ cat+idade+ecg+cat*idade+cat*ecg, family=binomial(link="cloglog"),data=data)$aic
glm(as.matrix(data[,c(1,2)]) ~ cat+idade+ecg+cat*idade+cat*ecg+idade*ecg, family=binomial(link="cloglog"),data=data)$aic

# Modelos Finais

ajust2 <- glm(as.matrix(data[,c(1,2)]) ~ cat+idade, family=binomial(link="probit"),data=data)
ajust3 <- glm(as.matrix(data[,c(1,2)]) ~ cat+idade, family=binomial(link="cauchit"),data=data)
ajust4 <- glm(as.matrix(data[,c(1,2)]) ~ cat+idade, family=binomial(link="cloglog"),data=data)

# Análise de Resíduos

# probito
dev2 <- residuals(ajust2,type='deviance')
QL2 <- sum(dev2^2)
p <- (1-pchisq(QL2,5))
round(cbind(QL2,p),3)
glm(as.matrix(data[,c(1,2)]) ~ cat+idade, family=binomial(link="probit"),data=data)$aic # AIC

# cauchy
dev3 <- residuals(ajust3,type='deviance')
QL3 <- sum(dev3^2)
p <- (1-pchisq(QL3,5))
round(cbind(QL3,p),3)
glm(as.matrix(data[,c(1,2)]) ~ cat+idade, family=binomial(link="cauchit"),data=data)$aic # AIC

# Clog-log
dev4 <- residuals(ajust4,type='deviance')
QL4 <- sum(dev4^2)
p <- (1-pchisq(QL4,5))
round(cbind(QL4,p),3)
glm(as.matrix(data[,c(1,2)]) ~ cat+idade, family=binomial(link="cloglog"),data=data)$aic # AIC

# Envelopes simulados
d4 <- envelope(ajust2,"probit")
d5 <- envelope(ajust3,"cauchit")
d6 <- envelope(ajust4,"cloglog")

plot1 <- d3 %>% ggplot() +
  geom_ribbon(data = d3, aes(ymin=e1,ymax=e2,x=q.norm), fill="grey", alpha="0.5") +
  geom_point(aes(y = td,x=q.norm)) +
  geom_line(data = d3, aes(x = q.norm, y = med),linetype = "dashed") +
  geom_line(data = d3, aes(x = q.norm, y = e1)) +
  geom_line(data = d3, aes(x = q.norm, y = e2)) +
  labs(x = "Percentil da N(0,1)", 
       y = "Componente do Desvio",
       title = "QQ-Plot: ligação Logito")

plot2 <- d4 %>% ggplot() +
  geom_ribbon(data = d3, aes(ymin=e1,ymax=e2,x=q.norm), fill="grey", alpha="0.5") +
  geom_point(aes(y = td,x=q.norm)) +
  geom_line(data = d3, aes(x = q.norm, y = med),linetype = "dashed") +
  geom_line(data = d3, aes(x = q.norm, y = e1)) +
  geom_line(data = d3, aes(x = q.norm, y = e2)) +
  labs(x = "Percentil da N(0,1)", 
       y = "Componente do Desvio",
       title = "QQ-Plot: ligação Probito")

plot3 <- d5 %>% ggplot() +
  geom_ribbon(data = d3, aes(ymin=e1,ymax=e2,x=q.norm), fill="grey", alpha="0.5") +
  geom_point(aes(y = td,x=q.norm)) +
  geom_line(data = d3, aes(x = q.norm, y = med),linetype = "dashed") +
  geom_line(data = d3, aes(x = q.norm, y = e1)) +
  geom_line(data = d3, aes(x = q.norm, y = e2)) +
  labs(x = "Percentil da N(0,1)", 
       y = "Componente do Desvio",
       title = "QQ-Plot: ligação Cauchy")

plot4 <- d5 %>% ggplot() +
  geom_ribbon(data = d3, aes(ymin=e1,ymax=e2,x=q.norm), fill="grey", alpha="0.5") +
  geom_point(aes(y = td,x=q.norm)) +
  geom_line(data = d3, aes(x = q.norm, y = med),linetype = "dashed") +
  geom_line(data = d3, aes(x = q.norm, y = e1)) +
  geom_line(data = d3, aes(x = q.norm, y = e2)) +
  labs(x = "Percentil da N(0,1)", 
       y = "Componente do Desvio",
       title = "QQ-Plot: ligação Clog-log")

grid.arrange(plot1, plot2, plot3, plot4, ncol=2)

detach(data)

# Exercício 3

x1 <- rep(0:5,13)
x2 <- rep(3:15,rep(6,13))
p.chapeu <- 1/(1+ exp(-(2.211 + 2.607*x1 - 0.52*x2)))
junto <- cbind(x1,x2,p.chapeu)
dados <- as.data.frame(junto)
ordem <- dados[order(dados$p.chapeu, dados$x1, dados$x2, decreasing=c(T,F,F)),]
rownames(ordem) <- NULL
