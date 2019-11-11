# lista de categorizados - MAE5060

# Exercício 1

LI <- c(8,8,10,10,12,12,12,14,14,14,16,16,16,18,20,20,20,22,22,24,26,28,32,34,38,38,38)
y <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,1,0,0,1,1,0,1,1,1,0)

mod.log <- glm(y~LI,family = binomial("logit"))

# item a

round(1/(1+exp(-(mod.log$coef[1]+mod.log$coef[2]*26))),2)

#item b

round(exp(mod.log$coef[2]),2)

# item c

q1 <- quantile(LI)[2]
q3 <- quantile(LI)[4]

round(1/(1+exp(-(mod.log$coef[1]+mod.log$coef[2]*q3))),2)-round(1/(1+exp(-(mod.log$coef[1]+mod.log$coef[2]*q1))),2)

# Exercício 2

# item a

mod.probit <- glm(y~LI,family = binomial("probit"))

summary(mod.probit)

# item b

# de carodo com o livro pg 161
LI.50 <- -as.numeric(coef(mod.probit)[1]/coef(mod.probit)[2])
LI.50

# item c ?

X <- seq(7,40,0.1)

m1 <- pnorm(coef(mod.probit)[1]+coef(mod.probit)[2]*LI)
plot(m1~LI,type="l")

# Exercício 5

data1 <- data.frame(
  y=c(1,0,0,0,1,1,0,0,1,1,1,1),
  x=c(0,0,0,0,1,1,1,1,2,2,2,2)
)

data1$x <- as.factor(data1$x)
data1$y <- as.factor(data1$y)

data2 <- data.frame(
  x <- c(0,1,2),
  s <- c(1,2,4),
  f <- c(3,2,0)
)

data2$x <- as.factor(data2$x)

# item a

# para data 1
mod1 <- glm(y~1,family = binomial("logit"), data = data1)
anova(mod1,test="Chisq")

mod2 <- glm(y~x,family = binomial("logit"), data = data1)
anova(mod2,test="Chisq")

# para data 2
mod3 <- glm(as.matrix(data2[,c(2,3)])~1,family = binomial("logit"), data = data2)
anova(mod3,test="Chisq")

mod4 <- glm(as.matrix(data2[,c(2,3)])~x,family = binomial("logit"), data = data2)
anova(mod4,test="Chisq")

# item b



# Exercício 4 cap 7

dados <- data.frame(
  droga=(c(rep("A",4),rep("B",4))),
  log.dose=log10(c(2,4,8,16,2,4,8,16)),
  sim=c(2,9,14,19,1,6,14,17),
  nao=c(18,11,6,1,19,14,6,3)
)

mod.log <- glm(as.matrix(dados[,c(3,4)]) ~ droga + log.dose + droga*log.dose, family=binomial(link="logit"), data=dados)
anova(mod.log,test="Chisq")

mod.final <- glm(as.matrix(dados[,c(3,4)]) ~ log.dose, family=binomial(link="logit"), data=dados)

summary(mod.final)

# Exercício 6 cap 7

data <- data.frame(
  log.dose=c(0.0,2.6,3.8,5.1,7.7,10.2),
  sim=c(0,6,16,24,42,44),
  nao=c(49,44,32,22,7,6)
)

mod.1 <- glm(as.matrix(data[,c(2,3)]) ~ log.dose, family=binomial(link="logit"), data=data)
anova(mod.1,test="Chisq")

mod.1$aic

summary(mod.1)

r.dev <- residuals(mod.1, type='deviance')
r.dev
QL <- sum(r.dev^2)
p1 <- 1-pchisq(QL,4)
cbind(QL,p1)

