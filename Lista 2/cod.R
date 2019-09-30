library(DescTools)

# Exercício 2

# item a

dat <- matrix(c(14,12,12,1),2,2,dimnames =
            list(c("Masc", "Fem"),
                 c("Presente", "Ausente")))

fisher.test(dat,alternative = "less") # teste de fisher no R

# teste de fisher na mão
x <- numeric()
for (i in 0:14){
  x[i]<-(choose(26,i)*choose(13,26-i)/choose(39,26))
}
sum(x)

# item b

c <- t(matrix(c(5,2,2,0,
                0,1,0,1,
                0,2,3,4),4,3))

fisher.test(c)

# item c

UncertCoef(dat)

# exercício 3
GoodmanKruskalGamma(matrix(c(11,05,0,14,34,07,2,13,11),3,3))

# Capitulo 4

# Exercício 2

# item a

dados <- matrix(c(26,26,23,18,9,6,7,9,14,25), nc=2,dimnames = list(c("Placebo","Dose_1","Dose_2","Dose_3","Dose_4"),c("Não","Sim"))) 

mosaicplot(dados,main="", xlab="Dosagens", ylab="Efeitos adversos", col=gray.colors(2))
title("Gráfico1: Mosaico")

# item b

chisq.test(dados,correct=F) 

x <- c(rep(0,32),rep(1,33),rep(2,32),rep(3,32),rep(4,34))  
y <- c(rep(0,26),rep(1,6),rep(0,26),rep(1,7),rep(0,23),rep(1,9), rep(0,18),rep(1,14),rep(0,9),rep(1,25)) 

rac <- cor(y,x)  

n <- length(x) 

QCS <- (n-1)*rac^2  

p <- 1-pchisq(QCS,1)

cbind(rac, QCS, p)

# exercício 4

x<-c(rep(6,21),rep(8.5,49),rep(10.5,50),rep(12.5,59),rep(14.5,44))  
y<-c(rep(1,7), rep(2,4), rep(3,3), rep(4,7), 
     rep(1,10),rep(2,15),rep(3,11),rep(4,13),
     rep(1,23),rep(2,9), rep(3,11),rep(4,7),
     rep(1,28),rep(2,9), rep(3,12),rep(4,10),
     rep(1,32),rep(2,5), rep(3,4), rep(4,3)) 
rac<-cor(y,x)  
n<-length(x) 
QCS<-(n-1)*rac^2  
p<-1-pchisq(QCS,1)
cbind(rac, QCS, p)

# Capitulo 5

# exercício 1

# item a

dados<- matrix(c(45,80,84,106,64,104,124,117,71,116,82,87),nc=3)
colnames(dados) <- c("0","1","2")
rownames(dados) <- c("Fem Urbana","Fem Rural","Masc Urbana","Masc Rural")
mosaicplot(dados,main="", xlab="Sexo e Região", ylab="Períodos com resfriado", col=gray.colors(3))
title("Gráfico mosaico")

# item b

escore<-c(0,1,2) 
fb11<-(sum(dados[1,]*escore))/sum(dados[1,]) 
fb12<-(sum(dados[2,]*escore))/sum(dados[2,]) 
fb21<-(sum(dados[3,]*escore))/sum(dados[3,]) 
fb22<-(sum(dados[4,]*escore))/sum(dados[4,]) 
fm1<-sum(c(sum(dados[1,]),sum(dados[3,]))*c(fb11,fb21)) 
esp1<-(c(sum(dados[1:2,1]),sum(dados[1:2,2]),sum(dados[1:2,3])))/sum(dados[1:2,]) 
mu1<-sum(escore*esp1) 
esp2<-(c(sum(dados[3:4,1]),sum(dados[3:4,2]),sum(dados[3:4,3])))/sum(dados[3:4,]) 
mu2<-sum(escore*esp2) 
mu<-sum(c(sum(dados[1,]),sum(dados[3,]))*c(mu1,mu2)) 
v1<- sum(((escore-mu1)^2)*esp1) 
v2<- sum(((escore-mu2)^2)*esp2) 
vfma<-(sum(dados[1,])*sum(dados[2,])*v1)/(sum(dados[1:2,])-1) 
vfmb<-(sum(dados[3,])*sum(dados[4,])*v2)/(sum(dados[3:4,])-1) 
vfm<- sum(c(vfma,vfmb)) 
QSMH<-((fm1-mu)^2)/vfm 
p<-1-pchisq(QSMH,1) 
round(c(QSMH,p),digits=5)

# Exercicio 3

# item a

x<-c(rep(0,32),rep(1,33),rep(2,32),rep(3,32),rep(4,34))  
y<-c(rep(0,26), rep(1,6),
     rep(0,26), rep(1,7),
     rep(0,23), rep(1,9),
     rep(0,18), rep(1,14),
     rep(0,9), rep(1,25)) 
rac<-cor(y,x)  
n<-length(x) 
QCS<-(n-1)*rac^2  
p<-1-pchisq(QCS,1)
cbind(rac, QCS, p)

# item b

x<-c(rep(0,32),rep(1,32),rep(2,33),rep(3,32),rep(4,32))  
y<-c(rep(0,26), rep(1,6),
     rep(0,12), rep(1,20),
     rep(0,13), rep(1,20),
     rep(0,1), rep(1,31),
     rep(0,1), rep(1,31)) 
rac<-cor(y,x)  
n<-length(x) 
QCS<-(n-1)*rac^2  
p<-1-pchisq(QCS,1)
cbind(rac, QCS, p)

# item c

x<-c(rep(0,64),rep(1,65),rep(2,65),rep(3,64),rep(4,66))
y<-c(rep(0,52),rep(1,12),rep(0,38),rep(1,27),rep(0,36),rep(1,29),rep(0,19),rep(1,45),rep(0,10),rep(1,56)) 
rac<-cor(y,x) 
n<-length(x) 
QCS<-(n-1)*rac^2 
p<-1-pchisq(QCS,1) 
cbind(rac,QCS,p) 

# Exercício 4

tab<-array(c(120,111,80,155,161,117,130,124),dim=c(2,2,2))
mantelhaen.test(tab, correct=F)







