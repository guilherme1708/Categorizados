# lista 1 categorizados

# Capitulo 2

# Exercício 5

# item a

Obitos <- c(42,22,29,93)
Nao_obitos <- c(161,181,176,518)
Total <- c(203,203,205,611)
Tabela <- data.frame(Obitos,Nao_obitos,Total)
colnames(Tabela) <- c("Obitos","Não obitos","Total")
rownames(Tabela) <- c("Medicamentosa","Cirurgia","Angioplastia","Total")
knitr::kable(caption = "Contingência",Tabela)

# item c

Tabela1 <- Tabela[1:3,1:2]
mosaicplot(Tabela1,main="", xlab="Terapias", ylab="Obitos", col=gray.colors(2))
title("Gráfico1: Mosaico")

# Exercício 6

# item b

Sim <- c(641,227,868)
Nao <- c(852,342,1194)
Totais <- c(1493,569,2062)
Tabela <- data.frame(Sim,Nao,Totais)
colnames(Tabela) <- c("Sim","Não","Totais")
rownames(Tabela) <- c("Feminino","Masculino","Totais")
knitr::kable(caption = "Contingência",Tabela)
Tabela1 <- Tabela[1:2,1:2]
mosaicplot(Tabela1,main="", xlab="Sexo", ylab="Artrite reumatoide", col=gray.colors(2))
title("Gráfico 2: Mosaico")

# Capítulo 3

# Exercício 2

#item b

Reincidentes <- c(47,26,73)
Nao <- c(43,21,64)
Totais <- c(90,47,137)
Tabela <- data.frame(Reincidentes,Nao,Totais)
colnames(Tabela) <- c("Reincidentes","Não reincidentes","Totais")
rownames(Tabela) <- c("A","B","Totais")
Tabela1 <- Tabela[1:2,1:2]
mosaicplot(Tabela1,main="", xlab="Grupo étnico", ylab="Status após um ano", col=gray.colors(2))
title("Gráfico 3: Mosaico")

# item c

dados<-matrix(c(47,26,43,21), nc=2)
p11<-(dados[1,1]/(sum(dados[1,])))
p21<-(dados[2,1]/(sum(dados[2,])))
RR<-p11/p21
vf1<-((1-p11)/(sum(dados[1,])*p11)) + ((1-p21)/(sum(dados[2,])*p21))
dpf1<-sqrt(vf1)
z<-qnorm(0.975)
li<-exp(log(RR) - z*dpf1)
ls<-exp(log(RR) + z*dpf1)
Tabela<-cbind(RR,li,ls)
knitr::kable(caption = "Risco Relativo",Tabela)

# Exercício 3

# item a

Sim <- c(83,19,102)
Nao <- c(34,16,50)
Totais <- c(117,35,152)
Tabela <- data.frame(Sim,Nao,Totais)
colnames(Tabela) <- c("Sim","Não","Totais")
rownames(Tabela) <- c("H","A","Totais")
Tabela1 <- Tabela[1:2,1:2]
mosaicplot(Tabela1,main="", xlab="Maternidade", ylab="Amamentação após 120 dias", col=gray.colors(2))
title("Gráfico 4: Mosaico")

# item b

dados<-matrix(c(83,19,34,16), nc=2)
Qp<-chisq.test(dados, correct=F) 
Qp


p11<-(dados[1,1]/(sum(dados[1,])))
p21<-(dados[2,1]/(sum(dados[2,])))
RR<-p11/p21
vf1<-((1-p11)/(sum(dados[1,])*p11)) + ((1-p21)/(sum(dados[2,])*p21))
dpf1<-sqrt(vf1)
z1<-qnorm(0.975)
li1<-exp(log(RR) - z1*dpf1)
ls1<-exp(log(RR) + z1*dpf1)
Tabela <- cbind(RR,li1,ls1)
knitr::kable(caption = "Risco Relativo 10%",Tabela)

z2<-qnorm(0.95)
li2<-exp(log(RR) - z2*dpf1)
ls2<-exp(log(RR) + z2*dpf1)
Tabela <- cbind(RR,li2,ls2)
knitr::kable(caption = "Risco Relativo 5%",Tabela)