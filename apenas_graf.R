library(dplyr)
library(ResourceSelection)
library(aod)
library(lmtest)
library(ggplot2)
library(patchwork)
library(ggpubr)
library(arm)

dados <- readxl::read_xlsx("dados_trabalho.xlsx")
variaveis <- c("Número de Identificação (ID)", "Idade", "Status socioeconômico",
               "Possui casa própria", "Setor da cidade", "Conta poupança")

set.seed(1121316)
obs <- sample(1:nrow(dados), 100)
amostra <- dados[obs,] # Para o ajuste do modelo
amostra_teste <- dados[-obs,] # Para a validação


amostra$X2 <- factor(amostra$X2, labels = c("Superior","Médio","Inferior"), levels = c(1:3))
amostra$X3 <- factor(amostra$X3, labels = c("'Não possui' ou \n'Sim, mas ainda\npagando financiamento'","Sim e quitada"), levels = c(1:2))
amostra$X4 <- factor(amostra$X4, labels = c("B","A"), levels = c(0:1))
amostra$X5 <- factor(amostra$X5, labels = c("Não","Sim"), levels = 0:1)


amostra_teste$X2 <- factor(amostra_teste$X2, labels = c("Superior","Médio","Inferior"), levels = c(1:3))
amostra_teste$X3 <- factor(amostra_teste$X3, labels = c("'Não possui' ou \n'Sim, mas ainda pagando financiamento'","Sim e quitada"), levels = c(1:2))
amostra_teste$X4 <- factor(amostra_teste$X4, labels = c("B","A"), levels = c(0:1))
amostra_teste$X5 <- factor(amostra_teste$X5, labels = c("Não","Sim"), levels = 0:1)


c <- round(1+3.3*log(100)) #quantidade de classes (regra de sturges). porém, fica melhor com 15 pois dá pra dividir por 75
c <- c-1
summary(amostra$X1)

breaks <- seq(0,75,5)
classes <-c("1-5","6-10","11-15","16-20","21-25", "26-30","31-35","36-40",
            "41-45","46-50","51-55","56-60","61-65","66-70","71-75") # nomes das classes

freq_age <- data.frame(1:15,table(cut(amostra$X1,breaks=breaks,right=TRUE,labels=classes)))

x1 <- ggplot(freq_age, aes(x=factor(X1.15), y=Freq)) +
  geom_col(fill = "#6F6DF7") +
  labs(title = "Distribuição de Pacientes por Idade",
       x = "Intervalo de Classe (anos)",
       y = "Frequência") +
  scale_x_discrete(labels= classes)+
  theme_classic()

x2 <- ggplot(amostra, aes(x= X2, fill=X2)) +
  geom_bar() +
  labs(title = "Frequência de Pacientes por\nStatus Socioeconômico",
       x = "Status",
       y = "Frequência") +
  scale_fill_manual(values = c("Superior" = "#00bfc4","Médio"="#6F6DF7","Inferior"="#f8766d")) +
  guides(fill="none") +
  theme_classic()

x3 <- ggplot(amostra, aes(x= X3, fill=X3)) +
  geom_bar() +
  labs(title = "Frequência de Pacientes por\nPosse da Casa Própria",
       x = "Posse",
       y = "Frequência") +
  scale_fill_manual(values = c("#f8766d","#00bfc4")) +
  guides(fill="none") +
  theme_classic()

x4 <- ggplot(amostra, aes(x= X4, fill=X4)) +
  geom_bar() +
  labs(title = "Frequência de Pacientes por\nSetor da Cidade",
       x = "Setor",
       y = "Frequência") +
  scale_fill_manual("Legenda", values = c("A" = "#f8766d","B" = "#00bfc4")) +
  guides(fill="none") +
  theme_classic()

x5 <- ggplot(amostra, aes(x= X5, fill=X5)) +
  geom_bar() +
  labs(title = "Frequência de Pacientes por\nPosse de Conta Poupança",
       x = "Posse",
       y = "Frequência") +
  scale_fill_manual("Legenda", values = c("Não" = "#f8766d","Sim" = "#00bfc4")) +
  guides(fill="none") +
  theme_classic()

(x1)/(x2+x3)/(x4+x5)

#BIVARIADA

#summary(amostra$X1) teste p lembrar os nomes dos status kkj

################# X5 ##################

x5x1 <- ggplot(data=amostra) +
  geom_boxplot(aes(X5,X1, fill =X5)) + 
  labs(title="Distribuição de Idade por\nposse de conta poupança",
       x=variaveis[6],y=variaveis[2])+
  guides(fill="none")+
  theme_classic()

#sum_x1x5 <- boxplot(amostra$X1~amostra$X5)
#sum_x1x5$stats


x5x2 <- ggplot(data=amostra) +
  geom_bar(aes(X5, fill=X2), position = "dodge") + 
  ylab("Frequência") +
  xlab(variaveis[6]) +
  scale_fill_manual(variaveis[3] ,values = c("#f8766d", "#6F6DF7","#00bfc4"))+
  theme_classic()

table(amostra$X5,amostra$X2)

x5x3 <- ggplot(data=amostra) +
  geom_bar(aes(X5, fill=X3), position = "dodge") + 
  ylab("Frequência") +
  xlab(variaveis[6]) +
  scale_fill_discrete(variaveis[4])+
  theme_classic()

table(amostra$X5,amostra$X3)

x5x4 <- ggplot(data=amostra) +
  geom_bar(aes(X5, fill=X4), position = "dodge") + 
  ylab("Frequência") +
  xlab(variaveis[6]) +
  scale_fill_discrete(variaveis[5])+
  theme_classic()


table(amostra$X5,amostra$X4)

x5x1

(x5x2/x5x3/x5x4)

############ OUTROS ############
x1x3 <- ggplot(data=amostra) +
  geom_boxplot(aes(X1,X3, fill = X3)) + 
  labs(title="Distribuição de Idade\npor posse da casa\nprópria",
       x=variaveis[2],y=variaveis[4])+
  scale_y_discrete(labels=c("'Não possui' ou\n'Sim, mas ainda\npagando \nfinanciamento'", "Sim e quitada"))+
  theme_classic()+
  guides(fill="none")

#sum_x1x3 <- boxplot(amostra$X1~amostra$X3)
#sum_x1x3$stats

x1x4 <- ggplot(data=amostra) +
  geom_boxplot(aes(X1,X4, fill=X4)) + 
  labs(title="Distribuição de Idade por\nsetor da cidade",
       x=variaveis[2],y=variaveis[5])+
  guides(fill="none")+
  theme_classic()

#sum_x1x4 <- boxplot(amostra$X1~amostra$X4)
#sum_x1x4$stats

x2x3 <- ggplot(data=amostra) +
  geom_bar(aes(X2, fill=X3), position = "dodge") + 
  ylab("Frequência") +
  xlab(variaveis[3]) +
  scale_fill_discrete(variaveis[4])+
  theme_classic()

x2x4 <- ggplot(data=amostra) +
  geom_bar(aes(X2, fill=X4), position = "dodge") + 
  ylab("Frequência") +
  xlab(variaveis[3]) +
  scale_fill_discrete(variaveis[5])+
  theme_classic()


#table(amostra$X3,amostra$X4)
#table(amostra$X3,amostra$X5)

x3x4 <- ggplot(data=amostra) +
  geom_bar(aes(X3, fill=X4), position = "dodge") + 
  ylab("Frequência") +
  xlab(variaveis[4]) +
  scale_fill_discrete(variaveis[5])+
  theme_classic()

x1x2 <- ggplot(data=amostra) +
  geom_boxplot(aes(X1,X2, fill = X2)) + 
  labs(title="Distribuição de Idade\npor status socioeco-\nnômico",
       x=variaveis[2],y=variaveis[3])+
  scale_fill_manual(values = c("#f8766d", "#6F6DF7","#00bfc4"))+
  theme_classic() +
  guides(fill="none")

#sum_x1x2 <- boxplot(amostra$X1~amostra$X2)
#sum_x1x2$stats

