#Carregando os pacotes que vamos precisar para essas análises----
library(psych)
library(glmmTMB)
library(piecewiseSEM)
library(ggplot2)
library(patchwork)

#Inserindo os dados que vamos usar nas análises----
#Há outras formas, mas eu geralmente uso essa.

data<-read.table("gall.txt", h=T)
summary(data)
data$ponto<-as.factor(data$ponto)
data$planta<-as.factor(data$planta)

#Estatística descritiva
describeBy(data$galhascaul, data$ponto)

cor<-cor(data$area_construida,data$vegetacao)
cor.test(data$area_construida,data$vegetacao)

#Análises da probabilidade de ocorrência das galhas em relação à área urbanizada e área de vegetação natural
#Nessas análises nossa variável resposta é a ocorrência de galhas na planta, codificada como 0 (quando a planta não apresentou galhas), ou 1 (quando a planta apresentou as galhas)
# Na planilha é possível perceber que de 140 plantas amostradas, 100 apresentavam galhas
100/140
#Cerca de 71% das nossas plantas apresentaram galhas, no geral.

#Abaixo estão as equações dos modelos que calculam a influência da área impermeável e da área com vegetação nativa na probabilidade de ocorrência de galhas.
#Estamos utilizando um modelo linear generalizado misto, porque sabemos que as condiçoes da planta (como vigor, metabólitos secundários) e do ponto (comunidade de galhadores) também pode influenciar

m0<-glmmTMB(probgal ~ 1 + (1|planta/ponto), 
            control=glmmTMBControl(optimizer=optim,
                                   optArgs=list(method="BFGS")),
            data = data, family = "binomial")

m1<-glmmTMB(probgal ~ area_construida + (1|planta/ponto), 
            control=glmmTMBControl(optimizer=optim,
                                   optArgs=list(method="BFGS")),
            data = data, family = "binomial")
anova(m0,m1, test="Chi")

#Comparando o modelo nulo e o modelo explicativo, vemos que a área construída não tem relação com a quantidade de galhas por planta.

#Agora testaremos o efeito da vegetação circundante, os espaços verdes

m1<-glmmTMB(probgal ~ vegetacao + (1|planta/ponto), 
            control=glmmTMBControl(optimizer=optim,
                                   optArgs=list(method="BFGS")),
            data = data, family = "binomial")

anova(m0,m1,test="Chi")

#Tivemos efeito significativo, agora olharemos para o modelo com a função summary

summary(m1)
rsquared(m1)

#Aqui vemos que o coeficiente (estimate) de efeito da vegetação na quanitdade de galhas é 1.8328. Podemos usar esse numero para gerar o efeito de aumento na probabilidade de ocorrência de galhas a cada vez que aumentamos uma unidade na nossa variável preditora, vegetação. Para isso, utilizamos a fórmula exp(x) - 1, onde x é o valor do estimate. A fórmula está implementada abaixo

exp(1.8328) - 1

#Para cada aumento de 1 ha na cobertura vegetal, esperamos um aumento de 525%
#Na incidência de galhas caulinares em T. subulata

#Agora vamos ao gráfico

fig1a <- ggplot(data, aes(x=vegetacao, y=probgal)) +
  geom_point(colour="black", 
             fill = "darkgreen",
             shape = 21,
             size = 4, 
             position = position_jitter(width = 0.1, height = 0), 
             alpha = 0.4) +
  geom_smooth(method='glm', method.args=list(family="binomial"), color="black")+
  scale_x_continuous(expand=c(0,0), limits=c(0,1.4), breaks=seq(0,1.4,0.2)) +
  scale_y_continuous(limits=c(-0.05,1.2), breaks=seq(0,1,0.25)) +
  theme_classic() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 16),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(x = "Vegetation cover (ha)", y="Probability of stem galls occurrence") +
  annotate(geom="text", x=0.1, y=1.2, label="A)",
           color="black", size = 10, fontface = "bold")

fig1a

#Relações entre quantidade de galhas por planta e vigor da planta-----
#Para testar efeitos no número de galhas, retiramos o ponto 11, pelo comportamento muito diferente de todos os outros.----
#Enquanto tínhamos um máximo de cerca de 27 galhas para os outros pontos, o 11 apresentou mais de 50

data1<-data[-c(101:110), ] # Isso retira todas as linhas entre 101 e 110 do nosso conjunto de dados inicial, data. Essas são as linhas relativas ao ponto 11. Então data1 é o conjunto de dados sem o ponto 11
mean(data1$galhascaul)
sd(data1$galhascaul)

#Análises gráficas dos dados da quantidade de galhas----
#O gráfico abaixo é um histograma, que nos permite observar a distribuição de frequência de ocorr~encia dos nossos dados. O eixo x representa as possibilidades de quantas galhas uma planta pode ter e o eixo y representa quantas plantas com essa quantidade de galhas acontecem nos nossos dados. Então, podemos ver que muitas plantas tem poucas galhas e poucas tem maus de dez galhas.

hist(data1$galhascaul)

#Os gráficos abaixo são uma primeira análise da relação entre número de galhas por planta e a área impermeável, bem como a área com vegetação natural nas áreas avaliadas.

plot(data1$galhascaul~data1$area_construida)

plot(data1$galhascaul~data1$vegetacao)

#Não são muito informativos, vamos aos testes.

#Relação entre galhas e área construída-----

m0<-glmmTMB(galhascaul ~ 1 + (1|planta/ponto), data = data1, family = poisson)
m1<-glmmTMB(galhascaul ~ area_construida + (1|planta/ponto), data = data1, family = poisson)

anova(m0,m1,test="F")

summary(m1)
rsquared(m1)

#Gráfico Galhas x área construída-----
fig1b <- ggplot(data1, aes(x=area_construida, y=galhascaul)) +
  geom_point(colour="black",
             fill = "darkgreen",
             shape = 21,
             size = 3,
             position = position_jitter(width = 0.1),
             alpha = 0.4) +
  geom_smooth(method='glm', method.args=list(family="poisson"), color="black")+
  scale_y_continuous(expand=c(0,0), limits=c(-0,25), breaks=seq(0,25,5)) +
  scale_x_continuous(expand=c(0,0), limits=c(1.5,6.5), breaks=seq(0,6.5,0.5)) +
  theme_classic() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 16),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(x = "Impervious area (ha)", y="Number of stem galls per plant") +
  annotate(geom="text", x=2, y=24, label="B)",
         color="black", size = 10, fontface = "bold")
fig1b

m0<-glmmTMB(galhascaul ~ 1 + (1|planta/ponto), 
            control=glmmTMBControl(optimizer=optim,
                                   optArgs=list(method="BFGS")),
            data = data1, family = poisson)
m1<-glmmTMB(galhascaul ~ vegetacao + (1|planta/ponto), 
            control=glmmTMBControl(optimizer=optim,
                                   optArgs=list(method="BFGS")),
            data = data1, family = poisson)
anova(m0,m1,test="F")

summary(m1)
rsquared(m1)

fig1c <- ggplot(data1, aes(x=vegetacao, y=galhascaul)) +
  geom_point(colour="black",
             fill = "darkgreen",
             shape = 21,
             size = 3,
             position = position_jitter(width = 0.1, height = 0.05),
             alpha = 0.4) +
  geom_smooth(method='glm', method.args=list(family="poisson"), color="black")+
  scale_y_continuous(expand=c(0,0), limits=c(0,25), breaks=seq(0,25,5)) +
  scale_x_continuous(expand=c(0,0), limits=c(0,1.4), breaks=seq(0,1.4,0.2)) +
  theme_classic() + 
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 16),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(x = "Vegetation cover (ha)", y="Number of stem galls per plant") +
  annotate(geom="text", x=0.1, y=24, label="C)",
         color="black", size = 10, fontface = "bold")

fig1a + fig1b + fig1c

#Testando o efeito do vigor da planta no número de galhas----

#Biomassa das flores----

m0<-glmmTMB(galhascaul ~ 1 + (1|planta/ponto), 
            control=glmmTMBControl(optimizer=optim,
                                   optArgs=list(method="BFGS")),
            data = data1, family = poisson)
m1<-glmmTMB(galhascaul ~ nflores + (1|planta/ponto), 
            control=glmmTMBControl(optimizer=optim,
                                   optArgs=list(method="BFGS")),
            data = data1, family = poisson)

anova(m0,m1,test="F")
#Sem efeito

#Biomassa de botões florais----
m0<-glmmTMB(galhascaul ~ 1 + (1|planta/ponto), 
            control=glmmTMBControl(optimizer=optim,
                                   optArgs=list(method="BFGS")),
            data = data1, family = poisson)
m1<-glmmTMB(galhascaul ~ botoes + (1|planta/ponto), 
            control=glmmTMBControl(optimizer=optim,
                                   optArgs=list(method="BFGS")),
            data = data1, family = "poisson")
anova(m0,m1,test="Chi")
summary(m1)
rsquared(m1)

fig2a<-ggplot(data1, aes(x=botoes, y=galhascaul)) +
  geom_point(colour="darkgreen",
             fill = "black",
             size = 3,
             position = position_jitter(width = 0.1, height = 0.05),
             alpha = 0.4) +
  geom_smooth(method='glm', method.args=list(family="poisson"), color="black")+
  scale_y_continuous(expand=c(0,0), limits=c(0,25), breaks=seq(0,25,5)) +
  scale_x_continuous(expand=c(0,0), limits=c(0,6), breaks=seq(0,6,2)) +
  theme_classic() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 16),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(x = "Flower buds biomass per plant (g)", y= "Number of stem galls per plant") +
  theme(legend.position = "NONE") +
  annotate(geom="text", x=0.2, y=23.5, label="A)",
           color="black", size = 10, fontface = "bold")


#Biomassa dos frutos-----
m1<-glmmTMB(galhascaul ~ frutos + (1|planta/ponto), 
            control=glmmTMBControl(optimizer=optim,
                                   optArgs=list(method="BFGS")),
            data = data1, family = "poisson")
summary(m1)
rsquared(m1)

fig2b<-ggplot(data1, aes(x=frutos, y=galhascaul)) +
  geom_point(colour="darkgreen",
             fill = "black",
             size = 3,
             position = position_jitter(width = 0.1, height = 0.05),
             alpha = 0.4) +
  geom_smooth(method='glm', method.args=list(family="poisson"), color="black")+
  scale_y_continuous(expand=c(0,0), limits=c(-0,25), breaks=seq(0,25,5)) +
  scale_x_continuous(expand=c(0,0), limits=c(0,12.5), breaks=seq(0,12.5,2.5)) +
  theme_classic() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 16),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(x = "Fruits biomass per plant (g)", y="Number of stem galls per plant") +
  theme(legend.position = "NONE") +
  annotate(geom="text", x=0.5, y=23.5, label="B)",
           color="black", size = 10, fontface = "bold")


#Biomassa das folhas-----
m1<-glmmTMB(galhascaul ~ folhas + (1|planta/ponto), 
            control=glmmTMBControl(optimizer=optim,
                                   optArgs=list(method="BFGS")),
            data = data1, family = "poisson")
summary(m1)
rsquared(m1)

fig2c<-ggplot(data1, aes(x=folhas, y=galhascaul)) +
  geom_point(colour="darkgreen",
             fill = "black",
             size = 3,
             position = position_jitter(width = 0.1, height = 0.05),
             alpha = 0.4) +
  geom_smooth(method='glm', method.args=list(family="poisson"), color="black")+
  scale_y_continuous(expand=c(0,0), limits=c(-0,25), breaks=seq(0,25,5)) +
  scale_x_continuous(expand=c(0,0), limits=c(0,45), breaks=seq(0,45,5)) +
  theme_classic() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 16),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(x = "Leaves biomass per plant (g)", y="Numer of stem galls per plant") +
  theme(legend.position = "NONE") +
  annotate(geom="text", x=1.6, y=23.5, label="C)",
         color="black", size = 10, fontface = "bold")

#Biomassa dos caules----
m1<-glmmTMB(galhascaul ~ caule + (1|planta/ponto), 
            control=glmmTMBControl(optimizer=optim,
                                   optArgs=list(method="BFGS")),
            data = data1, family = poisson)

summary(m1)
rsquared(m1)

fig2d<-ggplot(data1, aes(x=caule, y=galhascaul)) +
  geom_point(colour="darkgreen",
             fill = "black",
             size = 3,
             position = position_jitter(width = 0.1, height = 0.05),
             alpha = 0.4) + 
  geom_smooth(method='glm', method.args=list(family="poisson"), color="black")+
  scale_y_continuous(expand=c(0,0), limits=c(-0,25), breaks=seq(0,25,5)) +
  scale_x_continuous(expand=c(0,0), limits=c(0,75), breaks=seq(0,75,5)) +
  theme_classic() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 16),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(x = "Stem biomass per plant (g)", y="Numer of stem galls per plant") +
  theme(legend.position = "NONE") +
  annotate(geom="text", x=2.5, y=23.5, label="D)",
         color="black", size = 10, fontface = "bold")


#Biomassa dos total----
m1<-glmmTMB(galhascaul ~ massatotal + (1|planta/ponto), 
            control=glmmTMBControl(optimizer=optim,
                                   optArgs=list(method="BFGS")),
            data = data1, family = poisson)

summary(m1)
rsquared(m1)

fig2e<-ggplot(data1, aes(x=massatotal, y=galhascaul)) +
  geom_point(colour="darkgreen",
             fill = "black",
             size = 3,
             position = position_jitter(width = 0.1, height = 0.05),
             alpha = 0.4) + 
  geom_smooth(method='glm', method.args=list(family="poisson"), color="black")+
  scale_y_continuous(expand=c(0,0), limits=c(-0,25), breaks=seq(0,25,5)) +
  scale_x_continuous(expand=c(0,0), limits=c(0,75), breaks=seq(0,75,5)) +
  theme_classic() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 16),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(x = "Plant biomass (g)", y="Numer of stem galls per plant") +
  theme(legend.position = "NONE") +
  annotate(geom="text", x=2.5, y=23.5, label="E)",
           color="black", size = 10, fontface = "bold")

fig2e

fig2a + fig2b + fig2c + fig2d + fig2e
