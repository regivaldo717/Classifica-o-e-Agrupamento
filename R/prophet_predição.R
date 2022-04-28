#análise de dados COvid
install.packages("esquisse")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("data.table")
install.packages("prophet")
install.packages("tidyverse")

library(tidyverse)
library(data.table)
library(prophet)
library(esquisse)
library(dplyr)
library(ggplot2)
tacovid<- read.csv2("C:/Users/baixi/OneDrive/Doutorado/Classificação e agrupamento/dados/HIST_PAINEL_COVIDBR_2021_Parte1_17mar2022.csv")

#View(tacovid)
brasil<- tacovid[1:362,]
#View(brasil)

str (brasil)
head(brasil)
tail(brasil)

View(tacovid)

norte<- filter(tacovid, tacovid$regiao == "Norte")
sul<- filter(tacovid, tacovid$regiao == "Sul")
centro<- filter(tacovid,tacovid$regiao == "Centro-Oeste")
nordeste<- filter(tacovid,tacovid$regiao == "Nordeste")
sudeste<- filter(tacovid,tacovid$regiao == "Sudeste")

View (nordeste)

brasil$estado = NULL
brasil$municipio = NULL
brasil$codmun = NULL
brasil$coduf = NULL
brasil$codRegiaoSaude = NULL
brasil$nomeRegiaoSaude = NULL
brasil$interior.metropolitana = NULL

brasilia<- filter(centro, centro$municipio == "Brasilia") #3 094 325
salvador<- filter(nordeste, nordeste$municipio == "Salvador"  ) #2 900 319
fortaleza<- filter(nordeste, nordeste$municipio == "Fortaleza") #2 703 391
belohiz<- filter(sudeste, sudeste$municipio == "Belo Horizonte") #2 530 701
rm (brasilia)

#popul<- c(brasilia,salvador,fortaleza,belohiz)
ggplot(brasil, aes(y = brasil$casosNovos, x = brasil$data))+ geom_line() 

ggplot(salvador, aes(y = salvador$casosNovos, x = salvador$data))+ geom_line() 

esquisser(popul)
esquisser(brasil)

View(saldf)
#prophet
#tratamento do df salvador

saldf<- (salvador [,8:12] )
saldf<- (saldf [,-2:-4])
view(saldf)
saldf1<- saldf [,1]
saldf2<- saldf[,2]
names(saldf)[names(saldf) == "data"] <- "ds"
names(saldf)[names(saldf) == "casosNovos"] <- "y"
view (saldf)


head(saldf)
yearly.seasonallity = FALSE
daily.seasonallity = T
model<- prophet(saldf)
future<- make_future_dataframe(model, periods = 20)
tail(future)
tail
#teste 
forecast<- predict(model,future)

dyplot.prophet(model, forecast)

#class(salvador)
#plots
brasil %>%
 filter(casosNovos >= 87000L & casosNovos <= 116000L) %>%
 ggplot() +
 aes(x = data, weight = casosNovos) +
 geom_bar(fill = "#112446") +
 theme_minimal()

filter()
