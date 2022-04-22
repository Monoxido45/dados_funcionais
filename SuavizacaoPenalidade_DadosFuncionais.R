library(tidyverse)
library(fda)
library(refund)
library(mgcv)
if(!require(gratia)){install.packages("gratia")}
library(gratia)

if(!require(reshape2)){install.packages("reshape2")}
library(reshape2)
if(!require(ggpubr)){install.packages("ggpubr")}
library(ggpubr)
if(!require("ggplot2")){install.packages("ggplot2")}
library(ggplot2)
if(!require("RColorBrewer")){install.packages("RColorBrewer")}
library(RColorBrewer)


clima_dados <- CanadianWeather$dailyAv[,,1] |>
  as.data.frame() |> 
  dplyr::select(Vancouver) |>
  mutate(day = 1:365)

#####
# Para diferentes métodos
#####

clima_dados_pred = clima_dados
metodos = c("REML","P-REML","GCV.Cp","GACV.Cp")

for(i in 1:length(metodos)){
  
  clima_dados_pred = cbind(clima_dados_pred,
                           gam(Vancouver ~ s(day, bs = "cr"), 
                               method = metodos[i] ,
                               data = clima_dados)$fitted.values)
  
}

names(clima_dados_pred)[3:6]=metodos

clima_dados_pred2 = clima_dados_pred |>
  melt(id=c("Vancouver","day"))

ggplot(clima_dados_pred2,
       aes(x=day,y=value,group=variable, color=variable))+
  geom_point(aes(x=day,y=Vancouver),
             colour = "dodgerblue3", alpha = 0.3, size = 1)+
  geom_line(size=1.2)+
  theme_bw()+
  scale_color_brewer(palette ="Dark2")+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 14, 
                            family ="serif"))+
  labs(y = "Temperatura (ºC)",
       x = "Dia",
       colour = "Estação",
       title = "Penalização por diferentes métodos")




clima_dados_pred[1:5,]

xtable::xtable(clima_dados_pred[1:5,],
               digits=5)



#####
# Primeira e segunda derivada
#####

modelo_gam = gam(Vancouver ~ s(day, bs = "cr"), 
                  method = "REML" ,
                  data = clima_dados) 

clima_dados_deri = 
  clima_dados |>
  mutate(fd_d1=derivatives(modelo_gam,n=365,order=1L)$derivative,
         fd_d2=derivatives(modelo_gam,n=365,order=2L)$derivative,
         Predito=modelo_gam$fitted.values)


p1 =
  ggplot(clima_dados_deri,
         aes(x=day,y=Predito))+
  geom_point(aes(x=day,y=Vancouver),
             colour = "dodgerblue3", alpha = 0.3, size = 1)+
  geom_line(size=1.2)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 14, 
                            family ="serif"))+
  labs(y = "Temperatura (ºC)",
       x = "Dia",
       colour = "Estação",
       title = "Suaviação com penalização (REML)")


p2 =
  ggplot(clima_dados_deri,
         aes(x=day,y=fd_d1))+
  geom_line(size=1.2)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 14, 
                            family ="serif"))+
  labs(y = "Temperatura (ºC)",
       x = "Dia",
       colour = "Estação",
       title = "Primeira Derivada")

p3 =
  ggplot(clima_dados_deri,
         aes(x=day,y=fd_d2))+
  geom_line(size=0.7,
            colour = "dodgerblue3", alpha = 0.8)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 14, 
                            family ="serif"))+
  labs(y = "Temperatura (ºC)",
       x = "Dia",
       colour = "Estação",
       title = "Segunda Derivada")



ggarrange(p1,p2)
ggarrange(p1,p3)



#####
# Diferente estacoes
#####

clima_dados_estacoes <- CanadianWeather$dailyAv[,,1] |>
  as.data.frame() |>
  mutate(day = 1:365) |>
  dplyr::select(1:10,
                Vancouver,
                day) |>
  rename("St.Johns" = "St. Johns")
  
clima_dados_estacoes_pred = 
  clima_dados_estacoes


for(i in 1:11){
  
  clima_dados_estacoes_pred[,i] = 
    gam(as.formula(paste(names(clima_dados_estacoes)[1:11][i],
                                          " ~ s(day, bs = 'cr')")), 
        method = "REML" ,
        data = clima_dados_estacoes[,c(i,12)])$fitted.values
  
}


getPalette = colorRampPalette(brewer.pal(9, "Set1"))

clima_dados_estacoes_pred |>
  melt(id=c("day")) |>
  ggplot(aes(x=day,y=value,
             color=variable))+
  geom_line(size=1.2)+
  scale_color_manual(values=getPalette(11))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 14, 
                            family ="serif"))+
  labs(y = "Temperatura (ºC)",
       x = "Dia",
       colour = "Estação",
       title = "Suavização REML em diferentes estacões")


