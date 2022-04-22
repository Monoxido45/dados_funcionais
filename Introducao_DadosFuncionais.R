#####
# Pacotes
#####

if(!require(fda)){install.packages("fda")}
library(fda)


if(!require(dplyr)){install.packages("dplyr")}
library(dplyr)
if(!require(reshape2)){install.packages("reshape2")}
library(reshape2)

if(!require("ggplot2")){install.packages("ggplot2")}
library(ggplot2)
if(!require("RColorBrewer")){install.packages("RColorBrewer")}
library(RColorBrewer)


if(!require(xtable)){install.packages("xtable")}
if(!require("datasets")){install.packages("datasets")}

#####
# Exemplo CANCER
#####

data(Cancerrate)
plot(Cancerrate)

dados_cancer = Cancerrate$y |> 
  melt() |>
  mutate(Var2 = factor(Var2))

names(dados_cancer) =
  c("Idade","Ano","TaxaCancer")



qplot(Idade, TaxaCancer, data=dados_cancer, col=Ano) +
  geom_line() +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 14, 
                            family ="serif"),
        legend.position = "none")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 9))+
  labs(x = "Idade", 
       y = "Taxa de Câncer de Mama", 
       title = "",
       col = "Ano")


xtable(dados_cancer[c(1:9,41:43,729),c(1:3)])

#####
# Exemplo CO2
#####


qplot(conc, uptake, data=datasets::CO2, col=Plant) +
  geom_line() +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 14, 
                            family ="serif"))+
  facet_grid()+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 9))+
  labs(x = expression(CO[2]~"(mL/L)"), 
       y = "Uptake", 
       title = "",
       col = "Planta") +
  scale_color_manual(values = c(brewer.pal(8,'Dark2'),
                                brewer.pal(8,'Blues')[-c(1:3)]))


xtable(dados_CO2[c(1:8,41:44,84),c(1,3,5)])



#####
# Phase Plane Plot
#####

ggplot(data=data.frame(t=seq(1919,2000,len=length(nondurables)),
                       IndexLog=log10(nondurables)),
       aes(x=t,y=IndexLog))+
  geom_line()+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 14, 
                            family ="serif"))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 9))+
  labs(x = "Anos", 
       y = "Log do índice de bens não duráveis", 
       title = "")



dados_phase_plot = function(t){
  
  goodsbasis <- create.bspline.basis(rangeval=c(1919,2000),
                                     nbasis=161, norder=8)
  LfdobjNonDur <- int2Lfd(4)
  argvals = seq(1919,2000,len=length(nondurables))
  logNondurSm <- smooth.basisPar(argvals,
                                 y=log10(nondurables), fdobj=goodsbasis,
                                 Lfdobj=LfdobjNonDur, lambda=1e-11)
  
  dados = phaseplanePlot(t, logNondurSm$fd) |>
    as.data.frame()
  names(dados) = c("dx","dx2")
  return(dados)
}


legenda_phase_plot = function(t){
  dados = dados_phase_plot(t=ano)
  indices = round(seq(1,nrow(dados),nrow(dados)/12),digits=0)
  
  return(data.frame(x=dados[indices,1],
                    y=dados[indices,2],
                    label=names(fda::monthLetters)))
}

ano = 1964
ggplot(data=dados_phase_plot(t=ano),
       aes(x=dx,y=dx2))+
  geom_path()+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 14, 
                            family ="serif"))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 9))+
  geom_text(aes(y=y,label=label,x=x),
            data=legenda_phase_plot(t))+
  labs(x = "x' (velocidade)",
       y = "x'' (aceleração)",
       title = paste("Phase Plane para o ano de",ano))



ano = 1994
ggplot(data=dados_phase_plot(t=ano),
       aes(x=dx,y=dx2))+
  geom_path()+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 14, 
                            family ="serif"))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 9))+
  geom_text(aes(y=y,label=label,x=x),
            data=legenda_phase_plot(t))+
  labs(x = "x' (velocidade)",
       y = "x'' (aceleraçao)",
       title = paste("Phase Plane para o ano de",ano))
