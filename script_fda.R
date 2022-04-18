# pacotes sendo utilizados
library(tidyverse)
library(fda)
library(refund)


# Ilustrando as bases -----------------------------------------------------
# 11 componentes da base de fourier no intervalo de 0 a 1 com periodo 1
fbase <- create.fourier.basis(rangeval = c(0, 1),
                              nbasis = 30, period = 1) |>
  eval.basis(seq(0, 1, length.out = 300)) |>
  as.data.frame() |>
  mutate(x_grid = seq(0, 1, length.out = 300)) |>
  pivot_longer(const:cos3, names_to = "comp_base", values_to = "valores")

fbase |>
  ggplot(aes(x = x_grid, y = valores, colour = comp_base)) +
  geom_line() +
  scale_colour_manual(values = rainbow(7)) +
  labs(title = "Primeiros 7 componentes da base de fourier",
       y = "Valor",
       x = "x",
       colour = "Componente")

ggsave("fourier_base.pdf", 
       path = "figuras",
       width = 6, height = 4)

# B-splines lineares e cubicas com K = 10
bsbase_cub <- create.bspline.basis(rangeval = c(0, 1),
                              nbasis = 10, norder = 4) |>
  eval.basis(seq(0, 1, length.out = 300)) |>
  as.data.frame() |>
  mutate(x_grid = seq(0, 1, length.out = 300)) |>
  pivot_longer(bspl4.1:bspl4.10, 
               names_to = "comp_base", values_to = "valores")

p1 <- bsbase_cub |>
  ggplot(aes(x = x_grid, y = valores, colour = comp_base)) +
  geom_line() +
  scale_colour_manual(values = rainbow(10)) +
  labs(title = "B-spline de ordem 4 com K = 10",
       y = "Valor",
       x = "x")+
  theme(legend.position="none")

bsbase_lin <- create.bspline.basis(rangeval = c(0, 1),
                                   nbasis = 10, norder = 2) |>
  eval.basis(seq(0, 1, length.out = 300)) |>
  as.data.frame() |>
  mutate(x_grid = seq(0, 1, length.out = 300)) |>
  pivot_longer(bspl2.1:bspl2.10, 
               names_to = "comp_base", values_to = "valores")

p2 <- bsbase_lin |>
  ggplot(aes(x = x_grid, y = valores, colour = comp_base)) +
  geom_line() +
  scale_colour_manual(values = rainbow(10)) +
  labs(title = "B-spline de ordem 2 com K = 10",
       y = "Valor",
       x = "x")+
  theme(legend.position="none")


ggpubr::ggarrange(p2, p1, nrow = 2)
ggsave("b_splines_base.pdf", 
       path = "figuras",
       width = 8, height = 6)

# monomios
monbase <- create.monomial.basis(rangeval = c(0, 1),
                                   nbasis = 6) |>
  eval.basis(seq(0, 1, length.out = 300)) |>
  as.data.frame() |>
  mutate(x_grid = seq(0, 1, length.out = 300)) |>
  pivot_longer(monomial0:monomial5, 
               names_to = "comp_base", values_to = "valores")

monbase|>
  ggplot(aes(x = x_grid, y = valores, colour = comp_base)) +
  geom_line() +
  scale_colour_manual(values = rainbow(6)) +
  labs(title = "Base de monomios K = 6",
       y = "Valor",
       x = "x")+
  theme(legend.position="none")

ggsave("mon_base.pdf", 
       path = "figuras",
       width = 6, height = 4)



# minimos quadrados ordinarios para suavizacao ----------------------------
data("CanadianWeather")
vancouver_dados <- CanadianWeather$dailyAv[,,3] |>
  as.data.frame() |> 
  dplyr::select(Vancouver) |>
  mutate(day = 1:365)


# usando uma base de fourier
nbasis <-  5
Xbasis_vanc <- create.fourier.basis(range(vancouver_dados$day), 
                                nbasis = nbasis, period = 365) |>
  eval.basis(vancouver_dados$day)

# ajustando o modelo de suavizacao
vanc_lm <- lm(vancouver_dados$Vancouver ~ 0 + Xbasis_vanc)


vancouver_dados <- vancouver_dados |>
  mutate(y_fit = vanc_lm$fitted.values)

coef <- vanc_lm$coefficients

# graficos da curva suave e da derivada estimada
p1 <- vancouver_dados |>
  ggplot(aes(x = day, y = Vancouver)) +
  geom_point(colour = "dodgerblue3", alpha = 0.6)+
  geom_line(aes(y = y_fit), colour = "red") +
  labs(y = "Log-precipitação",
       x = "Dia",
       title = "Suavização com 5 funções de fourier")

# objeto FD
yfitfd <- fd(coef,create.fourier.basis(range(vancouver_dados$day), 
                                       nbasis = nbasis, period = 365))
# segunda derivada para cada dia
yfit2D <- eval.fd(vancouver_dados$day, yfitfd, 2)

p2 <- vancouver_dados |>
  mutate(yfit2D = yfit2D[, 1]) |>
  ggplot(aes(x = day, y = yfit2D))+
  geom_line(colour = "dodgerblue3")+
  labs(y = "Derivada segunda da Log-precipitação",
       x = "Dia",
       title = "Segunda derivada estimada")


ggpubr::ggarrange(p1, p2)
ggsave("fourier_ajuste_vancouver.pdf", 
       path = "figuras",
       width = 10, height = 6)

# ajustando a precipitacoes de varias regioes
precip_dados <- CanadianWeather$dailyAv[,,3] |>
  as.data.frame() |>
  dplyr::select(1:5,
                Vancouver) |>
  mutate(day = 1:365) |>
  pivot_longer(1:6, names_to = "vars", 
               values_to = "valores") |>
  group_split(vars)


precip_all_dados <- precip_dados |>
  map_dfr(function(.x){
    vanc_lm <- lm(.x |>  pull(valores) ~
                    0 + Xbasis_vanc)
    
    .x |>
      mutate(y_fit = vanc_lm$fitted.values)
  })

precip_all_dados |>
  ggplot(aes(x = day, y = y_fit, colour = vars)) +
  geom_line() +
  scale_colour_manual(values = rainbow(6)) +
  labs(y = "Log da precipitação",
       x = "Dia",
       colour = "Estação",
       title = "Suavização para precipitação de 5 outras estações")

ggsave("fourier_curvas_estacoes.pdf", 
       path = "figuras",
       width = 6, height = 4)

# validacao cruzada para vancouver
K_grid <- 3:24
Xbasis <- create.fourier.basis(range(vancouver_dados$day), 
                                    nbasis = max(K_grid), 
                               period = 365) |>
  eval.basis(vancouver_dados$day)

y <- vancouver_dados$Vancouver

CV_k <- function(K_grid, Xbasis, y){
  
CV <- matrix(0, nrow = vancouver_dados |> nrow(),
             ncol=length(K_grid))
n <- Xbasis |> nrow()

for(j in 1:n){
  
  y_temp <- y[-j]
  # fit using Fourier basis and K basis functions
  index <- 0
  for (K in K_grid){
    index <- index + 1
    Xb_temp <- Xbasis[, 1:K]
    Xbasis_j <-  Xb_temp[-j, ]
    lm_fit <- lm(y_temp ~ 0 + Xbasis_j)
    y_fit <- Xb_temp[j, ] %*% lm_fit$coefficients |> as.numeric()
    CV[j,index] <- (y[j] - y_fit)^2
  }
}
CV_L2 <- apply(CV, 2, sum)
return(CV_L2)
}

data.frame(k = K_grid,
           cv = CV_k(K_grid, Xbasis, y)) |>
  ggplot(aes(x = k, y = cv)) +
  geom_point(colour = "dodgerblue3", alpha = 0.45)+
  geom_line(colour = "dodgerblue3")+
  labs(x = "Número de funções de base",
       y = "Erro da validação cruzada",
       title = "Erro da validação cruzada para cada K")

ggsave("valid_cruzada_vancouver.pdf", 
       path = "figuras",
       width = 6, height = 4)

