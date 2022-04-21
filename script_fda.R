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


# usando tambem b-splines cubicas
Xbasis_vanc_bs <- create.bspline.basis(rangeval = 
                                         range(vancouver_dados$day),
                                   nbasis = 13, norder = 4) |>
  eval.basis(vancouver_dados$day)

# ajustando o modelo de suavizacao com b-splines
vanc_lm_bs <- lm(vancouver_dados$Vancouver ~ 0 + Xbasis_vanc_bs)
  
  
vancouver_dados <- vancouver_dados |>
    mutate(y_fit = vanc_lm_bs$fitted.values)
  
coef <- vanc_lm_bs$coefficients
  
# graficos da curva suave e da derivada estimada
p1 <- vancouver_dados |>
    ggplot(aes(x = day, y = Vancouver)) +
    geom_point(colour = "dodgerblue3", alpha = 0.6)+
    geom_line(aes(y = y_fit), colour = "red") +
    labs(y = "Log-precipitação",
         x = "Dia",
         title = "Suavização com 13 b-splines")
  
# objeto FD
yfitfd <- fd(coef,create.bspline.basis(rangeval = 
                                      range(vancouver_dados$day),
                                       nbasis = 13, norder = 4))
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
ggsave("b_splines_ajuste_vancouver.pdf", 
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
K_grid <- 3:45
Xbasis <- create.fourier.basis(range(vancouver_dados$day), 
                                    nbasis = max(K_grid), 
                               period = 365) |>
  eval.basis(vancouver_dados$day)

y <- vancouver_dados$Vancouver

CV_k <- function(K_grid, Xbasis, y){
  n <- Xbasis |> nrow()
  CV <- numeric(length(K_grid))
  
  for(j in 1:length(K_grid)){
    K <- K_grid[j]
    Xb_temp <- Xbasis[, 1:K]
    CV_err <- numeric(n)
    for(i in 1:n){
      # conjunto de validacao
      Xbasis_val <- Xb_temp[j, ]
      y_val <- y[i]
      
      # conjunto de treino
      Xbasis_train <-  Xb_temp[-i, ]
      y_train <- y[-i]
      
      lm_fit <- lm(y_train ~ 0 + Xbasis_train)
      y_fit <- Xbasis_val %*% lm_fit$coefficients |> as.numeric()
      CV_err[i] <- (y_val - y_fit)^2
    }
    CV[j] <- mean(CV_err)
  }
  return(CV)
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



# suavizacao por kernel ---------------------------------------------------
# temperatura diaria de vancouver
clima_dados <- CanadianWeather$dailyAv[,,1] |>
  as.data.frame() |> 
  dplyr::select(Vancouver) |>
  mutate(day = 1:365)

# ajuste do ksmooth
# testando varias bandas
fit_gauss <- c(1, 10, 30) |>
  setNames(c("1", "10", "30")) |>
  map_dfr(function(.x){ksmooth(clima_dados$day, 
                    clima_dados$Vancouver, 
                    kernel = c("normal"), bandwidth = .x)
      }, .id = "banda")

fit_gauss |> left_join(clima_dados,
                       by = c("x" = "day")) |>
  ggplot(aes(x = x, y = Vancouver)) +
  geom_point(colour = "dodgerblue3", alpha = 0.3, size = 0.5)+
  geom_line(aes(y = y, colour = banda)) +
  scale_colour_manual(values = rainbow(3)) +
  labs(y = "Temperatura (ºC)",
       x = "Dia",
       colour = "h",
       title = "Suavização por kernel para o clima na estação de Vancouver")

ggsave("kernel_vancouver.pdf",
       path = "figuras",
       width = 6, height = 4)


clima_dados <- CanadianWeather$dailyAv[,,1] |>
  as.data.frame() |>
  mutate(day = 1:365) |>
  dplyr::select(1:10,
         Vancouver,
         day) |>
  pivot_longer(1:11, names_to = "vars", 
               values_to = "valores") |>
  group_split(vars)


clima_all_dados <- clima_dados |>
  map_dfr(function(.x){ksmooth(.x$day, 
                              .x$valores, 
                              kernel = c("normal"), bandwidth = 20) |>
      as.data.frame() |>
      mutate(var = .x$vars)
  })

clima_all_dados |>
  ggplot(aes(x = x, y = y, colour = var)) +
  geom_line() +
  scale_colour_manual(values = rainbow(11)) +
  labs(y = "Temperatura (ºC)",
       x = "Dia",
       colour = "Estação",
       title = "Suavização por kernel para 11 estações")

ggsave("kernel_estacoes.pdf",
       path = "figuras",
       width = 6, height = 4)


# escolhendo h pelo leave-one-out
CV_h <- function(h_grid, x, y){
  n <- x |> length()
  CV <- numeric(length(h_grid))
  
  for(j in 1:length(h_grid)){
    CV_err <- numeric(n)
  for(i in 1:n){
    # conjunto de validacao
    x_val <- x[i]
    y_val <- y[i]
    
    # conjunto de treino
    x_tr <- x[-i]
    y_tr <- y[-i]
    
    y_val_predict = ksmooth(x = x_tr, y = y_tr,
                            kernel = "normal",
                            bandwidth = h[j],
                            x.points = x_val)
    CV_err <- (y_val - y_val_predict$y)^2
  }
    CV[j] <- mean(CV_err)
  }
  return(CV)
}

clima_dados <- CanadianWeather$dailyAv[,,1] |>
  as.data.frame() |> 
  dplyr::select(Vancouver) |>
  mutate(day = 1:365)

h <- seq(1, 30, 0.1)
cv_h <- CV_h(h, clima_dados$day, clima_dados$Vancouver)

data.frame(h, cv_h) |>
  ggplot(aes(x = h, y = cv_h))+
  geom_line(colour = "dodgerblue3") +
  labs(y = "Erro da validação cruzada",
       x = "Largura de banda h",
       title = "Erro da validação cruzada para h")+
  scale_x_continuous(breaks = scales::pretty_breaks(10))+
  scale_y_continuous(breaks = scales::pretty_breaks(6))

ggsave("h_valid_cruzada_ks.pdf",
       path = "figuras",
       width = 6, height = 4)



# suavizacao por kernel polinomial ----------------------------------------
# temperatura diaria de vancouver
clima_dados <- CanadianWeather$dailyAv[,,1] |>
  as.data.frame() |> 
  dplyr::select(Vancouver) |>
  mutate(day = 1:365)

h <- 6
lp1 <- KernSmooth::locpoly(x = clima_dados$day, 
                           y = clima_dados$Vancouver, 
                           bandwidth = h, degree = 4,
                           range.x = c(1, 365), gridsize = 365)


p1 <- lp1 |>
  as.data.frame() |>
  left_join(clima_dados,
             by = c("x" = "day")) |>
  ggplot(aes(x = x, y = Vancouver)) +
  geom_point(colour = "dodgerblue3", alpha = 0.3, size = 0.5)+
  geom_line(aes(y = y), colour = "dodgerblue3") +
  scale_colour_manual(values = rainbow(3)) +
  labs(y = "Temperatura (ºC)",
       x = "Dia",
       colour = "h",
       title = "Suavização por polinômios locais para o clima de Vancouver")

lpd <- KernSmooth::locpoly(x = clima_dados$day, 
                           y = clima_dados$Vancouver, 
                           bandwidth = h, degree = 4,
                           drv = 2,
                           range.x = c(1, 365), gridsize = 365)


p2 <- lpd |>
  as.data.frame() |>
  ggplot(aes(x = x, y = y)) +
  geom_line(colour = "dodgerblue3") +
  scale_colour_manual(values = rainbow(3)) +
  labs(y = "Temperatura (ºC)",
       x = "Dia",
       colour = "h",
       title = "Derivada de ordem 2")


ggpubr::ggarrange(p1, p2)
ggsave("poly_loc_fit_deriv.pdf", 
       path = "figuras",
       width = 11, height = 6)
