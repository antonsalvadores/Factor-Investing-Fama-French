###############################################################################
################################################################################
#                                ECONOMETRÍA                                   #
################################################################################
################################################################################
################################################################################
#                       ALEJANDRO MARTÍNEZ CASADO                              #
#                         RODRIGO SALORT ANTÓN                                 #
#                        ANTÓN SALVADORES MÚÑIZ                                #
#                       ALFONSO SAN MIGUEL VILLAR                              #
################################################################################
################################################################################
################################################################################


library(ggplot2)
library(ggh4x)
library(ggforce)
library(latex2exp)
library(patchwork)
library(tidyverse)
library(rgl)
library(formatR)
library(kableExtra)
library(magrittr)
library(tidyfinance)
library(tidyquant)
library(scales)
library(RColorBrewer)
library(gt)
library(gtsummary)
library(highcharter)
library(slider)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(frenchdata)
library(stargazer)
library(tidyverse)
library(tidyfinance)
library(scales)
library(slider)
library(sjPlot)
library(sjmisc)
library(sjlabelled)


# Limpiar workspace
rm(list = ls(all = TRUE))
#Fijar work directory
setwd("C:/MÁSTER QFB/ECONOMETRÍA FINANCIERA 1º 2T/AGUEDA MADOZ/TRABAJO")
rm(list = ls(all = TRUE))


################################################################################
#                            1. Importación Datos                              #
################################################################################

start_date <- ymd("1991-01-01")
end_date <- ymd("2023-12-30")
set.seed(1234)

time_series_months <- seq(start_date, end_date, "1 month")

# Importamos 3 factores web de French
library(frenchdata)
factors_ff3_monthly_raw <- download_french_data("Fama/French European 3 Factors")
factors_ff3_monthly <- factors_ff3_monthly_raw$subsets$data[[1]] |>
  mutate(date = floor_date(ymd(str_c(date, "01")), "month"),
         across(c(RF, `Mkt-RF`, SMB, HML), ~as.numeric(.)/100),
         .keep = "none") |>
  rename_with(str_to_lower) |>
  rename(mkt_excess = `mkt-rf`) |>
  filter(date >= start_date & date <= end_date)

# Importamos Momentum web de French
momentum_raw <- download_french_data("European Momentum Factor (Mom)")
momentum <- momentum_raw$subsets$data[[1]] |>
  mutate(date = floor_date(ymd(str_c(date, "01")), "month"),
         across(c(WML), ~as.numeric(.)/100),
         .keep = "none") |>
  rename_with(str_to_lower) |>
  filter(date >= start_date & date <= end_date)

# Juntamos datos quitando rf
df_factores <- factors_ff3_monthly |>
  left_join(momentum, join_by(date)) |> 
  select(-c(rf))


# Importamos las 25 Carteras de la web de French
carteras_raw <- download_french_data("25 European Portfolios Formed on Size and Book-to-Market (5 x 5)")
carteras <- carteras_raw$subsets$data[[1]] |>
  mutate(date = floor_date(ymd(str_c(date, "01")), "month"),
         across(seq(2, 26), ~as.numeric(.)/100),
         .keep = "none") |>
  rename(`ME1 BM1` = "SMALL LoBM") |>
  rename(`ME1 BM5` = "SMALL HiBM") |>
  rename(`ME5 BM1` = "BIG LoBM" ) |>
  rename(`ME5 BM5` = "BIG HiBM" ) |>
  filter(date >= start_date & date <= end_date)

# Juntamos los factores y carteras 
## Carhart
datos <- df_factores |>
  left_join(carteras, join_by(date)) 

## 3 factores
datos3f <- factors_ff3_monthly |>
  left_join(carteras, join_by(date)) |>
  select(-c(rf)) 

# Cambiamos forma tabla
df <- datos |>
  pivot_longer(cols = -c("date", "mkt_excess", "smb", "hml", "wml")) |>
  rename(ret_excess = value)

df_3f <- datos3f |>
  pivot_longer(cols = -c("date", "mkt_excess", "smb", "hml")) |>
  rename(ret_excess = value)

# Representación rentabilidades carteras

library(ggplot2)
library(dplyr)
library(ggrepel)
library(RColorBrewer)

df |>
  ggplot(aes(x = date, y = ret_excess, color = name)) +
  geom_line() +
  labs(
    x = NULL,
    y = NULL,
    color = "Carteras",
    title = "Rendimientos carteras"
  ) +
  theme_minimal() +
  theme(legend.position = "right") +
  scale_color_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(25)) +
  geom_text_repel(
    data = . %>%
      group_by(name) %>%
      drop_na() %>%
      filter(date == max(date) & !(name %in% colnames(carteras[seq(2,26)]))),
    aes(label = name),
    nudge_x = 0.5,
    nudge_y = 0.01,
    size = 3,
    segment.color = "grey50"
  )

# Creamos un gráfico de rentabilidades acumuladas con la misma inversión
# en los distintos activos descargados

# Rentabilidades acumuladas

library(dplyr)

df_acumulado <- df %>%
  arrange(name, date) %>%               # Asegura el orden cronológico dentro de cada cartera
  group_by(name) %>%                    # Agrupa por cartera
  mutate(wealth = cumprod(1 + ret_excess))  # Calcula la rentabilidad acumulada

df_acumulado %>%
  arrange(name, date) %>%  # Aseguramos el orden correcto
  ggplot(aes(x = date, y = wealth, color = name)) +
  geom_line() +
  labs(
    x = NULL,
    y = "Valor de 1€ invertido",
    color = "Carteras",
    title = "Evolución de 1€ invertido: Rentabilidad acumulada"
  ) +
  theme_minimal() +
  theme(legend.position = "right") +
  # Utilizamos 25 colores oscuros interpolados a partir de la paleta Dark2
  scale_color_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(25)) +
  # Añadimos etiquetas en el último punto de cada cartera
  geom_text_repel(
    data = . %>%
      group_by(name) %>%
      filter(date == max(date)),
    aes(label = name),
    nudge_x = 0.5,
    nudge_y = 0.01,
    size = 3,
    segment.color = "grey50"
  )


# Características datos
df |>
  select(mkt_excess, smb, hml,wml) |>
  summary()

carteras |>
  select(-c(date)) |>
  summary()

factores <- df %>% select(mkt_excess, smb, hml, wml)
correlation_matrix <- cor(factores)


# Momentum

df |>
  ggplot(aes(x = lag(ret_excess), y = ret_excess)) +
  geom_point(size=0.1) + facet_wrap(~name,
                              scales = "free") + geom_abline(aes(intercept = 0, slope = 1),
                             linetype = "dashed") + labs(x = "Rendimientos del día anterior",
                                                         y = "Rendimientos", title = "Rendmientos entre días consecutivos") +
  scale_x_continuous(labels = unit_format(unit = "%", scale = 1e-01)) +
  scale_y_continuous(labels = unit_format(unit = "%", scale = 1e-01))


################################################################################
#                               2. 3 Factores FF                               #
################################################################################


fit <- df |>
  group_by(name) |>
  do(model_lm = lm(ret_excess ~ mkt_excess + smb + hml, data = .)) |>
  mutate(alpha = model_lm$coefficients[1], 
         beta_m = model_lm$coefficients[2],
         beta_smb = model_lm$coefficients[3], 
         beta_hml = model_lm$coefficients[4])

# Resultados regresión
library(stargazer)
df %>%
  group_nest(name) %>%
  mutate(model = map(data, ~lm(ret_excess ~ mkt_excess + smb + hml, data = .))) %>%
  pull(model) %>%
  stargazer(title = "Regression Results 3 Factors", type = "html", out = "all_regressions_3f.html")

# p-values
p_all <- NULL
for (i in 1:25) {
  p_all[i] <- broom::glance(summary(fit[[2]][[i]]))$p.value
}

p_all

# Heterocedasticidad -> Contraste White 
library("skedastic")

fit_wh <- df_3f |>
  group_by(name) |>
  do(model_lm = lm(ret_excess ~ mkt_excess + smb + hml, data = .),
     skedastic_package_white = white(mainlm = lm(ret_excess ~ 
     mkt_excess + smb + hml, data = .), interactions = TRUE)) |>
  mutate(p_value_white = skedastic_package_white$p.value)


# Heterocedasticidad -> Contraste Breusch-Pagan

library("lmtest")
fit_bp <- df |>
  group_by(name) |>
  do(model_lm = lm(ret_excess ~ mkt_excess + smb + hml, data = .),
     lmtest_package_bp = bptest(formula = lm(ret_excess ~
                                               mkt_excess + smb + hml, data = .), studentize = TRUE)) |>
  mutate(p_value_bp = lmtest_package_bp$p.value)

# Autocorrelación -> Breusch-Godfrey

fit_bg <- df |>
  group_by(name) |>
  do(model_lm = lm(ret_excess ~ mkt_excess + smb + hml, data = .),
     bgtest_package_bg = bgtest(formula = lm(ret_excess ~
                                               mkt_excess + smb + hml, data = .), order = 12)) |>
  mutate(p_value_bg = bgtest_package_bg$p.value)

# Autocorrelación -> Durbin-Watson 
library("car")
fit_dw <- df |>
  group_by(name) |>
  do(model_lm = lm(ret_excess ~ mkt_excess + smb + hml, data = .),
     durbinWatson_package = durbinWatsonTest(lm(ret_excess ~
                                                  mkt_excess + smb + hml, data = .))) |>
  mutate(p_value_dw = durbinWatson_package$p)


# Normalidad -> Jarque-Bera
library("tseries")
fit_jb <- df |>
  group_by(name) |>
  do(model_lm = lm(ret_excess ~ mkt_excess + smb + hml, data = .)) |>
  mutate(res = list(model_lm$residuals), p_value_jb = jarque.bera.test(res)$p.value)

# Tabla con todo
ff3 <- tibble(cartera = colnames(carteras[-1]), 
       white = fit_wh$p_value_white,
       bp = fit_bp$p_value_bp, 
       bg = fit_bg$p_value_bg,
       dw = fit_dw$p_value_dw,
       jb = fit_jb$p_value_jb)



################################################################################
#                               2.1 Rolling FF3                                #
################################################################################


# Funciones estimación betas
estimate_capm <- function(data, min_obs = 1) {
  if (nrow(data) < min_obs) {
    beta <- as.numeric(NA)
  } else {
    fit <- lm(ret_excess ~ mkt_excess, data = data)
    beta <- as.numeric(coefficients(fit)[2])
  }
  return(beta)
}

estimate_smb <- function(data, min_obs = 1) {
  if (nrow(data) < min_obs) {
    beta <- as.numeric(NA)
  } else {
    fit <- lm(ret_excess ~ smb, data = data)
    beta <- as.numeric(coefficients(fit)[2])
  }
  return(beta)
}

estimate_hml <- function(data, min_obs = 1) {
  if (nrow(data) < min_obs) {
    beta <- as.numeric(NA)
  } else {
    fit <- lm(ret_excess ~ hml, data = data)
    beta <- as.numeric(coefficients(fit)[2])
  }
  return(beta)
}


# Funciones Rolling Beta 
roll_capm_estimation <- function(data, months, min_obs) {
  data <- data |>
    arrange(date)
  
  betas <- slide_period_vec(.x = data, .i = data$date, .period = "month",
                            .f = ~estimate_capm(., min_obs), .before = months - 1,
                            .complete = FALSE)
  
  return(tibble(date = unique(data$date), beta_mark = betas))
}

roll_smb_estimation <- function(data, months, min_obs) {
  data <- data |>
    arrange(date)
  
  betas <- slide_period_vec(.x = data, .i = data$date, .period = "month",
                            .f = ~estimate_smb(., min_obs), .before = months - 1,
                            .complete = FALSE)
  
  return(tibble(date = unique(data$date), beta_smb = betas))
}

roll_hml_estimation <- function(data, months, min_obs) {
  data <- data |>
    arrange(date)
  
  betas <- slide_period_vec(.x = data, .i = data$date, .period = "month",
                            .f = ~estimate_hml(., min_obs), .before = months - 1,
                            .complete = FALSE)
  
  return(tibble(date = unique(data$date), beta_hml = betas))
}

# Betas Estimadas del Rolling
beta_capm <- df_3f |>
  group_by(name) |>
  mutate(roll_capm_estimation(pick(date, ret_excess, mkt_excess),
                              months = 60, min_obs = 60)) |>
  ungroup() |>
  drop_na()

beta_smb <- df_3f |>
  group_by(name) |>
  mutate(roll_smb_estimation(pick(date, ret_excess, smb), months = 60,
                             min_obs = 60)) |>
  ungroup() |>
  drop_na()

beta_hml <- df_3f |>
  group_by(name) |>
  mutate(roll_hml_estimation(pick(date, ret_excess, hml), months = 60,
                             min_obs = 60)) |>
  ungroup() |>
  drop_na()

# Mostrar datos
beta_examples <- beta_capm %>%
  right_join(beta_smb, by = c("date", "mkt_excess", "smb",
                              "hml", "name", "ret_excess"))

beta_examples <- beta_examples %>%
  right_join(beta_hml, by = c("date", "mkt_excess", "smb",
                              "hml", "name", "ret_excess"))

################################################################################
#                               2.1.1 Estabilidad                              #
################################################################################


beta_examples |>
  ggplot(aes(x = date, y = beta_mark)) + geom_line() + 
  facet_wrap(~name, scales = "free")

beta_examples |>
  ggplot(aes(x = date, y = beta_smb)) + geom_line() + 
  facet_wrap(~name, scales = "free")

beta_examples |>
  ggplot(aes(x = date, y = beta_hml)) + geom_line() + 
  facet_wrap(~name,scales = "free")


#Gráficas CUSUM
library(strucchange)

## Mercado
cusum_betas_mkt <- df |>
  group_by(name) |>
  do(cusum = efp(ret_excess ~ mkt_excess, data = ., type = "Rec-CUSUM"))

library(gridExtra)
plot_list <- list()

for (i in 1:25) {
  new.date <- seq(as.Date(paste(c(start_date, 1), collapse = "/")),
                  by = "month", length.out = length(cusum_betas_mkt[[2]][[i]][["process"]]))
  dat <- data.frame(date = new.date, value = cusum_betas_mkt[[2]][[i]][["process"]],
                    bound = boundary(cusum_betas_mkt[[2]][[i]]))
  plot_list[[i]] <- ggplot(dat) + geom_line(aes(x = date, y = value)) +
    geom_line(aes(x = date, y = bound), linetype = "dashed",
              color = "red") + geom_line(aes(x = date, y = -bound),
                                         linetype = "dashed", color = "red") + ylab(NULL) + geom_hline(yintercept = 0,
                                                                                                       linetype = "solid", color = "blue") + ggtitle(cusum_betas_mkt$name[i])
}
grid.arrange(grobs = plot_list, ncol = 5, top = "Market")

## SMB
cusum_betas_smb <- df |>
  group_by(name) |>
  do(cusum = efp(ret_excess ~ smb, data = ., type = "Rec-CUSUM"))

plot_list <- list()

for (i in 1:25) {
  new.date <- seq(as.Date(paste(c(start_date, 1), collapse = "/")),
                  by = "month", length.out = length(cusum_betas_smb[[2]][[i]][["process"]]))
  dat <- data.frame(date = new.date, value = cusum_betas_smb[[2]][[i]][["process"]],
                    bound = boundary(cusum_betas_smb[[2]][[i]]))
  plot_list[[i]] <- ggplot(dat) + geom_line(aes(x = date, y = value)) +
    geom_line(aes(x = date, y = bound), linetype = "dashed",
              color = "red") + geom_line(aes(x = date, y = -bound),
                                         linetype = "dashed", color = "red") + ylab(NULL) + geom_hline(yintercept = 0,
                                                                                                       linetype = "solid", color = "blue") + ggtitle(cusum_betas_smb$name[i])
}
grid.arrange(grobs = plot_list, ncol = 5, top = "SMB")


## HML

cusum_betas_hml <- df |>
  group_by(name) |>
  do(cusum = efp(ret_excess ~ hml, data = ., type = "Rec-CUSUM"))

plot_list <- list()

for (i in 1:25) {
  new.date <- seq(as.Date(paste(c(start_date, 1), collapse = "/")),
                  by = "month", length.out = length(cusum_betas_hml[[2]][[i]][["process"]]))
  dat <- data.frame(date = new.date, value = cusum_betas_hml[[2]][[i]][["process"]],
                    bound = boundary(cusum_betas_hml[[2]][[i]]))
  plot_list[[i]] <- ggplot(dat) + geom_line(aes(x = date, y = value)) +
    geom_line(aes(x = date, y = bound), linetype = "dashed",
              color = "red") + geom_line(aes(x = date, y = -bound),
                                         linetype = "dashed", color = "red") + ylab(NULL) + geom_hline(yintercept = 0,
                                                                                                       linetype = "solid", color = "blue") + ggtitle(cusum_betas_hml$name[i])
}
grid.arrange(grobs = plot_list, ncol = 5, top = "HML")


################################################################################
#                                  2.2 Gammas                                  #
################################################################################


fit_ff3_SH <- beta_examples |>
  group_by(date) |>
  do(model_lm = lm(ret_excess ~ beta_mark + beta_smb + beta_hml,
                   data = .)) |>
  mutate(gamma0 = model_lm$coefficients[1], 
         gamma_mark = model_lm$coefficients[2],
         gamma_smb = model_lm$coefficients[3], 
         gamma_hml = model_lm$coefficients[4],
         p.value_mark = summary(model_lm)$coefficients[2, 4],
         p.value_smb = summary(model_lm)$coefficients[3, 4], 
         p.value_hml = summary(model_lm)$coefficients[4,4])

# Evolución gammas

## Mercado
mean_gamma_mark <- mean(fit_ff3_SH$gamma_mark)
desv_mark <- sd(beta_examples$mkt_excess)
max_gamma_mark <- max(max(fit_ff3_SH$gamma_mark), mean_gamma_mark +
                        desv_mark)
min_gamma_mark <- min(min(fit_ff3_SH$gamma_mark), mean_gamma_mark -
                        desv_mark)
ggplot(fit_ff3_SH, aes(x = date)) + geom_line(aes(y = gamma_mark)) +
  geom_line(aes(y = max_gamma_mark, color = "red")) + geom_line(aes(y = min_gamma_mark,
                                                                    color = "red")) + theme(legend.position = "none")


## SMB
mean_gamma_smb <- mean(fit_ff3_SH$gamma_smb)
desv_smb <- sd(beta_examples$smb)
max_gamma_smb <- max(max(fit_ff3_SH$gamma_smb), mean_gamma_smb +
                       desv_smb)
min_gamma_smb <- min(min(fit_ff3_SH$gamma_smb), mean_gamma_smb -
                       desv_smb)
ggplot(fit_ff3_SH, aes(x = date)) + geom_line(aes(y = gamma_smb)) +
  geom_line(aes(y = max_gamma_smb, color = "red")) + geom_line(aes(y = min_gamma_smb,
                                                                   color = "red")) + theme(legend.position = "none")

## HML
mean_gamma_hml <- mean(fit_ff3_SH$gamma_hml)
desv_hml <- sd(beta_examples$hml)
max_gamma_hml <- max(max(fit_ff3_SH$gamma_hml), mean_gamma_hml +
                       desv_hml)
min_gamma_hml <- min(min(fit_ff3_SH$gamma_hml), mean_gamma_hml -
                       desv_hml)
ggplot(fit_ff3_SH, aes(x = date)) + geom_line(aes(y = gamma_hml)) +
  geom_line(aes(y = max_gamma_hml, color = "red")) + geom_line(aes(y = min_gamma_hml,
                                                                   color = "red")) + theme(legend.position = "none")


################################################################################
#                          2.2.1 Significatividad                              #
################################################################################


contraste <- fit_ff3_SH |>
  select("gamma_mark", "gamma_smb", "gamma_hml")
values <- sapply(contraste, function(x) c(mean = mean(x), desv = sd(x)))

desv <- beta_examples |>
  select(c("mkt_excess", "smb", "hml")) |>
  sapply(function(x) c(desv = sd(x)))

shanken <- sqrt((1 + values[1, ]/desv))
t_st <- values[1, ]/(desv * shanken)
t_st

p_all <- NULL
for (i in 1:337) {
  p_all[i] <- broom::glance(summary(fit_ff3_SH[[2]][[i]]))$p.value
}

length(which(p_all > 0.05))






################################################################################
#                                  3. Carhart                                  #
################################################################################


fit <- df |>
  group_by(name) |>
  do(model_lm = lm(ret_excess ~ mkt_excess + smb + hml + wml, data = .)) |>
  mutate(alpha = model_lm$coefficients[1],
         beta_m = model_lm$coefficients[2], 
         beta_smb = model_lm$coefficients[3], 
         beta_hml = model_lm$coefficients[4], 
         beta_wml = model_lm$coefficients[5], 
  )


df %>%
  group_nest(name) %>%
  mutate(model = map(data, ~lm(ret_excess ~ mkt_excess + smb + hml + wml, data = .))) %>%
  pull(model) %>%
  stargazer(title = "Regression Results Carhart", type = "html", out = "all_regressions_Carhart.html")

# p-values
p_all <- NULL
for (i in 1:25) {
  p_all[i] <- broom::glance(summary(fit[[2]][[i]]))$p.value
}

p_all




# Heterocedasticidad -> Contraste White 

fit_wh <- df |>
  group_by(name) |>
  do(model_lm = lm(ret_excess ~ mkt_excess + smb + hml + wml, data = .),
     skedastic_package_white = white(mainlm = lm(ret_excess ~
     mkt_excess + smb + hml + wml, data = .), interactions = TRUE)) |>
  mutate(p_value_white = skedastic_package_white$p.value)

model <- fit[[2]][[1]]
model$residuals <- resid(object = model)
summary(model$residuals)


# Heterocedasticidad -> Contraste Breusch-Pagan

fit_bp <- df |>
  group_by(name) |>
  do(model_lm = lm(ret_excess ~ mkt_excess + smb + hml + wml, data = .),
     lmtest_package_bp = bptest(formula = lm(ret_excess ~
     mkt_excess + smb + hml + wml, data = .), studentize = TRUE)) |>
  mutate(p_value_bp = lmtest_package_bp$p.value)


# Autocorrelación -> Breusch-Godfrey
fit_bg <- df |>
  group_by(name) |>
  do(model_lm = lm(ret_excess ~ mkt_excess + smb + hml + wml, data = .),
     bgtest_package_bg = bgtest(formula = lm(ret_excess ~
     mkt_excess + smb + hml + wml, data = .), order = 12)) |>
  mutate(p_value_bg = bgtest_package_bg$p.value)


# Autocorrelación -> Durbin-Watson 
fit_dw <- df |>
  group_by(name) |>
  do(model_lm = lm(ret_excess ~ mkt_excess + smb + hml + wml, data = .),
     durbinWatson_package = durbinWatsonTest(lm(ret_excess ~
     mkt_excess + smb + hml + wml, data = .))) |>
  mutate(p_value_dw = durbinWatson_package$p)

# Normalidad -> Jarque-Bera
library("tseries")
fit_jb <- df |>
  group_by(name) |>
  do(model_lm = lm(ret_excess ~ mkt_excess + smb + hml + wml, data = .)) |>
  mutate(res = list(model_lm$residuals), p_value_jb = jarque.bera.test(res)$p.value)


# Tabla con todo
carhart <- tibble(cartera =  colnames(carteras[-1]), 
              white = fit_wh$p_value_white,
              bp = fit_bp$p_value_bp, 
              bg = fit_bg$p_value_bg,
              dw = fit_dw$p_value_dw,
              jb = fit_jb$p_value_jb)



################################################################################
#                            3.1 Rolling Carhart                               #
################################################################################


# Función estimación beta momentum
estimate_wml <- function(data, min_obs = 1) {
  if (nrow(data) < min_obs) {
    beta <- as.numeric(NA)
  } else {
    fit <- lm(ret_excess ~ wml, data = data)
    beta <- as.numeric(coefficients(fit)[2])
  }
  return(beta)
}

# Función Rolling Beta Momentum 
roll_wml_estimation <- function(data, months, min_obs) {
  data <- data |>
    arrange(date)
  
  betas <- slide_period_vec(.x = data, .i = data$date, .period = "month",
                            .f = ~estimate_wml(., min_obs), .before = months - 1,
                            .complete = FALSE)
  
  return(tibble(date = unique(data$date), beta_wml = betas))
}

# Beta Estimada Momentum
beta_capm <- df |>
  group_by(name) |>
  mutate(roll_capm_estimation(pick(date, ret_excess, mkt_excess),
                              months = 60, min_obs = 60)) |>
  ungroup() |>
  drop_na()

beta_smb <- df |>
  group_by(name) |>
  mutate(roll_smb_estimation(pick(date, ret_excess, smb), months = 60,
                             min_obs = 60)) |>
  ungroup() |>
  drop_na()

beta_hml <- df |>
  group_by(name) |>
  mutate(roll_hml_estimation(pick(date, ret_excess, hml), months = 60,
                             min_obs = 60)) |>
  ungroup() |>
  drop_na()

beta_wml <- df |>
  group_by(name) |>
  mutate(roll_wml_estimation(pick(date, ret_excess, wml),
                              months = 60, min_obs = 60)) |>
  ungroup() |>
  drop_na()

# Mostrar Datos
beta_carhart <- beta_capm %>%
  right_join(beta_smb, by = c("date", "mkt_excess", "smb",
                              "hml", "wml", "name", "ret_excess"))

beta_carhart <- beta_carhart %>%
  right_join(beta_hml, by = c("date", "mkt_excess", "smb",
                              "hml", "wml", "name", "ret_excess"))

beta_carhart <- beta_carhart %>%
  right_join(beta_wml, by = c("date", "mkt_excess", "smb",
                              "hml", "wml", "name", "ret_excess"))


################################################################################
#                              3.1.1 Estabilidad                               #
################################################################################


beta_carhart |>
  ggplot(aes(x = date, y = beta_mark)) + geom_line() + 
  facet_wrap(~name, scales = "free")

beta_carhart |>
  ggplot(aes(x = date, y = beta_smb)) + geom_line() + 
  facet_wrap(~name, scales = "free")

beta_carhart |>
  ggplot(aes(x = date, y = beta_hml)) + geom_line() + 
  facet_wrap(~name,scales = "free")

beta_carhart |>
  ggplot(aes(x = date, y = beta_wml)) + geom_line() + 
  facet_wrap(~name,scales = "free")


#Gráficas CUSUM

## WML
cusum_betas_wml <- df |>
  group_by(name) |>
  do(cusum = efp(ret_excess ~ wml, data = ., type = "Rec-CUSUM"))

plot_list <- list()

for (i in 1:25) {
  new.date <- seq(as.Date(paste(c(start_date, 1), collapse = "/")),
                  by = "month", length.out = length(cusum_betas_wml[[2]][[i]][["process"]]))
  dat <- data.frame(date = new.date, value = cusum_betas_wml[[2]][[i]][["process"]],
                    bound = boundary(cusum_betas_wml[[2]][[i]]))
  plot_list[[i]] <- ggplot(dat) + geom_line(aes(x = date, y = value)) +
    geom_line(aes(x = date, y = bound), linetype = "dashed",
              color = "red") + geom_line(aes(x = date, y = -bound),
                                         linetype = "dashed", color = "red") + ylab(NULL) + geom_hline(yintercept = 0,
                                                                                                       linetype = "solid", color = "blue") + ggtitle(cusum_betas_wml$name[i])
}
grid.arrange(grobs = plot_list, ncol = 5, top = "WML")


################################################################################
#                                3.2 Gammas                                    #
################################################################################
fit_carhart_SH <- beta_carhart |>
  group_by(date) |>
  do(model_lm = lm(ret_excess ~ beta_mark + beta_smb + beta_hml + beta_wml,
                   data = .)) |>
  mutate(gamma0 = model_lm$coefficients[1], 
         gamma_mark = model_lm$coefficients[2],
         gamma_smb = model_lm$coefficients[3], 
         gamma_hml = model_lm$coefficients[4],
         gamma_wml = model_lm$coefficients[5],
         p.value_mark = summary(model_lm)$coefficients[2, 4],
         p.value_smb = summary(model_lm)$coefficients[3, 4], 
         p.value_hml = summary(model_lm)$coefficients[4,4],
         p.value_wml = summary(model_lm)$coefficients[5,4])

# Evolución gammas

## Mercado
mean_gamma_mark <- mean(fit_carhart_SH$gamma_mark)
desv_mark <- sd(beta_examples$mkt_excess)
max_gamma_mark <- max(max(fit_carhart_SH$gamma_mark), mean_gamma_mark +
                        desv_mark)
min_gamma_mark <- min(min(fit_carhart_SH$gamma_mark), mean_gamma_mark -
                        desv_mark)
ggplot(fit_carhart_SH, aes(x = date)) + geom_line(aes(y = gamma_mark)) +
  geom_line(aes(y = max_gamma_mark, color = "red")) + geom_line(aes(y = min_gamma_mark,
                                                                    color = "red")) + theme(legend.position = "none")





## SMB
mean_gamma_smb <- mean(fit_carhart_SH$gamma_smb)
desv_smb <- sd(beta_examples$smb)
max_gamma_smb <- max(max(fit_carhart_SH$gamma_smb), mean_gamma_smb +
                       desv_smb)
min_gamma_smb <- min(min(fit_carhart_SH$gamma_smb), mean_gamma_smb -
                       desv_smb)
ggplot(fit_carhart_SH, aes(x = date)) + geom_line(aes(y = gamma_smb)) +
  geom_line(aes(y = max_gamma_smb, color = "red")) + geom_line(aes(y = min_gamma_smb,
                                                                   color = "red")) + theme(legend.position = "none")

## HML
mean_gamma_hml <- mean(fit_carhart_SH$gamma_hml)
desv_hml <- sd(beta_examples$hml)
max_gamma_hml <- max(max(fit_carhart_SH$gamma_hml), mean_gamma_hml +
                       desv_hml)
min_gamma_hml <- min(min(fit_carhart_SH$gamma_hml), mean_gamma_hml -
                       desv_hml)
ggplot(fit_carhart_SH, aes(x = date)) + geom_line(aes(y = gamma_hml)) +
  geom_line(aes(y = max_gamma_hml, color = "red")) + geom_line(aes(y = min_gamma_hml,
                                                                   color = "red")) + theme(legend.position = "none")

## WML
mean_gamma_wml <- mean(fit_carhart_SH$gamma_wml)
desv_hml <- sd(beta_examples$hml)
max_gamma_wml <- max(max(fit_carhart_SH$gamma_wml), mean_gamma_wml +
                       desv_hml)
min_gamma_wml <- min(min(fit_carhart_SH$gamma_wml), mean_gamma_wml -
                       desv_hml)
ggplot(fit_carhart_SH, aes(x = date)) + geom_line(aes(y = gamma_wml)) +
  geom_line(aes(y = max_gamma_wml, color = "red")) + geom_line(aes(y = min_gamma_wml,
                                                                   color = "red")) + theme(legend.position = "none")

################################################################################
#                            3.2.1 Significatividad                           #
################################################################################


contraste_carhart <- fit_carhart_SH |>
  select("gamma_mark", "gamma_smb", "gamma_hml", "gamma_wml")
values_carhart <- sapply(contraste_carhart, function(x) c(mean = mean(x), desv = sd(x)))

desv <- beta_carhart |>
  select(c("mkt_excess", "smb", "hml", "wml")) |>
  sapply(function(x) c(desv = sd(x)))

shanken <- sqrt((1 + values_carhart[1, ]/desv))
t_st <- values_carhart[1, ]/(desv * shanken)
t_st

alpha <- 0.05
qnorm(1 - alpha)


p_all <- NULL
for (i in 1:337) {
  p_all[i] <- broom::glance(summary(fit_carhart_SH[[2]][[i]]))$p.value
}

length(which(p_all > 0.05))










