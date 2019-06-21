
rm(list = ls())

# Load Library ------------------------------------------------------------

library(tidyverse)
library(GGally)
library(rstan)


theme_set(theme_bw() +
            theme(axis.text.x = element_text(size=15),
                  axis.text.y = element_text(size=15)) +
            theme(axis.title = element_text(size = 15)) +
            theme(text = element_text(family = "MEI"))
)

windowsFonts("MEI"=windowsFont("Meiryo"))


# Load data ---------------------------------------------------------------

d <- airquality %>% 
  as_tibble() %>% 
  select(-Month, -Day) %>% 
  na.omit()

d <- d %>%
  mutate(log_Ozone = log(Ozone)) %>% 
  select(Ozone, log_Ozone, everything())

# Rough plot --------------------------------------------------------------

# ggpairs(d)

ggpairs(d,
        aes(alpha = 0.5),
        upper=list(continuous=wrap("cor",size=5))) +
  theme(strip.text=element_text(size=10, colour = "white"),
        strip.background = element_rect(fill = "navy"))


# Settings of MCMC ----

dir <- "001__Model/"
scr <- str_c("./", dir, "QR_reparameterization_LinearModel.stan")
# scr <- str_c("./", dir, "PoissonModel.stan")
par <- c("alpha", "theta", "beta", "sigma", "y_pred")
war <- 1000
ite <- 11000
see <- 123
dig <- 3
cha <- 4

# Data For MCMC ----

# Scaling data for Model selection by coefficient velue 
d <- d %>%
  scale() %>% 
  as_tibble() %>% 
  filter(log_Ozone > -3.8)

data = list(N = NROW(d),
            K = NCOL(d) - 2,
            y = d$log_Ozone,
            x = d %>% select(Solar.R, Wind, Temp) %>% as.matrix())

stanmodel <- stan_model(file = scr)

fit <- sampling(stanmodel,
                data=data,
                pars=par,
                warmup=war, 
                iter=ite,
                seed=see,
                chains=cha,
                verbose=F)

print(fit, pars=par, digits_summary=dig)

ms <- summary(fit)$summary %>%
  as_tibble(rownames = NA) %>% 
  mutate(name = row.names(.)) %>% 
  filter(grepl("y_pred", name)) %>% 
  bind_cols(d, .)

ms %>% glimpse()

model <- ms %>% lm(mean ~ log_Ozone, data = .)

r2 <- model %>%
  summary %>%
  .$r.squared %>% 
  round(., 2)

ggplot(ms, aes(x = log_Ozone)) +
  geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`),
              fill = "lightblue",
              alpha = 0.4) +
  geom_ribbon(aes(ymin = `25%`, ymax = `75%`),
              fill = "lightblue",
              alpha = 0.8) +
  geom_point(aes(y = mean),
             alpha = 0.6,
             size = 3) +
  geom_text(aes(x = -2.5, y = 2.5,
                label = str_c("R^2 : ", r2)),
            hjust = 0,
            size = 10) +
  coord_cartesian(xlim = c(-3, 3),
                  ylim = c(-3, 3)) +
  geom_abline(slope = 1,
              colour = "red") +
  NULL



# Formattable ----

library(formattable)

Summary <- summary(fit)$summary[, c(1,3,4,8,9,10)]
df <- data.frame(Summary)
df$Rhat[is.nan(df$Rhat)] <- 0

for(i in 1:length(df)){
  df[, i] <- digits(df[, i], 3)
}

df <- df[-length(df$mean), ]

formattable(df[1:8, ], list(
  mean = formatter('span', style=x ~ style(color=ifelse(x<0, 'steelblue', 'tomato'))),
  sd = color_bar('orange'),
  X2.5. = formatter('span', style=x ~ ifelse(x<0, style(color='steelblue', font.weight='bold'), NA)),
  X97.5. = formatter('span', style=x ~ ifelse(x<0, style(color='steelblue', font.weight='bold'), NA)),
  n_eff = formatter('span', style = x ~ style(color=ifelse(rank(-x)<=c(length(df$n_eff)-1), 
                                                           'gray', 'tomato')),
                    x ~ sprintf('%.2f (rank: %g)', x, rank(-x))),
  Rhat = formatter('span', x ~ digits(x, 2),
                   style = x ~ style(color=ifelse(x>=1.1, 'tomato', 'green')),
                   x ~ icontext(ifelse(x<1.1, 'ok', 'remove'), ifelse(x<1.1, 'OK', 'NG')))
))



# Fin ---------------------------------------------------------------------


