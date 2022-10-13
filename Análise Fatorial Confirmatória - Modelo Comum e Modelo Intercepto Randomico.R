#### liberando os pacotes ####
library(lavaan)
library(readr)

#### carregar o banco ####
banco <- read.csv(
  url(
    "https://github.com/maynarapriscila/interceptorandomico/raw/main/bancoconexaoIR.csv"
  ), sep =  ";"
)


#### modelo fator comum ####

uniconex <- 'f1 =~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11
+ V12 + V13 + V14 + V15'

uniconex.fit <- cfa(uniconex,
                    data = banco,
                    estimator = 'WLSMV')

summary(uniconex.fit, standardized = TRUE, fit.measures = TRUE, modindices = FALSE)


#### modelo intercepto randomico ####
aquiconex <- 'f1 =~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11
+ V12 + V13 + V14 + V15
aq =~ 1*V1 + 1*V2 + 1*V3 + 1*V4 + 1*V5 + 1*V6 + 1*V7 + 1*V8 + 1*V9 + 1*V10 + 1*V11 + 
  1*V12 + 1*V13 + 1*V14 + 1*V15
f1 ~~ 0*aq
V1 ~~ V15;'

aquiconex.fit <- cfa(aquiconex,
                     data = banco,
                     estimator = 'WLSMV')

summary(aquiconex.fit, standardized = TRUE, fit.measures = TRUE, modindices = FALSE)
