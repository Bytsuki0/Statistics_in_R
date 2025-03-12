library(tidyverse)
library(xts)
library(lubridate)
library(highfrequency)

# Leitura do banco de dados tratado ####

dt.intra <- read.csv("D:/Code/R_studio/Petr4_ana/dt_1min_PETR4_2021_metatrader.csv", header=T, sep = ";",dec = ",")

dt1 <- as_tibble(dt.intra) %>% 
  mutate(Period = ymd_hms(X)) %>%
  select(-X) 

ret.1min <- as.xts(dt1$Ret.1min, order.by = dt1$Period)

#

# descritiva ####


plot.xts(ret.1min,major.format="%b/%d/%Y",
         main="ret.IBOV 1min",ylab="",xlab="")

library(PerformanceAnalytics)

boxplot(as.double(ret.1min))
min(ret.1min)
which(ret.1min==min(ret.1min))

ret.1min[which(ret.1min==min(ret.1min))]
dt1$Period[which(ret.1min==min(ret.1min))]
#https://economia.uol.com.br/cotacoes/noticias/redacao/2021/02/22/acoes-petrobras-bolsa-de-valores.htm

par(mfrow=c(1,2))
chart.Histogram(ret.1min,xlab="(a)",ylab="",main="Histograma Ret PETR4 1min",
                breaks = 500, xlim = c(-0.005,0.005))
chart.QQPlot(ret.1min, xlab="(b)",ylab="",main="QxQ plot normal",distribution = "norm")

# Fun??es de autocorrela??o e autocorrela??o parcial


library(forecast)
ggtsdisplay(ret.1min, main = "Retornos intradi?rios IBOV 1min brutos")

ggtsdisplay(ret.1min^2, main = "Quadrado dos retornos intradi?rios IBOV 1min brutos")

#####

# Expl. dados ####

dt1 <- as_tibble(dt.intra) %>% 
  mutate(Period = ymd_hms(X)) %>%
  select(-X) %>%
  separate(Period, c("date", "time"), " ", extra = "merge") %>% 
  select(- Close.1min)
dt1

dt2 <- spread(dt1, key = time, value = Ret.1min)

dt2$na_count <- apply(is.na(dt2), 1, sum)
dt2$nobs <- 419-dt2$na_count
write.csv2(dt2, "dataXhora_PETR4.csv",  row.names = T)