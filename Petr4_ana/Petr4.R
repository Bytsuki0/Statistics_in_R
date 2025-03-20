library(dplyr)
library(ggplot2)
library(tidyr)
library(xts)
library(forecast)
library(lubridate)
library(PerformanceAnalytics)
library(highfrequency)         
library(quantmod)    
library(tseries)     
library(FinTS)       
library(stochvol)  
library(fpp2)     
library(fpp3)       
library(modeltime)    
library(timetk)      
library(parsnip)      
library(rsample)      
library(cowplot)

os = Sys.info()["sysname"]

if(os == "Windows")
  data <- data <- read.csv("D:/Code/R_studio/Petr4_ana/dt_1min_PETR4_2021_metatrader.csv", 
                           header = TRUE, 
                           stringsAsFactors = FALSE, 
                           sep = ";",dec = ",")

if (os =="Linux")
  data <- data <- read.csv("~/Documentos/Coding/Statistics_in_R/Petr4_ana/dt_1min_PETR4_2021_metatrader.csv", 
                         header = TRUE, 
                         stringsAsFactors = FALSE, 
                         sep = ";",dec = ",")


dt1 <- as_tibble(data) %>% 
  mutate(Period = ymd_hms(X)) %>%
  select(-X) 
#grafico do preço de fechamento

ggplot(dt1, aes(x= Period, y = Close.1min))+
  geom_line()


#gráfico de retornos de log 

price_xts <- as.xts(dt1$Ret.1min, order.by = dt1$Period)
plot(price_xts, main="PETR4 Price (1-min data)", ylab="Price", col="black")
par(mfrow=c(1,1))

#tentando remover as linhas que aparecem entre os dias 

dt1_minusf15 <- as_tibble(data) %>% 
  mutate(Period = ymd_hms(X)) %>%
  select(-X) %>%
  filter(format(Period, "%H:%M:%S") >= "10:15:00",
         format(Period, "%H:%M:%S") <= "17:45:00") %>%
  arrange(Period)

dt1_minusf15 <- dt1_minusf15 %>%
  mutate(obs = row_number())

n_obs <- nrow(dt1_minusf15)
label_positions <- c(1, round(n_obs/2), n_obs)
label_times <- format(dt1_minusf15$Period[label_positions], "%m:%d:%H:%M:%S")

ggplot(dt1_minusf15, aes(x = obs, y = Ret.1min)) +
  geom_line(color = "black", size = 1) +
  scale_x_continuous(breaks = label_positions, labels = label_times) +
  labs(x = "Tempo Mes/Dia/Hora/Minuto", y = "Retorno logaritmico",
       title = "Serie temporal Petr4 2021 (linha entre os dias removida)")

#dt1_minus15f remove todos os tempos de antes de 10:15 e depois de 17:45 para padronização do tempo


#histograma e qxq plot


hist(coredata(dt1_minusf15$Ret.1min), breaks=50, main="Histograma do log de retornos", xlab="Log de retornos", ylab= "Densidade", probability = TRUE,xlim = c(-0.05,0.05))
lines(density(coredata(dt1_minusf15$Ret.1min)), col="blue", lwd=2)


qqnorm(coredata(dt1$Ret.1min), main = "Qxq Plot dos retornos"); qqline(coredata(dt1$Ret.1min), col="red")


#Plot do retorno logaritimo mais função de alto correlação e função parcial de alta correlação 

ggtsdisplay(dt1_minusf15$Ret.1min, main = "Retornos 1min brutos")

#plot retorno log acf e pacf ao quadrado

ggtsdisplay(dt1_minusf15$Ret.1min^2, main = "Quadrado dos retornos 1min brutos ")

