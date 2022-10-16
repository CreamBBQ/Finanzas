rm(list = ls())
setwd("C:/Users/JGMUNOZD/Desktop/RIF/Domiciliario 1") 
library("dplyr"); library("ggplot2"); library("expss")
library("ggthemes"); library("tidyquant"); library("tidyverse")

#---------------------------------EJERCICIO 1-----------------------------------#

#La siguiente función recibe una matriz unidemensional con la abreviación de 
#cada compañia, una fecha de inicio y una final. Carga en "prices" una lista 
#donde cada una contiene diferentes datos para cada empresa (precio de apertura,
#precio de cierre, promedio, etc). La función se que queda el precio de cierre 
#de la primera compañía y los días transitados dentro del intervalo propuesto.
#luego itera sobre las compañia restantes repitiendo el mismo proceso y al final
#de cada iteación combina los datos con los anteriormente obtenidos. 

get_data <- function(tickers, start, finish){
  prices <- lapply(tickers, getSymbols, auto.assign=FALSE, from = start, to = finish)
  date <- rownames(as.data.frame(prices[1]))
  main <- as.data.frame(as.data.frame(prices[1])[,paste(tickers[1],"Close",sep=".")])
  names(main) <- tickers[1]
  for(i in seq(2,length(tickers))){
    temp <- as.data.frame(as.data.frame(prices[i])[,paste(tickers[i],"Close",sep=".")])
    names(temp) <- tickers[i]
    main <- cbind(main, temp)
  }
  main <- main %>% mutate(Date = date) %>% relocate(Date, .before = tickers[1])
  return(main)
}

#---------------------------------EJERCICIO 2-----------------------------------#

#Simplemente se ejecuta la función antes descripta dando en "tickers" la abreviacion
#de las compañias solicitadas en el enunciado. 

tickers <- c("AAPL", "GOOG", "KO", "MCD", "META", "AMZN", "TSLA", "JPM", "CVX", "MELI")
prices <- get_data(tickers = tickers, start = "2017-12-31", finish = "2022-01-01")

#---------------------------------EJERCICIO 3-----------------------------------#

#Para crear un dataframe de retornos utilizo el conjunto de datos de precios 
#y muto todas sus valores por el logaritmo del precio en t menos el logatirmo
#del precio en t-1 para cada registro (notar que la primera fila tendrá un 
#valor desconocido)

returns <- prices %>% 
  mutate(across(!Date, function(x) c(NA ,diff(log(x)))))

#---------------------------------EJERCICIO 4-----------------------------------#

#El primer gráfico es la evolución del precio de META para cada día (cambio el 
#formato de la columna "Date" para que ggplot2 no tenga problemas de interpretación)

prices %>% 
  mutate(Date = as.Date(as.POSIXct(as.Date(Date), format = "%Y-%m-%d"))) %>% 
  ggplot(aes(x = Date, y = META, group = 1)) +
  geom_line(color = "#004c69", size= .5) +
  theme_economist() + 
  labs(
    title= "Evolución del precio de META a lo largo del tiempo",
    subtitle = '\n(2018-2021)',
    x = "\nFecha (día/mes/año)",
    y = "Precio de la acción\n") + 
  scale_x_date(date_breaks = "5 month",
               date_labels = "%d/%m/%Y") + 
  scale_y_continuous(breaks = seq(0, 500, 50))

#El segundo gráfico hace lo análogo para los retornos 

returns %>%  
  mutate(Date = as.Date(as.POSIXct(as.Date(Date), format = "%Y-%m-%d"))) %>% 
  ggplot(aes(x = Date, y = META, group = 1)) +
  geom_line(color = "#004c69", size= .5) +
  theme_economist() + 
  labs(
    title= "Evolución del retorno diario de META a lo largo del tiempo",
    subtitle = '\n(2018-2021)',
    x = "\nFecha (día/mes/año)",
    y = "Retorno") + 
  scale_x_date(date_breaks = "5 month",
               date_labels = "%d/%m/%Y") + 
  scale_y_continuous(breaks = seq(-1, 1, .1))

#---------------------------------EJERCICIO 5-----------------------------------#

#Se computan medias, varianzas y covarianzas para todos los activos con los comandos 
#usuales. Notar que pido que AAPL no tenga datos faltantes, esto es solo para 
#excluir la primera fila, funcionaria igual para cualquier activo.

means_returns_vector <- t(colMeans(returns %>% 
                                   filter(is.na(AAPL) == FALSE) %>% 
                                   select(-Date)))
cov_returns_matrix <- cov(returns %>% 
                            filter(is.na(AAPL) == FALSE) %>%  
                            select(-Date))
var_return_vector <- diag(cov_returns_matrix)

#---------------------------------EJERCICIO 6-----------------------------------#

max(means_returns_vector) #Tesla 
min(means_returns_vector) #Chevron


prices %>% 
  mutate(Date = as.Date(as.POSIXct(as.Date(Date), format = "%Y-%m-%d"))) %>% 
  ggplot() + 
  geom_line(aes(x=Date,y=TSLA,color='TSLA'), group = 1,size=.8) + 
  geom_line(aes(x=Date,y=CVX,color='CVX'), group = 1,size=.8) + 
  theme_economist() +
  scale_x_date(date_breaks = "5 month",
               date_labels = "%d/%m/%Y") + 
  scale_y_continuous(breaks = seq(0, 1000, 100)) + 
  labs(
    title= "Evolución del precio de TSLA (mayor retorno) y CVX (menor retorno)",
    subtitle = '\n(2018-2021)',
    x = "\nFecha (día/mes/año)",
    y = "Precio de la acción\n") + 
  scale_color_manual(name=element_blank(),
                     breaks=c('TSLA', 'CVX'),
                     values=c('TSLA'='#004c69', 'CVX'='#72737e'))

#---------------------------------EJERCICIO 7-----------------------------------#

#Se crean nuevas variables de año y mes para luego separar los datos por semestre.

returns %>% 
  mutate(Date = as.Date(as.POSIXct(as.Date(Date), format = "%Y-%m-%d")), 
         month = as.integer(strftime(Date, format = "%m")), 
         year = as.integer(strftime(Date, format = "%Y")), 
         semester = case_when(year == 2018 & month <= 6 ~ "(I)", 
                              year == 2018 & month > 6 ~ "(II)", 
                              year == 2019 & month <= 6 ~ "(III)", 
                              year == 2019 & month > 6 ~ "(IV)", 
                              year == 2020 & month <= 6 ~ "(V)", 
                              year == 2020 & month > 6 ~ "(VI)", 
                              year == 2021 & month <= 6 ~ "(VII)", 
                              year == 2021 & month > 6 ~ "(VIII)")) %>% 
  group_by(semester) %>% 
  summarise(across(tickers, ~ mean(.x, na.rm = TRUE))) %>% 
  melt(., id = "semester") %>% 
  ggplot(., aes(x = semester, y = value, fill = variable)) +
  geom_bar(stat="identity", position="dodge") + 
  scale_fill_brewer(palette="Paired", 
                    name = element_blank()) + 
  scale_y_continuous(breaks = seq(-1, 1, .002)) +
  theme_economist() +
  labs(
    title= "Evolución semestral del retorno medio de acciones",
    subtitle = '(2018-2021)',
    x = "\nSemestre",
    y = "Retorno medio\n") 

#-----------------------------EJERCICIO BONUS-----------------------------------#

#Se definine una función para calcular la media movil (n) de, en principio, 
#un vector cualquiera. Crea un dataframe donde pone los primeros n valores 
#como desconocidos, dado que no hay datos para calcular dichas medias moviles 
#luego itera desde uno hasta la longitud del vector y calcula la media de los 
#últimos n días para el mismo. 

MA <- function(x,n){
  df <- data.frame(MA = rep(NA,n-1))
  for(i in seq(1,length(x))) {
   df[i+n-1,"MA"] <- mean(x[i:(n+i-1)])
  }
  return(df[1:length(x), ])
}

#Se repite la función, está vez para calcular el desvio estándar 

sdMA <- function(x,n){
  df <- data.frame(sdMA = rep(NA,n-1))
  for(i in seq(1,length(x))) {
    df[i+n-1,"sdMA"] <- sd(x[i:(n+i-1)])
  }
  return(df[1:length(x), ])
}

returns %>%  
  mutate(Date = as.Date(as.POSIXct(as.Date(Date), format = "%Y-%m-%d")), 
         MA_AAPL = MA(returns$AAPL, 60)) %>% #Acá hago uso de la función
  filter(is.na(MA_AAPL)==FALSE) %>% 
  ggplot(aes(x = Date, y = MA_AAPL, group = 1)) +
  geom_line(color = "#004c69", size= .7) +
  theme_economist() + 
  labs(
    title= "Evolución temporal de la media movil (60) de retornos para AAPL",
    subtitle = '\n(2018-2021)',
    x = "\nFecha (día/mes/año)",
    y = "Retorno\n") + 
  scale_x_date(date_breaks = "5 month",
               date_labels = "%d/%m/%Y") + 
  scale_y_continuous(breaks = seq(-1,1,.002))

returns %>%  
  mutate(Date = as.Date(as.POSIXct(as.Date(Date), format = "%Y-%m-%d")), 
         sdMA_AAPL = sdMA(returns$AAPL, 60)) %>% #Acá hago uso de la función
  filter(is.na(sdMA_AAPL)==FALSE) %>% 
  ggplot(aes(x = Date, y = sdMA_AAPL, group = 1)) +
  geom_line(color = "#004c69", size= .7) +
  theme_economist() + 
  labs(
    title= "Evolución temporal del desvio estándar movil (60) para AAPL",
    subtitle = '\n(2018-2021)',
    x = "\nFecha (día/mes/año)",
    y = "Desvio estándar\n") + 
  scale_x_date(date_breaks = "5 month",
               date_labels = "%d/%m/%Y") + 
  scale_y_continuous(breaks = seq(-1, 1, .005))
