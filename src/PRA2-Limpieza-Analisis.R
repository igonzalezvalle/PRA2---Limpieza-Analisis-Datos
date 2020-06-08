
library(knitr)
library(stringr)
library(psych)
library(VIM)
library(graphics)
library(RColorBrewer)
library(coin)
library(ISLR)
library(MASS)
library(dplyr)
library(pROC)
library(faraway)
library(kableExtra)
library(ggplot2)
library(data.table)
library(agricolae)
library(kableExtra)
library(sampling)
library(corrplot)
library(leaflet)
library(cluster)
library(caret)
library(lubridate) #Para trabajar con fechas
library(DescTools) #Transformada de Box-Cox
library(randomForest)
require(caTools)
library(gridExtra) #para usar grid.arrange
#Cargamos el dataset
vuelos <- read.csv("flights.csv", sep=c(","), header = TRUE)

head(vuelos)


#Reducción de la cantidad
set.seed(222)
index <- sample(1:nrow(vuelos), size=0.07*nrow(vuelos))
vuelos_reduc <- vuelos[index,]
str(vuelos_reduc)

summary(vuelos_reduc)

length(vuelos_reduc$YEAR)

airports <- read.csv("datasets_810_1496_airports.csv", header=TRUE)
head(airports)
colnames(vuelos_reduc)[8]  <- "ORIGIN_CODE"
colnames(airports) <- c("ORIGIN_CODE","ORIGIN_AIRPORT", "ORIGIN_CITY", "ORIGIN_STATE", "ORIGIN_COUNTRY", "ORIGIN_LATITUDE", "ORIGIN_LONGITUDE" )

flight_airports <- left_join(vuelos_reduc,airports,by="ORIGIN_CODE")

combined <- sort(union(levels(vuelos_reduc$ORIGIN_CODE), levels(airports$ORIGIN_CODE)))
flight_airports <- left_join(mutate(vuelos_reduc, ORIGIN_CODE=factor(ORIGIN_CODE, levels=combined)),
                             mutate(airports, ORIGIN_CODE=factor(ORIGIN_CODE, levels=combined)))

#visualización del volumen de vuelos de cada aeropuerto
longitude <- unique(flight_airports$ORIGIN_LONGITUDE)
latitude <- unique(flight_airports$ORIGIN_LATITUDE)
df = data.frame(Lat = latitude, Long = longitude)
leaflet(df) %>% addTiles() %>% addCircleMarkers() #map visualization


popular_airports <- sort(table(flight_airports$ORIGIN_CODE), decreasing = TRUE )
barplot(popular_airports[1:10], col = "blue", ylim = c(0,40000))

sort(table(flight_airports$DAY_OF_WEEK), decreasing = TRUE )

res <- sapply(vuelos_reduc,class)
kable(data.frame(variables=names(res),clase=as.vector(res)), 
      caption = "asignación de clase de objeto R a cada variable")

vuelos[1:4] <- lapply(vuelos[1:4], as.numeric)
vuelos[6] <- lapply(vuelos[6], as.numeric)
vuelos[10:25] <- lapply(vuelos[10:25], as.numeric)
vuelos[27:31] <- lapply(vuelos[27:31], as.numeric)
res <- sapply(vuelos,class)
tabla_datos <- data.frame(variables=names(res),clase=as.vector(res))
kable(tabla_datos, 
      caption = "asignación de clase de objeto R a cada variable")

str(vuelos)


vuelos_reduc <- dplyr::select(vuelos_reduc, -"YEAR",-"TAIL_NUMBER",-"AIR_SYSTEM_DELAY",-"SECURITY_DELAY",
                              -"AIRLINE_DELAY",-"LATE_AIRCRAFT_DELAY",-"WEATHER_DELAY",
                              -"TAXI_OUT",-"TAXI_IN",-"WHEELS_OFF", -"WHEELS_ON",-"DIVERTED",
                              -"CANCELLED",-"CANCELLATION_REASON")

str(vuelos_reduc)


#Comprobamos valore nulos o valores perdidos
sapply(vuelos_reduc, function(x) sum(is.na(x)))
#Otra forma de sacar los valores nulos
colSums(is.na(vuelos_reduc))

head(vuelos %>% filter(is.na(vuelos$DEPARTURE_DELAY)))

head(vuelos %>% filter(is.na(vuelos$ARRIVAL_DELAY)))

vuelos_reduc <- vuelos_reduc[!is.na(vuelos_reduc$DEPARTURE_DELAY),]
vuelos_reduc <- vuelos_reduc[!is.na(vuelos_reduc$ARRIVAL_DELAY),]


colSums(is.na(vuelos_reduc))
summary(vuelos_reduc)

#Comprobamos si hay valores extraños en las variables día, día de la semana y mes
month_wrong <- which(vuelos_reduc$MONTH > 12 | vuelos_reduc$MONTH < 1)
month_wrong

day_wrong <- which(vuelos_reduc$DAY > 31 | vuelos_reduc$DAY < 1)
day_wrong

day_week_wrong <- which(vuelos_reduc$DAY_OF_WEEK > 7 | vuelos_reduc$DAY_OF_WEEK <1)
day_week_wrong


boxplot(vuelos_reduc$DEPARTURE_DELAY, main="DEPARTURE_DELAY") 
table(vuelos_reduc$DEPARTURE_DELAY [1:200])
head(table(boxplot.stats(vuelos_reduc$DEPARTURE_DELAY)$out))

boxplot(vuelos_reduc$SCHEDULED_TIME, main="SCHEDULED_TIME") 
head(table(vuelos_reduc$SCHEDULED_TIME))

boxplot(vuelos_reduc$AIR_TIME, main="AIR_TIME") 
head(table(boxplot.stats(vuelos_reduc$AIR_TIME)$out))

boxplot(vuelos_reduc$DISTANCE, main="DISTANCE") 
head(table(boxplot.stats(vuelos_reduc$DISTANCE)$out))

boxplot(vuelos_reduc$ARRIVAL_DELAY, main="ARRIVAL_DELAY") 
head(table(vuelos_reduc$ARRIVAL_DELAY))

vuelos_reduc$SCHEDULED_DEPARTURE_HOUR=format(round(trunc(vuelos_reduc$SCHEDULED_DEPARTURE/100),digits=0), nsmall=0)
vuelos_reduc$SCHEDULED_DEPARTURE_HOUR <- as.numeric(vuelos_reduc$SCHEDULED_DEPARTURE_HOUR)
head(vuelos_reduc$SCHEDULED_DEPARTURE_HOUR)

table(vuelos_reduc$SCHEDULED_DEPARTURE_HOUR)

vuelos_reduc$DEPARTURE_HOUR=format(round(trunc(vuelos_reduc$DEPARTURE_TIME/100),digits=0), nsmall=0)
vuelos_reduc$DEPARTURE_HOUR <- as.factor(vuelos_reduc$DEPARTURE_HOUR)
head(vuelos_reduc$DEPARTURE_HOUR)

table(vuelos_reduc$DEPARTURE_HOUR)

vuelos_reduc$ARRIVAL_HOUR=format(round(trunc(vuelos_reduc$ARRIVAL_TIME/100),digits=0), nsmall=0)
vuelos_reduc$ARRIVAL_HOUR <- as.factor(vuelos_reduc$ARRIVAL_HOUR)
head(vuelos_reduc$ARRIVAL_HOUR)

table(vuelos_reduc$ARRIVAL_HOUR)%>% knitr::kable("html")  %>% kable_styling(position='center', font_size=12, fixed_thead=list(enabled=T))

vuelos_reduc <- mutate(vuelos_reduc, RETRASO_TOTAL=ARRIVAL_DELAY - DEPARTURE_DELAY) 

str(vuelos_reduc)
boxplot(vuelos_reduc$RETRASO_TOTAL , xlab="Retrasos", ylab="minutos", ylim=c(-100, 400) )
table(boxplot.stats(vuelos_reduc$RETRASO_TOTAL)$out)
hist(vuelos_reduc$RETRASO_TOTAL)

vuelos_reduc %>% 
  ggplot( aes(x=RETRASO_TOTAL)) +
  geom_density(fill="#99A3A4", color="#99A3A4", alpha=0.9)


shapiro.test(vuelos_reduc$DEPARTURE_DELAY[1:5000])

shapiro.test(vuelos_reduc$ARRIVAL_DELAY[1:5000])

shapiro.test(vuelos_reduc$DISTANCE[1:5000])
shapiro.test(vuelos_reduc$RETRASO_TOTAL[1:5000])

hist(vuelos_reduc$RETRASO_TOTAL)


par(mfrow=c(2,1))
qqnorm(vuelos_reduc$RETRASO_TOTAL, pch = 1, frame = FALSE)
qqline(vuelos_reduc$RETRASO_TOTAL, col = "steelblue", lwd = 2)
boxplot(vuelos_reduc$RETRASO_TOTAL, horizontal=T)

str(vuelos_reduc)

plot(RETRASO_TOTAL ~ as.factor(DAY_OF_WEEK), data=vuelos_reduc)

LeveneTest(RETRASO_TOTAL ~ as.factor(DAY_OF_WEEK), vuelos_reduc, center=median)

fligner.test(RETRASO_TOTAL ~ as.factor(DAY_OF_WEEK), data=vuelos_reduc)

plot(RETRASO_TOTAL ~ as.factor(MONTH), data=vuelos_reduc)
LeveneTest(RETRASO_TOTAL ~ as.factor(MONTH), vuelos_reduc, center=mean)

plot(RETRASO_TOTAL ~ DEPARTURE_HOUR, data=vuelos_reduc)

LeveneTest(RETRASO_TOTAL ~ DEPARTURE_HOUR, vuelos_reduc, center=mean)




bx_vuelos_reduc <- BoxCox(vuelos_reduc$RETRASO_TOTAL, lambda = BoxCoxLambda(vuelos_reduc$RETRASO_TOTAL))


par(mfrow=c(2,2))
qqnorm(vuelos_reduc$RETRASO_TOTAL, main="Lognormal")
qqline(vuelos_reduc$RETRASO_TOTAL,col=2)
qqnorm(bx_vuelos_reduc, main="Box-Cox")
qqline(bx_vuelos_reduc,col=2)
hist(vuelos_reduc$RETRASO_TOTAL,main="Lognormal")
hist(bx_vuelos_reduc, main="Box-Cox")

correlacion <- dplyr::select(vuelos_reduc, "SCHEDULED_DEPARTURE", "DEPARTURE_TIME", "DEPARTURE_DELAY", 
                      "AIR_TIME", "DISTANCE", "SCHEDULED_ARRIVAL", "ARRIVAL_TIME","ARRIVAL_DELAY",
                      "RETRASO_TOTAL","DAY_OF_WEEK","MONTH")

corr.res<-cor(correlacion)
corrplot(corr.res,method="circle")

retraso <- data.frame (RETRASO=vuelos_reduc$RETRASO_TOTAL)
retraso$RETRASO <- ifelse (retraso$RETRASO>0, "SI", "NO")
retraso <- data.frame (retraso, WEEKEND=vuelos_reduc$DAY_OF_WEEK)
#Contamos como fin de semana los viernes, sábados y domingos
retraso$WEEKEND <- ifelse ((retraso$WEEKEND=="5" | retraso$WEEKEND=="6" | retraso$WEEKEND=="7"), "WEEKEND", "WEEKDAY")
str(retraso)
table (retraso$RETRASO)

tabla_retraso_dias <- with(retraso, table(retraso$RETRASO,retraso$WEEKEND))
tabla_retraso_dias %>% knitr::kable("html")  %>% kable_styling(position='center', font_size=12, fixed_thead=list(enabled=T))

chisq.test(tabla_retraso_dias, correct=FALSE)

library(epitools)
oddsratio(tabla_retraso_dias, verbose = TRUE)
oddsratio(tabla_retraso_dias, rev="columns", verbose = TRUE)

ggplot(vuelos_reduc, aes(x=SCHEDULED_ARRIVAL, y=ARRIVAL_DELAY)) +
  geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE)


retraso_modelo1 <- lm(ARRIVAL_DELAY~SCHEDULED_ARRIVAL, vuelos_reduc)
retraso_modelo1 
summary(retraso_modelo1)


retraso_modelo2 <- lm(ARRIVAL_DELAY~SCHEDULED_ARRIVAL + DISTANCE, vuelos_reduc)
retraso_modelo2 
summary(retraso_modelo2)

retraso_modelo3 <- lm(ARRIVAL_DELAY~SCHEDULED_DEPARTURE + DISTANCE + SCHEDULED_ARRIVAL, vuelos_reduc)
retraso_modelo3 
summary(retraso_modelo3)

#y=-4.216+0.006x1+0.0013x2+0.003x3

retraso1 <- data.frame(retraso, DEPARTURE_HOUR=vuelos_reduc$DEPARTURE_HOUR)
retraso$WEEKEND <- ifelse(retraso1$RETRASO=="NO", 0, 1)
retraso1[1:2] <- lapply(retraso1[1:2], as.factor)
str(retraso1)

retraso_glm1 <- glm(RETRASO~WEEKEND , data=retraso1, family=binomial(logit))
retraso_glm1

summary(retraso_glm1)

retraso2 <- data.frame(retraso1, DISTANCE=vuelos_reduc$DISTANCE)
str(retraso2)
retraso_glm2 <- glm(RETRASO~DEPARTURE_HOUR + DISTANCE, data=retraso2, family=binomial(logit))
retraso_glm2
summary(retraso_glm2)

retraso3 <- data.frame(retraso2, MONTH=vuelos_reduc$MONTH,
                      SCHEDULED_DEPARTURE=vuelos_reduc$SCHEDULED_DEPARTURE)
str(retraso3)
retraso_glm3 <- glm(RETRASO~SCHEDULED_DEPARTURE + DISTANCE + MONTH, data=retraso3, family=binomial)
retraso_glm3
summary(retraso_glm3)

newdata = data.frame(SCHEDULED_DEPARTURE = 0800 , DISTANCE=1000, MONTH=6)
predict(retraso_glm3, newdata , type="response")

newdata2 = data.frame(SCHEDULED_DEPARTURE =1600, DISTANCE=1000, MONTH=6)
predict(retraso_glm3, newdata2 , type="response")

retraso4 <- data.frame(retraso3, ORIGIN_AIRPORT=vuelos_reduc$ORIGIN_CODE)

# Para que los resultados estén acotados, elegimos los 10 aeropuertos con más tráfico:
retraso$ORIGIN_AIRPORT <- ifelse (retraso4$ORIGIN_AIRPORT %in% 
                            c("ATL","ORD","DFW","DEN","LAX","SFO","PHX","IAH","LAS","MSP"),
                            retraso4$ORIGIN_AIRPORT,
                            "OTROS")
str(retraso4)
#retraso_glm4 <- glm(RETRASO~DEPARTURE_HOUR + DISTANCE + MONTH + ORIGIN_AIRPORT, data=retraso4,
#                    family=binomial)
#retraso_glm4
#summary(retraso_glm4)


head(vuelos_reduc)
vuelos_reduc$DELAYED <- ifelse(vuelos_reduc$RETRASO_TOTAL>0, 1, 0)

vuelos_reduc %>% 
  group_by(DELAYED) %>%
  summarize(num_obs = n(), 
            mean_delayed = round(mean(RETRASO_TOTAL),0),
            sd_delayed = round(sd(RETRASO_TOTAL),0))

DELAYED_GROUPED <- vuelos_reduc %>% group_by(DELAYED) %>% summarize(num_obs = n(), 
                                                                    obs_totales =length(vuelos_reduc$DAY), 
                                                                    p = num_obs/obs_totales)
DELAYED_GROUPED %>% knitr::kable("html")  %>% kable_styling(position='center', font_size=12, fixed_thead=list(enabled=T))

n1=DELAYED_GROUPED[1,2]
n2=DELAYED_GROUPED[2,2]

prop.test(x=c(209810,75840), n=c(285650,285650), p=NULL, alternative="less", correct=TRUE)


str(retraso4)
#Seleccionamos solo las variables que vamos a utilizar para aplicar el modelo. Dejaremos fuera SCHEDULED_DEPARTURE.
retraso_rf <- select (retraso4, -"SCHEDULED_DEPARTURE")

RANGO_DISTANCIA <- vector()
RANGO_DISTANCIA[retraso_rf$DISTANCE<=500] <- 1
RANGO_DISTANCIA[retraso_rf$DISTANCE>500 & retraso_rf$DISTANCE<=1000] <- 2
RANGO_DISTANCIA[retraso_rf$DISTANCE>1000 & retraso_rf$DISTANCE<=1500] <- 3
RANGO_DISTANCIA[retraso_rf$DISTANCE>1500 & retraso_rf$DISTANCE<=2000] <- 4
RANGO_DISTANCIA[retraso_rf$DISTANCE>2000 & retraso_rf$DISTANCE<=2500] <- 5
RANGO_DISTANCIA[retraso_rf$DISTANCE>2500 & retraso_rf$DISTANCE<=3000] <- 6
RANGO_DISTANCIA[retraso_rf$DISTANCE>3000] <- 7

retraso_rf$RANGO_DISTANCIA <- as.factor(RANGO_DISTANCIA)

levels(retraso_rf$RANGO_DISTANCIA) <- c("Menor 500","Entre 500 y 1000","Entre 1000 y 1500",
                                        "Entre 1500 y 2000","Entre 2000 y 2500","Entre 2500 y 3000",
                                        "Mayor 3000")
table(retraso_rf$RANGO_DISTANCIA)

retraso_rf <- select (retraso_rf, -"DISTANCE")

retraso_rf$MONTH <- as.factor(retraso_rf$MONTH)
}
retraso_rf$ORIGIN_AIRPORT <- ifelse (retraso_rf$ORIGIN_AIRPORT %in% 
                            c("ATL","ORD","DFW","DEN","LAX","SFO","PHX","IAH","LAS","MSP"),
                            retraso_rf$ORIGIN_AIRPORT,
                            "OTROS")
retraso_rf$ORIGIN_AIRPORT <- as.factor(retraso_rf$ORIGIN_AIRPORT)
head(retraso_rf)


grid.newpage()
plotbyweekend<-ggplot(retraso_rf,aes(WEEKEND,fill=RETRASO))+geom_bar() +labs(x="WEEKEND", y="RETRASO")+ guides(fill=guide_legend(title=""))+ scale_fill_manual(values=c("black","#008000"))+ggtitle("RETRASO POR DÍA DE LA SEMANA")
plotbydistance<-ggplot(retraso_rf,aes(RANGO_DISTANCIA,fill=RETRASO))+geom_bar() +labs(x="RANGO_DISTANCIA", y="RETRASO")+ guides(fill=guide_legend(title=""))+ scale_fill_manual(values=c("black","#008000"))+ggtitle("Retraso por Distancia")
plotbyMeses<-ggplot(retraso_rf,aes(MONTH,fill=RETRASO))+geom_bar() +labs(x="MONTH", y="RETRASO")+ guides(fill=guide_legend(title=""))+ scale_fill_manual(values=c("black","#008000"))+ggtitle("Retraso por Mes")
plotbySalida<-ggplot(retraso_rf,aes(DEPARTURE_HOUR,fill=RETRASO))+geom_bar() +labs(x="DEPARTURE_HOUR", y="RETRASO")+ guides(fill=guide_legend(title=""))+ scale_fill_manual(values=c("black","#008000"))+ggtitle("Retraso por Hora Salida")

grid.arrange(plotbyweekend,plotbydistance,plotbyMeses,plotbySalida,ncol=2)

head(retraso_rf,10)



set.seed(300)
indexes = sample(1:nrow(retraso_rf), size=floor((0.6)*nrow(retraso_rf)))
train <- retraso_rf[indexes,]
test <- retraso_rf[-indexes,]

str(train)
}
#Creamos el modelo
model <- randomForest(RETRASO ~.,data=train, importance=T, ntree=150, mtry=5)

varImpPlot(model)

plot(model)

print(model)


mat_confusion<-table(train$RETRASO,model$predicted)
mat_confusion

porcentaje_correct<-100 * sum(diag(mat_confusion)) / sum(mat_confusion)
print(sprintf("El %% de registros correctamente clasificados es: %.4f %%",porcentaje_correct))

resultado<- predict( model, test, type="class" )

confusionMatrix(model$predicted, train$RETRASO)

confusionMatrix(resultado, test$RETRASO)

#Comprobamos en un gráfico de barras los aeropuertos más populares.
popular_airports <- sort(table(vuelos_reduc$ORIGIN_CODE), decreasing = TRUE )
barplot(popular_airports[1:10], col = "blue", ylim = c(0,20000))

#visualización del volumen de vuelos de cada día de la semana

dias_semana <- sort(table(vuelos_reduc$DAY_OF_WEEK), decreasing=TRUE)


barplot(dias_semana, col = "blue", ylim = c(0,50000))

ggplot(vuelos_reduc, aes(x=DAY_OF_WEEK, fill=DAY_OF_WEEK )) + 
  geom_bar( ) +
  scale_fill_hue(c = 40)

ggplot(vuelos_reduc, aes(x=MONTH, fill=MONTH )) + 
  geom_bar( ) +
  scale_fill_hue(c = 20)

ggplot(vuelos_reduc, aes(x=AIRLINE, fill=AIRLINE )) + 
  geom_bar( ) +
  scale_fill_hue(c = 40)

vuelos_reduc %>% 
  filter( RETRASO_TOTAL>0) %>%
  ggplot(aes(AIRLINE,fill=RETRASO_TOTAL))+geom_bar() +labs(x="ORIGIN_CODE", y="RETRASO_TOTAL")+ guides(fill=guide_legend(title=""))+ scale_fill_manual(values=c("black","#008000"))+ggtitle("Retraso por Distancias")

#Horas de salida de los vuelos
ggplot(vuelos_reduc, aes(x=DEPARTURE_HOUR, fill=DEPARTURE_HOUR )) + 
  geom_bar( ) +
  scale_fill_hue(c = 20) +
  theme(legend.position="none")

#Horas de llegada
ggplot(vuelos_reduc, aes(x=ARRIVAL_HOUR, fill=ARRIVAL_HOUR )) + 
  geom_bar( ) +
  scale_fill_hue(c = 20) +
  theme(legend.position="none")


hist(vuelos_reduc$ARRIVAL_DELAY,breaks = 1000, xlim = c(-100,150))

#Retraso por aerolínea
#Analizamos los vuelos retrasados cuando el retraso es menor de 200 minutos
vuelos_reduc %>% 
  filter( DEPARTURE_DELAY<200 & DEPARTURE_DELAY>0) %>%
  ggplot( aes(x=DEPARTURE_DELAY)) +
  geom_density(fill="#99A3A4", color="#99A3A4", alpha=0.9)


vuelos_reduc %>% 
  filter( RETRASO_TOTAL>0) %>%
  ggplot( aes(x=AIRLINE)) +
  geom_density(fill="#99A3A4", color="#99A3A4", alpha=0.9)

vuelos_reduc %>% 
  filter( RETRASO_TOTAL>0) %>%
  ggplot(aes(DEPARTURE_HOUR,fill=RETRASO_TOTAL))+geom_bar() +labs(x="DEPARTURE_HOUR", y="RETRASO_TOTAL")+ guides(fill=guide_legend(title=""))+ scale_fill_manual(values=c("black","#008000"))+ggtitle("Retraso por Hora Salida")

vuelos_reduc %>% 
  filter( RETRASO_TOTAL>0) %>%
  ggplot(aes(DAY_OF_WEEK,fill=RETRASO_TOTAL))+geom_bar() +labs(x="DAY_OF_WEEK", y="RETRASO_TOTAL")+ guides(fill=guide_legend(title=""))+ scale_fill_manual(values=c("black","#008000"))+ggtitle("Retraso por Día de la Semana")

vuelos_reduc %>% 
  filter( RETRASO_TOTAL>0) %>%
  ggplot(aes(MONTH,fill=RETRASO_TOTAL))+geom_bar() +labs(x="MONTH", y="RETRASO_TOTAL")+ guides(fill=guide_legend(title=""))+ scale_fill_manual(values=c("black","#008000"))+ggtitle("Retraso por Meses")

vuelos_reduc %>% 
  filter( RETRASO_TOTAL>0 & DISTANCE<2000) %>%
  ggplot(aes(DISTANCE,fill=RETRASO_TOTAL))+geom_bar() +labs(x="DISTANCE", y="RETRASO_TOTAL")+ guides(fill=guide_legend(title=""))+ scale_fill_manual(values=c("black","#008000"))+ggtitle("Retraso por Distancias")

vuelos_reduc %>% 
  filter( RETRASO_TOTAL>0 & DISTANCE<500) %>%
  ggplot(aes(DISTANCE,fill=RETRASO_TOTAL))+geom_bar() +labs(x="DISTANCE", y="RETRASO_TOTAL")+ guides(fill=guide_legend(title=""))+ scale_fill_manual(values=c("black","#008000"))+ggtitle("Retraso por Distancias")

indice <- which(is.na(vuelos$AIR_SYSTEM_DELAY)) #locations of obs with missing values
vuelos_delay <- vuelos[-indice,]
dim(vuelos_delay)

vuelos_no_delay <- vuelos[indice,]
dim(vuelos_no_delay)

barplot(table(vuelos_delay$AIRLINE) / table(vuelos$AIRLINE) * 100)

table(vuelos_delay$AIRLINE) / table(vuelos$AIRLINE) * 100

barplot(table(vuelos_delay$MONTH) / table(vuelos$MONTH) * 100)
table(vuelos_delay$MONTH) / table(vuelos$MONTH) * 100

barplot(table(vuelos_delay$DAY_OF_WEEK) / table(vuelos$DAY_OF_WEEK) * 100)
table(vuelos_delay$DAY_OF_WEEK) / table(vuelos$DAY_OF_WEEK) * 100

barplot(table(vuelos_delay$ARRIVAL_TIME) / table(vuelos$ARRIVAL_TIME) * 100)

t <- sort(table(vuelos_delay$ORIGIN_AIRPORT) / table(vuelos$ORIGIN_AIRPORT) * 100, decreasing = TRUE)

head(t)

t1 <- sort(table(vuelos_delay$DESTINATION_AIRPORT) / table(vuelos$DESTINATION_AIRPORT) * 100, decreasing = TRUE)

head(t1)
