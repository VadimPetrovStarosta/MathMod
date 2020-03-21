#Задание 1
#Петров Вадим Николаевич студент ПАЭ 121 - для региона 61 Ростовская область
#для региона 61 рассчитайте урожайность пшеницы в 1999 году, взяв для рассчета средние суммы активных температур за предля региона 61 рассчитайте урожайность пшеницы в 1999 году, взяв для рассчета средние суммы активных температур за предыдущие 2 года, с метеостанций в радиусе не более 100 кмдыдущие 2 года, с метеостанций в радиусе не более 100 км
setwd("W:/Matmod")
getwd()

#Присоеденяем необходимые пакеты

library(tidyverse)

library(rnoaa)

#Один раз (!) скачиваем список метеостанций
#station_data = ghcnd_stations() 
#write.csv(station_data,"station_data2020.csv")
station_data = read.csv("station_data2020.csv")

#После получения списка всех станций, выберем из него список станций ближайших к 
#столице вашего региона,создав таблицу с именем региона и координатами его столицы
#координаторы должны быть в десятых градусов

rostov = data.frame(id = "ROSTOV", latitude = 47.222531,  longitude = 39.718705)
? meteo_nearby_stations

#выбираем метеостанции в радиусе 100 км от Ростова 
#в период с 1997-1998, и выбираем переменные, которые обязательно должны быть в наличии

rostov_around = meteo_nearby_stations(lat_lon_df = rostov, station_data = station_data,
radius = 100, var = c("PRCP", "TAVG"),year_min = 1997, year_max = 1998)

#Открываем список метеостанции в радиусе 100 км от Ростова, их всего 3

rostov_around

#идентификатор метеостанции Ростова

rostov_id = rostov_around[["ROSTOV"]][["id"]][1]
summary (rostov_id)
str(rostov_id)
rostov_id[3]

#Получение таблицы ближайших метеостанций
rostov_table = data.frame (rostov_around)
summary (rostov_table)




all_rostov_data = meteo_tidy_ghcnd(stationid = rostov_id)
all_rostov_data
summary (all_rostov_data)


all_i = data.frame()
all_rostov_meteodata = data.frame()
for (i in 1:8)
  #
{
  all_i = meteo_tidy_ghcnd(stationid = rostov_id[i],
                         var="TAVG",
                         date_min="1997-01-01",
                         date_max="1998-12-31")
all_rostov_meteodata=rbind(all_rostov_meteodata,all_i)
}

#Записываем полученные данные в файл
write.csv (all_rostov_meteodata,"all_rostov_meteodata.csv")
all_rostov_meteodata
