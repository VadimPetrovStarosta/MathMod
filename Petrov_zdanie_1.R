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
data.frame (rostov_around)
# # Загрузка погодных данных для выбранных метеостанций
# Для получения всех данных с 1 метеостанции

all_rostov_data = meteo_tidy_ghcnd(stationid = rostov_id)
all_rostov_data
summary (all_rostov_data)


all_i = data.frame()
all_rostov_meteodata = data.frame()

#Создадим цикл, чтобы скачать все данные со всех трех метеостанций
for (i in 1:3)
  {
  all_i = meteo_tidy_ghcnd ( stationid = rostov_id,
                         var="TAVG",
                         date_min="1997-01-01",
                         date_max="1998-12-31")
all_rostov_meteodata=rbind(all_rostov_meteodata,all_i)
}

#Записываем полученные данные в файл
write.csv (all_rostov_meteodata,"all_rostov_meteodata.csv")
all_rostov_meteodata

# 4. Разбивка даты на составляющие(год, месяц, день года) 
# считываем данные из файла all_rostov_meteodata.csv
all_rostov_meteodata = read.csv ( "all_rostov_meteodata.csv" )
  #посмотрим на данные
  str(all_rostov_meteodata)

#ищем библиотеку из tidyverse, которая может помочь с датой
library(lubridate)
 
  
# вытащить год
y = year(all_rostov_meteodata$date); y
all_rostov_meteodata [,"year"]= year(all_rostov_meteodata$date)
  #добавим месяц
  all_rostov_meteodata [,"month"]= month (all_rostov_meteodata$date)
  
  #вытащить день от начала года
  all_rostov_meteodata [,"day_of_the_year"]= yday(all_rostov_meteodata$date)
  
  #проверим результат
  str(all_rostov_meteodata)
  all_rostov_meteodata
  
  #отфильтруем данные
  years_rostov_meteodata =filter(all_rostov_meteodata, year %in% c(1997:1998))
  str(years_rostov_meteodata)
  summary(years_rostov_meteodata)
  
#температура в десятых долях градуса, нужно разделить на 10
  years_rostov_meteodata [,"tavg"] = years_rostov_meteodata$tavg/10
  summary (years_rostov_meteodata)
  
  #Подсчет средних активных температур по месяцам, приводим к нормальному виду, превращаем в 0 все NA и значения меньше 5 градусов
  years_rostov_meteodata [is.na(years_rostov_meteodata$tavg),"tavg"] = 0
  years_rostov_meteodata [years_rostov_meteodata$tavg<5, "tavg"] = 0
  
  #Проверяем, какие данные получились
  summary(years_rostov_meteodata)
  str(years_rostov_meteodata)
  years_rostov_meteodata
  #Считаем суммарную температуру за месяц для всех станций
  #Группируем по id станциям, годами и месяцам
 
  
  alldays=group_by (years_rostov_meteodata,id,year,month)
  alldays
  str(alldays)
  summary(alldays)
  
  ###Как мне кажется что-то пошло не так здесь
  #X                   
  #Min.   :   1.0    
  #1st Qu.: 548.2                     
  #Median :1095.5                     
  #Mean   :1095.5                     
  #3rd Qu.:1642.8                     
  #Max.   :2190.0
  #Не могу понять почему месячные температуры такие высокие получились
  # В предыдущей строке, все идет по плану
  
  
  #Суммируем температуру по этим группам
  sum_t=summarize (alldays, tsum = sum(tavg))
  str(sum_t)
  summary(sum_t)
  sum_t
  #Группируем данные по месяцам
  group_months=group_by(sum_t,month)
  group_months
  #Находим для метеостанций среднее по месяцам
  sum_t_months=summarize(group_months, St = mean(tsum))
  sum_t_months
  
  #Ввод констант для расчета урожайности
  afi = c(0.000,0.000,0.000,32.110,26.310,25.640,23.200,18.730,16.300,13.830,0.000,0.000)
  bfi = c(0.000,0.000,0.000,11.300,9.260,9.030,8.160,6.590,5.730,4.870,0.000,0.000)
  di = c(0.000,0.000,0.000,0.330,1.000,1.000,1.000,0.320,0.000,0.000,0.000,0.000) #отношение числа дней i-го месяца входящих в период вегетации культуры, к общему числу дней в месяце
  Kf = 300 #коэф использования ФАР посевом
  Qj = 1600 #калорийность урожая культуры
  Lj = 2.2 #сумма частей основной и побочной продукции
  Ej = 25 # Стандартная влажность культуры
  
 
  #Рассчитываем Fi по месяцам
  sum_t_months = mutate(sum_t_months, Fi = afi+bfi*1*St)
  #Рассчитываем Yi
  sum_t_months = mutate (sum_t_months, Yi = ((Fi*di)*Kf)/(Qj*Lj*(100-Ej)))
  #Рассчитываем урожай как сумму по месяцам
  Yield = sum(sum_t_months$Yi)
  Yield
  #Результат = 66.08 ц/га урожайность пшеницы
  
  
  
  
  