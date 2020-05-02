#Задание 1
#Петров Вадим Николаевич студент ПАЭ 121
#для региона 61 рассчитайте урожайность пшеницы в 1999 году, 
#взяв для рассчета средние суммы активных температур за предыдущие 2 года, 
#с метеостанций в радиусе не более 100 км
  
  
# Проверка рабочей дирректории 
setd("W:/Matmod")
getwd()
  
#Присоеденяем необходимые пакеты
  
library(tidyverse)
library(rnoaa)
library(lubridate)
  
#Скачиванием список метеостанций 
#station_data = ghcnd_stations()
# Сохраним в файл
#write.csv(station_data, "station_data2020.csv")
 
station_data = read.csv("station_data2020.csv") 
  
  
#После получения списка всех станций, выберем из него список станций ближайших к 
#столице вашего региона,создав таблицу с именем региона и координатами его столицы
 
rostov = data.frame(id = "rostov", latitude = 47.222531,longitude= 39.718705)

?meteo_nearby_stations
   
#выбираем метеостанции в радиусе 100 км от Ростова в период 1997-1998 гг. 
#и выбираем переменные, которые обязательно должны быть в наличии
 
rostov_around=meteo_nearby_stations(lat_lon_df = rostov, station_data = station_data,
                                      radius = 100,var = c("TAVG"),
                                      year_min = 1997, year_max = 1998)
   
    
#Открываем список метеостанции в радиусе 100 км от Ростова, их всего 3
    
rostov_around
    
    
#идентификатор метеостанции Ростова
    
rostov_id=rostov_around[["rostov"]][["id"]][1]
rostov_table=rostov_around[[1]]

#Получение таблицы ближайших метеостанций
rostov_table = data.frame(rostov_around)

# Промежуточный объект, куда скачиваются данные с конкретной метеостанции
all_i = data.frame()

# Объект куда скачиваются все данные со всех метеостанций
all_rostov_meteodata = data.frame()

#Создадим цикл, чтобы скачать все данные со всех трех метеостанций
  
  for(i in 1:3)
  {
    rostov_id =  rostov_around[["rostov"]] [["id"]] [ i]
    data = meteo_tidy_ghcnd(stationid = rostov_id,
                            var = "TAVG",
                            date_min = "1993-01-01",
                            date_max = "2001-12-31")
    all_rostov_meteodata =  bind_rows(all_rostov_meteodata, data)
    
  }
  
#Записываем полученные данные в файл
write.csv(all_rostov_meteodata, "all_rostov_meteodata.csv")
all_rostov_meteodata
  
#Разбивка даты на составляющие(год, месяц, день года) 
# считываем данные из файла all_rostov_meteodata.csv
all_rostov_meteodata = read.csv("all_rostov_meteodata.csv")

# Добавим год, месяц,день
all_rostov_meteodata = mutate(all_rostov_meteodata, year = year(date), month = month(date), day = day(date))
str(all_rostov_meteodata)
  
# Отфильтруем данные за 1997 - 1998 годы
years_rostov_meteodata = filter(all_rostov_meteodata, year %in% c( 1997:1998))
 
# Проверим результат
str(years_rostov_meteodata)
summary(years_rostov_meteodata)
  
#температура в десятых долях градуса, нужно разделить на 10
years_rostov_meteodata[,"tavg"]= years_rostov_meteodata$tavg / 10 
  
#Подсчет средних активных температур по месяцам, приводим к нормальному виду, превращаем в 0 все NA и значения меньше 5 градусов
years_rostov_meteodata[is.na(years_rostov_meteodata$tavg),"tavg"] = 0
years_rostov_meteodata[years_rostov_meteodata$tavg<5, "tavg"] = 0

#Группируем по id станциям, годами и месяцам
alldays = group_by(years_rostov_meteodata, id, year, month)
  
  
#Суммируем температуру по этим группам
sumT_alldays_rostov = summarize(alldays, tsum = sum(tavg))
summary(sumT_alldays_rostov)
  
#Группируем данные по месяцам  
groups_rostov_months = group_by(sumT_alldays_rostov, month)
summary(groups_rostov_months)
 
#Находим для метеостанций среднее по месяцам
sumT_months=summarize(groups_rostov_months,St=mean(tsum))
sumT_months
  
# Подготовка к расчету по формуле Урожая
#Ввод констант для расчета урожайности
afi = c(0.00, 0.00, 0.00, 32.11, 26.31, 25.64, 23.20, 18.73, 16.30, 13.83, 0.00, 0.00)
bfi = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03, 8.16, 6.59, 5.73, 4.87, 0.00, 0.00)
di = c(0.00, 0.00, 0.00, 0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00, 0.00, 0.00)
#Коэффициент для экспозиции склона - считаем, что все поля идеально ровные
y = 1.0
#Коэффициент использования ФАР посевом
Kf = 300
#Калорийность урожая культуры
Qj = 1600
#Коэффициент "сумма частей основной и побочной продукции"
Lj = 2.2
#Коэффициент "стандартная влажность культуры"
Ej = 25 
  
  
#Рассчитываем Fi по месяцам
sumT_months = mutate(sumT_months, Fi = afi + bfi * y * St)
  
#Рассчитываем Yi
sumT_months = mutate(sumT_months, Yi = ((Fi * di) * Kf) / (Qj * Lj * (100 - Ej)))
 
#Расчитываем урожай, как сумму по месяцам
Yield = sum(sumT_months$Yi)
Yield
# Ответ: 20,87 ц/га
# Для региона 61 урожайность пшеницы в  1999 году составит 20,87 ц/га
  
  
  