#Петров В.Н. ПАЭ 121 
# создайте модель множественной линейной регрессии 
#ночных потоков углекислого газа за летний период 2013 года 
#по данным измерений методом турбулентной пульсации


#Установка и проверка рабочей директории

setwd("W:/MatMod/Zadani2")

getwd()


#подключаем необходимые пакеты для дальнейшей работы


library("tidyverse") 
library("stringr")    
library("dplyr")      
library("ggplot2")  
library("readr")

#Считываем данные из файла 
#При этом пропускаем первую строку,заменяем все не числовые значения на NA
# и ингорируем строчки с "["

eddypro = read.csv("eddypro.csv", skip = 1, na=c("","NA","-9999","-9999.0"), comment=c("["))

#Удаляем перрвую строчку

eddypro = eddypro[-1, ]

#Удаляем пустой столбец

eddypro = select(eddypro, -(roll))

#Преобразуем строковые значения в факторные

eddypro = eddypro %>% mutate_if(is.character, factor)

#Заменяем символы в названии столбцов на допустимые для переменных названия

names(eddypro) = names(eddypro) %>% 
  str_replace_all("[!]", "_exclam_") %>% 
  str_replace_all("[?]", "_quest_") %>% 
  str_replace_all("[*]", "_star_") %>% 
  str_replace_all("[+]", "_plus_") %>%
  str_replace_all("[-]", "_minus_") %>%
  str_replace_all("[@]", "_at_") %>%
  str_replace_all("[$]", "_dollar_") %>%
  str_replace_all("[#]", "_hash_") %>%
  str_replace_all("[/]", "_slash_") %>%
  str_replace_all("[%]", "__pecent_") %>%
  str_replace_all("[&]", "_amp_") %>%
  str_replace_all("[\\^]", "_power_") %>%
  str_replace_all("[()]", "_")

#Превращаем столбцы таблицы в виде векторов для проверки

glimpse(eddypro)

#Удаляем строки с NA

eddypro = drop_na(eddypro)

#Выбираем летний период (июнь -152 день, август -244 день)

eddypro = filter(eddypro, DOY >= 152 & DOY < 244)

#Оставлем данные ночного периода

eddypro = filter(eddypro, daytime==F)

#Получаем таблицу состоящую из цифр

eddypro_numeric = eddypro[,sapply(eddypro,is.numeric)]

#Таблица с остальными колонками

eddypro_non_numeric = eddypro[,!sapply(eddypro,is.numeric)]


#Создаем непересекающиеся выборки

row_numbers = 1:length(eddypro_numeric$co2_flux)
teach = sample(row_numbers, floor(length(eddypro_numeric$co2_flux)*.7))
test = row_numbers[-teach]

#Обучающая выборка

teaching_tbl = eddypro_numeric[teach,]

#Тестирующая выборка

testing_tbl = eddypro_numeric[test,]

#######################################Модель №1 по обучающей выборке

mod1 = lm(co2_flux~ (.) , data = teaching_tbl)

#Информация о моделе

summary(mod1)

#Коэффициенты модели

coef(mod1)

#Дисперсионный анализ модели - Анализируем переменные по значимости, решаем:
#какие переменные добавить во вторую модель - добавляем значимые :***, **, *.

anova(mod1)

#Графическое представление модели:

plot(mod1)



####################################################Модель №2 

mod2 = lm ( co2_flux~  DOY + file_records + qc_Tau + rand_err_Tau + H + rand_err_H + LE + qc_LE + rand_err_LE + qc_co2_flux
          + rand_err_co2_flux + h2o_flux  + rand_err_h2o_flux + h2o_v.adv + co2_molar_density + co2_mole_fraction + co2_mixing_ratio
          + h2o_molar_density + h2o_mixing_ratio + h2o_time_lag + sonic_temperature + air_temperature + air_pressure 
          + air_heat_capacity + air_molar_volume + water_vapor_density + e + es + specific_humidity + RH + Tdew
          + u_unrot + v_unrot + u_rot + v_rot + max_speed + wind_dir + yaw + pitch + u. + TKE + L + X.z.d..L + T. + x_peak + x_offset 
          + x_10. + x_30. + x_50. + x_70. + un_Tau + Tau_scf + un_H + H_scf + un_LE + un_co2_flux + un_h2o_flux  
          + w_spikes + co2_var + h2o_var + w.ts_cov + w.h2o_cov + co2 + h2o , data = teaching_tbl )

#Информация о модели

summary(mod2)

#Дисперсионный анализ

anova(mod2)

#Сравнение двух моделей

anova(mod2, mod1)

#Графическое представление 2 модели

plot(mod2) 



############################Модель №3 - Дорабатываем предыдущую модель, отбрасывая незначимые переменные

mod3 = lm ( co2_flux~  DOY + file_records + qc_Tau + rand_err_Tau + H + rand_err_H + LE + qc_LE + rand_err_LE + qc_co2_flux
            + rand_err_co2_flux + h2o_flux  + rand_err_h2o_flux + h2o_v.adv + co2_molar_density + co2_mole_fraction + co2_mixing_ratio
            + h2o_molar_density + h2o_mixing_ratio + h2o_time_lag + sonic_temperature + air_temperature + air_pressure 
            + air_heat_capacity + air_molar_volume + water_vapor_density + e + es + specific_humidity + Tdew
            + u_unrot + v_unrot + u_rot + v_rot + max_speed + wind_dir + yaw + pitch + TKE + L + X.z.d..L + T. + x_peak + x_offset 
            + x_10. + x_30. + x_50. + x_70. + un_Tau + Tau_scf + H_scf + un_LE + un_co2_flux + un_h2o_flux  
            + w_spikes + co2_var + h2o_var + w.ts_cov + w.h2o_cov + co2 + h2o , data = teaching_tbl )



#Информация о модели

summary(mod3)

#Дисперсионный анализ

anova(mod3)

#Сравнение двух моделей

anova(mod3, mod2)

#Графическое представление 3 модели

plot(mod3) 



####################################################Модель №4 - опять отбрасываем

mod4 = lm ( co2_flux~  DOY + file_records + qc_Tau + H + rand_err_H + LE + qc_LE + rand_err_LE + qc_co2_flux
            + rand_err_co2_flux + h2o_flux  + rand_err_h2o_flux + h2o_v.adv + co2_molar_density + co2_mole_fraction + co2_mixing_ratio
            + h2o_molar_density + h2o_mixing_ratio + h2o_time_lag + sonic_temperature + air_temperature + air_pressure 
            + air_heat_capacity + air_molar_volume + water_vapor_density + e + es + specific_humidity + Tdew
            + u_unrot + v_unrot + u_rot + v_rot + max_speed + wind_dir + yaw + pitch + TKE + L + X.z.d..L + T. + x_peak + x_offset 
            + x_10. + x_30. + x_50. + x_70. + Tau_scf + H_scf + un_LE + un_co2_flux + un_h2o_flux  
            + w_spikes + co2_var + h2o_var + w.ts_cov + w.h2o_cov + co2 , data = teaching_tbl )


#Информация о модели

summary(mod4)

#Дисперсионный анализ

anova(mod4)

#Сравнение двух моделей

anova(mod4, mod3)

#Графическое представление 4 модели

plot(mod4) 


######################################Модель №5 - получили модель с только значимыми переменными

mod5 = lm ( co2_flux~  DOY + file_records + qc_Tau + H + rand_err_H + LE + qc_LE + rand_err_LE + qc_co2_flux
            + rand_err_co2_flux + h2o_flux  + rand_err_h2o_flux + h2o_v.adv + co2_molar_density + co2_mole_fraction + co2_mixing_ratio
            + h2o_molar_density + h2o_mixing_ratio + h2o_time_lag + sonic_temperature + air_temperature + air_pressure 
            + air_heat_capacity + air_molar_volume + water_vapor_density + e + es + specific_humidity + Tdew
            + u_unrot + v_unrot + u_rot + v_rot + max_speed + wind_dir + yaw + pitch + TKE + L + X.z.d..L + T. + x_peak + x_offset 
            + x_30. + x_50. + x_70. + Tau_scf + H_scf + un_LE + un_co2_flux + un_h2o_flux  
            + w_spikes + co2_var + h2o_var + w.ts_cov + w.h2o_cov + co2 , data = teaching_tbl )


#Информация о модели

summary(mod5)

#Дисперсионный анализ

anova(mod5)

#Сравнение двух моделей

anova(mod5, mod4)

#Графическое представление 5 модели

plot(mod5) 




#Корреляционный анализ переменных участвующих в линейной модели
cor_teaching_tbl = select(teaching_tbl, DOY , file_records , qc_Tau , H , rand_err_H , LE , qc_LE , rand_err_LE , qc_co2_flux,                  
                           rand_err_co2_flux , h2o_flux  , rand_err_h2o_flux , h2o_v.adv , co2_molar_density , co2_mole_fraction , co2_mixing_ratio,
                           h2o_molar_density , h2o_mixing_ratio , h2o_time_lag , sonic_temperature , air_temperature , air_pressure, 
                           air_heat_capacity , air_molar_volume , water_vapor_density , e , es , specific_humidity , Tdew,
                           u_unrot , v_unrot , u_rot , v_rot , max_speed , wind_dir , yaw , pitch , TKE , L , X.z.d..L , T. , x_peak , x_offset, 
                           x_30. , x_50. , x_70. , Tau_scf , H_scf , un_LE , un_co2_flux , un_h2o_flux,  
                           w_spikes , co2_var , h2o_var , w.ts_cov , w.h2o_cov , co2 )
                          

#Получение таблицы коэффициентов корреляций
cor_td = cor(cor_teaching_tbl) %>% as.data.frame


#Построение графиков по полученной моделе
#Построение точек по значениями обучающей выборки и наложение предсказанных значений по 5 модели
qplot(co2_flux , co2_flux, data = teaching_tbl) + geom_line(aes(y = predict(mod5, teaching_tbl)))
#Построение точек по значением тестирующей выборки и наложение предсказанных значений по 5 модели
qplot(co2_flux , co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod5, testing_tbl)))

#Примеры
qplot(DOY, co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod5, testing_tbl)))
qplot(co2, co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod5, testing_tbl)))
qplot(water_vapor_density, co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod5, testing_tbl)))
