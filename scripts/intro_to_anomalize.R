# ---------------------- Инсталляция необходимых пакетов -----------------------

required_packages <- c("tidyverse", "anomalize", "tibbletime")
existing_packages <- installed.packages()[,"Package"]
to_install <- required_packages[!required_packages %in% existing_packages]
if (length(to_install) > 0) {install.packages(required_packages)}

require(tidyverse)
require(anomalize)
require(tibbletime)


# ------------------- Загрузка данных знакомство с ними ------------------------

dat <- read_csv("data/expedia_hotel_prices.csv") %>% 
    group_by(prop_id) %>% 
    arrange(date_time) %>%
    tbl_time(date_time)

dat %>% glimpse()

dat %>% 
    ggplot(., aes(date_time, price_usd)) +
    geom_line() +
    facet_wrap(~prop_id)


# ----------------------------- Первый пример ----------------------------------

result_13252 <- dat %>% 
    filter(prop_id == 13252) %>% 
    time_decompose(price_usd, merge = TRUE) %>%
    anomalize(remainder) %>%
    time_recompose()

result_13252 %>% glimpse()

# Визуализация компонентов временного ряда:
result_13252 %>% plot_anomaly_decomposition()

# Визуализация исходного временного ряда с аномалиями:
result_13252 %>% plot_anomalies()


# ------------------------ Настройка параметров --------------------------------

# Просмотр "справочника" типичных значений frequency и trend:
get_time_scale_template()

# Ручное изменение значений frequency и trend:
dat %>% 
    filter(prop_id == 13252) %>% 
    time_decompose(price_usd, 
                   frequency = 2,
                   trend = 6) %>%
    anomalize(remainder) %>%
    time_recompose() %>% 
    plot_anomaly_decomposition()


# Изменение метода декомпозиции ряда:
dat %>% 
    filter(prop_id == 13252) %>% 
    time_decompose(price_usd, 
                   frequency = 2,
                   trend = 6,
                   method = "twitter") %>%
    anomalize(remainder) %>%
    time_recompose() %>% 
    plot_anomaly_decomposition()


# Изменение чувствительности метода выявляения аномалий:
dat %>% 
    filter(prop_id == 13252) %>% 
    time_decompose(price_usd, 
                   frequency = 2,
                   trend = 6) %>%
    anomalize(remainder, alpha = 0.025) %>%
    time_recompose() %>% 
    plot_anomaly_decomposition()

# -------------- Одновременный анализ нескольких рядов -------------------------

dat %>% 
    time_decompose(price_usd) %>%
    anomalize(remainder) %>%
    time_recompose() %>% 
    plot_anomalies(time_recomposed = TRUE)
