install.packages("tidymodels")
installed.packages("rlang")

# 1. 导入模块
library(tidyverse)
library(tidymodels)
library(rlang)

# 2. 下载并解压数据集
url <- 'https://dax-cdn.cdn.appdomain.cloud/dax-noaa-weather-data-jfk-airport/1.1.4/noaa-weather-sample-data.tar.gz'
download.file(url, destfile='weather_data.tar.gz')
untar('weather_data.tar.gz')

# 3. 读取数据集
data <- read_csv('noaa-weather-sample-data/jfk_weather_sample.csv')

# 4. 选择列的子集
data_subset <- data %>% select(HOURLYRelativeHumidity, HOURLYDRYBULBTEMPF, HOURLYPrecip, HOURLYWindSpeed, HOURLYStationPressure)

# 5. 清理列
data_cleaned <- data_subset %>% 
  mutate(HOURLYPrecip = str_replace(HOURLYPrecip, "T", "0.0"),
         HOURLYPrecip = str_replace(HOURLYPrecip, "s$", ""))

# 6. 转换列类型
data_numeric <- data_cleaned %>% 
  mutate_at("HOURLYPrecip", as.numeric)

# 7. 重命名列
data_final <- data_numeric %>% 
  rename(relative_humidity = HOURLYRelativeHumidity, 
         dry_bulb_temp_f = HOURLYDRYBULBTEMPF, 
         precip = HOURLYPrecip, 
         wind_speed = HOURLYWindSpeed, 
         station_pressure = HOURLYStationPressure)

# 8. 划分数据集
set.seed(1234)
data_split <- initial_split(data_final, prop = 0.8)
train_data <- training(data_split)
test_data <- testing(data_split)

# 9. 探索性数据分析
ggplot(train_data, aes(x = dry_bulb_temp_f)) + geom_histogram()

# 10. 建立模型
recipe <- recipe(precip ~ ., data = train_data)
workflow <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(linear_reg())

# 11. 改进模型
workflow$add_model(linear_reg())

# 12. 模型评估
trained_model <- workflow %>%
  fit(data = train_data)

# 13. 模型比较
# 在测试集上评估各模型，比较指标

