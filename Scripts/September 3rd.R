install.packages("tidyverse")

library(tidyverse)

pop <-  read_csv("Data/Raw/API_SP.POP.TOTL_DS2_en_csv_v2_285942.csv", skip = 4)

glimpse(pop)

pop %>%
  filter(!is.na("...71"))

clean_pop <- pop %>%
  select(-...71)

glimpse(clean_pop)

install.packages("readxl")
library(readxl)

pop_xl <- read_excel("Data/Raw/API_SP.POP.TOTL_DS2_en_excel_v2_290931.xls", skip = 3, sheet = "Data")

install.packages("jsonlite")
library(jsonlite)

pop_json <- read_json("")