library(tidyverse)

da_data <-  read_csv("/home/willi/Documents/GitHub/data2004-hargis/Data/Raw/chocolate.csv")

glimpse(da_data)

da_data %>%
  filter(company_location == "U.S.A." | rating >= 3.5)

da_data %>%
  filter(company_location %in% c("U.S.A.", "France", "Canada") | rating >= 3.5)

da_data %>%
  filter(company_location %in% c("U.S.A.", "Vietnam") & rating >= 3.5 & review_date == 2021)

da_data %>%
  select(rating, country_of_bean_origin) %>%
  arrange(desc(rating)) %>%
  slice_head(n = 10)

da_data <- da_data %>%
  mutate(
    coca_num = parse_number(cocoa_percent)
  )

da_data %>%
  group_by(company_location) %>%
  summarise(
    n = n(),
    avg_rating = mean(rating, na.rm = TRUE),
    avg_cocoa = mean(coca_num, na.rm = TRUE)
  ) %>%
  slice_head(n = 10)

