# 2024-12-24
# update fishery data

library(tidyverse)

# data cleaning
# country groups
df1 = read_csv("./data/CL_FI_COUNTRY_GROUPS.csv") |> 
  select(UN_Code, Identifier, Name_En, GeoRegion_Group_En) |> 
  filter(str_detect(GeoRegion_Group_En, "Asia")) |> 
  filter(
    Name_En %in% c("Japan", "Republic of Korea", "China", "Taiwan Province of China")
  )

# Water Area
df2 = water_area = read_csv("./data/CL_FI_WATERAREA_GROUPS.csv")

# production data
df3 = production = read_csv("./data/Global_production_quantity.csv") |> 
  dplyr::rename(UN_Code = COUNTRY.UN_CODE)

write_rds(df1, "./data/country_groups.rds")
write_rds(df2, "./data/water_area.rds")
write_rds(df3, "./data/global_fishery_production.rds")

