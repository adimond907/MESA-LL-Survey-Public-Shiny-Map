library(RODBC)
library(tidyverse)

conn_str <- "Driver={ODBC Driver 18 for SQL Server};
             Server=161.55.120.71, 1919;
             Database=Longline;
             Trusted_Connection=yes;
             TrustServerCertificate=yes;"

conn <- odbcDriverConnect(conn_str)

area_cpue <- sqlQuery(conn,"select * from dbo.WebAreaView") %>%
  filter(survey == "United States",
         species %in% c("Giant grenadier","Shortspine thornyhead","Sablefish","Pacific cod","Pacific halibut","Shortraker rockfish","Rougheye rockfish"))
  
station_cpue <- sqlQuery(conn,"select * from dbo.WebStationView") %>%
  filter(Survey == "United States") %>%
  mutate(Species=if_else(Species=="Rougheye rockfish","Rougheye/Blackspotted rockfish",Species),
         TotalCatch=round(TotalCatch,0))

write.csv(station_cpue,"data/station_cpue.csv")
write.csv(area_cpue,"data/area_cpue.csv")



