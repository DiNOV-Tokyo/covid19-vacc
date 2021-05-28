library(tidyverse)

dat <- read.csv("vacc.csv", fileEncoding = "UTF-8")

library(rgdal)

# From https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html
japan <- readOGR("gadm36_JPN_shp/gadm36_JPN_1.shp",
                  GDAL1_integer64_policy = TRUE)                  
                #  layer = "cb_2013_us_state_20m", GDAL1_integer64_policy = TRUE)

library(leaflet)

leaflet(japan) %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
#              fillColor = ~colorQuantile("YlOrRd", ALAND)(ALAND),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))

library(tidyverse)
library(maps)
library(mapproj)
library(leaflet)

# 1.地図データの読み込み：ダウンロードしたシェープファイルを使う
jpn.shp <- readRDS("gadm36_JPN_1_sp.rds")
plot(jpn.shp)

# 2.統計データの読み込み
#library("choroplethr")
#data(df_japan_census)

vacc_dat <- read.csv("vacc.csv", fileEncoding = "UTF-8") %>% 
  slice(2:48)

vacc_dat %>% left_join(jpn)
jpn.shp@data[["NL_NAME_1"]]
# 2-1.都道府県名などで一致する列を見つける（toupper()は英文字列を大文字にする関数）
#table(toupper(jpn.shp@data[["NAME_1"]]) == toupper(df_japan_census$region))
# 長崎が'Naoasakiになっているスペルミスなどで一致しない行が２つあったが，データの順番は問題ないのでプロットに使用可能


# 塗る色（連続値のカラーパレット）をセット
pal <- colorNumeric("YlOrRd", domain=jpn.shp@data$vacc, reverse=F)

# マウスオーバー時の表示内容を設定（sprintf()で実数表記など指定）
labels <- sprintf("<strong>%s</strong><br/>接種回数：%5.0f<br/>内1回目：%5.0f<br/>内2回目：%5.0f",
                  paste0(jpn.shp@data$NL_NAME_1),
                  jpn.shp@data$vacc,
                  jpn.shp@data$vacc1,
                  jpn.shp@data$vacc2) %>% lapply(htmltools::HTML)

library("sf")
va <- vacc_dat %>% mutate(NL_NAME_1=pref)
sp_data <- jpn.shp@data %>% 
  inner_join(va, by="NL_NAME_1") %>% 
  select(pref, vacc, vacc1, vacc2)

  
jpn.shp@data$pref <- sp_data$pref
jpn.shp@data$vacc <- sp_data$vacc
jpn.shp@data$vacc1 <- sp_data$vacc1
jpn.shp@data$vacc2 <- sp_data$vacc2

jpn.shp@data$pref

d_tokyo_ <- d_tokyo %>%
  mutate(市区名 = `Encoding<-`(市区名, "UTF-8")) %>%
  inner_join(d_chika) %>% st_as_sf(.)

# 地図にプロット
jpn.shp %>% 
  leaflet() %>% 
  # setView() : 地図を日本にズームした状態で表示する
  setView(lat=37, lng=139, zoom=5) %>% 
  # addProviderTiles() : 背景のタイルを指定
  addProviderTiles(providers$CartoDB.Positron) %>% 
  # addPolygons() : 塗り分け地図の描画
  addPolygons(fillOpacity = 0.5,
              weight=1,
              fillColor = ~pal(jpn.shp@data$vacc),
              color = "orange",
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              highlight = highlightOptions(
                weight = 5,
                color = "#888",
                fillOpacity = 0.5,
                bringToFront = TRUE)
              ) %>% 
  addLegend("bottomright", pal = pal, values = ~jpn.shp@data$vacc,
            title = "新型コロナウイルス　ワクチン接種状況"  )
