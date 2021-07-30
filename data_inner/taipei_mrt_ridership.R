library(dplyr)
library(ggplot2)
library(ggnewscale)
library(sf)
library(ggspatial)
library(rgeos)
library(raster)
library(xml2)
library(httr)
options(digits=10)
windowsFonts(A=windowsFont("Times New Roman"))

#PTX api
get_ptx_data <- function (app_id, app_key, url, Windows = FALSE){
  # First save your current locale
  loc <- Sys.getlocale("LC_TIME")
  
  if (Windows){
    Sys.setlocale(category = "LC_TIME", locale = "English_United States.1252")
  } else {
    Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF8")
  }
  
  # "Tue, 21 Aug 2018 01:18:42 GMT"
  xdate <- format(as.POSIXlt(Sys.time(), tz = "GMT"), "%a, %d %b %Y %H:%M:%S GMT")
  sig <- hmac_sha1(app_key, paste("x-date:", xdate)) 
  
  # hmac username="APP ID", algorithm="hmac-sha1", headers="x-date", 
  # signature="Base64(HMAC-SHA1("x-date: " + x-date , APP Key))"
  
  authorization <- paste0(
    'hmac username="', app_id, '", ',
    'algorithm="hmac-sha1", ',
    'headers="x-date", ',
    'signature="', sig, '\"', sep = '')
  
  auth_header <- c(
    'Authorization'= authorization,
    'x-date'= as.character(xdate))
  
  dat <- GET(url, 
             config = config(ssl_verifypeer = 0L), 
             add_headers(.headers = auth_header))
  
  print(http_status(dat)$message)
  
  # Set back to origin locale
  Sys.setlocale('LC_TIME', loc)
  
  # return(dat)
  return(content(dat))
}

app_id = '8f35504e01eb4a43abfd41c920955690'
app_key = 'H9MfljykHDeGiifyr2zKJ0XsKFQ'

# 台北捷運路線
url="https://ptx.transportdata.tw/MOTC/v2/Rail/Metro/Shape/TRTC?&$format=XML"
x = get_ptx_data(app_id, app_key, url, Windows = TRUE)

taipei_mrt=data.frame(
  cbind(
    LineName=xml_text(xml_find_all(x, xpath = "//MetroShape/LineName/Zh_tw")),
    Geometry=xml_text(xml_find_all(x, xpath = "//MetroShape/Geometry"))
  )
)
taipei_mrt=mutate(taipei_mrt, Geometry=st_as_sfc(Geometry))
taipei_mrt=st_sf(taipei_mrt, crs=4326)
taipei_mrt=st_transform(taipei_mrt, crs=3826)

# 台北捷運站點
url="https://ptx.transportdata.tw/MOTC/v2/Rail/Metro/Station/TRTC?&$format=XML"
x = get_ptx_data(app_id, app_key, url, Windows = TRUE)

taipei_mrt_station=data.frame(
  cbind(
    StationName=xml_text(xml_find_all(x, xpath = "//Station/StationName/Zh_tw")),
    PositionLat=xml_text(xml_find_all(x, xpath = "//Station/StationPosition/PositionLat")),
    PositionLon=xml_text(xml_find_all(x, xpath = "//Station/StationPosition/PositionLon"))
  )
)
taipei_mrt_station=mutate(taipei_mrt_station, Geometry=st_as_sfc(paste0("POINT(", PositionLon, " ", PositionLat, ")")))
taipei_mrt_station=st_sf(taipei_mrt_station, crs=4326)
taipei_mrt_station=st_transform(taipei_mrt_station, crs=3826)


taipei_mrt_station=distinct(taipei_mrt_station, .keep_all = TRUE)%>%
  arrange(desc(StationName))

ridership=read.table("./taipei_mrt_ridership.txt", sep=",", quote="", fileEncoding="UTF-8", header=T)
ridership=ridership %>% arrange(desc(StationName))

taipei_mrt_station=left_join(taipei_mrt_station, ridership, by="StationName")%>%
  arrange(desc(StationName))


ggplot(taipei_mrt)+
  geom_sf(aes(color=LineName), size=1, show.legend=F)+
  scale_color_manual(values=c("板南線"="#0070BD", "松山新店線"="#008659", "淡水信義線"="#E3002C", "中和新蘆線"="#F8B61C", "文湖線"="#C48B30", "環狀線"="#FCDA34"),
                     name="捷運路線")+
  geom_sf(data=taipei_mrt_station, aes(size=st_in+st_out))+
  scale_size_continuous(range=c(1,7),
                        name="Riderships")+
  theme(axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        legend.text=element_text(size=15, family="A"),
        legend.title=element_text(size=20, family="A"),
        legend.key=element_blank(),
        legend.key.size=unit(0.5, 'cm'),
        panel.background=element_blank())




