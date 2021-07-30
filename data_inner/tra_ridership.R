library(jsonlite)
library(dplyr)
library(xml2)
library(httr)
library(sf)
library(ggplot2)
library(ggsflabel)
library(ggspatial)
library(tmap)
library(classInt)
windowsFonts(A=windowsFont("Times New Roman"))
windowsFonts(B=windowsFont("標楷體"))

ridership=fromJSON("http://ods.railway.gov.tw/tra-ods-web/ods/download/dataResource/8ae4cabf6973990e0169947ed32454b9")

ridership=mutate(ridership, month=as.numeric(substring(trnOpDate, 5, 6)), year=as.numeric(substring(trnOpDate, 1, 4)))
ridership=filter(ridership, month==11, year==2020)

ridership=ridership[,c(-5,-6)]

ridership$gateInComingCnt=as.numeric(ridership$gateInComingCnt)
ridership$gateOutGoingCnt=as.numeric(ridership$gateOutGoingCnt)
write.csv(ridership, "./TRA_ridership.csv", row.names=F)


#台鐵路線
#自公共運輸平台PTX 登入api
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
url="https://ptx.transportdata.tw/MOTC/v3/Rail/TRA/Shape?$format=XML"
x = get_ptx_data(app_id, app_key, url, Windows = TRUE)

TRA_line=data.frame(cbind(LineName=xml_text(xml_find_all(x, xpath = "//Shapes/Shape/LineName")),
                          Geometry=xml_text(xml_find_all(x, xpath = "//Shapes/Shape/Geometry"))))

write.csv(TRA_line, "./TRA_line.csv", row.names=F, fileEncoding="UTF-8")
TRA_line=read.csv("./TRA_line.csv", fileEncoding="UTF-8")


#台鐵車站
url="https://ptx.transportdata.tw/MOTC/v3/Rail/TRA/Station?$format=XML"
x = get_ptx_data(app_id, app_key, url, Windows = TRUE)

TRA_station=data.frame(cbind(StationID=xml_text(xml_find_all(x, xpath = "//Stations/Station/StationID")),
                          StationName=xml_text(xml_find_all(x, xpath = "//Stations/Station/StationName/Zh_tw")),
                          PositionLat=xml_text(xml_find_all(x, xpath = "//Stations/Station/StationPosition/PositionLat")),
                          PositionLon=xml_text(xml_find_all(x, xpath = "//Stations/Station/StationPosition/PositionLon"))))

write.csv(TRA_station, "./TRA_station.csv", row.names=F, fileEncoding="UTF-8")


# Process
ridership=group_by(ridership, staCode)%>%
  summarise(riderships=sum(gateInComingCnt+gateOutGoingCnt))

TRA_line$Geometry=st_as_sfc(TRA_line$Geometry, crs=4326)
TRA_line=st_sf(TRA_line, geometry=TRA_line$Geometry)

TRA_station=mutate(TRA_station, geomety=paste("POINT (",PositionLon," ",PositionLat,")"))
TRA_station$geomety=st_as_sfc(TRA_station$geomety)
TRA_station=st_sf(TRA_station, crs=4326)

TRA_station$StationID=as.numeric(TRA_station$StationID)

TRA_station=left_join(TRA_station, ridership, by=c("StationID"="staCode"))


rd_cla=classIntervals(filter(TRA_station, !is.na(riderships))$riderships, n=4, style="jenks")[[2]]

ggplot(TRA_line)+
  geom_sf()+
  geom_sf(data=TRA_station, aes(size=riderships))+
  scale_size_continuous(range=c(1,6)*1.5,
                        breaks=rd_cla)+
  geom_sf_text_repel(data=filter(TRA_station, StationID %in% c(1000, 3300, 4400, 7000)), aes(label=StationName), color="red", size=5, family="B", nudge_x=-0.6, nudge_y=0.1)+
  theme(axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        legend.text=element_text(size=12, family="A"),
        legend.title=element_text(size=15, family="A"),
        legend.key=element_blank(),
        legend.key.size=unit(0.5, 'cm'),
        panel.background=element_blank())


tmap_mode("view")
tm_shape(TRA_line)+
  tm_lines()+
  tm_shape(TRA_station)+
  tm_dots(size="riderships")



