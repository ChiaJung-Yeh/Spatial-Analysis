library(dplyr)
library(sf)
library(ggplot2)
library(ggsflabel)
library(ggspatial)
library(ggnewscale)
library(ggrepel)
library(knitr)
library(kableExtra)
library(spData)
library(tmap)
library(leaflet)
library(classInt)
library(cowplot)
library(DT)
library(nngeo)
library(RColorBrewer)
library(ggpubr)
library(webshot)
library(TWspdata)
library(osmar)
library(osmdata)
library(lwgeom)
library(ggtext)
library(TDX)
library(TWspdata)
library(imager)
sf_use_s2(FALSE)

tmap_mode("view")
windowsFonts(A=windowsFont("標楷體"))
windowsFonts(B=windowsFont("Times New Roman"))
windowsFonts(C=windowsFont("Consolas"))

png("./圖 1.1.4  點子圖示例（臺北市學校分布點子圖）.png", width=840*2, height=790*2, res=200)
school_taipei=taiwan_school[grepl(paste("臺北市", "台北市", sep="|"), taiwan_school$address),]%>%
  mutate(geometry=st_as_sfc(paste0("POINT(", lon, " ", lat, ")")))%>%
  st_sf(crs=4326)
school_taipei=mutate(school_taipei, type=case_when(
  grepl(paste("國小", "小學", "附小", "實小", sep="|"), name) ~ "國小",
  grepl(paste("國中", "私立立人", sep="|"), name) ~ "國中",
  grepl(paste("高工", "高中", "中學", "高商", "家商", "工農", "護家", "餐飲", "工商", "附中", "女中", sep="|"), name) ~ "高中職",
  grepl(paste("大學", "專科學校", "藝校", "學院", sep="|"), name) ~ "大專院校",
  grepl(paste("特殊教育學校", "啟聰學校", "啟明學校", sep="|"), name) ~ "特殊教育學校"
))
temp=group_by(taipei_village_map, TOWNNAME)%>%
  summarise()
school_taipei$type=factor(school_taipei$type, levels=c("國小","國中","高中職","大專院校","特殊教育學校"))
ggplot()+
  geom_sf(data=temp, fill="#D9D9D9", color="#F5F5F5")+
  geom_sf(data=school_taipei, aes(color=type, shape=type, stroke=1.2))+
  scale_color_brewer(palette="Set2", name="學校級別")+
  scale_shape_manual(values=c(15:(15+length(unique(school_taipei$type)))), name="學校級別")+
  theme(panel.background=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        legend.key=element_blank(),
        legend.title=element_text(family="A", size=25),
        legend.text=element_text(family="A", size=20))+
  guides(colour = guide_legend(override.aes=list(size=4)))
dev.off()



png("./圖 2.5.1  美國區域類別地圖.png", width=1060*2, height=580*2, res=220)
ggplot()+
  geom_sf(data=us_states, aes(fill=REGION))+
  scale_fill_grey()+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=15),
        legend.title=element_text(size=15),
        legend.text=element_text(size=12))
dev.off()



png("./圖 2.5.3  美國區域類別地圖（調色板）.png", width=1060*2, height=580*2, res=220)
ggplot()+
  geom_sf(data=us_states, aes(fill=REGION))+
  scale_fill_brewer(palette="Set3")+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=15),
        legend.title=element_text(size=15),
        legend.text=element_text(size=12))
dev.off()



png("./圖 2.6.1  依州名開頭字母繪製規則地圖.png", width=1060*3, height=580*3, res=220)
us_states_rule1=us_states
us_states_rule1$CLASS=""
us_states_rule1$CLASS=ifelse(substr(us_states_rule1$NAME, 1, 1) %in% LETTERS[1:9], "A~I",
                             ifelse(substr(us_states_rule1$NAME, 1, 1) %in% LETTERS[10:18], "J~R", "S~Z"))
ggplot()+
  geom_sf(data=us_states_rule1, aes(fill=CLASS))+
  scale_fill_grey()+
  geom_sf_text_repel(data=us_states_rule1, aes(label=NAME, color=CLASS), size=3, nudge_x=-0.1, nudge_y=0.4, show.legend=F)+
  scale_color_manual(values=c("A~I"="white", "J~R"="black", "S~Z"="black"))+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=15),
        legend.title=element_text(size=15),
        legend.text=element_text(size=12))
dev.off()



png("./圖 2.6.2  多屬性規則標記意圖.png", width=1060*2, height=580*2, res=220)
pop_mean=mean(us_states$total_pop_15)
area_mean=mean(us_states$AREA)
us_states_rule3=mutate(us_states,CLASS=case_when(
  total_pop_15>pop_mean & AREA>area_mean ~ "HPHA",   #人口多、面積大
  total_pop_15>pop_mean & AREA<area_mean ~ "HPLA",   #人口多、面積小
  total_pop_15<pop_mean & AREA>area_mean ~ "LPHA",   #人口少、面積大
  total_pop_15<pop_mean & AREA<area_mean ~ "LPLA"    #人口少、面積小
))
ggplot()+
  geom_sf(data=us_states_rule3, aes(fill=CLASS))+
  scale_fill_grey()+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=15),
        legend.title=element_text(size=15),
        legend.text=element_text(size=12))
dev.off()



png("./圖 2.8.4  調整圖例順序.png", width=1060*2, height=580*2, res=220)
us_states_rule3$CLASS=factor(us_states_rule3$CLASS, levels=c("LPLA", "LPHA", "HPLA", "HPHA"))
ggplot()+
  geom_sf(data=us_states_rule3, aes(fill=CLASS))+
  scale_fill_grey()+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=15),
        legend.title=element_text(size=15),
        legend.text=element_text(size=12))
dev.off()


png("./圖 2.9.1  紐西蘭基礎地圖繪製.png", width=550*2, height=790*2, res=220)
tmap_mode("plot")
tm_shape(nz)+
  tm_polygons(col="blue", border.col="yellow", lty="dashed")
dev.off()



png("./圖 4.2.1  空間與屬性聚合出圖結果.png", width=1060*2, height=580*2, res=220) 
# aggregate(taipei_village_map["PP"], by=list(taipei_village_map$TOWNNAME), FUN=sum, na.rm=T)
# aggregate(world["pop"], by=list(world$continent), FUN=sum, na.rm=T)
us_states_sf=aggregate(us_states["total_pop_15"], by=list(us_states$REGION), FUN=sum, na.rm=T)
ggplot()+
  geom_sf(data=us_states_sf, aes(fill=Group.1))+
  scale_fill_grey()+
  theme_void()+
  theme(title=element_text(size=20, family="A"),
        legend.title=element_text(size=20, family="B"),
        legend.text=element_text(size=15, family="B"))
dev.off()





