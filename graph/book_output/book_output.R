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
library(aspace)
library(ggpubr)
library(webshot)
library(TWspdata)
library(osmar)
library(osmdata)
library(lwgeom)
library(ggtext)
library(TDX)
sf_use_s2(FALSE)

tmap_mode("view")
windowsFonts(A=windowsFont("標楷體"))
windowsFonts(B=windowsFont("Times New Roman"))
windowsFonts(C=windowsFont("Consolas"))


png("./圖 1.1.4  點子圖示例（臺北市學校分布點子圖）.png", width=840*2, height=790*2, res=200)
school_taipei=school[grepl(paste("臺北市", "台北市", sep="|"), school$address),]
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
        legend.text=element_text(family="A", size=20))
dev.off()



png("./圖 1.1.5  面量圖示例（臺北市人口數面量圖）.png", width=840*2, height=790*2, res=200)
ggplot()+
  geom_sf(data=taipei_village_map, aes(fill=PP), color=NA)+
  scale_fill_distiller(palette="Reds", direction=1, name="人口數")+
  theme(panel.background=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        legend.title=element_text(family="A", size=20),
        legend.text=element_text(family="B", size=15))
dev.off()



png("./圖 2.1.1  ggplot2美國地圖產出結果.png", width=1060*2, height=630*2, res=200)
ggplot()+
  geom_sf(data=us_states)+
  theme(axis.text=element_text(size=15))
dev.off()



png("./圖 2.1.2  地圖顏色與大小修正.png", width=1060*2, height=630*2, res=200)
ggplot()+
  geom_sf(data=us_states, size=2, color="red", fill="blue")+
  theme(axis.text=element_text(size=15))
dev.off()



png("./圖 2.3.1  美國地圖州名標記.png", width=1060*2, height=630*2, res=200)
ggplot()+
  geom_sf(data=us_states)+
  geom_sf_text(data=us_states, mapping=aes(label=NAME))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=15))
dev.off()



png("./圖 2.3.2  美國地圖州名標記（修正）.png", width=1060*2, height=630*2, res=180)
ggplot()+
  geom_sf(data=us_states)+
  geom_sf_text_repel(data=us_states, aes(label=NAME), nudge_x=-0.1, nudge_y=0.4, size=4, color="blue")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=15))
dev.off()



png("./圖 2.4.1  人口數量漸層地圖.png", width=1060*2, height=630*2, res=180)
ggplot()+
  geom_sf(data=us_states, aes(fill=total_pop_15))+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=15),
        legend.title=element_text(size=15),
        legend.text=element_text(size=12))
dev.off()



png("./圖 2.4.2  人口數量漸層地圖（修正）.png", width=1060*2, height=630*2, res=180)
ggplot()+
  geom_sf(data=us_states, aes(fill=total_pop_15))+
  scale_fill_continuous(low="#D2E9FF", high="#004B97")+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=15),
        legend.title=element_text(size=15),
        legend.text=element_text(size=12))
dev.off()



png("./圖 2.4.3  人口數量漸層地圖（三段漸層）.png", width=1060*2, height=630*2, res=180)
ggplot()+
  geom_sf(data=us_states, aes(fill=total_pop_15))+
  scale_fill_gradient2(low="red", mid="orange", high="green")+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=15),
        legend.title=element_text(size=15),
        legend.text=element_text(size=12))
dev.off()



png("./圖 2.4.4  RColorBrewer套件階層顏色.png", width=1000*2, height=800*2, res=220)
display.brewer.all()
dev.off()



png("./圖 2.4.5   人口數量漸層地圖（階層顏色）.png", width=1060*2, height=630*2, res=220)
ggplot()+
  geom_sf(data=us_states, aes(fill=total_pop_15))+
  scale_fill_distiller(palette="YlOrRd", direction=1)+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=15),
        legend.title=element_text(size=15),
        legend.text=element_text(size=12))
dev.off()



png("./圖 2.5.1  美國區域類別地圖.png", width=1060*2, height=580*2, res=220)
ggplot()+
  geom_sf(data=us_states, aes(fill=REGION))+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=15),
        legend.title=element_text(size=15),
        legend.text=element_text(size=12))
dev.off()



png("./圖 2.5.2  美國區域類別地圖（客製化顏色調整）.png", width=1060*2, height=580*2, res=220)
ggplot()+
  geom_sf(data=us_states, aes(fill=REGION))+
  scale_fill_manual(values=c("Norteast"="#FFC1E0", "Midwest"="#97CBFF", "South"="#A6FFA6", "West"="#FFFFE0"))+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=15),
        legend.title=element_text(size=15),
        legend.text=element_text(size=12))
dev.off()



png("./圖 2.5.3  美國區域類別地圖（調色板）.png", width=1060*2, height=580*2, res=220)
ggplot()+
  geom_sf(data=us_states, aes(fill=REGION))+
  scale_fill_brewer(palette="Set2")+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=15),
        legend.title=element_text(size=15),
        legend.text=element_text(size=12))
dev.off()



png("./圖 2.6.1  依州名開頭字母繪製規則地圖.png", width=1060*2, height=580*2, res=220)
us_states_rule1=us_states
us_states_rule1$CLASS=""
us_states_rule1$CLASS=ifelse(substr(us_states_rule1$NAME, 1, 1) %in% LETTERS[1:9], "A~I",
                             ifelse(substr(us_states_rule1$NAME, 1, 1) %in% LETTERS[10:18], "J~R", "S~Z"))
ggplot()+
  geom_sf(data=us_states_rule1, aes(fill=CLASS))+
  geom_sf_text_repel(data=us_states_rule1, aes(label=NAME), size=2, nudge_x=-0.1, nudge_y=0.4,)+
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
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=15),
        legend.title=element_text(size=15),
        legend.text=element_text(size=12))
dev.off()



png("./圖 2.7.1  紐西蘭高峰疊圖.png", width=650*2, height=790*2, res=220)
ggplot()+
  geom_sf(data=nz)+
  geom_sf(data=nz_height, color="red")+
  theme(axis.text=element_text(size=17),
        axis.title=element_text(size=20),
        legend.title=element_text(size=20),
        legend.text=element_text(size=17))
dev.off()



png("./圖 2.7.2  紐西蘭高峰疊圖（指北針與比例尺）.png", width=650*2, height=790*2, res=220)
ggplot()+
  geom_sf(data=nz)+
  geom_sf(data=nz_height, color="red")+
  annotation_scale(location="br", height=unit(2.5, "mm"), text_cex=1.2, pad_x=unit(0.5, "cm"), pad_y=unit(0.5, "cm"))+
  annotation_north_arrow(location="tl", which_north="true", width=unit(1.2, "cm"), height=unit(1.5, "cm"), style=north_arrow_orienteering(text_size=15))+
  theme(axis.text=element_text(size=17),
        axis.title=element_text(size=20),
        legend.title=element_text(size=20),
        legend.text=element_text(size=17))
dev.off()



png("./圖 2.7.3  紐西蘭高峰疊圖（標題）.png", width=650*2, height=790*2, res=220)
ggplot()+
  geom_sf(data=nz)+
  geom_sf(data=nz_height, color="red")+
  annotation_scale(location="br", height=unit(2.5, "mm"), text_cex=1.2, pad_x=unit(0.5, "cm"), pad_y=unit(0.5, "cm"))+
  annotation_north_arrow(location="tl", which_north="true", width=unit(1.2, "cm"), height=unit(1.5, "cm"), style=north_arrow_orienteering(text_size=15))+
  ggtitle("New Zealand Map")+
  theme(axis.text=element_text(size=17),
        axis.title=element_text(size=20),
        legend.title=element_text(size=20),
        legend.text=element_text(size=17),
        plot.title=element_text(size=23))
dev.off()



png("./圖 2.8.2  修正地圖點與線樣式.png", width=650*2, height=790*2, res=220)
ggplot()+
  geom_sf(data=nz, color="blue", linetype="dashed")+
  geom_sf(data=nz_height, color="red", size=2, shape=4)+
  theme(axis.text=element_text(size=17),
        axis.title=element_text(size=20),
        legend.title=element_text(size=20),
        legend.text=element_text(size=17),
        plot.title=element_text(size=23))
dev.off()



png("./圖 2.8.3  修正圖例名稱.png", width=1060*2, height=580*2, res=220)
ggplot()+
  geom_sf(data=us_states, aes(fill=total_pop_15))+
  scale_fill_distiller(palette="YlOrRd", direction=1, name="Population")+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=15),
        legend.title=element_text(size=15),
        legend.text=element_text(size=12))
dev.off()



png("./圖 2.8.4  調整圖例順序.png", width=1060*2, height=580*2, res=220)
us_states_rule3$CLASS=factor(us_states_rule3$CLASS, levels=c("LPLA", "LPHA", "HPLA", "HPHA"))
ggplot()+
  geom_sf(data=us_states_rule3, aes(fill=CLASS))+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=15),
        legend.title=element_text(size=15),
        legend.text=element_text(size=12))
dev.off()



png("./圖 2.8.5  地圖主題調整.png", width=1060*2, height=580*2, res=220)
ggplot()+
  geom_sf(data=us_states, aes(fill=REGION))+
  ggtitle("美國地圖")+
  theme(panel.border=element_rect(color="black", fill=NA),
        panel.background=element_rect(fill="#A3B3C1"),
        panel.grid.major=element_line(color="#808080", linetype=2),
        axis.text=element_text(size=15, family="B"),
        axis.ticks=element_line(size=3),
        legend.background=element_rect(fill=alpha("#778899", 0.4)),
        legend.key=element_rect(fill=NA, color=NA),
        legend.text=element_text(size=15, family="B"),
        legend.title=element_text(size=15, family="B", hjust=0.5),
        legend.position=c(0.91, 0.2),
        plot.title=element_text(size=25, hjust=0.5, family="A"))
dev.off()




png("./圖 2.8.6  地圖範圍調整.png", width=685*2, height=790*2, res=220)
nz_wc=filter(nz, Name=="West Coast")
st_bbox(nz_wc)
ggplot()+
  geom_sf(data=nz)+
  geom_sf(data=filter(nz, Name=="West Coast"), fill="#B5B5B5")+
  geom_sf(data=nz_height, color="red")+
  # xlim(1205019.303, 1571336.246) +  ylim(5062352.347, 5485976.072)+
  coord_sf(xlim=c(1205019.303, 1571336.246), ylim=c(5062352.347, 5485976.072))+
  theme(panel.background=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank())
dev.off()



png("./圖 2.8.7  地圖縮圍.png", width=685*2, height=790*2, res=220)
p1=ggplot()+
  geom_sf(data=nz)+
  geom_sf(data=filter(nz, Name=="West Coast"), fill="#B5B5B5")+
  geom_sf(data=nz_height, color="red")+
  coord_sf(xlim=c(1205019.303, 1571336.246), ylim=c(5062352.347, 5485976.072))+
  theme(panel.background=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank())
p2=ggplot()+
  geom_sf(data=nz)+
  geom_sf(data=filter(nz, Name=="West Coast"), fill="#B5B5B5")+
  theme(panel.border=element_rect(color="black", fill=NA, size=1),
        panel.background=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())
ggdraw(p1)+
  draw_plot(p2, x=-0.35, y=0.35, scale=0.27)
dev.off()



png("./圖 2.8.9  合併多張地圖.png", width=930*2, height=660*2, res=220)
p1=ggplot()+
  geom_sf(data=us_states, aes(fill=total_pop_15))+
  ggtitle("人口數量漸層地圖")+
  theme(panel.background=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        title=element_text(size=12, family="A"),
        legend.title=element_text(size=10, family="B"),
        legend.text=element_text(size=8, family="B"))
p2=ggplot()+
  geom_sf(data=us_states, aes(fill=total_pop_15))+
  scale_fill_continuous(low="#D2E9FF", high="#004B97")+
  ggtitle("人口數量漸層地圖（修正）")+
  theme(panel.background=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        title=element_text(size=12, family="A"),
        legend.title=element_text(size=10, family="B"),
        legend.text=element_text(size=8, family="B"))
p3=ggplot()+
  geom_sf(data=us_states, aes(fill=total_pop_15))+
  scale_fill_gradient2(low="red", mid="orange", high="green")+
  ggtitle("人口數量漸層地圖（三段漸層）")+
  theme(panel.background=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        title=element_text(size=12, family="A"),
        legend.title=element_text(size=10, family="B"),
        legend.text=element_text(size=8, family="B"))
p4=ggplot()+
  geom_sf(data=us_states, aes(fill=total_pop_15))+
  scale_fill_distiller(palette="YlOrRd", direction=1)+
  ggtitle("人口數量漸層地圖（階層顏色）")+
  theme(panel.background=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        title=element_text(size=12, family="A"),
        legend.title=element_text(size=10, family="B"),
        legend.text=element_text(size=8, family="B"))
plot_grid(p1, p2, p3, p4, ncol=2, nrow=2)+
  draw_label("合併多張地圖", fontface='bold', fontfamily="A", 
             size=20, x=0.5, y=0.97)
dev.off()



png("./圖 2.8.10  合併多張地圖（共用圖例）.png", width=700*2, height=790*2, res=220)
nz_revised=nz
maxmin=function(x) (x - min(x))/(max(x)-min(x))
nz_revised$Land_area=maxmin(nz_revised$Land_area)
nz_revised$Population=maxmin(nz_revised$Population)
nz_revised$Median_income=maxmin(nz_revised$Median_income)
nz_revised$Sex_ratio=maxmin(nz_revised$Sex_ratio)

p1=ggplot()+
  geom_sf(data=nz_revised, aes(fill=Land_area))+
  scale_fill_distiller(palette="YlOrRd", direction=1, name="歸一化數值")+
  ggtitle("土地面積")+
  theme(panel.background=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        title=element_text(size=15, family="A"),
        legend.title=element_text(size=15, family="A"),
        legend.text=element_text(size=12, family="B"))
p2=ggplot()+
  geom_sf(data=nz_revised, aes(fill=Population))+
  scale_fill_distiller(palette="YlOrRd", direction=1, name="歸一化數值")+
  ggtitle("人口數")+
  theme(panel.background=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        title=element_text(size=15, family="A"),
        legend.title=element_text(size=15, family="A"),
        legend.text=element_text(size=12, family="B"))
p3=ggplot()+
  geom_sf(data=nz_revised, aes(fill=Median_income))+
  scale_fill_distiller(palette="YlOrRd", direction=1, name="歸一化數值")+
  ggtitle("收入中位數")+
  theme(panel.background=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        title=element_text(size=15, family="A"),
        legend.title=element_text(size=15, family="A"),
        legend.text=element_text(size=12, family="B"))
p4=ggplot()+
  geom_sf(data=nz_revised, aes(fill=Sex_ratio))+
  scale_fill_distiller(palette="YlOrRd", direction=1, name="歸一化數值")+
  ggtitle("性別比")+
  theme(panel.background=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        title=element_text(size=15, family="A"),
        legend.title=element_text(size=15, family="A"),
        legend.text=element_text(size=12, family="B"))
ggarrange(p1, p2, p3, p4, ncol=2, nrow=2, common.legend=T, legend="right")
dev.off()



png("./圖 2.8.11  使用多重地理資料顏色調整之錯誤.png", width=685*2, height=790*2, res=220)
ggplot()+
  geom_sf(data=nz, aes(color=Land_area))+
  scale_color_distiller(palette="YlOrRd", direction=1)+
  geom_sf(data=nz_height, aes(color=elevation))+
  scale_color_distiller(palette="YlGn", direction=1)+
  theme_void()
dev.off()



png("./圖 2.8.12  ggnewscale套件修正多重顏色設定.png", width=685*2, height=790*2, res=220)
ggplot()+
  geom_sf(data=nz, aes(color=Land_area))+
  scale_color_distiller(palette="YlOrRd", direction=1)+
  new_scale_color()+
  geom_sf(data=nz_height, aes(color=elevation))+
  scale_color_distiller(palette="YlGn", direction=1)+
  theme_void()
dev.off()



png("./圖 2.9.1  紐西蘭基礎地圖繪製.png", width=550*2, height=790*2, res=220)
tm_shape(nz)+
  tm_polygons(col="blue", border.col="red", lty="dashed")
dev.off()



png("./圖 2.9.2  依屬性繪製地圖.png", width=550*2, height=790*2, res=220)
tm_shape(nz)+
  tm_polygons(col="Population")
dev.off()



png("./圖 2.9.3_A  依屬性繪製地圖（修正）.png", width=550*2, height=790*2, res=220)
tm_shape(nz)+
  tm_polygons(col="Population", n=5, style="pretty", palette="Reds")
dev.off()



png("./圖 2.9.3_B  依屬性繪製地圖（修正）.png", width=550*2, height=790*2, res=220)
tm_shape(nz)+
  tm_polygons(col="Population", n=5, style="equal")
dev.off()



png("./圖 2.9.3_C  依屬性繪製地圖（修正）.png", width=550*2, height=790*2, res=220)
tm_shape(nz)+
  tm_polygons(col="Population", n=4, style="quantile")
dev.off()



png("./圖 2.9.3_D  依屬性繪製地圖（修正）.png", width=550*2, height=790*2, res=220)
tm_shape(nz)+
  tm_polygons(col="Population", n=5, style="jenks")
dev.off()



png("./圖 2.9.3_E  依屬性繪製地圖（修正）.png", width=550*2, height=790*2, res=220)
tm_shape(nz)+
  tm_polygons(col="Population", style="cont")
dev.off()



png("./圖 2.9.3_F  依屬性繪製地圖（修正）.png", width=550*2, height=790*2, res=220)
tm_shape(nz)+
  tm_polygons(col="Population", style="cat")
dev.off()



png("./圖 2.9.4  tm_fill()繪製地圖.png", width=550*2, height=790*2, res=220)
tm_shape(nz)+
  tm_fill(col="Population", n=5, style="jenks")
dev.off()



png("./圖 2.9.5  tm_symbols()繪製地圖.png", width=550*2, height=790*2, res=220)
tm_shape(nz)+
  tm_polygons()+
  tm_shape(nz_height)+
  tm_symbols(col="elevation", size=1, shape=4, palette="Reds", style="quantile")
dev.off()



png("./圖 2.9.6  地圖文字標記.png", width=550*2, height=790*2, res=220)
tm_shape(nz)+
  tm_polygons(col="Population", n=5, style="jenks")+
  tm_text(text="Name", size=1, fontfamily="B")
dev.off()



png("./圖 2.9.7  指北針與比例尺繪製.png", width=550*2, height=790*2, res=220)
tm_shape(nz)+
  tm_polygons(col="Population", n=5, style="jenks")+
  tm_compass(north=0, type="rose", size=4, position=c(0.83, 0.89))+
  tm_scale_bar(breaks=c(0, 50, 100, 150, 200), text.size=1, position=c(0.7, 0.02))
dev.off()



png("./圖 2.9.8  地圖主題設定.png", width=550*2, height=790*2, res=220)
tm_shape(nz)+
  tm_polygons(col="Population", n=5, style="jenks")+
  tm_shape(nz)+
  tm_text(text="Name", size=1, fontfamily="B")+
  tm_compass(north=0, type="rose", size=4, position=c(0.83, 0.89))+
  tm_scale_bar(breaks=c(0, 50, 100, 150, 200), text.size=1, position=c(0.7, 0.02))+
  tm_layout(title="紐西蘭人口地圖", title.fontfamily="A", title.size=2,
            legend.title.size=1.7, legend.title.fontfamily="B", legend.title.fontface="bold",
            legend.text.size=1, legend.text.fontfamily="B",
            legend.frame=T, legend.frame.lwd=1, legend.bg.color="#CCCCFF", legend.bg.alpha=0.4,
            bg.color="#A3B3C1")
dev.off()



png("./圖 2.9.9  tm_style()經典地圖繪製.png", width=550*2, height=790*2, res=220)
tm_shape(nz)+
  tm_polygons(col="Population", n=5, style="jenks")+
  tm_style("classic")
dev.off()



# tmap_mode("view")
# p=tm_shape(nz)+
#   tm_polygons(col="Population", n=5, style="jenks")
# tmap_save(p, "temp.html", selfcontained=F)
# webshot("./temp.html", file="./圖 2.9.10  tmap套件動態地圖呈現.png", cliprect="viewport")



p=leaflet()%>%
  addTiles()
saveWidget(p, "temp.html", selfcontained=F)
webshot("./temp.html", file="./圖 2.9.11  leaflet套件繪製世界動態地圖.png", cliprect="viewport")



for (i in c("Stamen","Esri","Wikimedia","CartoDB")){
  p=leaflet()%>%
    addProviderTiles(i)
  saveWidget(p, "temp.html", selfcontained=F)
  webshot("./temp.html", file=paste0("./圖 2.9.12  修正介接線上地圖_", i, ".png"), cliprect="viewport")
}



p=leaflet()%>%
  addProviderTiles(providers$CartoDB)%>%
  addPolygons(data=st_transform(nz, 4326), color="red", fillColor="#CCCCFF", weight=2, label=~Name)%>%
  addCircleMarkers(data=st_transform(nz_height, 4326), stroke=F, radius=8, fillOpacity=0.2, label=~elevation)
saveWidget(p, "temp.html", selfcontained=F)
webshot("./temp.html", file="./圖 2.9.12  動態地圖繪製範例.png", cliprect="viewport")



png("./圖 3.1.1  點的簡單圖徵幾何元素圖示.png", width=680*2, height=660*2, res=220)
point_eg=st_point(c(2,3))
ggplot()+
  geom_sf(data=point_eg, size=8)+
  scale_x_continuous(limits=c(0,4))+
  scale_y_continuous(limits=c(0,4))+
  theme(axis.text=element_text(size=25, family="B", face="bold"))
dev.off()



png("./圖 3.1.2  線的簡單圖徵幾何元素圖示.png", width=835*2, height=660*2, res=220)
linestring_eg=st_linestring(rbind(c(2,3), c(4,4), c(3,5), c(1,4)))
ggplot()+
  geom_sf(data=linestring_eg)+
  scale_x_continuous(limits=c(1,4), breaks=c(1:4))+
  scale_y_continuous(limits=c(3,5), breaks=c(3:5))+
  theme(axis.text=element_text(size=25, family="B", face="bold"))
dev.off()



png("./圖 3.1.3  面的簡單圖徵幾何元素圖示.png", width=835*2, height=660*2, res=220)
polygon_eg=st_polygon(list(rbind(c(2,3), c(4,4), c(3,5), c(1,4), c(2,3))))
ggplot()+
  geom_sf(data=polygon_eg, fill="#99CCEF")+
  scale_x_continuous(limits=c(1,4), breaks=c(1:4))+
  scale_y_continuous(limits=c(3,5), breaks=c(3:5))+
  theme(axis.text=element_text(size=25, family="B", face="bold"))
dev.off()



png("./圖 3.1.4  多點的簡單圖徵幾何元素圖示.png", width=835*2, height=660*2, res=220)
mpoint_eg=st_multipoint(rbind(c(2,3), c(4,4), c(3,5), c(1,4)))
ggplot()+
  geom_sf(data=mpoint_eg, size=8)+
  scale_x_continuous(limits=c(1,4), breaks=c(1:4))+
  scale_y_continuous(limits=c(3,5), breaks=c(3:5))+
  theme(axis.text=element_text(size=25, family="B", face="bold"))
dev.off()



png("./圖 3.1.5  多線的簡單圖徵幾何元素圖示.png", width=835*2, height=660*2, res=220)
mlinestring_eg=st_multilinestring(list(rbind(c(2,3), c(4,4), c(3,5)), rbind(c(2,5), c(1,2))))
ggplot()+
  geom_sf(data=mlinestring_eg)+
  scale_x_continuous(limits=c(1,4), breaks=c(1:4))+
  scale_y_continuous(limits=c(2,5), breaks=c(2:5))+
  theme(axis.text=element_text(size=25, family="B", face="bold"))
dev.off()



png("./圖 3.1.6  多面的簡單圖徵幾何元素圖示.png", width=835*2, height=660*2, res=220)
mpolygon_eg=st_multipolygon(list(list(rbind(c(2,3), c(4,4), c(3,5), c(1,4), c(2,3))),
                                 list(rbind(c(1,5), c(2,5), c(3,6), c(1,5)))))
ggplot()+
  geom_sf(data=mpolygon_eg, fill="#99CCEF")+
  scale_x_continuous(limits=c(1,4), breaks=c(1:4))+
  scale_y_continuous(limits=c(2,6), breaks=c(2:6))+
  theme(axis.text=element_text(size=25, family="B", face="bold"))
dev.off()



png("./圖 3.1.7  point123簡單圖徵向量圖示.png", width=700*2, height=700*2, res=220)
point1=st_point(c(3, 5))
point2=st_point(c(2, 6))
point3=st_point(c(1, 4))
point123=st_sfc(point1, point2, point3)
ggplot()+
  geom_sf(data=point123, size=8)+
  scale_x_continuous(limits=c(1,3), breaks=c(1:3))+
  scale_y_continuous(limits=c(4,6), breaks=c(4:6))+
  theme(axis.text=element_text(size=25, family="B", face="bold"))
dev.off()



png("./圖 3.1.8  point123簡單圖徵向量圖示（crs設定為4326）.png", width=700*2, height=700*2, res=220)
point123=st_sfc(point1, point2, point3, crs=4326)
ggplot()+
  geom_sf(data=point123, size=8)+
  scale_x_continuous(limits=c(1,3), breaks=c(1:3))+
  scale_y_continuous(limits=c(4,6), breaks=c(4:6))+
  theme(axis.text=element_text(size=25, family="B", face="bold"))
dev.off()



png("./圖 3.1.10  簡單圖徵建構範例（新竹市區公所）.png", width=870*2, height=790*2, res=220) 
office_geom=st_sfc(
  st_point(c(120.973255, 24.805162)),  # 東區區公所經緯度
  st_point(c(120.970314, 24.816374)),   # 北區區公所經緯度
  st_point(c(120.942268, 24.794044)),   # 香山區區公所經緯度
  crs=4326)                                             # 設定座標參考系統
office=data.frame(
  name=c("東區","北區","香山區"),                         # 區域名稱
  address=c("民族路40號","國華街69號","育德街188號"),     # 區公所地址
  phone=c("03-5218231","03-5152525","03-5307105"),        # 區公所電話
  office_geom                                             # 放入空間資料
)
office=st_sf(office)
# # 擷取新竹公路資料
# road=opq(bbox=st_bbox(st_transform(filter(taiwan_town, COUNTYNAME=="新竹市"), crs=4326)))%>%
#   add_osm_feature(key="highway")%>%
#   osmdata_sf()
# road=st_sf(road$osm_lines)%>%
#   select(name.en, osm_id, highway)%>%
#   filter(highway %in% c("motorway_link","secondary","trunk_link","primary","tertiary","motorway","tertiary_link","trunk","secondary_link","primary_link"))%>%
#   st_intersection(st_transform(filter(taiwan_town, COUNTYNAME=="新竹市"), crs=4326)$geometry)
ggplot()+
  geom_sf(data=filter(taiwan_town, COUNTYNAME=="新竹市"), fill="#D0D0D0", color="#6C6C6C")+
  geom_sf(data=road, color="#99CCEF", size=0.1)+
  geom_sf(data=st_boundary(filter(taiwan_town, COUNTYNAME=="新竹市")), color="#6C6C6C")+
  geom_sf(data=office)+
  geom_sf_text(data=office, aes(label=name), size=7, family="A", vjust=1.2)+
  geom_sf_text(data=office, aes(label=phone), size=5, family="B", hjust=-0.5, color="red", vjust=1.5)+
  theme(panel.background=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank())
dev.off()



png("./圖 3.2.1  文字資料建構地理資料範例（新竹市區公車）.png", width=870*2, height=790*2, res=220) 
hsinchu_bus_route=read.csv("C:/Users/ASUS/Desktop/R Transportation/R Github Project/Spatial-Analysis/data/csv_files/hsinchu_bus_route.csv")
hsinchu_bus_route$Geometry=st_as_sfc(hsinchu_bus_route$Geometry)
hsinchu_bus_route=st_sf(hsinchu_bus_route, crs=4326)
ggplot()+
  geom_sf(data=filter(taiwan_town, COUNTYNAME=="新竹市"), fill="#D0D0D0", color="#6C6C6C")+
  geom_sf(data=hsinchu_bus_route, color="#0066CC", size=0.5)+
  theme_void()
dev.off()



png("./圖 3.2.3  經緯度欄位建構地理資料範例（新竹市觀光景點）.png", width=870*2, height=790*2, res=220) 
hsinchu_scenicSpot=read.csv("C:/Users/ASUS/Desktop/R Transportation/R Github Project/Spatial-Analysis/data/csv_files/hsinchu_scenicSpot.csv")
hsinchu_scenicSpot=mutate(hsinchu_scenicSpot, Geometry=paste("POINT(", PositionLon, " ", PositionLat, ")"))
hsinchu_scenicSpot$Geometry=st_as_sfc(hsinchu_scenicSpot$Geometry)
hsinchu_scenicSpot=st_sf(hsinchu_scenicSpot, crs=4326)
ggplot()+
  geom_sf(data=filter(taiwan_town, COUNTYNAME=="新竹市"), fill="#D0D0D0", color="#6C6C6C")+
  geom_sf(data=hsinchu_scenicSpot, color="#46A3FF", size=1)+
  geom_sf(data=filter(hsinchu_scenicSpot, 
                      Name %in% c("新竹都城隍廟","新竹市立動物園","十七公里海岸風景區","南寮漁港(南寮舊港)","香山溼地","十九公頃青青草原")), color="#0066CC", size=2)+
  geom_sf_text_repel(data=filter(hsinchu_scenicSpot, 
                                 Name %in% c("新竹都城隍廟","新竹市立動物園","十七公里海岸風景區","南寮漁港(南寮舊港)","香山溼地","十九公頃青青草原")), aes(label=Name), family="A", size=7)+
  theme_void()
dev.off()



png("./圖 3.3.2  st_bbox()地理邊界繪圖.png", width=650*2, height=790*2, res=220) 
ggplot()+
  geom_sf(data=nz, color="#E0E0E0", fill="#BEBEBE", size=0.5)+
  geom_sf(data=nz_height, color="#01814A", shape=4)+
  geom_sf(data=st_as_sfc(st_bbox(nz_height)), alpha=0.5, fill="#99CCEF", color=NA)+
  theme_void()
dev.off()



png("./圖 3.5.1  合併地理資料與新增屬性資料（美國COVID-19死亡率圖）.png", width=1570*2, height=790*2, res=220) 
us_covid=read.csv("https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2Fnytimes%2Fcovid-19-data%2Fmaster%2Fus-states.csv&filename=us-states.csv")
us_covid$date=as.Date(us_covid$date)
us_covid=filter(us_covid, date==as.Date("2021/9/30"))
us_states_covid=left_join(us_states, us_covid, by=c("NAME"="state"))
us_states_covid$death_rate=us_states_covid$deaths/us_states_covid$cases
ggplot()+
  geom_sf(data=us_states_covid, aes(fill=death_rate*100))+
  scale_fill_distiller(palette="YlOrRd", direction=1, name="確診死亡率 (%)")+
  theme_void()+
  theme(legend.text=element_text(size=18, family="B"),
        legend.title=element_text(size=23, family="A"))
dev.off()



png("./圖 4.2.1  空間與屬性聚合出圖結果.png", width=1060*2, height=580*2, res=220) 
# aggregate(taipei_village_map["PP"], by=list(taipei_village_map$TOWNNAME), FUN=sum, na.rm=T)
# aggregate(world["pop"], by=list(world$continent), FUN=sum, na.rm=T)
us_states_sf=aggregate(us_states["total_pop_15"], by=list(us_states$REGION), FUN=sum, na.rm=T)
ggplot()+
  geom_sf(data=us_states_sf, aes(fill=Group.1))+
  scale_fill_brewer(palette="Set3")+
  theme_void()+
  theme(title=element_text(size=20, family="A"),
        legend.title=element_text(size=20, family="B"),
        legend.text=element_text(size=15, family="B"))
dev.off()



png("./圖 4.2.2  空間與屬性聚合出圖結果.png", width=650*2, height=790*2, res=220) 
nz_ave_ele=aggregate(x=nz_height["elevation"], by=nz, FUN=mean)
ggplot()+
  geom_sf(data=nz_ave_ele, aes(fill=elevation))+
  scale_fill_distiller(palette="Greens", direction=1)+
  theme_void()+
  theme(legend.title=element_text(size=20, family="B"),
        legend.text=element_text(size=15, family="B"))
dev.off()



png("./圖 4.3.2  空間插值計算結果（人口密度圖）.png", width=1069*2, height=790*2, res=220) 
PPDENS_interpo=st_interpolate_aw(taipei_village_map["PPDENS"], taipei_mrt_station_buf, extensive=F, keep_NA=T)
PPDENS_interpo=cbind(Station=taipei_mrt_station_buf$Zh_tw, PPDENS_interpo)
ggplot()+
  geom_sf(data=taipei_village_map, color="#F0F0F0", fill="#D0D0D0")+
  geom_sf(data=PPDENS_interpo, aes(fill=PPDENS))+
  scale_fill_distiller(palette="Reds", direction=1, name="捷運站環域200公尺\n人口密度\n(人/平方公里)")+
  theme_void()+
  theme(legend.title=element_text(size=20, family="A"),
        legend.text=element_text(size=15, family="B"))
dev.off()



png("./圖 4.4.1  地理資料聯集（臺北市各村里聯集）.png", width=1069*2, height=790*2, res=220)
vil_uni=st_union(taipei_village_map)
p1=ggplot()+
  geom_sf(data=taipei_village_map, color="#F0F0F0", fill="#D0D0D0")+
  ggtitle("原村里面圖層")+
  theme_void()+
  theme(plot.title=element_text(hjust=0.5, family="A", size=20, face="bold"))
p2=ggplot()+
  geom_sf(data=vil_uni, color="#F0F0F0", fill="#D0D0D0")+
  ggtitle("聯集後村里面圖層")+
  theme_void()+
  theme(plot.title=element_text(hjust=0.5, family="A", size=20, face="bold"))
ggarrange(p1, p2, ncol=2, nrow=1)
dev.off()
  


png("./圖 4.4.2  兩地理資料各別聯集之聯集.png", width=810*2, height=790*2, res=220)
vil_mrt_uni=st_union(st_union(taipei_village_map), st_union(taipei_mrt_station))
ggplot()+
  geom_sf(data=vil_mrt_uni, color="#6C6C6C", fill="#6C6C6C")+
  theme_void()
dev.off()



png("./圖 4.5.4  臺北捷運站與臺北市村里圖層取交集結果之地圖.png", width=1222*2, height=650*2, res=220)
mrt_clip=st_intersection(taipei_mrt_station, taipei_village_map$geometry)
p1=ggplot()+
  geom_sf(data=taipei_village_map, color=NA, fill="#D0D0D0")+
  geom_sf(data=taipei_mrt_station, color="#6C6C6C")+
  theme_void()
p2=ggplot()+
  geom_sf(data=taipei_village_map, color=NA, fill="#D0D0D0")+
  geom_sf(data=mrt_clip, color="#6C6C6C")+
  theme_void()
ggarrange(p1, p2)
dev.off()



png("./圖 4.5.5  地理操作之集合操作元件示意圖.png", width=1370*2, height=770*2, res=220)
X=st_buffer(st_point(c(1,1)), dist=1)
Y=st_buffer(st_point(c(2,1)), dist=1)
intersection_XY=st_intersection(X, Y)
difference_XY=st_difference(X, Y)
difference_YX=st_difference(Y, X)
union_XY=st_union(X, Y)
sym_differnece=st_sym_difference(X, Y)
fg1=ggplot()+
  geom_sf(data=X, fill=NA)+
  geom_sf(data=Y, fill=NA)+
  geom_sf_text(data=X, aes(label="X"), nudge_x=-0.4, size=6, family="B")+
  geom_sf_text(data=Y, aes(label="Y"), nudge_x=0.4, size=6, family="B")+
  theme_minimal()+
  theme(axis.title=element_blank(),
        axis.text=element_text(family="B", size=12))
fg2=ggplot()+
  geom_sf(data=X, fill=NA)+
  geom_sf(data=Y, fill=NA)+
  geom_sf(data=intersection_XY, fill="#75AADB")+
  geom_sf_text(data=X, aes(label="X"), nudge_x=-0.4, size=6, family="B")+
  geom_sf_text(data=Y, aes(label="Y"), nudge_x=0.4, size=6, family="B")+
  theme_minimal()+
  theme(axis.title=element_blank(),
        axis.text=element_text(family="B", size=12))
fg3=ggplot()+
  geom_sf(data=X, fill=NA)+
  geom_sf(data=Y, fill=NA)+
  geom_sf(data=difference_XY, fill="#75AADB")+
  geom_sf_text(data=X, aes(label="X"), nudge_x=-0.4, size=6, family="B")+
  geom_sf_text(data=Y, aes(label="Y"), nudge_x=0.4, size=6, family="B")+
  theme_minimal()+
  theme(axis.title=element_blank(),
        axis.text=element_text(family="B", size=12))
fg4=ggplot()+
  geom_sf(data=X, fill=NA)+
  geom_sf(data=Y, fill=NA)+
  geom_sf(data=difference_YX, fill="#75AADB")+
  geom_sf_text(data=X, aes(label="X"), nudge_x=-0.4, size=6, family="B")+
  geom_sf_text(data=Y, aes(label="Y"), nudge_x=0.4, size=6, family="B")+
  theme_minimal()+
  theme(axis.title=element_blank(),
        axis.text=element_text(family="B", size=12))
fg5=ggplot()+
  geom_sf(data=X, fill=NA)+
  geom_sf(data=Y, fill=NA)+
  geom_sf(data=union_XY, fill="#75AADB")+
  geom_sf_text(data=X, aes(label="X"), nudge_x=-0.4, size=6, family="B")+
  geom_sf_text(data=Y, aes(label="Y"), nudge_x=0.4, size=6, family="B")+
  theme_minimal()+
  theme(axis.title=element_blank(),
        axis.text=element_text(family="B", size=12))
fg6=ggplot()+
  geom_sf(data=X, fill=NA)+
  geom_sf(data=Y, fill=NA)+
  geom_sf(data=sym_differnece, fill="#75AADB")+
  geom_sf_text(data=X, aes(label="X"), nudge_x=-0.4, size=6, family="B")+
  geom_sf_text(data=Y, aes(label="Y"), nudge_x=0.4, size=6, family="B")+
  theme_minimal()+
  theme(axis.title=element_blank(),
        axis.text=element_text(family="B", size=12))
plot_grid(fg1, fg2, fg3, fg4, fg5, fg6,
          labels=c("","st_intersection(X, Y)","st_difference(X, Y)","st_difference(Y, X)","st_union(X, Y)","st_sym_difference(X, Y)"),
          label_fontfamily="C",
          label_size=18,
          label_x=0.1,
          hjust=0,
          ncol=3,
          nrow=2)
dev.off()



png("./圖 4.5.6  新竹科學園區與新竹縣寶山鄉相對位置圖.png", width=1238*2, height=788*2, res=200)
sipa=filter(taiwan_factory, FNAME=="竹科新竹園區")
baoshan=filter(taiwan_town, TOWNNAME=="寶山鄉")
st_crs(sipa)$epsg
st_crs(baoshan)$epsg
sipa=st_transform(sipa, crs=3826)
baoshan=st_transform(baoshan, crs=3826)
sipa_bao=st_intersection(sipa, baoshan)
# 簡介圖
ggplot()+
  annotation_map_tile("cartolight", zoom=13)+
  geom_sf(data=baoshan, fill="#C4E1FF", color=NA)+
  geom_sf(data=sipa, fill="#F9F900", alpha=0.6, color=NA)+
  geom_sf(data=sipa_bao, fill="#B7FF4A", color=NA)+
  geom_sf(data=st_boundary(baoshan), color="#4F4F4F", linetype=5, size=1)+
  geom_sf(data=st_boundary(sipa), color="#4F4F4F")+
  geom_sf_text_repel(data=sipa, aes(label="新竹科學園區"), family="A", nudge_y=0.02, size=9)+
  geom_sf_text_repel(data=baoshan, aes(label=paste0(COUNTYNAME, "\n", TOWNNAME)), family="A", size=10, nudge_x=-0.03)+
  theme_void()
dev.off()


png("./圖 4.5.7  集合論之示範（以新竹科學園區為例）.png", width=1180*2, height=780*2, res=200)
ggplot()+
  geom_sf(data=baoshan, fill="#C4E1FF", color=NA)+
  geom_sf(data=sipa, fill="#F9F900", alpha=0.6, color=NA)+
  geom_sf(data=sipa_bao, fill="#B7FF4A", color=NA)+
  geom_sf_text_repel(data=sipa, aes(label="新竹科學園區"), family="A", nudge_y=0.02, size=9)+
  geom_sf_text_repel(data=baoshan, aes(label=paste0(COUNTYNAME, "\n", TOWNNAME)), family="A", size=10, nudge_x=-0.03)+
  theme_void()
dev.off()
ggplot()+
  geom_sf(data=st_difference(sipa, baoshan), fill="#F9F900", alpha=0.6, color=NA)+
  theme_void()
ggplot()+
  geom_sf(data=st_difference(baoshan, sipa), fill="#C4E1FF", color=NA)+
  theme_void()
ggplot()+
  geom_sf(data=st_intersection(sipa, baoshan), fill="#B7FF4A", color=NA)+
  theme_void()
ggplot()+
  geom_sf(data=st_difference(sipa, baoshan), fill="#F9F900", alpha=0.6, color=NA)+
  geom_sf(data=st_difference(baoshan, sipa), fill="#C4E1FF", color=NA)+
  theme_void()
st_area(st_intersection(sipa, baoshan))/st_area(sipa)
(st_area(sipa)-st_area(st_intersection(sipa, baoshan)))/st_area(sipa)



png("./圖 4.6.1  st_buffer()函式endCapStyle=參數樣式示意圖.png", width=1180*2, height=150*2, res=200)
p1=ggplot()+
  geom_sf(data=st_buffer(st_linestring(rbind(c(1,1), c(2,1))), 0.1, endCapStyle="ROUND"), color="#004B97", fill="#C4E1FF")+
  geom_sf(data=st_linestring(rbind(c(1,1), c(2,1))), color="#CE0000", size=4)+
  ggtitle("ROUND")+
  theme_void()+
  theme(plot.title=element_text(hjust=0.5, family="B", size=20, face="bold", vjust=20))
p2=ggplot()+
  geom_sf(data=st_buffer(st_linestring(rbind(c(1,1), c(2,1))), 0.1, endCapStyle="SQUARE"), color="#004B97", fill="#C4E1FF")+
  geom_sf(data=st_linestring(rbind(c(1,1), c(2,1))), color="#CE0000", size=4)+
  ggtitle("SQUARE")+
  theme_void()+
  theme(plot.title=element_text(hjust=0.5, family="B", size=20, face="bold", vjust=20))
p3=ggplot()+
  geom_sf(data=st_buffer(st_linestring(rbind(c(1,1), c(2,1))), 0.1, endCapStyle="FLAT"), color="#004B97", fill="#C4E1FF")+
  geom_sf(data=st_linestring(rbind(c(1,1), c(2,1))), color="#CE0000", size=4)+
  ggtitle("FLAT")+
  theme_void()+
  theme(plot.title=element_text(hjust=0.5, family="B", size=20, face="bold", vjust=20)) 
plot_grid(p1, p2, p3, ncol=3)
dev.off()



png("./圖 4.6.2  cycle_hire 100公尺環域圖（一）.png", width=918*2, height=552*2, res=200)
ggplot()+
  annotation_map_tile("cartolight", zoom=12)+
  geom_sf(data=st_buffer(cycle_hire, 0.0009), color="#003D79", fill="#C4E1FF")+
  theme_void()+
  annotation_scale(location="br", pad_x=unit(0, "cm"), pad_y=unit(0.2, "cm"), text_family="B", text_cex=1, text_face="bold")+
  annotation_north_arrow(location="tl", which_north="true", style=north_arrow_fancy_orienteering(text_family="B", text_size=14, text_face="bold"), pad_x=unit(0.1, "cm"),  height=unit(1.5, "cm"), width=unit(1.5, "cm"))
dev.off()



png("./圖 4.6.3  cycle_hire 100公尺環域圖（二）.png", width=918*2, height=552*2, res=200)
ggplot()+
  annotation_map_tile("cartolight", zoom=12)+
  geom_sf(data=st_buffer(st_transform(cycle_hire, 27700), 100), color="#003D79", fill="#C4E1FF")+
  theme_void()+
  annotation_scale(location="br", pad_x=unit(0, "cm"), pad_y=unit(0.2, "cm"), text_family="B", text_cex=1, text_face="bold")+
  annotation_north_arrow(location="tl", which_north="true", style=north_arrow_fancy_orienteering(text_family="B", text_size=14, text_face="bold"), pad_x=unit(0.1, "cm"),  height=unit(1.5, "cm"), width=unit(1.5, "cm"))
dev.off()



png("./圖 4.6.4  臺北市各捷運站300公尺環域內YouBike場站數.png", width=720*2, height=790*2, res=200)
mrt_station=st_intersection(taipei_mrt_station, taipei_village_map$geometry)
mrt_station=mrt_station[!(duplicated(mrt_station$Zh_tw)),]
mrt_station_buf=st_buffer(mrt_station, 300)
ubike_mrt_buf=st_intersection(taipei_youbike[, c("SttnUID","StatnNm")], mrt_station_buf[, c("StationID","Zh_tw")])
ubike_mrt_buf_count=group_by(st_drop_geometry(ubike_mrt_buf), Zh_tw)%>%
  summarise(ubike_count=n())%>%
  arrange(desc(ubike_count))
mrt_station_buf=left_join(mrt_station_buf, ubike_mrt_buf_count)
mrt_station_buf$ubike_count[is.na(mrt_station_buf$ubike_count)]=0
temp=st_intersection(taipei_mrt_route, taipei_village_map$geometry)
ggplot()+
  geom_sf(data=filter(taiwan_town, COUNTYNAME=="臺北市"), color="#F0F0F0", fill="#D0D0D0")+
  geom_sf(data=temp, aes(color=RAILNAME), show.legend=F, size=1)+
  scale_color_manual(values=c("淡水信義線"="#d90023", "板南線"="#0a59ae", "松山新店線"="#107547",
                              "中和新蘆線"="#f5a818", "文湖線"="#b57a25", "環狀線"="#fedb00"),
                     name="路線")+
  geom_sf(data=mrt_station_buf, color=NA, aes(fill=ubike_count))+
  scale_fill_distiller(palette="YlOrRd", direction=1, name="環域內\nYouBike\n場站數")+
  theme_void()+
  theme(legend.title=element_text(size=20, family="A"),
        legend.text=element_text(size=15, family="B"))
dev.off()



png("./圖 4.7.3  臺北市各村里中心點.png", width=620*2, height=790*2, res=200)
ggplot()+
  geom_sf(data=taipei_village_map, color="#F0F0F0", fill="#BEBEBE")+
  geom_sf(data=st_centroid(taipei_village_map), color="red", size=1)+
  theme_void()
dev.off()



png("./圖 4.7.4  臺北市捷運路線各路段中心點.png", width=730*2, height=780*2, res=200)
ggplot()+
  geom_sf(data=taipei_village_map, color="#F0F0F0", fill="#D0D0D0")+
  geom_sf(data=taipei_mrt_route, aes(color=RAILNAME), show.legend=F, size=0.1)+
  scale_color_manual(values=c("淡水信義線"="#d90023", "板南線"="#0a59ae", "松山新店線"="#107547",
                              "中和新蘆線"="#f5a818", "文湖線"="#b57a25", "環狀線"="#fedb00"),
                     name="路線")+
  geom_sf(data=taipei_mrt_station)+
  geom_sf(data=st_centroid(taipei_mrt_route), color="red", shape=18)+
  theme_void()
dev.off()


png("./圖 4.7.5  高雄市各區中心點.png", width=1134*2, height=774*2, res=200)
temp=filter(taiwan_town, COUNTYNAME=="高雄市")%>%
  st_transform(crs=4326)%>%
  st_intersection(st_as_sfc(st_bbox(c(xmin=116.70691, xmax=121.04903, ymin=20.287904, ymax=23.47171), crs=4326)))
ggplot()+
  annotation_map_tile("cartolight", zoom=9)+
  geom_sf(data=temp, size=1)+
  geom_sf(data=st_centroid(temp), color="red", size=1)+
  theme_void()
dev.off()



png("./圖 4.7.6  高雄市旗津區修正前後之中心點.png", width=1134*2, height=780*2, res=200)
temp=filter(taiwan_town, TOWNNAME=="旗津區")%>%
  st_transform(crs=4326)%>%
  st_intersection(st_as_sfc(st_bbox(c(xmin=116.70691, xmax=121.04903, ymin=20.287904, ymax=23.47171), crs=4326)))
ggplot()+
  annotation_map_tile("cartolight", zoom=9)+
  geom_sf(data=temp, size=2)+
  geom_sf(data=st_centroid(temp), color="red")+
  geom_sf(data=st_centroid(temp, of_largest_polygon=T), color="green")+
  theme_void()
dev.off()



png("./圖 4.7.7  澎湖中心點與面上點地圖.png", width=584*3, height=780*3, res=200)
penghu=filter(taiwan_county, COUNTYNAME=="澎湖縣")
ggplot()+
  geom_sf(data=penghu, color="#F0F0F0", fill="#D0D0D0")+
  geom_sf(data=st_centroid(penghu), color="red", size=6, shape=18)+
  geom_sf_text(data=st_centroid(penghu), aes(label="Centroid"), hjust=1.2, color="red", size=8, family="B", fontface="bold")+
  geom_sf(data=st_centroid(penghu, of_largest_polygon=T), color="#00A600", size=6, shape=18)+
  geom_sf_text(data=st_centroid(penghu, of_largest_polygon=T), aes(label="Centroid (largest)"), hjust=1.2, color="#00A600", size=8, family="B", fontface="bold")+
  geom_sf(data=st_point_on_surface(penghu), color="blue", size=6, shape=18)+
  geom_sf_text(data=st_point_on_surface(penghu), aes(label="Point on Surface"), hjust=-0.1, color="blue", size=8, family="B", fontface="bold")+
  theme_void()
dev.off()



png("./圖 4.8.1  美國各州邊界地圖.png", width=1060*2, height=580*2, res=200)
ggplot()+
  geom_sf(data=us_states)+
  geom_sf(data=st_boundary(us_states), color="red")+
  theme_void()
dev.off()



png("./圖 4.8.2  美國全國邊界地圖.png", width=1060*2, height=580*2, res=200)
ggplot()+
  geom_sf(data=us_states)+
  geom_sf(data=st_boundary(st_union(us_states)), color="red", size=1)+
  theme_void()
dev.off()



png("./圖 4.9.1  塞納河與美國地圖簡化.png", width=1000*2, height=550*2, res=200)
p1=ggplot()+
  geom_sf(data=seine, size=1)+
  ggtitle("seine\ndTolerance=0")+
  theme_void()+
  theme(plot.title=element_text(hjust=0.5, family="C", size=15, face="bold")) 
p2=ggplot()+
  geom_sf(data=st_simplify(seine, dTolerance=2000), size=1)+
  ggtitle("seine\ndTolerance=2000")+
  theme_void()+
  theme(plot.title=element_text(hjust=0.5, family="C", size=15, face="bold")) 
p3=ggplot()+
  geom_sf(data=st_simplify(seine, dTolerance=5000), size=1)+
  ggtitle("seine\ndTolerance=5000")+
  theme_void()+
  theme(plot.title=element_text(hjust=0.5, family="C", size=15, face="bold")) 
p4=ggplot()+
  geom_sf(data=st_transform(us_states, 2163), size=1, fill="#BEBEBE", color="#E0E0E0")+
  ggtitle("us_states\ndTolerance=0")+
  theme_void()+
  theme(plot.title=element_text(hjust=0.5, family="C", size=15, face="bold")) 
p5=ggplot()+
  geom_sf(data=st_simplify(st_transform(us_states, 2163), dTolerance=50000), size=1, fill="#BEBEBE", color="#E0E0E0")+
  ggtitle("us_states\ndTolerance=50000")+
  theme_void()+
  theme(plot.title=element_text(hjust=0.5, family="C", size=15, face="bold")) 
p6=ggplot()+
  geom_sf(data=st_simplify(st_transform(us_states, 2163), dTolerance=100000), size=1, fill="#BEBEBE", color="#E0E0E0")+
  ggtitle("us_states\ndTolerance=100000")+
  theme_void()+
  theme(plot.title=element_text(hjust=0.5, family="C", size=15, face="bold")) 
plot_grid(p1, p2, p3, p4, p5, p6, ncol=3)
dev.off()



png("./圖 4.10.1  移動美國地圖.png", width=1000*2, height=550*2, res=200)
us_states_2163=st_transform(us_states, crs=2163)
us_states_shift=us_states_2163$geometry+c(500000,-300000)
us_states_shift=st_sf(us_states_shift, crs=2163)
ggplot()+
  geom_sf(data=us_states_2163, color=NA, fill="#BEBEBE")+
  geom_sf(data=us_states_shift, fill="#75AADB", color=NA, alpha=0.6)+
  theme(axis.text=element_text(family="B", size=15),
        axis.ticks=element_blank(),
        axis.title=element_blank())
dev.off()



png("./圖 4.10.3  美國地圖縮放.png", width=1530*2, height=500*2, res=200)
us_states_global=us_states_2163
us_states_global$geometry=us_states_global$geometry*0.8
us_states_local=us_states_2163
us_states_local$geometry=(us_states_local$geometry-st_centroid(us_states_local)$geometry)*0.8+st_centroid(us_states_local)$geometry
us_states_global=st_sf(us_states_global, crs=2163)
us_states_local=st_sf(us_states_local, crs=2163)
p1=ggplot()+
  geom_sf(data=us_states_2163, color=NA, fill="#BEBEBE")+
  geom_sf(data=us_states_global, fill="#75AADB", color=NA, alpha=0.6)+
  ggtitle("全域縮放")+
  theme(plot.title=element_text(hjust=0.5, family="A", size=25, face="bold"),
        axis.text=element_text(family="B", size=15),
        axis.ticks=element_blank(),
        axis.title=element_blank())
p2=ggplot()+
  geom_sf(data=us_states_2163, color=NA, fill="#BEBEBE")+
  geom_sf(data=us_states_local, fill="#75AADB", color=NA, alpha=0.6)+
  ggtitle("區域縮放")+
  theme(plot.title=element_text(hjust=0.5, family="A", size=25, face="bold"),
        axis.text=element_text(family="B", size=15),
        axis.ticks=element_blank(),
        axis.title=element_blank())
plot_grid(p1, p2, ncol=2)
dev.off()



png("./圖 4.10.4  美國地圖旋轉.png", width=838*2, height=500*2, res=200)
rotation=function(x){
  r=x*pi/180
  ro=matrix(c(cos(r), sin(r), -sin(r), cos(r)), nrow=2, ncol=2)
  return(ro)
}
us_states_rotation=st_transform(us_states, crs=2163)
us_states_center=st_centroid(us_states_rotation)$geometry
us_states_rotation$geometry=(us_states_rotation$geometry-us_states_center)*rotation(30)+us_states_center
us_states_rotation=st_sf(us_states_rotation, crs=2163)
ggplot()+
  geom_sf(data=us_states_2163, color=NA, fill="#BEBEBE")+
  geom_sf(data=us_states_rotation, fill="#75AADB", color=NA, alpha=0.6)+
  theme(plot.title=element_text(hjust=0.5, family="A", size=25, face="bold"),
        axis.text=element_text(family="B", size=15),
        axis.ticks=element_blank(),
        axis.title=element_blank())
dev.off()



png("./圖 4.11.1  臺北捷運路線各路段長度.png", width=873*2, height=750*2, res=200)
mrt_route_length=cbind(taipei_mrt_route, mrt_length=as.numeric(st_length(taipei_mrt_route)))
ggplot()+
  geom_sf(data=mrt_route_length, aes(size=mrt_length, color=RAILNAME))+
  scale_color_manual(values=c("淡水信義線"="#d90023", "板南線"="#0a59ae", "松山新店線"="#107547",
                              "中和新蘆線"="#f5a818", "文湖線"="#b57a25", "環狀線"="#fedb00"),
                     name="捷運路線")+
  scale_size_continuous(range=c(0,3))+
  guides(size=F)+
  theme_void()+
  theme(legend.title=element_text(size=20, family="A"),
        legend.text=element_text(size=15, family="A"))
dev.off()  



png("./圖 4.11.2  距離計算示意圖.png", width=1250*2, height=550*2, res=200)
temp1=st_linestring(rbind(c(1,2), c(3,4)))
temp2=st_linestring(rbind(c(1,6), c(6,6)))
st_distance(temp1, temp2)
p1=ggplot()+
  geom_sf(data=temp1, size=1.2)+
  geom_sf(data=temp2, size=1.2)+
  geom_sf(data=st_nearest_points(temp1, temp2), color="red", linetype="dashed", size=1.2)+
  geom_sf(data=st_startpoint(st_nearest_points(temp1, temp2)), color="red", size=4)+
  geom_sf(data=st_endpoint(st_nearest_points(temp1, temp2)), color="red", size=4)+
  geom_sf_label(data=st_centroid(st_nearest_points(temp1, temp2)), aes(label=st_length(st_nearest_points(temp1, temp2))), fill="#75AADB", family="B", size=8)+
  scale_x_continuous(breaks=c(1:6))+
  scale_y_continuous(breaks=c(2:6))+
  theme_minimal()+
  theme(axis.text=element_text(size=20, family="B", face="bold"),
        axis.title=element_blank())

temp1=st_polygon(list(rbind(c(2,2), c(4,2), c(4,4), c(2,4), c(2,2))))
temp2=st_polygon(list(rbind(c(1,6), c(6,6), c(3,5), c(1,6))))
temp3=st_buffer(st_point(c(5,3)), 1)
st_distance(temp1, temp2)
st_distance(temp1, temp3)
st_distance(temp2, temp3)
p2=ggplot()+
  geom_sf(data=temp1, size=1.2)+
  geom_sf(data=temp2, size=1.2)+
  geom_sf(data=temp3, size=1.2)+
  geom_sf(data=st_nearest_points(temp1, temp2), color="red", linetype="dashed", size=1.2)+
  geom_sf(data=st_startpoint(st_nearest_points(temp1, temp2)), color="red", size=4)+
  geom_sf(data=st_endpoint(st_nearest_points(temp1, temp2)), color="red", size=4)+
  geom_sf(data=st_nearest_points(temp2, temp3), color="red", linetype="dashed", size=1.2)+
  geom_sf(data=st_startpoint(st_nearest_points(temp2, temp3)), color="red", size=4)+
  geom_sf(data=st_endpoint(st_nearest_points(temp2, temp3)), color="red", size=4)+
  geom_sf(data=st_nearest_points(temp1, temp3), color="red", linetype="dashed", size=1.2)+
  geom_sf(data=st_startpoint(st_nearest_points(temp1, temp3)), color="red", size=4)+
  geom_sf(data=st_endpoint(st_nearest_points(temp1, temp3)), color="red", size=4)+
  geom_sf_label(data=st_centroid(st_nearest_points(temp1, temp2)), aes(label=st_length(st_nearest_points(temp1, temp2))), fill="#75AADB", family="B", size=8)+
  geom_sf_label(data=st_centroid(st_nearest_points(temp2, temp3)), aes(label=round(st_length(st_nearest_points(temp2, temp3)), 3)), fill="#75AADB", family="B", size=8)+
  geom_sf_label(data=st_centroid(st_nearest_points(temp1, temp3))-c(0.2, 0), aes(label=round(st_length(st_nearest_points(temp1, temp3)), 3)), fill="#75AADB", family="B", size=8)+
  scale_x_continuous(breaks=c(1:6))+
  scale_y_continuous(breaks=c(2:6))+
  theme_minimal()+
  theme(axis.text=element_text(size=20, family="B", face="bold"),
        axis.title=element_blank())
plot_grid(p1, p2, ncol=2, labels=c("(A)", "(B)"), label_fontfamily="B", label_y=1, label_size=22)
dev.off()



png("./圖 4.12.3  st_nearest_points()函式範例結果地圖.png", width=583*2, height=614*2, res=200)
mrt1=filter(taipei_mrt_station, Zh_tw %in% c("善導寺", "忠孝敦化"))
mrt1=st_buffer(mrt1, 300)
mrt2=filter(taipei_mrt_station, Zh_tw %in% c("龍山寺","圓山","台電大樓","六張犁","松山機場"))
mrt12_near=st_nearest_points(mrt1, mrt2)
ggplot()+
  geom_sf(data=taipei_mrt_route, aes(color=RAILNAME), show.legend=F, size=1.5)+
  scale_color_manual(values=c("淡水信義線"="#d90023", "板南線"="#0a59ae", "松山新店線"="#107547",
                              "中和新蘆線"="#f5a818", "文湖線"="#b57a25", "環狀線"="#fedb00"),
                     name="路線")+
  geom_sf(data=mrt1, color=NA, fill="#7B7B7B", alpha=0.7)+
  geom_sf(data=mrt12_near, size=1, color="#6C6C6C", linetype=6)+
  geom_sf(data=st_startpoint(mrt12_near), color="red")+
  geom_sf(data=mrt2, size=8, color="#75AADB", shape=18)+
  geom_text(data=mrt1, aes(x=st_coordinates(st_centroid(mrt1))[,1], y=st_coordinates(st_centroid(mrt1))[,2], label=Zh_tw), family="A", size=4, fontface="bold")+
  geom_sf_text_repel(data=mrt2, aes(label=Zh_tw), family="A", size=4)+
  coord_sf(xlim=c(st_bbox(mrt12_near)[1], st_bbox(mrt12_near)[3]), ylim=c(st_bbox(mrt12_near)[2], st_bbox(mrt12_near)[4]))+
  theme_void()
dev.off()



png("./圖 4.12.4  各YouBike場站與最近臺北捷運站點之連線.png", width=710*2, height=790*2, res=200)
nearest_mrt=st_nearest_feature(taipei_youbike, taipei_mrt_station)
nearest_mrt=taipei_mrt_station[nearest_mrt,]
mrt_ubike_line=st_nearest_points(nearest_mrt, taipei_youbike, pairwise=T)
ggplot()+
  # geom_sf(data=st_boundary(st_union(taipei_village_map)))+
  geom_sf(data=taipei_mrt_route, aes(color=RAILNAME), show.legend=F, size=0.5)+
  scale_color_manual(values=c("淡水信義線"="#d90023", "板南線"="#0a59ae", "松山新店線"="#107547",
                              "中和新蘆線"="#f5a818", "文湖線"="#b57a25", "環狀線"="#fedb00"),
                     name="路線")+
  geom_sf(data=taipei_mrt_station)+
  geom_sf(data=mrt_ubike_line, size=0.1, color="#6C6C6C")+
  geom_sf(data=taipei_youbike, color="#F6570E", size=0.3)+
  theme_void()
dev.off()



png("./圖 4.13.2  臺北市所有YouBike場站之凸包.png", width=610*2, height=790*2, res=200)
ggplot()+
  geom_sf(data=taipei_village_map, color="#F0F0F0", fill="#D0D0D0")+
  geom_sf(data=st_convex_hull(st_union(taipei_youbike)), color=NA, fill="#75AADB", alpha=0.7)+
  geom_sf(data=taipei_youbike, color="#F6570E", size=1)+
  theme_void()
dev.off()



png("./圖 4.13.3  最近捷運站相同之YouBike站點凸包.png", width=610*2, height=790*2, res=200)
mrt_station=st_intersection(taipei_mrt_station, taipei_village_map$geometry)
nearest_mrt=st_nearest_feature(taipei_youbike, mrt_station)
nearest_mrt=mrt_station[nearest_mrt,]
mrt_ubike_nearest=cbind(taipei_youbike[, c("SttnUID", "StatnNm")], nearest_mrt[, c("Zh_tw")])
mrt_ubike_nearest=group_by(mrt_ubike_nearest, Zh_tw)%>%
  summarise()
mrt_ubike_nearest_convex=st_convex_hull(mrt_ubike_nearest)
ggplot()+
  geom_sf(data=taipei_village_map, color="#F0F0F0", fill="#D0D0D0")+
  geom_sf(data=mrt_ubike_nearest_convex, color=NA, fill="#75AADB", alpha=0.7)+
  geom_sf(data=st_intersection(taipei_mrt_route, taipei_village_map$geometry), aes(color=RAILNAME), show.legend=F, size=0.5)+
  scale_color_manual(values=c("淡水信義線"="#d90023", "板南線"="#0a59ae", "松山新店線"="#107547",
                              "中和新蘆線"="#f5a818", "文湖線"="#b57a25", "環狀線"="#fedb00"),
                     name="路線")+
  geom_sf(data=mrt_station)+
  geom_sf(data=taipei_youbike, color="#F6570E", size=0.3)+
  theme_void()
dev.off()



png("./圖 4.14.3  臺北市內捷運站點沃羅諾伊圖.png", width=654*2, height=680*2, res=200)
tp_union=st_union(taipei_village_map)
mrt_station=st_intersection(taipei_mrt_station, tp_union)
mrt_vor=st_voronoi(st_union(mrt_station))
mrt_vor=st_collection_extract(mrt_vor)
ggplot()+
  geom_sf(data=filter(taipei_metropolitian, COUNTYNAME=="臺北市"), color=NA, fill="#D0D0D0")+
  geom_sf(data=mrt_vor, fill=NA, size=0.5)+
  theme_void()
dev.off()



png("./圖 4.14.4  臺北市內捷運站點沃羅諾伊圖（修正）.png", width=680*2, height=740*2, res=200)
tp_union=st_union(taipei_village_map)
mrt_station=st_intersection(taipei_mrt_station, tp_union)
mrt_vor=st_voronoi(st_union(mrt_station))
mrt_vor=st_collection_extract(mrt_vor)
mrt_vor=st_intersection(mrt_vor, tp_union)
ggplot()+
  geom_sf(data=filter(taipei_metropolitian, COUNTYNAME=="臺北市"), color=NA, aes(fill=TOWNNAME), alpha=0.3)+
  scale_fill_brewer(palette="Paired", name="行政區")+
  geom_sf(data=mrt_vor, fill=NA, size=0.5)+
  geom_sf(data=st_intersection(taipei_mrt_route, taipei_village_map$geometry), aes(color=RAILNAME), show.legend=F, size=0.8)+
  scale_color_manual(values=c("淡水信義線"="#d90023", "板南線"="#0a59ae", "松山新店線"="#107547",
                              "中和新蘆線"="#f5a818", "文湖線"="#b57a25", "環狀線"="#fedb00"))+
  geom_sf(data=mrt_station)+
  theme_void()+
  theme(legend.title=element_text(size=20, family="A"),
        legend.text=element_text(size=15, family="A"))
dev.off()



png("./圖 4.14.5  臺北市內捷運站點kmeans分群與沃羅諾伊圖.png", width=680*2, height=740*2, res=200)
mrt_kmeans=kmeans(st_coordinates(mrt_station), 5)
taipei_mrt_kmeans=cbind(mrt_station, cluster=factor(mrt_kmeans$cluster))
mrt_center=data.frame(mrt_kmeans$centers)%>%
  mutate(geometry=st_as_sfc(paste0("POINT(", X, " ", Y, ")")))%>%
  st_sf(crs=3826)
mrt_center_vor=st_voronoi(st_union(mrt_center))
mrt_center_vor=st_collection_extract(mrt_center_vor)
mrt_center_vor=st_intersection(mrt_center_vor, tp_union)
ggplot()+
  geom_sf(data=st_intersection(taipei_mrt_route, taipei_village_map$geometry), color="#8E8E8E", show.legend=F, size=1)+
  geom_sf(data=taipei_mrt_kmeans, aes(color=cluster), size=2)+
  scale_color_brewer(palette="Set2", name="群集")+
  geom_sf(data=mrt_center_vor, fill=NA, size=0.5, color="#AFAF61")+
  theme_void()+
  theme(legend.title=element_text(size=20, family="A"),
        legend.text=element_text(size=15, family="B"))
dev.off()



png("./圖 4.15.1  網格範例示意圖.png", width=800*2, height=800*2, res=200)
grid=st_polygon(list(rbind(c(0,0), c(10,0), c(10,10), c(0,10), c(0,0))))
ggplot()+
  geom_sf(data=st_make_grid(grid, cellsize=2, what="polygons"), fill="#ECF5FF")+
  geom_sf(data=st_make_grid(grid, cellsize=2, what="centers"), color="#005AB5", size=3)+
  geom_sf(data=st_make_grid(grid, cellsize=2, what="corners"), color="#FF0000", size=2)+
  theme_minimal()+
  theme(axis.text=element_text(family="B", size=22, face="bold"))
dev.off()



png("./圖 4.15.2  網格範例示意圖（設定參數n=）.png", width=800*2, height=800*2, res=200)
ggplot()+
  geom_sf(data=st_make_grid(grid, n=c(2,5), what="polygons"), fill="#ECF5FF")+
  theme_minimal()+
  theme(axis.text=element_text(family="B", size=22, face="bold"))
dev.off()



png("./圖 4.15.3  臺北市500公尺網格.png", width=670*2, height=730*2, res=200)
tp_union=st_union(taipei_village_map)
tp_grid=st_make_grid(tp_union, cellsize=500)
ggplot()+
  geom_sf(data=tp_union, fill="#75AADB", color=NA)+
  geom_sf(data=tp_grid, fill=NA)+
  theme_void()
dev.off()



png("./圖 4.15.4  臺北市500公尺網格YouBike場站數分布圖.png", width=670*2, height=730*2, res=200)
tp_union=st_union(taipei_village_map)
tp_grid=st_make_grid(tp_union, cellsize=500)
tp_grid=st_intersection(tp_grid, tp_union)
tp_grid=st_sf(tp_grid, crs=3826)
tp_grid=cbind(GridID=c(1:nrow(tp_grid)), tp_grid)
grid_youbike=st_intersects(tp_grid, taipei_youbike)
tp_grid=cbind(tp_grid, youbike=lengths(grid_youbike))
ggplot()+
  geom_sf(data=tp_grid, aes(fill=log(youbike)),)+
  scale_fill_distiller(palette="YlOrRd", direction=1, breaks=c(0:2, max(log(tp_grid$youbike))), label=c(round(c(apply(t(0:2), 1, exp)), 0), max(tp_grid$youbike)), name="<span style='font-family:B;'>YouBike<br></span>站點數")+
  theme_void()+
  theme(legend.title=element_markdown(size=20, family="A"),
        legend.text=element_text(size=15, family="B"))
dev.off()



png("./圖 5.2.1  臺北捷運路網與站點地圖.png", width=816*2, height=755*2, res=200)
TRTC_railshape=Rail_Shape(app_id, app_key, "TRTC", dtype="sf")
TRTC_station=Rail_Station(app_id, app_key, "TRTC", dtype="sf")

TRTC_railshape$LineName=substr(TRTC_railshape$LineName, 1, regexpr("線", TRTC_railshape$LineName))
ggplot()+
  geom_sf(data=TRTC_railshape, aes(color=LineName), show.legend="line", size=1)+
  scale_color_manual(values=c("淡水信義線"="#d90023", "板南線"="#0a59ae", "松山新店線"="#107547",
                              "中和新蘆線"="#f5a818", "文湖線"="#b57a25", "環狀線"="#fedb00"),
                     name="路線")+
  geom_sf(data=TRTC_station, size=1)+
  theme_void()+
  theme(legend.title=element_text(family="A", size=20),
        legend.text=element_text(family="A", size=18))
dev.off()



png("./圖 5.3.2  新竹市市區公車路線與站點地圖.png", width=1060*2, height=700*2, res=200)
Hsinchu_bus_shape=Bus_Shape(app_id, app_key, "Hsinchu", dtype="sf")
Hsinchu_bus_station=Bus_StopOfRoute(app_id, app_key, "Hsinchu", dtype="sf")
coul=brewer.pal(12, "Paired")
coul=colorRampPalette(coul)(length(unique(Hsinchu_bus_station$RouteName)))
ggplot()+
  geom_sf(data=Hsinchu_bus_station, aes(color=RouteName))+
  scale_color_manual(values=coul, name="新竹市市區公車路線")+
  geom_sf(data=Hsinchu_bus_shape, aes(color=RouteName))+
  theme_void()+
  theme(legend.title=element_text(family="A", size=18),
        legend.text=element_text(family="A", size=15),
        legend.key=element_blank())
dev.off()



Taipei_bike_station=Bike_Station(app_id, app_key, "Taipei", dtype="sf")
head(Taipei_bike_station)
tmap_mode("view")
p=tm_shape(Taipei_bike_station)+
  tm_dots(col="ServiceType", size="BikesCapacity", title="YouBike 系統", labels=c("YouBike 1.0","YouBike 2.0"),
          popup.vars=c("站點名稱"="StationName","YouBike 系統"="ServiceType","站點車樁數"="BikesCapacity"))
tmap_save(p, "temp.html", selfcontained=F)
webshot("./temp.html", file="./圖 5.4.1  YouBike線上地圖.png", cliprect="viewport")



png("./圖 5.4.2  臺北市自行車線型地圖.png", width=662*2, height=740*2, res=200)
Taipei_bike_shape=Bike_Shape(app_id, app_key, "Taipei", dtype="sf")
ggplot()+
  annotation_map_tile("cartolight", zoom=12)+
  geom_sf(data=Taipei_bike_shape)+
  theme_void()
dev.off()



png("./圖 5.5.1  花蓮縣觀光景點分布圖.png", width=600*2, height=790*2, res=200)
Hualien_scenicspot=ScenicSpot(app_id, app_key, county="HualienCounty", dtype="sf")
Hualien_scenicspot=st_transform(Hualien_scenicspot, crs=3826)
hualien=filter(TWspdata::taiwan_town, COUNTYNAME=="花蓮縣")
hualien=st_transform(hualien, crs=3826)
Hualien_scenicspot_count=st_intersection(Hualien_scenicspot, hualien[, c("TOWNNAME")])%>%
  st_drop_geometry()%>%
  group_by(TOWNNAME)%>%
  summarise(count=n())%>%
  arrange(desc(count))
hualien=left_join(hualien, Hualien_scenicspot_count)
ggplot()+
  geom_sf(data=hualien, aes(fill=count), color="#F0F0F0")+
  scale_fill_distiller(palette="YlOrRd", direction=1, name="觀光景點數")+
  geom_sf(data=Hualien_scenicspot, color="#75AADB")+
  annotation_scale(location="br", text_family="B", text_size=18, pad_y=unit(1.5, "cm"))+
  annotation_north_arrow(location="tl", style=north_arrow_fancy_orienteering(text_family="B", text_size=12), pad_x=unit(1.5, "cm"))+
  geom_sf_text_repel(data=hualien, aes(label=TOWNNAME), family="A", color="#3C3C3C")+
  theme_void()+
  theme(legend.title=element_text(family="A", size=18),
        legend.text=element_text(family="B", size=15),
        legend.key=element_blank())
dev.off()



png("./圖 5.5.2  臺中市公路路網地圖.png", width=1320*2, height=650*2, res=200)
Taichung_road=Road_Network(county="Taichung", roadclass="ALL", dtype="sf")
Taichung_road$RoadClassName[Taichung_road$RoadClassName=="省道一般道路"]="省道一般公路"
Taichung_road$RoadClassName=factor(Taichung_road$RoadClassName, c("國道","省道快速公路","省道一般公路"))
taichung_city=filter(TWspdata::taiwan_town, COUNTYNAME=="臺中市")
Taichung_road=group_by(Taichung_road, RoadName)%>%
  summarise()%>%
  left_join(distinct(st_drop_geometry(Taichung_road)[, c("RoadName","RoadClassName")]))
ggplot()+
  geom_sf(data=taichung_city, color="#F0F0F0", fill="#D0D0D0")+
  geom_sf(data=Taichung_road, aes(color=RoadClassName), show.legend="line")+
  geom_sf_text_repel(data=Taichung_road, aes(label=RoadName, color=RoadClassName), family="A", size=4, fontface="bold", show.legend=F)+
  scale_color_manual(values=c("國道"="#00923F", "省道快速公路"="#9B0D15", "省道一般公路"="#003876"), name="道路等級")+
  theme_void()+
  theme(legend.title=element_text(family="A", size=25),
        legend.text=element_text(family="A", size=20),
        legend.key=element_blank(),
        legend.position=c(0.8, 0.15))
dev.off()



png("./圖 5.5.4  房價資料地理編碼結果.png", width=815*2, height=768*2, res=200)
# house_price_geocode=Geocoding(house_price$ADDRESS, dtype="sf")
# house_price_geocode=left_join(house_price_geocode, house_price, by=c("AddressOriginal"="ADDRESS"))
# TRTC_railshape=Rail_Shape(app_id, app_key, "TRTC", dtype="sf")
# TRTC_railshape$LineName=substr(TRTC_railshape$LineName, 1, regexpr("線", TRTC_railshape$LineName))
ggplot()+
  geom_sf(data=TRTC_railshape)+  #確保後面的地圖是3826
  annotation_map_tile("cartolight", zoom=14, alpha=0.9)+
  geom_sf(data=filter(taiwan_town, COUNTYNAME=="新北市"), fill=NA, color="#5B5B5B", linetype=5)+
  geom_sf(data=house_price_geocode, aes(color=PRICE/10000), size=3)+
  scale_color_distiller(palette="YlOrRd", direction=1, name="總房價（萬）")+
  new_scale("color")+
  geom_sf(data=TRTC_railshape, aes(color=LineName), show.legend=F, size=1)+
  scale_color_manual(values=c("淡水信義線"="#d90023", "板南線"="#0a59ae", "松山新店線"="#107547",
                              "中和新蘆線"="#f5a818", "文湖線"="#b57a25", "環狀線"="#fedb00"))+
  geom_sf_text_repel(data=filter(taiwan_town, COUNTYNAME=="新北市", TOWNNAME %in% c("板橋區","新莊區","樹林區","中和區","土城區")), aes(label=TOWNNAME), family="A", size=5, fontface="bold")+
  coord_sf(xlim=c(st_bbox(house_price_geocode)[1], st_bbox(house_price_geocode)[3]),
           ylim=c(st_bbox(house_price_geocode)[2], st_bbox(house_price_geocode)[4]))+
  theme_void()+
  theme(legend.title=element_text(family="A", size=15, hjust=0.5, face="bold"),
        legend.text=element_text(family="B", size=12),
        legend.background=element_rect(fill=alpha("#D4DADC", 0.9), color=NA),
        legend.position=c(0.9, 0.11))
dev.off()



school_taipei=school[grepl(paste("臺北市", "台北市", sep="|"), school$address),]
school_taipei=filter(school_taipei, grepl(paste("大學", "專科學校", "藝校", "學院", sep="|"), name),
                     !(grepl(paste("附中", "附小", sep="|"), name)))






