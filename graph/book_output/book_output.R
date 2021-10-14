library(dplyr)
library(sf)
library(ggplot2)
library(ggsflabel)
library(ggspatial)
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
sf_use_s2(FALSE)


windowsFonts(A=windowsFont("標楷體"))
windowsFonts(B=windowsFont("Times New Roman"))


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



