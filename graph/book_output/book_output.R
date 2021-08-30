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
sf_use_s2(FALSE)

windowsFonts(A=windowsFont("標楷體"))
windowsFonts(B=windowsFont("Times New Roman"))

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




LETTERS[seq(1:5)]



