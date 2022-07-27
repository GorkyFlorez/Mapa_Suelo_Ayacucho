library(sf)
library(ggplot2)
library(ggspatial)
library(raster)

Sueloo = st_read("SHP/Suelos/suelo_ayacucho.shp")

Suelo  <- st_transform(Sueloo ,
                           crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

PeRR           <- getData('GADM', country='Peru', level=2) %>% st_as_sf()
Ayacucho       <- subset(PeRR, NAME_1  == "Ayacucho")
library(elevatr)
elev = get_elev_raster(Suelo, z=10)
Poligo_alt    <- crop(elev, Suelo)                           #   
Poligo_alt   <- Poligo_alt <- mask(Poligo_alt, Suelo)
plot(Poligo_alt)

slopee    = terrain(Poligo_alt  , opt = "slope") 
aspecte    = terrain(Poligo_alt, opt = "aspect")
hille     = hillShade(slopee, aspecte, angle = 40, direction = 270)

hill.p        <-  rasterToPoints(hille)
hill.pa_      <-  data.frame(hill.p)
colores = c( 
  "#8e9aaf",#celeste
  "#dda15e", # maroon 
  "#faedcd")#amarillo pastel


Geo_data       <-  rasterToPoints(Poligo_alt)
Geo_data_frame <-  data.frame(Geo_data)
colnames(Geo_data_frame) <- c("x","y", "alt")

library(ggnewscale) 

SurA= ggplot()+
  geom_sf(data = Ayacucho, fill="white", color="black", size=0.5)+
  geom_sf_text(data = Ayacucho, aes(label=NAME_2),fontface="italic", 
               family="serif",size = 2.5)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "#f4f3ee"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))


col=c("#DACBB1", "#D6642D", "#F7BB5A", "#C88864", "#BC782F", 
  "#FABA82", "#BAA599", "#F1995A", "#C99C58", "#BE683F",
  "#BDA074", "#F99944", "#D59533", "#EDB695", "#E5C48E", "#F7823A", "#F1874D", 
  "#D19861", "#B2814E", "#E6A759",
  "#D6842E", "#CD6F31", "#DA875B", "#89D79E", "#949956", 
  "#74A6A5", "#E0E080", "#689962", "#7EDDD5", "#82E07F",
  "#ACC38B", "#88BF6D", "#759E80", "#A1D9BE", "#6AB9A2", "#B9DB85" ,
  "#E1E3A1", "#7AB388", "#85E2C1", "#569986",
  "#B8BA71", "#999973", "#C7C595", "#A4DADA", "#92C487", "#8BAD71"
)
General =ggplot()+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_sf(data = Suelo, aes(fill=DESSUELO), alpha=0.6, color=NA)+
  scale_fill_manual(values = col,name='Tipo de suelo')+
  theme(legend.position = c(0.85, 0.5),
        axis.text.x  = element_text(face="bold", color="black", size=8,
                                    family="serif"),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                    family="serif",size=8),
        axis.title = element_text(face="bold", color="black"),
        legend.text=element_text(size=6),
        legend.key.size = unit(0.2, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.5,"cm"), #ancho de cuadrados de referencia 
        legend.background = element_blank(),
        panel.background = element_rect(fill = "#f4f3ee"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 0.8))+
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  labs(title = '', fill = '',  x = 'Longitud', y = 'Latitud') +
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))+
  coord_sf(xlim = c(-75.13983, -71.8), ylim = c(-15.63006,-12.16788)) +
  guides(fill= guide_legend(nrow = 46, ncol=1))

colores = c( 
  "#588157",#celeste
  "#dda15e", # maroon 
  "#90e0ef")#amarillo pastel

Micro =ggplot()+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_raster(data = Geo_data_frame  ,aes(x,y, fill = alt), alpha=0.6, 
              show.legend = T)+
  scale_fill_gradientn(colours = colores, 
                       name='Elevacion \n(msnm)')+
  theme(legend.position = c(0.85, 0.62),
        axis.text.x  = element_text(face="bold", color="black", size=8,
                                    family="serif"),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                    family="serif",size=8),
        axis.title = element_text(face="bold", color="black"),
        legend.title =element_text(size=7, face = "bold"), #tamaño de titulo de leyenda
        legend.text=element_text(size=6),
        legend.key.size = unit(0.1, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.3,"cm"), #ancho de cuadrados de referencia 
        legend.background = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  labs(title = '', fill = '',  x = '', y = '') +
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))+
  guides(fill= guide_legend(nrow = 46, ncol=2))

# Mapa final
library(cowplot)
Final=ggdraw() +
  coord_equal(xlim = c(0, 26), ylim = c(0, 17.78), expand = FALSE) +
  draw_plot(General , width = 18.1, height = 18.1,x = 8.5, y = 0.01)+
  draw_plot(Micro, width = 6, height = 10,x = 3, y = 0.001)+
  draw_plot(SurA, width = 8, height = 8,x = 2.3, y = 9.5)+
  
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "black", fill = NA, size = 1))+
  annotate(geom = "text", x = 8, y = 3, hjust = 0, vjust = 1, angle=45,
           label = "Gorky Florez Castillo            Gorky Florez Castillo        Gorky Florez Castillo",
           size = 7, family="serif", color = "grey20",
           alpha=0.2)

ggsave(plot=Final,"Mapa/Mapa de Suelo3.png",units = "cm",width = 26, #alto
       height = 17.78, #ancho
       dpi=1200)







