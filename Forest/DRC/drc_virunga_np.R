# LOAD LIBRARIES
library(tidyverse)
library(terra)
library(sf)
library(MetBrewer)
library(showtext)
library(ggtext)

## SET URL TO LAND COVER DATA
stard_url <- "https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/2019/"
lab_url <- c("E020N20/E020N20", "E000N00/E000N00", "E020N00/E020N00")
end_url <- "_PROBAV_LC100_global_v3.0.1_2019-nrt_Tree-CoverFraction-layer_EPSG-4326.tif"
all_url <- paste0(stard_url, lab_url, end_url)
## DOWNALOAD DATA
if (!dir.exists("lc")) {dir.create("lc")}

for (u in all_url) {
  downloader::download(url = u, destfile = paste0("./lc/", basename(u)), mode = "wb")
}

lc_list <- list.files(path = "./lc", pattern = ".tif$", full.names = T)
lc_ras <- lapply(lc_list, terra::rast)
lc_mosaic <- do.call(terra::mosaic, lc_ras)

## Write to disk
terra::writeRaster(x =lc_mosaic, filename = "lc_forest.tif")
lc_mosaic <- rast("lc_forest.tif")

## IMPORT VIRUNGA PROTECTED AREA SHP (Download the shapefile from https://drive.google.com/file/d/1CDiWuWYT3z5H5-jkIYIJQ_jmTXH5cnLt/view?usp=sharing)
prot_area <- read_sf("./protected_area/protected_area.shp")

virunga <- prot_area %>% 
  filter(NOM == "Parc National des Virunga") %>% 
  st_transform(crs = terra::crs(lc_mosaic))

edouard <- read_sf("./lake/edouard_lake.shp") %>% # Access
  st_transform(crs = terra::crs(lc_mosaic)) %>% 
  st_intersection(y = virunga) %>% 
  mutate(name = "Lake\nEdouard")

## CROP LC TO VIRUNGA BOUNDARY
virunga_lc <- lc_mosaic %>% 
  terra::crop(y = terra::vect(virunga), snap = "in", mask = T) 


## CONVERTE VIRUNGA LC TO DF
virunga_lc_df <- terra::as.data.frame(x = virunga_lc, xy = T) %>% 
  rename("percent" = 3)

## PARAMETERS FOR GGPLOT VIZ
w <- ncol(virunga_lc)
h = nrow(virunga_lc)
min_val <- min(virunga_lc_df$percent)
max_val <- max(virunga_lc_df$percent)
limits <- c(min_val, max_val)
color <- colorRampPalette(met.brewer(name = "VanGogh3"))(256)
bg <- "transparent"#e9f9e5"

## FONTS
monts_bold <- font_add("monts_bold", "fonts/Montserrat Bold 700.ttf")
monts_reg <- font_add("monts_reg","fonts/Montserrat Medium 500.ttf")
monts_it <- font_add("monts_it","fonts/Montserrat Light 300.ttf")
showtext_auto()

## GGPLOT
ggplot(data = virunga_lc_df)+
  geom_raster(aes(x = x, y = y, fill = percent))+
  scale_fill_gradientn(colours = color, 
                       limits = limits,
                       name = "% of forest:")+
  geom_sf(data = edouard, color = "#0f6eff", fill = "#0f6eff")+
  geom_sf_text(data = edouard, aes(label = name), color = color[4], 
               family = "monts_bold", size = 4)+
  coord_sf(crs = terra::crs(lc_mosaic))+
  labs(title = paste0("<p><br><i style = 'color:#ff4109';>Virunga</i> National Park</p>"), 
       subtitle = "Democratic Republic of Congo",
       tag = paste0("<p>Copyright © 2024 - <i style = 'font-family:monts_reg;' >Stanislas Mahussi Gandaho</i>  <br> 
                        <i style = 'font-family:monts_it; text-align:center;'>Data - Copernicus Global Land Service: Land Cover 100m</i>
                        </p>"))+
  guides(
    fill = guide_legend(
      direction = "vertical",
      breaks = seq(min_val, max_val, 20),
      keywidth = unit(0.25, units = "lines"),
      keyheight = unit(2.5, units = "lines"),
      title.position = "bottom",
      title.hjust = 0.5,
      ticks.linewidth = 1/.pt,
      label.position = "left",
      label.hjust = 0.5,
      reverse = T,
      label.theme = element_text(angle = 90, size = 14, family = "monts_reg")
    ))+
  theme_void()+
  theme(
    ##Title
    plot.title = element_markdown(size = 40, family = "monts_bold", color = "gray20", 
                                  hjust = 0.4),
    plot.title.position = "plot",
    ## Subtitle
    plot.subtitle = element_text(size = 24, family = "monts_reg", color = "gray40", hjust = 0.8),
    ## Caption
    plot.tag = element_markdown(size = 18, family = "monts_it", hjust = 0, vjust = 1, 
                                angle = 90, halign = 0, lineheight = 0.8),
    plot.tag.position = c(1.2, 0.09),
    ## Legende
    legend.background = element_rect(fill = "transparent", color = "transparent"),
    legend.position = c(1.1, 0.29),
    legend.title = element_text(size = 16, family = "monts_bold", angle = 90),
    ## backround
    panel.background = element_rect(fill = bg, color = bg),
    panel.grid.major = element_blank(),
    plot.background = element_rect(fill = bg, colour = bg),
    ## axis
    axis.title = element_blank(),
    axis.text = element_blank(),
    ## margin
    plot.margin = margin(t=0,l=0,r=4,b=0, unit = "lines")
  )

# SAVE
showtext_opts(dpi = 300)
if (!dir.exists("output")) {dir.create("output")}
ggsave("virunga.jpeg", width = 20, height = 30, units = "cm", dpi = 300)

