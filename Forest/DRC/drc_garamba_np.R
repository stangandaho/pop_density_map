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

## IMPORT GARAMBA PROTECTED AREA SHP
prot_area <- read_sf("./protected_area/protected_arae.shp")
garamba <- prot_area %>% 
  filter(NOM == "Parc National de la Garamba") %>% 
  st_transform(crs = terra::crs(lc_mosaic))

## CROP LC TO GARAMBA BOUNDARY
garamba_lc <- lc_mosaic %>% 
  terra::crop(y = terra::vect(garamba), snap = "in", mask = T)
plot(garamba_lc)

## CONVERTE GARAMBA LC TO DF
garamba_lc_df <- terra::as.data.frame(x = garamba_lc, xy = T) %>% 
  rename("percent" = 3)

## PARAMETERS FOR GGPLOT VIZ
w <- ncol(garamba_lc)
h = nrow(garamba_lc)
min_val <- min(garamba_lc_df$percent)
max_val <- max(garamba_lc_df$percent)
limits <- c(min_val, max_val)
color <- colorRampPalette(met.brewer(name = "VanGogh3"))(256)
bg <- "#e9f9e5"

## FONTS 
# downloadable at https://fonts.google.com/specimen/Montserrat?query=Montserra
monts_bold <- font_add("monts_bold", "fonts/Montserrat Bold 700.ttf")
monts_reg <- font_add("monts_reg", "fonts/Montserrat Medium 500.ttf")
monts_it <- font_add("monts_it", "fonts/Montserrat Light 300.ttf")
showtext_auto()

## GGPLOT
p <- ggplot(data = garamba_lc_df)+
  geom_raster(aes(x = x, y = y, fill = percent))+
  scale_fill_gradientn(colours = color, 
                       limits = limits,
                       name = "% of forest:")+
  coord_sf(crs = terra::crs(lc_mosaic))+
  labs(title = paste0("<p>National Park of <i style = 'color:#2f7119';>Garamba</i></p>"), 
       subtitle = "Democratic Republic of Congo",
       caption = paste0("<p>Copyright © 2024 - <i style = 'font-family:monts_reg;' >Stanislas Mahussi Gandaho</i>  <br> 
                        <i style = 'font-family:monts_it; text-align:center;'>Data - Copernicus Global Land Service: Land Cover 100m</i>
                        </p>"))+
  guides(
    fill = guide_legend(
      direction = "horizontal",
      breaks = seq(min_val, max_val, 20),
      keywidth = unit(1, units = "lines"),
      keyheight = unit(0.25, units = "lines"),
      title.position = "left",
      title.hjust = 0.5,
      ticks.linewidth = 1/.pt,
      label.position = "top",
      label.hjust = 0.5,
    ))+
  theme_minimal()+
  theme(
    ##Title
    plot.title = element_markdown(size = 34, family = "monts_bold", color = "gray20", hjust = 0.02),
    plot.title.position = "plot",
    ## Subtitle
    plot.subtitle = element_text(size = 22, family = "monts_reg", color = "gray40", hjust = 0.02),
    ## Caption
    plot.caption = element_markdown(size = 18, family = "monts_it", hjust = 1),
    plot.caption.position = "plot",
    ## Legende
    legend.background = element_rect(fill = "transparent", color = "transparent"),
    legend.position = c(0.17, 0.95),
    legend.title = element_text(size = 16, family = "monts_bold"),
    legend.text = element_text(size = 14, family = "monts_reg"),
    ## backround
    panel.background = element_rect(fill = bg, color = bg),
    panel.grid.major = element_blank(),
    plot.background = element_rect(fill = bg, colour = bg),
    ## axis
    axis.title = element_blank(),
    axis.text = element_blank()
  )
p
# SAVE
showtext::showtext_opts(dpi = 300)
if (!dir.exists("output")) {dir.create("output")}
ggsave("output/garamba.jpeg", plot = p, width = 25, height = 28, units = "cm", dpi = 300)
showtext::showtext_opts(dpi = 96)