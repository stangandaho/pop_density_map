# Load source
source("sources/packages.R") # packages
source("sources/fonts.R") #fonts

# Import and process raster and shapefile data
bj_density <- rast("sources/ben_pd_2020_1km_UNadj.tif")
atacora_shp <- st_read(dsn = "sources/Shapefiles", 
                       layer = "ben_admbnda_adm2_1m_salb_20190816") |> 
  filter(ADM1_FR == "Atacora")

atacora_raster <- raster::mask(x = bj_density, mask = atacora_shp) |> 
  raster::as.data.frame(xy = TRUE) |> 
  drop_na() |> 
  rename("value"=ben_pd_2020_1km_UNadj)

# Plot
density_break <- round(seq(min(atacora_raster$value), max(atacora_raster$value), 80))

ggplot(data = atacora_raster) +
  geom_raster(aes(x = x, y = y, fill = value))+
  scale_fill_gradient(low = "white", high = "#c51010",
                      breaks =density_break)+
  guides(fill = guide_colorbar(barwidth = unit(0.6, "lines"),
                               barheight = unit(16, "lines"), 
                               title.position = "left"))+
  geom_sf(data = atacora_shp, aes(), size = 2, color = "gray1", fill = "NA")+
  geom_sf(data = atacora_shp %>% st_union() %>% st_buffer(dist = 100), aes(), 
          linewidth = 2, color = "gray1", fill = "NA")+
  geom_sf_label(data = atacora_shp, aes(label = ADM2_FR),
                label.r = unit(0.5, units = "lines"), label.size = 0.0, fill = "NA", 
                color = "#504441", family = "montserrat_regular", size = 3)+
  theme_void()+
  labs(fill = expression("People.km"^-2), title = "Department of Atacora in Benin",
       subtitle = "Population density map",
       caption = "<p style='font-family:nexa_book; color:#f7f5f5;font-size:14px'>Stanislas Mahussi GANDAHO<br>@stangandaho   | Linkedin<br><i style='font-family:montserrat_regular; color:#e3dcdb;font-size:12px'>Data source: WorldPop - www.worldpop.org </i><br></p>")+
  theme(
    #plot and panel background 
    plot.background = element_rect(fill = "gray1", color = "NA"),
    panel.background = element_rect(fill = "gray1", color = "NA"),
    #title and subtitle
    plot.title = element_text(family = "Narkisim", color = "white",
                              size = 32, hjust = 0.5),
    plot.subtitle = element_text(family = "Vivaldi", color = "white",
                                 size = 27, hjust = 0.5),
    plot.title.position = "plot",
    #legend
    plot.margin = margin(r = 1, b = 0.2, t = 0.2, unit = "lines"),
    #legend.box.margin = margin(r = 2, unit = "lines"),
    legend.background = element_blank(),
    legend.title = element_text(color = "white", angle = 90, hjust = 0.5, vjust = 0.5,
                                family = "montserrat_regular", size = 12,
                                margin = margin(r = 0.7, unit = "lines")),
    legend.text = element_text(color = "#e3dcdb", family = "montserrat_regular", 
                               size = 12),
    #caption
    plot.caption = element_markdown(margin = margin(b=0.3, unit = "lines")
    ),
    plot.caption.position = "plot"
        )


ggsave("output/Atacora.png", dpi=300, width = 20, height = 20, units = "cm")

