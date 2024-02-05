
# Load packages
source("sources/packages.R")

# Import data
alibori_raster <- rast("./alibori/Alibori.tif")
alibori_shp <- st_read(dsn = "./alibori",layer = "Alibori")
alibori_shp <- st_transform(x = alibori_shp, crs = crs(alibori_raster))

# Plotting
alibori_df <- raster::as.data.frame(alibori_raster, xy = TRUE) |> 
  na.omit() |> 
  rename("value" = Alibori)

# Add fonts to session
source("sources/fonts.R")

# Plot
ggplot()+
  geom_raster(data = alibori_df, aes(x = x, y = y, fill = value))+
  scale_fill_gradient(low = "#e5e4e0", high = "#d00021",
                      breaks = c(0, 175, 350, 525, 700, 875, 1050, 1225, 1400))+
  geom_sf(data = alibori_shp, aes(geometry=geometry), 
          fill = "#FFFFFF63", color = "#0e0836", size = 2)+
  geom_sf_label(data = alibori_shp, aes(label = NAME_2),
                label.r = unit(0.5, units = "lines"), label.size = 0.3, fill = "NA", 
                color = "#490058", family = "Comic Sans MS", size = 6)+
  geom_text(aes(x = 3.65, y=12.31, label = "2022"), family = "montserrat_bold", 
            color = "#e5b7b4", size = 16)+
  geom_text(aes(x = 3.65, y=12.32, label = "2022"), family = "montserrat_bold", 
            color = "white", size = 16)+
  guides(fill = guide_colorbar(barwidth = unit(0.6, "lines"),
                               barheight = unit(16, "lines"), 
                               title.position = "left"))+
  labs(title = paste0("<p style = 'font-family:montserrat_bold; color:#FFFFFF;font-size:53px'>
                      <span style = 'color:#ff6400; font-size:58px'>Alibori</span>'s Population density<br></p>"),#str_wrap(source("sources/data_description.R")), 
       x = "Longitude", y = "Latitude", fill = "People/Km²",
       caption = "Stanislas Mahussi GANDAHO | LinkedIn - @stangandaho \nData source: WorldPop - www.worldpop.org")+
  theme_void()+
  theme(
    plot.title.position = "plot",
    plot.title = element_markdown(family = "montserrat_bold", 
                                  margin = margin(t = 18), hjust = .5),
    plot.background = element_rect(fill = "#0e0836", colour ="NA" ),
    panel.grid = element_blank(),
    
    legend.background = element_rect(fill = "#0e0836", colour ="NA"),
    legend.title = element_text(color = "#f7eace", angle = 90, hjust = 0.5, vjust = 0.5,
                                family = "montserrat_bold", size = 16),
    legend.text = element_text(color = "#f7eace", family = "montserrat_regular", 
                               size = 14, margin = margin(r = 12)),
    
    plot.caption = element_text(color = "gray60", family = "nexa_book",
                                size = 15, hjust = 1, margin = margin(r = 12, b = 12)),
    plot.caption.position = "plot"
  )

# Save plot
ggsave("output/Alibori.jpeg", dpi = 300, width = 24, height = 26, units = "cm")

