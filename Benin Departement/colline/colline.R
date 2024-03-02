#load necessaries packages
source("sources/packages.R")

#import and wrangle raster and shapefile data
bj_density <- terra::rast("sources/ben_pd_2020_1km_UNadj.tif")
collines_shp <- st_read(dsn = "./sources/Shapefiles/ben_admbnda_adm2_1m_salb_20190816.shp") |> 
  filter(ADM1_FR == "Collines")

collines_raster <- terra::mask(x = bj_density, mask = collines_shp) |> 
  terra::as.data.frame(xy = TRUE) |> 
  drop_na() |> 
  rename("value"=ben_pd_2020_1km_UNadj)

#set fonts
source("sources/fonts.R")

# miniature set up
source("./Benin Departement/colline/miniature.R")
#plotting
ggplot() +
  geom_raster(data = collines_raster, aes(x = x, y = y, fill = value))+
  scale_fill_gradient(low = "white", high = "#c51010", 
                      breaks = seq(round(min(collines_raster$value)), 
                                   round(max(collines_raster$value)), 100))+
  guides(fill = guide_colorbar(barwidth = unit(0.6, "lines"),
                               barheight = unit(16, "lines"),
                               title.position = "left", title.hjust = 0.5))+
  geom_sf(data = collines_shp, aes(), linewidth = 0.5, color = "#320029", fill = "NA")+
  geom_sf_label(data = collines_shp, aes(label = ADM2_FR),
                label.r = unit(0.2, units = "lines"), label.size = 0.5, fill = "NA", color = "#26051c", family = "Narkisim", size = 8)+
  scalebar(data = collines_shp, location = "bottomleft", dist = 20,
           dist_unit = "km", transform = TRUE, model = "International",
           st.dist = 0.04, st.bottom = FALSE, box.fill = c("#b3a6a1", "#e3dcdb"),
           box.color = NA, st.color = "#e3dcdb",
           anchor = c(x = 1.6, y = 7.42), st.size = 6)+
  north(data = collines_shp, location = "bottomright", symbol = 4, scale = 0.15, anchor = c(x = 2.7, y = 7.42))+
  theme_void()+
  labs(fill = expression("People.km"^-2), title = "Population density Map",
       subtitle = "Department of  <span style = 'color:#ff8109; 
       font-size:55px'> Collines</span> in <span style = 'color:#c6c6c6; 
       font-size:56px'></span>Benin",
       caption = "<p style='font-family:Nyala; color:#f7f5f5;font-size:22px'>Graphic: Stanislas Mahussi GANDAHO<br>@stangandaho  | Linkedin<br><i style='font-family:Gabriola; color:#e3dcdb;font-size:24px'>Data source: WorldPop - www.worldpop.org </i><br></p>")+
  draw_plot(africa_plot, x = 1.07, y = 8.4, width = 0.5, height = 0.5)+
  draw_plot(benin_plot, x = 0.96, y = 7.3, width = 0.7, height = 0.7)+
  geom_segment(data = arrow_df, aes(x = x, y = y, xend = xend, yend = yend), arrow = arrow(length = unit(0.2, "cm")), color = "#fffaf9")+
  annotate(geom = "text", label = "Africa", x = 1.4, y = 8.9, 
           color = "white", size = 11, family = "Comic Sans MS")+
  annotate(geom = "text", label = "Benin", x = 1.3, y = 8.03,
           color = "white", size = 11, family = "Comic Sans MS")+
  theme(
    #background 
    #plot.margin = margin(r = 3, unit = "lines"),
    plot.background = element_rect(fill = "#320029", color = "NA"),
    panel.background = element_rect(fill = "#320029", color = "NA"),
    #title
    plot.title = element_text(family = "Onyx", color = "white",
                              size = 65, hjust = 0.5, 
                              margin = margin(t = 0.7, unit = "lines")),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(family = "montserrat_bold", color = "#d6bca3",
                                     size = 30, hjust = 0.5, 
                                     margin = margin(b = 4, t=0.2, unit = "lines")),
    #legend
    legend.background = element_blank(),
    legend.title = element_text(color = "#f7eace", angle = 90, hjust = 0.5, vjust = 0.5,
                                family = "montserrat_bold", size = 16),
    legend.text = element_text(color = "#f7eace", family = "montserrat_regular", 
                               size = 14, margin = margin(r = 12)),
    #caption
    plot.caption = element_markdown(margin = margin(b=0.3, r = 1, unit = "lines")
    ),
    plot.caption.position = "plot"
  )


ggsave("output/Collines.jpg", dpi=300, width = 27, height = 30, units = "cm")
