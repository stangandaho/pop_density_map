benin_shp <- st_read(dsn = "./sources/Shapefiles/ben_admbnda_adm1_1m_salb_20190816.shp")

benin_plot <- ggplot()+
  geom_sf(data = benin_shp, color = "#320029", size = 0.2)+
  geom_sf(data = collines_shp, fill = "#ff8109", color = "NA")+
  theme_void()

africa_shp <- st_read("./sources/Shapefiles/Africa_adm0/Africa_adm0.shp")
africa_plot <- ggplot()+
  geom_sf(data = africa_shp, 
          color = if_else(africa_shp$COUNTRY=="BENIN", "white", "#320029"), 
          fill = if_else(africa_shp$COUNTRY=="BENIN", "#ff8109", "#c9c9c9"), size = 0.2)+
  theme_void()

arrow_df <- data.frame(x = 1.245, y = 8.65, xend = 1.245, yend = 8.1)
