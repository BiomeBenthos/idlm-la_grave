library(pao)
bathymetrie <- st_read('./data/bathymetrie.geojson')
recharge <- st_read('./data/recharge.geojson')

# Centroide du site de recharge le long de la ligne de 0 bathymétrie
uid <- recharge$description == 'Recharge haut équilibre'
xy <- st_point_on_surface(recharge[uid,]) %>%
      st_sf()

distance_to_0 <- xy %>%
                 st_nearest_points(bathymetrie) %>%
                 st_cast("POINT") %>%
                 st_distance() %>%
                 .[2,1] %>%
                 units::drop_units()

bathy_adj1 <- bathymetrie %>%
             st_buffer(-(distance_to_0), endCapStyle = "SQUARE", singleSide = TRUE) %>%
             st_cast("LINESTRING") %>%
             st_difference(st_buffer(bathymetrie, (distance_to_0-1), endCapStyle = "SQUARE"))


bathy_adj2 <- bathymetrie %>%
             st_buffer(-(distance_to_0+10), endCapStyle = "SQUARE", singleSide = TRUE) %>%
             st_cast("LINESTRING") %>%
             st_difference(st_buffer(bathymetrie, (distance_to_0-1), endCapStyle = "SQUARE"))


bathy_adj3 <- bathymetrie %>%
              st_buffer(-(distance_to_0-10), endCapStyle = "SQUARE", singleSide = TRUE) %>%
              st_cast("LINESTRING") %>%
              st_difference(st_buffer(bathymetrie, (3), endCapStyle = "SQUARE"))

# Zones
xy <- st_cast(recharge[uid,], "POINT") %>%
       .[c(1,nrow(.)),] %>%
       st_sf()

xym <- st_point_on_surface(recharge[uid,]) %>%
       st_sf()
d <- 660
xyz <- xym %>%
       st_nearest_points(bathy_adj1) %>%
       st_cast("POINT") %>%
       .[2,] %>%
       st_as_sf()
xyz <- xym
# Pour identifier les sites autour de la recharge
buf <- st_buffer(xyz, d) %>%
       st_cast("LINESTRING") %>%
       st_intersection(bathy_adj1) %>%
       st_cast("POINT") %>%
       st_as_sf()

# Recharge ouest
# ReO <- bind_rows(buf[1,], xyz) %>%
ReO <- bind_rows(xym, xy[1, ]) %>%
       st_union() %>%
       st_cast("LINESTRING") %>%
       st_buffer(-250, endCapStyle = "SQUARE", singleSide = TRUE) %>%
       st_as_sf(data.frame(nom = 'Recharge ouest'))


# Recharge est
# ReE <- bind_rows(buf[2,], xyz) %>%
ReE <- bind_rows(xym, xy[2, ]) %>%
       st_union() %>%
       st_cast("LINESTRING") %>%
       st_buffer(-250, endCapStyle = "SQUARE", singleSide = TRUE) %>%
       st_as_sf(data.frame(nom = 'Recharge est'))


# Sous-influence est
SiE <- buf[2, ] %>%
       st_buffer(1000) %>%
       st_cast("LINESTRING") %>%
       st_intersection(bathy_adj1) %>%
       st_cast("POINT") %>%
       .[2,]

SiE <- SiE %>%
       st_buffer(d) %>%
       st_cast("LINESTRING") %>%
       st_intersection(bathy_adj1) %>%
       st_cast("POINT") %>%
       .[1,] %>%
       bind_rows(SiE) %>%
       st_union() %>%
       st_cast("LINESTRING") %>%
       st_buffer(-250, endCapStyle = "SQUARE", singleSide = TRUE) %>%
       st_as_sf(data.frame(nom = 'Sous influence est'))


# Référence est
RfE <- buf[2, ] %>%
       st_buffer(2000) %>%
       st_cast("LINESTRING") %>%
       st_intersection(bathy_adj2) %>%
       st_cast("POINT") %>%
       .[2,]

RfE <- RfE %>%
       st_buffer(d) %>%
       st_cast("LINESTRING") %>%
       st_intersection(bathy_adj2) %>%
       st_cast("POINT") %>%
       .[2,] %>%
       bind_rows(RfE) %>%
       st_union() %>%
       st_cast("LINESTRING") %>%
       st_buffer(-250, endCapStyle = "SQUARE", singleSide = TRUE) %>%
       st_as_sf(data.frame(nom = 'Référence est'))

# Sous-influence ouest
xy <- data.frame(x = -68.42132, y = 49.04103)
quai <- st_as_sf(xy, coords = c('x','y'), crs = 4326) %>%
        st_transform(st_crs(buf))

SiO <- quai %>%
       st_buffer(1000) %>%
       st_cast("LINESTRING") %>%
       st_intersection(bathy_adj1) %>%
       st_cast("POINT") %>%
       .[1,]

SiO <- SiO %>%
       st_buffer(d) %>%
       st_cast("LINESTRING") %>%
       st_intersection(bathy_adj1) %>%
       st_cast("POINT") %>%
       .[2,] %>%
       bind_rows(SiO) %>%
       st_union() %>%
       st_cast("LINESTRING") %>%
       st_buffer(-250, endCapStyle = "SQUARE", singleSide = TRUE) %>%
       st_as_sf(data.frame(nom = 'Sous influence ouest'))

# Référence ouest
RfO <- quai %>%
       st_buffer(2000) %>%
       st_cast("LINESTRING") %>%
       st_intersection(bathy_adj3) %>%
       st_cast("POINT") %>%
       .[1,]

RfO <- RfO %>%
       st_buffer(d) %>%
       st_cast("LINESTRING") %>%
       st_intersection(bathy_adj3) %>%
       st_cast("POINT") %>%
       .[1,] %>%
       bind_rows(RfO) %>%
       st_union() %>%
       st_cast("LINESTRING") %>%
       st_buffer(-250, endCapStyle = "SQUARE", singleSide = TRUE) %>%
       st_as_sf(data.frame(nom = 'Référence ouest'))

# Export zones
zones <- rbind(ReO, ReE, SiE, RfE, SiO, RfO)
mapview(zones)
# Export
st_write(zones, './data/zones.geojson', delete_dsn = TRUE)
