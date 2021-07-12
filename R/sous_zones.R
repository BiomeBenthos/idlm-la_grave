library(pao)
bathymetrie <- st_read('./data/bathymetrie.geojson')
recharge <- st_read('./data/recharge.geojson')
zones <- st_read('./data/zones.geojson')
source('./R/fnc_segments.R')
source('./R/fnc_subzones.R')
source('./R/fnc_subzones_recharge.R')

ReOsub <- subzones_recharge(zones[zones$nom == 'Recharge ouest', ], 'ReO', r = TRUE)
ReEsub <- subzones_recharge(zones[zones$nom == 'Recharge est', ], 'ReE', r = FALSE)

uid <- c('ReO-Pr-225','ReO-Mo-225','ReO-El-225')
ReOsub2 <- subzones(zones[zones$nom == 'Recharge ouest', ], 'ReO', r = TRUE) %>%
           .[.$name %in% uid,]

uid <- c('ReE-Pr-225','ReE-Mo-225','ReE-El-225')
ReEsub2 <- subzones(zones[zones$nom == 'Recharge est', ], 'ReE', r = FALSE) %>%
           .[.$name %in% uid,]

SiEsub <- subzones(zones[zones$nom == 'Sous influence est', ], 'SiE', r = FALSE)
RfEsub <- subzones(zones[zones$nom == 'Référence est', ], 'RfE', r = FALSE)
SiOsub <- subzones(zones[zones$nom == 'Sous influence ouest', ], 'SiO', r = TRUE)
RfOsub <- subzones(zones[zones$nom == 'Référence ouest', ], 'RfO', r = TRUE, pt = TRUE)

sous_zones <- rbind(ReOsub, ReOsub2, ReEsub, ReEsub2, SiEsub, RfEsub, SiOsub, RfOsub)
st_write(sous_zones, './data/sous_zones.geojson', delete_dsn = TRUE)











# Modifier manuellement les sites de la recharge près de la plage
create_segments <- function(x, d) {
  x %>%
  st_union() %>%
  st_cast("LINESTRING") %>%
  st_segmentize(d) %>%
  st_cast("POINT") %>%
  st_sf()
}

# Create polygons from
uid <- recharge$description == 'Recharge haut équilibre'
x <- st_cast(recharge[uid, ], "POINT")
uid <- recharge$description == 'Recharge bas'
y <- st_cast(recharge[uid, ], "POINT")
uid <- recharge$description == 'Recharge bas équilibre'
z <- st_cast(recharge[uid, ], "POINT")

# zone 1
z1 <- rbind(x,y[nrow(y):1, ]) %>%
      st_combine() %>%
      st_cast("POLYGON") %>%
      st_sf()

# zone 2
z2 <- rbind(y,z[nrow(y):1, ]) %>%
      st_combine() %>%
      st_cast("POLYGON") %>%
      st_sf()

# Full zone
ReEz <- st_cast(ReE, "POINT") %>% rename(description = nom, geometry = x)
ReOz <- st_cast(ReO, "POINT") %>% rename(description = nom, geometry = x)

# Recharge est
uid1 <- recharge$description == 'Recharge haut équilibre'
uid2 <- recharge$description == 'Recharge bas équilibre'
poly <- rbind(st_point_on_surface(recharge[uid1,]),
              st_point_on_surface(recharge[uid2,]),
              ReEz[2, ], ReEz[3, ]) %>%
        st_combine()

mapview(z1) + z2 + poly


xy <- st_cast(recharge[uid,], "POINT") %>%
       .[c(1,nrow(.)),] %>%
       st_sf()

xym <-

seg_r1 <- create_segments(rbind(xym, xy[1, ]),
                          st_distance(xym, xy[1,])/3) %>%
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
