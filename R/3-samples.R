library(grave)
zones <- st_read('./data/data-format/zones2022.geojson')
source('./R/fnc_sampling.R')

# Samples
samples <- list()
for(i in 1:nrow(zones)) {
  samples[[i]] <- sampling(zones[i, ], ns = 50, wd = 2)
}
samples <- bind_rows(samples)
st_write(samples, './data/data-format/samples2022.geojson', delete_dsn = TRUE)
st_write(samples, './data/data-format/samples2022.shp', delete_dsn = TRUE)
st_write(samples, './data/data-format/stations2022.kml', delete_dsn = TRUE, quiet = TRUE)

mapviewOptions(fgb = FALSE)
mapview(zones) + mapview(samples)


samples_csv <- cbind(samples, st_coordinates(samples)) %>%
           rename(Long_epsg2946 = X, Lat_epsg2946 = Y) %>%
           st_drop_geometry()

write.csv(samples_csv, './data/data-format/samples2022.csv', row.names = FALSE)

