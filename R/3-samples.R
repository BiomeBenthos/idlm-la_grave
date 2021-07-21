library(grave)
zones <- st_read('./data/data-format/zones.geojson')
source('./R/fnc_sampling.R')

# Samples
samples <- list()
for(i in 1:nrow(zones)) {
  samples[[i]] <- sampling(zones[i, ], ns = 50, wd = 2)
}
samples <- bind_rows(samples)
st_write(samples, './data/data-format/samples.geojson', delete_dsn = TRUE)

mapview(zones) + mapview(samples)
