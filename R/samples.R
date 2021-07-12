library(pao)
sous_zones <- st_read('./data/sous_zones.geojson')
source('./R/fnc_sampling.R')

# Samples
samples <- list()
for(i in 1:nrow(sous_zones)) {
  samples[[i]] <- sampling(sous_zones[i, ])
}
samples <- bind_rows(samples)
st_write(samples, './data/samples.geojson', delete_dsn = TRUE)

mapview(sous_zones) + mapview(samples)
