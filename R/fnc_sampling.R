library(pao)
library(pao)
zones <- st_read('./data/zones.geojson')
sous_zones <- st_read('./data/sous_zones.geojson')
# =~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-
# Random sampling function
sampling <- function(x, ns = 10, wd = 2) {
  tmp <- list()
  for(i in 1:ns) {
    # Random sample
    tmp[[i]] <- st_sample(x, 1) %>%
                st_sf(data.frame(name = paste0(x$name, '-', i)))

    # Remove 2m buffer around sampled point
    x <- st_buffer(tmp[[i]], 2) %>%
         st_difference(x, .)
  }

  bind_rows(tmp)
}

# Samples
samples <- list()
for(i in 1:nrow(sous_zones)) {
  samples[[i]] <- sampling(sous_zones[i, ])
}
samples <- bind_rows(samples)
st_write(samples, './data/samples.geojson', delete_dsn = TRUE)

mapview(zones) + mapview(sous_zones) + mapview(samples)
