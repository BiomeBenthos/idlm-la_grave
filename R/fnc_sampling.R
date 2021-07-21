# library(grave)
# zones <- st_read('./data/zones.geojson')
# =~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-
# Random sampling function
sampling <- function(x, ns = 10, wd = 2) {
  tmp <- list()
  for(i in 1:ns) {
    # Random sample
    tmp[[i]] <- st_sample(x, 1) %>%
                st_sf(data.frame(name = paste0(x$Nom, '-', i)))

    # Remove 2m buffer around sampled point
    x <- st_buffer(tmp[[i]], 2) %>%
         st_difference(x, .)
  }

  bind_rows(tmp)
}
