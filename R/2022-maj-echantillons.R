library(grave)
mapviewOptions(fgb = FALSE)

samples <- st_read("data/data-format/samples2022.geojson", quiet = TRUE)

# Rename
samples$name_full <- samples$name
samples$name <- gsub("Site recharge", "Re",samples$name)
samples$name <- gsub("Site sous-influence large", "SiL",samples$name)
samples$name <- gsub("Site sous-influence ouest", "SiO",samples$name)
samples$name <- gsub("Site référence large", "RfL",samples$name)
samples$name <- gsub("Site référence ouest", "RfO",samples$name)
samples$name <- gsub("-1$", "-01", samples$name)
samples$name <- gsub("-2$", "-02", samples$name)
samples$name <- gsub("-3$", "-03", samples$name)
samples$name <- gsub("-4$", "-04", samples$name)
samples$name <- gsub("-5$", "-05", samples$name)
samples$name <- gsub("-6$", "-06", samples$name)
samples$name <- gsub("-7$", "-07", samples$name)
samples$name <- gsub("-8$", "-08", samples$name)
samples$name <- gsub("-9$", "-09", samples$name)

# Update 2022-08-26
samples$echantillon <- "Stations non-échantillonnées"
done <- c(
  "SiO-24","SiO-06","SiO-26","SiO-13","SiO-05","SiO-02",
  "SiO-04","SiO-21","SiO-31","SiO-35","SiO-20","SiO-37",
  "SiO-08","SiO-07","SiO-10","SiO-03","SiO-17","SiO-09",
  "SiO-18","SiO-39","SiO-28","SiO-23","SiO-14","SiO-34",
  "SiO-33"
)
uid <- samples$name %in% done
samples$echantillon[uid] <- "Stations faites"
stations_echantillonnees <- samples$echantillon == "Stations faites"
stations_non_echantillonnees <- !stations_echantillonnees

# Update 2022-08-29
# Stations faites 
done <- c(
  "SiO-02","SiO-03","SiO-04","SiO-05","SiO-06","SiO-07","SiO-08","SiO-09","SiO-10",
  "SiO-13","SiO-14","SiO-17","SiO-18","SiO-20","SiO-21","SiO-23","SiO-24","SiO-26",
  "SiO-28","SiO-31","SiO-33","SiO-34","SiO-35","SiO-37","SiO-39","RfO-01","RfO-06",
  "RfO-11","RfO-14","RfO-22","RfO-24","RfO-25","RfO-28","SiL-01","SiL-02","SiL-03",
  "SiL-04","SiL-26","SiL-06","SiL-08","SiL-09","SiL-10","SiL-11","SiL-12","SiL-13",
  "SiL-14","SiL-15","SiL-17","SiL-18","SiL-19","SiL-20","SiL-21","SiL-22","SiL-23",
  "SiL-24","SiL-27","SiL-29","SiL-30","SiL-31","SiL-32","SiL-33","SiL-36","SiL-37",
  "SiL-39","SiL-42","RfL-01","RfL-02","RfL-03","RfL-05","RfL-07","RfL-08","RfL-09",
  "RfL-11","RfL-12","RfL-13","RfL-14","RfL-15","RfL-16","RfL-17","RfL-18","RfL-19",
  "RfL-20","RfL-21","RfL-22","RfL-23","RfL-24","RfL-25","RfL-26","RfL-27","RfL-28",
  "RfL-29","RfL-30","RfL-31","RfL-32","RfL-33","Re-02","Re-03","Re-04","Re-05",
  "Re-08","Re-10","Re-12","Re-14","Re-17","Re-19","Re-20","Re-21","Re-22","Re-23",
  "Re-24","Re-25","Re-28"
)
uid <- samples$name %in% done
samples$echantillon[uid] <- "Stations faites"
stations_echantillonnees <- uid

# Stations impossibles à échantillonner
impossible <- c(
  "RfL-05","RfL-16","RfL-28","SiL-04","SiL-10","SiL-25","SiL-28"
)
uid <- samples$name %in% impossible
samples$echantillon[uid] <- "Stations impossibles"
stations_impossibles <- uid

# Stations non échantillonnées
stations_non_echantillonnees <- samples$echantillon == "Stations non-échantillonnées"


# Update coordinates 
coords <- function(samples = samples, name, lat, lon) {
  samp <- st_transform(samples, crs = 4326)
  uid <- samp$name == name
  geom <- st_geometry(samp[uid, ]) 
  geom <- st_as_sf(
            data.frame(lon = lon, lat = lat), 
            coords = c("lon","lat"),
            crs = 4326
          ) |> 
          st_geometry()
  st_geometry(samp[uid,]) <- geom
  samp <- st_transform(samp, crs = st_crs(samples))
  samp
}


samp <- samples 
samp <- coords(samp, "SiO-33", 47.23775, -61.84280)
samp <- coords(samp, "RfO-06", 47.23800, -61.84534)
samp <- coords(samp, "RfO-11", 47.23788, -61.84452)
samp <- coords(samp, "RfO-28", 47.23785, -61.84466)

# Export new samples, keep in mind that this overwrites the original samples 
st_write(samp, './data/data-format/samples2022.geojson', delete_dsn = TRUE)
st_write(samp, './data/data-format/samples2022.shp', delete_dsn = TRUE)
st_write(samp, './data/data-format/stations2022.kml', delete_dsn = TRUE, quiet = TRUE)

samples_csv <- cbind(samp, st_coordinates(samp)) %>%
           rename(Long_epsg2946 = X, Lat_epsg2946 = Y) %>%
           st_drop_geometry()

write.csv(samples_csv, './data/data-format/samples2022.csv', row.names = FALSE)

