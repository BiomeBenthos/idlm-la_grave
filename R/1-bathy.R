#' Bathymétrie
#'
#' @export

bathymetrie <- function() {
  library(raster)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Smoothing parameters
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Resolution
  resolution <- 1

  # Bandwidth
  bandwidth <- 5

  # Study area
  # x <- mapedit::editMap()
  # x <- st_transform(x, crs = 2946)
  # st_write(x, "data/data-raw/studyarea.geojson", delete_dsn = TRUE)
  sa <- st_read("data/data-raw/studyarea.geojson")

  # Bounding box
  zone_bbox <- st_bbox(sa)

  # Grid
  zone_xy <- zoneGrid(sa, resolution, bandwidth) %>%
             as.data.frame()

  # Rastergrid
  rasterGrid <- st_rasterize(sa, dy = resolution, dx = resolution) %>%
                as("Raster")
  values(rasterGrid) <- 0

  # Data
  # bathy <- read.table("data/data-raw/Bathymetrie_LaGrave/Bathy_2019_NMM_WSP_modif.txt", header = TRUE) %>%
  bathy <- read.table("data/data-raw/Bathymetrie_LaGrave/Bathy_CIDCO_NMM.txt", header = TRUE) %>%
  x <- read.table("data/data-raw/Bathymetrie_LaGrave/delete.txt", header = FALSE, sep = ",")

   %>%
           st_as_sf(coords = c("X","Y"), crs = 2946)
  uid <- st_intersects(sa, bathy) %>% unlist()
  bathy <- bathy[uid, ]

  # Kernel
  kernel <- df %>%
    cbind(., st_coordinates(.)) %>%
    st_set_geometry(NULL) %>%
    dplyr::select(x = X, y = Y, Z) %>%
    btb::kernelSmoothing(dfObservations = .,
                         sEPSG = 2946,
                         iCellSize = resolution,
                         iBandwidth = bandwidth,
                         vQuantiles = NULL,
                         dfCentroids = zone_xy)




  dat <- lissage(df = bathy,
                 field = "Z",
                 bandwidth = bandwidth,
                 resolution = resolution,
                 grid = zone_xy,
                 rasterGrid = rasterGrid)
  mapview(dat)


  #
  xy <- as.data.frame(zone_xy) %>%
        mutate(z = dat) %>%
        st_as_sf(coords = c("x","y"), crs = 2946)
}
