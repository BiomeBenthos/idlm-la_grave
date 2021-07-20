fnc_zones <- function() {
  recharge <- st_read("data/data-format/recharge.geojson")

  # --------------------------------------------------------------------
  # Zone 1 : site recharge
  ## Haut recharge équilibre
  uid <- recharge$descriptio == "Crête Recharge Équilibre"
  xy1 <- st_cast(recharge[uid, ], "POINT")

  ## Bas recharge construction
  uid <- recharge$descriptio == "Bas Recharge Construction"
  xy2 <- st_cast(recharge[uid, ], "POINT") %>% .[c(nrow(.):1), ]

  ## Haut recharge équilibre + Bas recharge construction
  xy <- bind_rows(xy1,xy2)
  zone1 <- do.call(c, st_geometry(xy)) %>%
           st_cast("POLYGON") %>%
           st_sfc(crs = st_crs(recharge)) %>%
           st_sf(data.frame(Nom = "Site recharge"))

  # --------------------------------------------------------------------
  # Zone 2 : site sous-influence large
  ## Bas recharge construction
  uid <- recharge$descriptio == "Bas Recharge Construction"
  xy1 <- st_cast(recharge[uid, ], "POINT") %>% .[c(nrow(.):1), ]

  ## Bas recharge à l'équipe + 20m
  uid <- recharge$descriptio == "Bas Recharge Équilibre"
  xy2 <- recharge[uid, ] %>%
         st_buffer(20, endCapStyle = "SQUARE", singleSide = TRUE) %>%
         st_cast("POINT")
  ### ID points on appropriate line
  uid2 <- st_intersects(recharge[uid, ], xy2) %>% unlist()
  xy2 <- xy2[-uid2, ] %>%
         .[c(nrow(.):1), ]

  ## Bas recharge construction + (Bas recharge à l'équipe + 20m)
  xy <- bind_rows(xy1,xy2)
  zone2 <- do.call(c, st_geometry(xy)) %>%
           st_cast("POLYGON") %>%
           st_sfc(crs = st_crs(recharge)) %>%
           st_sf(data.frame(Nom = "Site sous-influence large"))

  # --------------------------------------------------------------------
  # Zone 3 : site sous-influence ouest

  ## --------
  ## NOTE:
  ## Un peu plus difficile à faire, alors un peu d'improvisation s'impose.
  ## L'idée est de conserver la ligne de rive et d'utiliser l'extension des
  ## lignes de recharge pour créer les zones.
  ##
  ## Je vais donc:
  ##  1. Manuellement générer une ligne de rivage à l'aide de mapEdit et la ligne de recharge
  ##  2. Calculer la distance moyenne entre les points de la crête de la recharge à l'équilibre et le bas de la recharge après construction
  ##  3. Utiliser la distance moyenne comme buffer pour étendre la ligne du bas de la recharge après construction
  ## --------

  ## Haut recharge équilibre
  uid <- recharge$descriptio == "Crête Recharge Équilibre"
  xy1 <- st_cast(recharge[uid, ], "POINT")

  ## Bas recharge construction
  uid <- recharge$descriptio == "Bas Recharge Construction"
  xy2 <- st_cast(recharge[uid, ], "POINT") %>% .[c(nrow(.):1), ]

  ## Distance moyenne
  d <- st_distance(xy1, xy2[nrow(xy2):1, ], by_element = TRUE) %>%
       mean() %>%
       as.numeric()

  ## Manuellement tracer le rivage en suivant les images satellitaires de Google
  # library(mapedit)
  # rive <- mapview(xy1) %>% editMap()
  # rive <- st_transform(rive[[1]], crs = st_crs(recharge))
  # st_write(rive, "data/data-format/rive.geojson", delete_dsn = TRUE)
  rive <- st_read("data/data-format/rive.geojson")

  # La limite ouest de la zone 3 être à 100m de la limite ouest de la recharge
  lim <- st_cast(xy1, "POINT") %>%
         .[1,] %>%
         st_buffer(100)

         # -> Maintenant il faut que je coupe la ligne de rivage avec le buffer de 100m 


  %>%
          st_buffer(-d, endCapStyle = "SQUARE", singleSide = TRUE) %>%
          st_cast("POINT") %>%
          # C'est pour s'assurer que les point à l'est sont
          # bien ceux des limites ouest de la recharge
          bind_rows(xy1[1, ], xy2[nrow(xy2), ], .) %>%


          zone2 <- do.call(c, st_geometry(xy)) %>%
                   st_cast("POLYGON") %>%
                   st_sfc(crs = st_crs(recharge)) %>%
                   st_sf(data.frame(Nom = "Site sous-influence large"))



  # -----
  st_write(recharge, "data/data-format/recharge.geojson", delete_dsn = TRUE)
}
