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
  xy2 <- xy2[-uid2, ]

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
  # La limite ouest de la zone 3 être à 100m de la limite ouest de la recharge
  rive <- st_cast(xy1, "POINT") %>%
          .[1,] %>%
          st_buffer(100) %>%
          st_intersection(st_read("data/data-format/rive.geojson")) %>%
          st_buffer(-d, endCapStyle = "SQUARE", singleSide = TRUE) %>%
          st_cast("POINT")

  # Ajouter limites ouest de la recharge
  rive <- bind_rows(xy1[1, ],
                    rive[-nrow(rive), ],
                    xy2[nrow(xy2), ])

  zone3 <- do.call(c, st_geometry(rive)) %>%
           st_cast("POLYGON") %>%
           st_sfc(crs = st_crs(recharge)) %>%
           st_sf(data.frame(Nom = "Site sous-influence ouest"))

  # --------------------------------------------------------------------
  # Zone 4 : référence rive
  # même genre de polygone que site sous-influence ouest
  # 200 m de la recharge
  # 526 m de large
  ## Haut recharge équilibre
  uid <- recharge$descriptio == "Crête Recharge Équilibre"
  xy1 <- st_cast(recharge[uid, ], "POINT")

  ## Bas recharge construction
  uid <- recharge$descriptio == "Bas Recharge Construction"
  xy2 <- st_cast(recharge[uid, ], "POINT") %>% .[c(nrow(.):1), ]

  # Buffer 1
  buf1 <- st_cast(xy1, "POINT") %>%
          .[1,] %>%
          st_buffer(200)

  # Buffer 2
  buf2 <- st_cast(xy1, "POINT") %>%
          .[1,] %>%
          st_buffer(526)

  # Rive
  rive <- st_difference(buf2, buf1) %>%
          st_intersection(st_read("data/data-format/rive.geojson")) %>%
          st_buffer(-d, endCapStyle = "SQUARE", singleSide = TRUE) %>%
          st_cast("POINT")

  # Ajouter limites ouest de la recharge
  # rive <- bind_rows(xy1[1, ],
  #                   rive[-nrow(rive), ],
  #                   xy2[nrow(xy2), ])

  zone4 <- do.call(c, st_geometry(rive)) %>%
           st_cast("POLYGON") %>%
           st_sfc(crs = st_crs(recharge)) %>%
           st_sf(data.frame(Nom = "Site référence ouest"))

  # --------------------------------------------------------------------
  # Zone 5 : référence large
  # profil à l'équilibre
  # Distance entre crête à l'équipe et bas recharge à l'équilibre x2 = début de la zone référence
  # Profondeur = zone sous-influence au large
  ## Haut recharge équilibre
  uid <- recharge$descriptio == "Bas Recharge Construction"
  xy1 <- st_cast(recharge[uid, ], "POINT")

  ## Bas recharge à l'équipe
  uid <- recharge$descriptio == "Bas Recharge Équilibre"
  xy2 <- st_cast(recharge[uid, ], "POINT")

  ## Distance moyenne
  d <- st_distance(xy1, xy2, by_element = TRUE) %>%
       mean() %>%
       as.numeric()

  ## Ligne du bas de la zone de référence au large à partir d'un buffer autour le bas de la recharge à l'équilibre
  uid <- recharge$descriptio == "Bas Recharge Équilibre"
  l1 <- recharge[uid, ] %>%
        st_buffer((d+20)*2, endCapStyle = "SQUARE", singleSide = TRUE) %>%
        st_cast("POINT")

  ### ID points on appropriate line
  uid2 <- st_intersects(recharge[uid, ], l1) %>% unlist()
  l1 <- l1[-uid2, ]

  ## Ridiculous but true
  l1 <- l1[-1, ]
  l1 <- l1[c(6:nrow(l1), 1,3),]

  # Buffer avec la distance moyenne entre bas et crête de la recharge à l'équilibre
  zone5 <- do.call(c, st_geometry(l1)) %>%
           st_cast("LINESTRING") %>%
           st_sfc(crs = st_crs(recharge)) %>%
           st_buffer(d+20, endCapStyle = "SQUARE", singleSide = TRUE) %>%
           st_sf(data.frame(Nom = "Site référence large"))


  # --------------------------------------------------------------------
  zones <- bind_rows(zone1, zone2, zone3, zone4, zone5)

  # -----
  st_write(zones, "data/data-format/zones.geojson", delete_dsn = TRUE)
}
