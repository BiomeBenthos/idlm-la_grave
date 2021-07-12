x = zones[zones$nom == 'Recharge ouest', ]
name = 'ReE'
subzones_recharge <- function(x, name, r = FALSE, pt = FALSE) {
  dat <- st_cast(x, "POINT")
  if (r) {
    p1 <- create_segments(dat[c(1,2), ], 800/3) %>%
          st_combine() %>%
          st_cast("LINESTRING") %>%
          st_buffer(100, endCapStyle = "SQUARE", singleSide = TRUE) %>%
          st_cast("LINESTRING") %>%
          st_cast("POINT") %>%
          .[5:8, ] %>%
          st_sf()
    p2 <- create_segments(dat[c(4,3), ], 800/3)
  } else {
    p1 <- create_segments(dat[c(1,2), ], 800/3) %>%
          st_combine() %>%
          st_cast("LINESTRING") %>%
          st_buffer(100, endCapStyle = "SQUARE", singleSide = TRUE) %>%
          st_cast("LINESTRING") %>%
          st_cast("POINT") %>%
          .[8:5, ] %>%
          st_sf()
    p2 <- create_segments(dat[c(4,3), ], 800/3) %>% .[4:1, ]
  }

  # Create polygons from
  uid1 <- recharge$description == 'Recharge haut équilibre'
  uid2 <- recharge$description == 'Recharge bas'
  uid3 <- recharge$description == 'Recharge bas équilibre'
  x <- st_cast(recharge[uid1, ], "POINT")
  y <- st_cast(recharge[uid2, ], "POINT")
  z <- st_cast(recharge[uid3, ], "POINT")

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


  # Proche
  ## 50-75m
  pr50 <- rbind(p1[3:4, ], p2[4:3,]) %>%
          st_combine() %>%
          st_cast("POLYGON") %>%
          st_intersection(z1, .) %>%
          mutate(name = paste0(name, '-Pr-50'))

  ## 75-100m
  pr75 <- rbind(p1[3:4, ], p2[4:3,]) %>%
          st_combine() %>%
          st_cast("POLYGON") %>%
          st_intersection(z2, .) %>%
          mutate(name = paste0(name, '-Pr-75'))

  # Moyenne
  ## 50-75m
  mo50 <- rbind(p1[2:3, ], p2[3:2,]) %>%
          st_combine() %>%
          st_cast("POLYGON") %>%
          st_intersection(z1, .) %>%
          mutate(name = paste0(name, '-Mo-50'))

  ## 75-100m
  # mo75 <- rbind(p5[4:5, ], p4[5:4,]) %>%
  mo75 <- rbind(p1[2:3, ], p2[3:2,]) %>%
          st_combine() %>%
          st_cast("POLYGON") %>%
          st_intersection(z2, .) %>%
          mutate(name = paste0(name, '-Mo-75'))


  # Éloigné
  ## 50-75m
  el50 <- rbind(p1[1:2, ], p2[2:1,]) %>%
          st_combine() %>%
          st_cast("POLYGON") %>%
          st_intersection(z1, .) %>%
          mutate(name = paste0(name, '-El-50'))

  ## 75-100m
  el75 <- rbind(p1[1:2, ], p2[2:1,]) %>%
          st_combine() %>%
          st_cast("POLYGON") %>%
          st_intersection(z2, .) %>%
          mutate(name = paste0(name, '-El-75'))

  # Bind all
  rbind(pr50, pr75 ,mo50, mo75, el50, el75)
}
