subzones <- function(x, name, r = FALSE, pt = FALSE) {
  dat <- st_cast(x, "POINT")
  p1 <- create_segments(dat[c(1,2), ], 800/3)
  p2 <- create_segments(dat[c(4,3), ], 800/3)

  if (r) {
    p3 <- create_segments(rbind(p1[1,], p2[1,]), 25)
    p4 <- create_segments(rbind(p1[2,], p2[2,]), 25)
    p5 <- create_segments(rbind(p1[3,], p2[3,]), 25)
    p6 <- create_segments(rbind(p1[4,], p2[4,]), 25)
  } else {
    p3 <- create_segments(rbind(p1[4,], p2[4,]), 25)
    p4 <- create_segments(rbind(p1[3,], p2[3,]), 25)
    p5 <- create_segments(rbind(p1[2,], p2[2,]), 25)
    p6 <- create_segments(rbind(p1[1,], p2[1,]), 25)
  }

  if (pt) {
    p3 <- p3[nrow(p3):1, ]
    p4 <- p4[nrow(p4):1, ]
    p5 <- p5[nrow(p5):1, ]
    p6 <- p6[nrow(p6):1, ]
  }

  # Proche
  ## 50-75m
  # pr50 <- rbind(p6[3:4, ], p5[4:3,]) %>%
  pr50 <- rbind(p6[1:2, ], p5[2:1,]) %>%
          st_combine() %>%
          st_cast("POLYGON") %>%
          st_sf(data.frame(name = paste0(name, '-Pr-50')))

  ## 75-100m
  # pr75 <- rbind(p6[4:5, ], p5[5:4,]) %>%
  pr75 <- rbind(p6[2:3, ], p5[3:2,]) %>%
          st_combine() %>%
          st_cast("POLYGON") %>%
          st_sf(data.frame(name = paste0(name, '-Pr-75')))

  ## 225-250
  # pr225 <- rbind(p6[10:11, ], p5[11:10,]) %>%
  pr225 <- rbind(p6[8:9, ], p5[9:8,]) %>%
          st_combine() %>%
          st_cast("POLYGON") %>%
          st_sf(data.frame(name = paste0(name, '-Pr-225')))

  # Moyenne
  ## 50-75m
  # mo50 <- rbind(p5[3:4, ], p4[4:3,]) %>%
  mo50 <- rbind(p5[1:2, ], p4[2:1,]) %>%
          st_combine() %>%
          st_cast("POLYGON") %>%
          st_sf(data.frame(name = paste0(name, '-Mo-50')))

  ## 75-100m
  # mo75 <- rbind(p5[4:5, ], p4[5:4,]) %>%
  mo75 <- rbind(p5[2:3, ], p4[3:2,]) %>%
          st_combine() %>%
          st_cast("POLYGON") %>%
          st_sf(data.frame(name = paste0(name, '-Mo-75')))

  ## 225-250
  # mo225 <- rbind(p5[10:11, ], p4[11:10,]) %>%
  mo225 <- rbind(p5[8:9, ], p4[9:8,]) %>%
          st_combine() %>%
          st_cast("POLYGON") %>%
          st_sf(data.frame(name = paste0(name, '-Mo-225')))

  # Éloigné
  ## 50-75m
  # el50 <- rbind(p4[3:4, ], p3[4:3,]) %>%
  el50 <- rbind(p4[1:2, ], p3[2:1,]) %>%
          st_combine() %>%
          st_cast("POLYGON") %>%
          st_sf(data.frame(name = paste0(name, '-El-50')))

  ## 75-100m
  # el75 <- rbind(p4[4:5, ], p3[5:4,]) %>%
  el75 <- rbind(p4[2:3, ], p3[3:2,]) %>%
          st_combine() %>%
          st_cast("POLYGON") %>%
          st_sf(data.frame(name = paste0(name, '-El-75')))

  ## 225-250
  # el225 <- rbind(p4[10:11, ], p3[11:10,]) %>%
  el225 <- rbind(p4[8:9, ], p3[9:8,]) %>%
          st_combine() %>%
          st_cast("POLYGON") %>%
          st_sf(data.frame(name = paste0(name, '-El-225')))

  # Bind all
  rbind(pr50, pr75, pr225 ,mo50, mo75, mo225, el50, el75, el225) %>%
  rename(geometry = ".")
}
