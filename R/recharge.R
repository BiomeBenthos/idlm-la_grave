library(pao)
# Site de recharge
recharge_arriere <- st_read('analysis/data/PTO/Recharge_shp/arriere_recharge_construction.shp', quiet = TRUE) %>%
                    st_zm() %>% mutate(description = 'Recharge arriève') %>%
                    select(-OBJECTID, -Shape_Leng)

recharge_bas <- st_read('analysis/data/PTO/Recharge_shp/bas_recharge_construction.shp', quiet = TRUE) %>%
                st_zm() %>% mutate(description = 'Recharge bas') %>%
                select(-OBJECTID, -Shape_Leng)

recharge_crete <- st_read('analysis/data/PTO/Recharge_shp/crete_recharge_construction.shp', quiet = TRUE) %>%
                  st_zm() %>% mutate(description = 'Recharge crête') %>%
                  select(-OBJECTID, -Shape_Leng)

recharge_equilibre <- st_read('analysis/data/recharge_equilibre_shp/recharge_equilibre.shp', quiet = TRUE) %>%
                      mutate(description = c("Recharge arrière équilibre","Recharge haut équilibre", "Recharge bas équilibre")) %>%
                      select(-descrption, -OBJECTID, -SHAPE_Leng)

recharge <- rbind(recharge_arriere, recharge_bas, recharge_crete, recharge_equilibre)
uid <- recharge$description %in% c('Recharge bas équilibre','Recharge haut équilibre','Recharge bas')
recharge <- recharge[uid, ]
st_write(recharge, './data/recharge.geojson', delete_dsn = TRUE)
