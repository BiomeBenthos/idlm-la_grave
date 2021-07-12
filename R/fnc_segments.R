# =~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-=~-
# Create sub zones
create_segments <- function(x, d) {
  x %>%
  st_union() %>%
  st_cast("LINESTRING") %>%
  st_segmentize(d) %>%
  st_cast("POINT") %>%
  st_sf()
}
