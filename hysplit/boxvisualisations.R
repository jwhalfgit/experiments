library(sf)
library(ggplot2)
library(rnaturalearth)

# Create boxes as sf polygons
create_box <- function(lat_min, lat_max, lon_min, lon_max) {
  lat_min <- as.numeric(lat_min)
  lat_max <- as.numeric(lat_max)
  lon_min <- as.numeric(lon_min)
  lon_max <- as.numeric(lon_max)
  # Create polygon coordinates
  coords <- matrix(c(
    lon_min, lat_min,
    lon_max, lat_min,
    lon_max, lat_max,
    lon_min, lat_max,
    lon_min, lat_min
  ), ncol = 2, byrow = TRUE)
  
  # Create polygon
 st_polygon(list(coords))
}

# Create all boxes

# Atlantic Ocean
# boxes <- st_sfc(
#   create_box(35, 59, -55, -10),
#   create_box(20, 43, -75, -20), # bottom peninsula
#   create_box(40, 71, -60, -55),# bottom middle
#   create_box(25, 43, -71, -75),# bottom right
#   create_box(15, 43, -70, -30),# bottom right
#   create_box(10, 59, -60, -30), # middle middle
#   create_box(5, 59, -55, -15),
#   create_box(0, 59, -50, -17),
#   create_box(55, 71, -60, -55),
#   create_box(60, 71, -65, -60),
#   create_box(29, 41, -27, -10), # Portugal / Africa
#   create_box(30, 59, -20, -15),
#   create_box(45, 51, -10, -5) # top top
# )


#North Atlantic
# boxes <- st_sfc(
# 
# create_box(70, 80, -17, 55),# North Atlantic
# create_box(64, 71, -13, 10),
# create_box(60, 71, -13, 5),
# create_box(59, 65, -41,-25),
# create_box(59, 63, -41,4),
# create_box(67, 69, -25,11),
# create_box(67, 75, -21,1),
# 
# create_box(63, 68, -34, -25)
# )


# Greenland box
# boxes <- st_sfc(
#   create_box(76, 79, -73, -18), # Widest slice
#   create_box(59, 66, -54, -40), # bottom peninsula
#   create_box(65, 73, -56, -35),# bottom middle
#   create_box(68, 74, -57, -20),# bottom right
#   create_box(65, 78, -40, -32),# bottom right
#   create_box(74, 76, -61, -17), # middle middle
#   create_box(79, 81, -68, -14), # top middle
#   create_box(81, 84, -64, -10) # top top
# )

# Europe box
boxes <- st_sfc(
  create_box(45, 55, 10, 55), # Widest slice
  create_box(35, 45, -10, 55), # bottom peninsula
  create_box(50, 55, 4, 55), # bottom middle
  create_box(45, 55, 4, 55), # bottom right
  create_box(45, 50, -10, 7), # France
  create_box(55, 64, 4, 20), # Scandy Bottom
  create_box(60, 71, 5, 55) # Scandinavia
)


# africa box
boxes <- st_sfc(
  create_box(30, 37, -15, -5),
  create_box(25, 37, -20, -10),
  create_box(10, 37, -20, -15),
  create_box(5, 37, -15, -10),
  create_box(0, 37, -10, 10),
  create_box(5, 37, -5, 55),
  create_box(0, 37, 10, 55),
  create_box(5, 37, -10, -5),
  create_box(10, 37, -15, -10)
)


# North Sea
# boxes <- st_sfc(
#   create_box(55, 58, -1, 8), # Between Scotland and Danmark
#   create_box(54, 55, 0, 8), # 
#   create_box(53, 59, 0, 5) # bottom middle
# )


# UK Box
# boxes <- st_sfc(
#   create_box(51, 55, -10, -5), # Ireland
#   create_box(50, 59, -7, -1), #  Great Britain (west of York)
#   create_box(51, 54, -3, 2) # bottom Right
# )

# Create sf object with box numbers
boxes_sf <- st_sf(geometry = boxes, box_number = 1:length(boxes))

# Set CRS to WGS84
st_crs(boxes_sf) <- 4326

# Get world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Create the plot
ggplot() +
  # Add world map
  geom_sf(data = world, fill = "lightgray", color = "black") +
  # Add boxes with different colors for each
  geom_sf(data = boxes_sf, 
          aes(fill = factor(box_number), alpha = 0.5),
          color = "black") +
  # Add box numbers
  geom_sf_label(data = boxes_sf, 
                aes(label = box_number),
                check_overlap = TRUE) +
  # Customize appearance
  scale_fill_discrete(name = "Box Number") +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  ) +
  coord_sf(crs = 4326) +
  labs(title = "Greenland Region Detection Boxes")

