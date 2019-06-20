# There is a hex2rgb, but not the opposite, so just implement one,
# with room for setting alpha values too
rgb2hex <- function(r,g,b,alpha) 
  rgb(r, g, b, alpha, maxColorValue = 255)

# Add alpha\in [0,1] value to given hex colour
add_alpha_to_hex <- function(h,alpha) {
  if ((alpha <0) || (alpha>1))
    return(NA)
  a = as.hexmode(round(alpha*255))
  # R format for hex colours with alpha is RGBA
  return(paste0(h,a))
}

# Given a vector v of colors given in hex, return similar vector
# but with alpha applied
apply_alpha_to_hex <- function(v,alpha = 255) {
  tmp = matrix(col2rgb(v, alpha = TRUE),
               nr = length(v),
               nc = 4,
               byrow = TRUE)
  colnames(tmp) = c("r","g","b","alpha")
  # If alpha is less than 1, assume it comes as a fraction
  if (alpha < 1)
    alpha <- 255*alpha
  tmp[,"alpha"] = alpha
  out = rgb2hex(tmp[,"r"],tmp[,"g"],tmp[,"b"],tmp[,"alpha"])
  return(out)
}

# Set the corners of the region to download
upperLeft = c(49.8833,-97.1818)
lowerRight = c(49.86310,-97.15081)

# Take a cute map type in which alpha effect is easy to see
map_type = "stamen-watercolor"

# Download the map
Winnipeg_map <- OpenStreetMap::openmap(upperLeft = upperLeft,
                                       lowerRight = lowerRight,
                                       type = map_type)

# Store the original colours for convenience
stored_colours <- Winnipeg_map$tiles[[1]]$colorData

# Plot the maps
for (alpha in seq(0.1, 1, by = 0.1)) {
  fileName = sprintf("~/Documents/DATA/tmp/Winnipeg_part_alpha%1.2f.png",
                     alpha)
  Winnipeg_map$tiles[[1]]$colorData = add_alpha_to_hex(stored_colours,
                                                       alpha)
  png(file = fileName, 
      width = 800, height = 800)
  plot(Winnipeg_map)
  dev.off()
}

