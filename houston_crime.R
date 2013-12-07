library(ggmap)
head(crime)

violent.crimes <- subset(crime, offense != "auto theft" & offense != "theft" & offense != "burglary")
violent.crimes$offense <- factor(violent.crimes$offense, levels=c('robbery', 'aggravated assault', 'rape', 'murder'))
violent.crimes <- subset(violent.crimes, -95.39681 <= lon & lon <= -95.34188 & 29.73631 <= lat & lat <= 29.78400)

# Bubble map
theme_set(theme_bw(16))
HoustonMap <- qmap("houston", zoom = 14, color = "bw", legend = "topleft")
HoustonMap +
   geom_point(aes(x = lon, y = lat, colour = offense, size = offense),
              data = violent.crimes)
HoustonMap +
   stat_bin2d(
      aes(x = lon, y = lat, colour = offense, fill = offense),
      size = .5, bins = 30, alpha = 1/2,
      data = violent.crimes
   )

# Density map
houston <- get_map("houston", zoom = 14)
HoustonMap <- ggmap(houston, extent = "device", legend = "topleft")
HoustonMap +
   stat_density2d(
      aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
      size = 2, bins = 4, data = violent.crimes,
      geom = "polygon"
   )
overlay <- stat_density2d(
   aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
   bins = 4, geom = "polygon",
   data = violent.crimes
)
HoustonMap + overlay + inset(
   grob = ggplotGrob(ggplot() + overlay + theme_inset()),
   xmin = -95.35836, xmax = Inf, ymin = -Inf, ymax = 29.75062
)

# Faceted density map
houston <- get_map(location = "houston", zoom = 14, color = "bw",
                   source = "osm")
HoustonMap <- ggmap(houston, base_layer = ggplot(aes(x = lon, y = lat),
                                                 data = violent.crimes))
HoustonMap +
   stat_density2d(aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
                  bins = 5, geom = "polygon",
                  data = violent.crimes) +
   scale_fill_gradient(low = "black", high = "red") +
   facet_wrap(~ day)
