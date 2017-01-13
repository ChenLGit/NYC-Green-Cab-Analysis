######################## Geographical Plot of Pickup Density #######################

# Geo Heat Map
#If runwith Error: ScalesList was built with an incompatible version of ggproto. Please reinstall ggplot2 and ggmap.
#devtools::install_github("dkahle/ggmap")
#devtools::install_github("hadley/ggplot2")

# Change Map Color Style
theme_set(theme_bw(16))

# Get Map through Web
newyorkcity <- get_map(location = c(-73.9485,40.7447), zoom = 11, color = "bw")

# Set up map
NewYorkCity <- ggmap(newyorkcity, extent = "device", 
                     base_layer = ggplot(data = NYTaxi_Sub, aes(x = PickupLongitude, y = PickupLatitude))) 
# Plot Geo Heat Map
NewYorkCity + 
  geom_point(aes(x = PickupLongitude, y = PickupLatitude), 
             colour = "black", alpha = 0.5, size = 0.1, data = NYTaxi_Sub) +
  stat_density2d(data = NYTaxi_Sub,
                 aes(x = PickupLongitude, y = PickupLatitude, fill = ..level.., alpha = ..level..), 
                 size = 0.01, bins = 180, geom = "polygon", n = 50, h=0.05) +
                 scale_fill_gradient(low = "blue", high = "red") + 
                 scale_alpha(range = c(0.05,0.08), guide = FALSE)+
  geom_density2d(data = NYTaxi_Sub, 
                 aes(x = PickupLongitude, y = PickupLatitude), 
                 colour = "yellow", size = 0.1, alpha = 0.8) + 
  theme(legend.position='top', 
        legend.direction = "horizontal",
        legend.text = element_text(size = 12, colour = "black", angle = 0),
        #legend.key.size = unit(1, "cm"),
        legend.key.width = unit(2, "cm"),
        plot.margin = unit(c(1,1,1,1), "cm")) +
  scale_colour_brewer(name = "My Legend") +
  xlab("Day") +
  ylab("Week") + 
  facet_wrap(~PickupDate, ncol=7)
