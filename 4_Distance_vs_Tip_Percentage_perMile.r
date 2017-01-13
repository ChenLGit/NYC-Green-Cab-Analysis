#################### Trip Distance vs Tip Amount/Tip Percentage/Tip per Mile ####################

# Tip Amount vs Trip_Distance
TipAmount <- ggplot(NYTaxi_Sub, aes(x = NYTaxi_Sub$Trip_distance, y = NYTaxi_Sub$TipAmount)) + 
  geom_jitter(size = 0.1, aes(color = Rush_Overnight)) + 
  ylab("Tip Amount") +
  xlab("Trip Distance") +
  scale_colour_gradient(name = "Extra Charge", low = "#9BE0F9", high = "blue") +
  theme(legend.position=c(0.7,0.85)) +
  stat_smooth(method = "lm",  col = "red") 

# Tip Percentage vs Trip Distance
TipPercentage <- ggplot(NYTaxi_Sub, aes(x = NYTaxi_Sub$Trip_distance, y = NYTaxi_Sub$TipPercentage)) + 
  geom_jitter(size = 1, aes(color = Rush_Overnight)) +
  ylab("Tip Percentage") +
  xlab("Trip Distance") +
  scale_colour_gradient(name = "Extra Charge", low = "#9BE0F9", high = "blue") +
  theme(legend.position=c(0.7,0.85)) +
  stat_smooth(method = "lm",  col = "red", formula = y ~ poly(x, 2)) 

# Fare per Mile vs Trip Distance
FarePerMile <- ggplot(NYTaxi_Sub, aes(x = NYTaxi_Sub$Trip_distance, y = NYTaxi_Sub$FareperMile)) + 
  geom_point(size = 1, aes(color = Rush_Overnight)) +
  coord_cartesian(ylim = c(3, 10)) +
  ylab("Fare per Mile") +
  xlab("Trip Distance") +
  scale_colour_gradient(name = "Extra Charge", low = "#9BE0F9", high = "blue") +
  theme(legend.position=c(0.7,0.85)) +
  stat_smooth(method = "lm",  col = "red", formula = y ~ poly(x, 2)) 

# Grid Draw
grid.arrange(TipAmount , TipPercentage, FarePerMile, ncol=3,
             heights=c(10, 1),widths =c(2,2,2) ,as.table =TRUE)
