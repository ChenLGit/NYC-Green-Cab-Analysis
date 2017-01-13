################### Pickup Trip Frequency based on Hour/Day #####################

# Creat Frequency Table for PickupTime
PickupTimeFreq = sqldf('select PickupDate, PickupHour, count(PickupTime) as PickupFreq
                        from NYTaxi_Sub 
                        group by 1,2')
PickupTimeDailyFreq = sqldf('select PickupDate, count(PickupTime) as PickupDailyFreq
                        from NYTaxi_Sub 
                            group by 1')

# color blind friendly palette:
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", 
               "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Hourly Pickup Frequency 
HourlyPickupbyDay <- ggplot(NYTaxi_Sub, aes(PickupDateHour, fill = PickupDate)) + geom_bar(stat="count" ) + 
  scale_fill_manual(values = cbPalette)+
  #xlab("Pickup Date") +
  ylab("Pickup Frequency")+
  #coord_cartesian(ylim = c(17, 42)) +
  theme(panel.background = element_rect(fill = "grey92", colour = "white"),
        panel.grid.minor = element_line(colour = "white", linetype = "solid"),
        panel.grid.major = element_line(colour = "white", linetype = "solid"),
        axis.ticks = element_line(colour = "white"),
        #axis.line = element_line(colour = "black"),
        #axis.text = element_text(size = 0, angle = 45, hjust = 1),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        text = element_text(size=10),
        legend.position='none', 
        legend.direction = "horizontal",
        legend.title = element_text(size = 5),
        legend.text = element_text(size = 5, colour = "black", angle = 0))

# Daily Pickup Frequency
WeeklyTrend <- ggplot(PickupTimeDailyFreq, aes(x=PickupDate, y=PickupDailyFreq, fill = PickupDate))+ geom_bar(stat="identity") +
  scale_fill_manual(values = cbPalette)+
  xlab("Pickup Date") +
  ylab("Pickup Frequency")+
  coord_cartesian(ylim = c(200, 610)) +
  theme(panel.background = element_rect(fill = "grey92", colour = "white"),
        panel.grid.minor = element_line(colour = "white", linetype = "solid"),
        panel.grid.major = element_line(colour = "white", linetype = "solid"),
        axis.ticks = element_line(colour = "white"),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 8, angle = 0, hjust = 0.5),
        #axis.text.x=element_blank(),
        #axis.title.x=element_blank(),
        text = element_text(size=10),
        legend.position='none', 
        legend.direction = "horizontal",
        legend.title = element_text(size = 5),
        legend.text = element_text(size = 5, colour = "black", angle = 0))

# Hourly Pickup Frequency Trend *********
HourlyPickupTrend <- ggplot (PickupTimeFreq, aes(x=PickupHour, y=PickupFreq, colour= PickupDate, alpha = 0.8))+ 
  geom_line(aes(colour = PickupDate, linetype = PickupDate, size = PickupDate)) + 
  geom_point() +
  stat_smooth(method = "lm",  formula = y ~ poly(x, 7), col = "red", level = 0.9995)+
  scale_linetype_manual(values=c("dotdash", "dotdash", "dotdash", "dotdash", "dotdash", "solid","solid",
                                 "dotdash", "dotdash", "dotdash", "dotdash", "dotdash", "solid","solid")) +
  scale_size_manual(values=c(1, 1, 1, 1, 1, 2, 2,
                             1, 1, 1, 1, 1, 2, 2)) +
  scale_colour_manual(values=cbPalette)+
  xlab("Pickup Hour") +
  ylab("Pickup Frequency")+
  theme(panel.background = element_rect(fill = "grey92", colour = "white"),
        panel.grid.minor = element_line(colour = "white", linetype = "solid"),
        panel.grid.major = element_line(colour = "white", linetype = "solid"),
        axis.ticks = element_line(colour = "white"),
        #axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 8, angle = 0, hjust = 0),
        text = element_text(size=10),
        legend.position='bottom', 
        legend.direction = "horizontal",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10, colour = "black", angle = 0))

# Plot on Grid 
grid.arrange(HourlyPickupbyDay , WeeklyTrend, HourlyPickupTrend, nrow = 3,
             heights=c(5,6.5, 20),widths =c(10) ,as.table =TRUE)
