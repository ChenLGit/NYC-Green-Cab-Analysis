######################### Pickup Time vs Revenue #########################

# Creat Hourly Daily Revenue Table 
# Hourly Revenue
PickupDailyHourlyReveue = sqldf('select PickupDateHour, PickupDate, avg(FareAmount) as PickupDHRev
                                from NYTaxi_Sub 
                                group by 1,2')

# Hourly Daily Revenue
PickupDailyHourlyRev <- ggplot(PickupDailyHourlyReveue, aes(x=PickupDateHour, y=PickupDHRev, fill = PickupDate)) + geom_bar(stat="identity") + 
  scale_fill_manual(values = cbPalette)+
  #xlab("Pickup Date") +
  ylab("Hourly Revenue")+
  coord_cartesian(ylim = c(17, 42)) +
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

# Creat Daily Revenue Table
PickupDailyReveue = sqldf('select PickupDate, avg(FareAmount) as PickupDRev
                          from NYTaxi_Sub 
                          group by 1')

# Daily Revenue
PickupDailyRev <- ggplot(PickupDailyReveue, aes(x=PickupDate, y=PickupDRev, fill = PickupDate))+ geom_bar(stat="identity") +
  scale_fill_manual(values = cbPalette)+
  xlab("Pickup Date") +
  ylab("Daily Revenue")+
  coord_cartesian(ylim = c(20, 25.5)) +
  theme(panel.background = element_rect(fill = "grey92", colour = "white"),
        panel.grid.minor = element_line(colour = "white", linetype = "solid"),
        panel.grid.major = element_line(colour = "white", linetype = "solid"),
        axis.ticks = element_line(colour = "white"),
        #axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 8, angle = 0, hjust = 0.5),
        text = element_text(size=10),
        legend.position='none', 
        legend.direction = "horizontal",
        legend.title = element_text(size = 5),
        legend.text = element_text(size = 5, colour = "black", angle = 0))

# Creat Hourly Revenue Table 
PickupHourlyRevenue = sqldf('select PickupDate, PickupHour, avg(FareAmount) as PickupHRev
                            from NYTaxi_Sub 
                            group by 1,2')
# Hourly Revenue
PickupHourlyRev <- ggplot (PickupHourlyRevenue, aes(x=PickupHour, y=PickupHRev, colour= PickupDate, alpha = 0.8))+ 
  geom_line(aes(colour = PickupDate, linetype = PickupDate, size = PickupDate)) + 
  geom_point() +
  stat_smooth(method = "lm",  formula = y ~ poly(x, 7), col = "red", level = 0.9995)+
  scale_linetype_manual(values=c("dotdash", "dotdash", "dotdash", "dotdash", "dotdash", "solid","solid",
                                 "dotdash", "dotdash", "dotdash", "dotdash", "dotdash", "solid","solid")) +
  scale_size_manual(values=c(1, 1, 1, 1, 1, 2, 2,
                             1, 1, 1, 1, 1, 2, 2)) +
  scale_colour_manual(values=cbPalette) +
  xlab("Pickup Hour") +
  ylab("Hourly Revenue")+
  theme(panel.background = element_rect(fill = "grey92", colour = "white"),
        panel.grid.minor = element_line(colour = "white", linetype = "solid"),
        panel.grid.major = element_line(colour = "white", linetype = "solid"),
        axis.ticks = element_line(colour = "white"),
        #axis.line = element_line(colour = "black"),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        #axis.text = element_text(size = 8, angle = 0, hjust = 0),
        text = element_text(size=10),
        legend.position='none', 
        legend.direction = "vertical",
        legend.title = element_text(size = 5),
        legend.text = element_text(size = 5, colour = "black", angle = 0))

# Creat Revenue per Mile Table
PickupMileRevenue = sqldf('select PickupDate, PickupHour, avg(FareAmount)/avg(Trip_distance) as FareperMile
                            from NYTaxi_Sub 
                            group by 1,2')

# Revenue per Mile
PickupMileRev <- ggplot (PickupMileRevenue, aes(x=PickupHour, y=FareperMile, colour= PickupDate, alpha = 0.8 ))+ 
  geom_line(aes(colour = PickupDate, linetype = PickupDate, size = PickupDate)) + 
  geom_point() +
  stat_smooth(method = "lm",  formula = y ~ poly(x, 7), col = "red", level = 0.9995)+
  scale_linetype_manual(values=c("dotdash", "dotdash", "dotdash", "dotdash", "dotdash", "solid","solid",
                                 "dotdash", "dotdash", "dotdash", "dotdash", "dotdash", "solid","solid")) +
  scale_size_manual(values=c(1, 1, 1, 1, 1, 2, 2,
                             1, 1, 1, 1, 1, 2, 2)) +
  scale_colour_manual(values=cbPalette) +
  xlab("Pickup Hour") +
  ylab("Fare Amount per Mile")+
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

# Grid Draw
grid.arrange(PickupDailyHourlyRev , PickupDailyRev, PickupHourlyRev, PickupMileRev,nrow = 4,
             heights=c(7.5,10, 13.5, 24),widths =c(10) ,as.table =TRUE)
