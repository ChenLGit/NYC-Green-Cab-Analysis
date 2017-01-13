################### Check Data Status ################### 
# Open a plot object
par( mar=c(8,5,5,5), mfrow=c(1,1))

# melt data
meltOriginalData <- melt(NYTaxi)
# boxplot data to check variation
boxplot(data=meltOriginalData, value~variable, las=2, cex.axis = 0.7, ylab="Data Range")

# melt data
meltSubsetData <- melt(NYTaxi_Sub_Stg1)
# boxplot data to check variation
boxplot(data=meltSubsetData, value~variable, las=2, cex.axis = 0.7, ylim = c(-100,150), ylab="Data Range")

dev.off()

