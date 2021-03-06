temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", temp)
data <- read.csv(unz(temp, "household_power_consumption.txt"), col.names=c("Date", "Time", "Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_3", "Sub_metering_3"), header=FALSE, skip=66637, nrows=2879, sep=";", na.strings="?")
unlink(temp)
png(file="plot1.png", width=480, height=480)
with(data, hist(Global_active_power, main="Global Active Power", col="red", xlab="Global Active Power (kilowatts)"))
dev.off()