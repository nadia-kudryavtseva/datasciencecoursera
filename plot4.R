temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", temp)
data <- read.csv(unz(temp, "household_power_consumption.txt"), col.names=c("Date", "Time", "Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_3", "Sub_metering_3"), header=FALSE, skip=66637, nrows=2879, sep=";", na.strings="?")
unlink(temp)
png(file="plot4.png", width=480, height=480)
data$newtime <- paste(data$Date, data$Time)
par(mfcol=c(2,2))
with(data, {
   plot(strptime(newtime, format="%d/%m/%Y %H:%M:%S"), Global_active_power, ylab="Global Active Power (kilowatts)", xlab="", type="l")
   plot(strptime(newtime, format="%d/%m/%Y %H:%M:%S"), Sub_metering_1, ylab="Energy submetering", xlab="", type="l")
   lines(strptime(newtime, format="%d/%m/%Y %H:%M:%S"), Sub_metering_3, ylab="Energy submetering", xlab="", type="l", col="red")
   lines(strptime(newtime, format="%d/%m/%Y %H:%M:%S"), Sub_metering_3.1, ylab="Energy submetering", xlab="", type="l", col="blue")
   legend('topright', c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), bty="n", col=c("black", "red", "blue"), lty=1)
   plot(strptime(newtime, format="%d/%m/%Y %H:%M:%S"), Voltage, type='l', xlab="datetime")
   plot(strptime(newtime, format="%d/%m/%Y %H:%M:%S"), Global_reactive_power, type='l', xlab="datetime")
   })
dev.off()