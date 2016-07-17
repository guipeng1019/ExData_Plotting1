plot4 <- function(con= "./household_power_consumption.txt"){
  
  ## Check if hpc is already generated,to aviod repeated reading. 
  lg <- any("hpc" %in% ls(pos.to.env(1)))
  if(lg==FALSE) 
  {                                                
    ## Process rawdata
    print("The loading process will takes sometime, thank you for your patience!")
    print("A dataframe named < hpc > will be generated in global environment for plotting and checking,") 
    print("in which a POSIXct varialbe is added in 1st col.")
    print("Once the data is loaded other plot functions will call it from cache.")
    
    dat1 <- read.table(con, colClasses = "character", sep=";", header=TRUE)
    dat2 <- subset(dat1, dat1[[1]]=="1/2/2007"|dat1[[1]]=="2/2/2007")         ### Subset by dates
    fulltime0 <- paste(dat2[[1]], dat2[[2]], sep=" ")                         ### Set a string of date+time
    fulltime <- strptime(fulltime0, "%d/%m/%Y %H:%M:%S")                      ### Convert it into POSIXct
    dat3 <- cbind(fulltime, dat2)                                             ### Then put it into data
    for (i in 4:10) {dat3[[i]] <- as.numeric(dat3[[i]]) }                     ### Set some variables as numeric
    hpc <<- dat3
  }
  
  
  ## Initialize device  
  png()
  png(file="plot4.png", width=480, height=480)   
  par("mfrow"=c(2,2), mar=c(4,4,2,2), oma=c(0,0,1,0) )
  
  ## Plot 1st pic(upper left)
  with(hpc, plot(fulltime, Global_active_power,  ### Set a blank pic
                  ylab="Global Active Power",
                  xlab = NA,
                  type="n"))
  with(hpc, lines(fulltime, Global_active_power,lwd=1.5)) ### Add the line
  
  
  ## Plot 2nd pic(upper right)
  with(hpc, plot(fulltime, Voltage,  ### Set a blank pic
                  ylab="Voltage",
                  xlab = "datetime",
                  type="n"))
  with(hpc, lines(fulltime,Voltage,lwd=1.5)) ### Add the line
  
  
  ## Plot 3rd pic(lower left)
  with(hpc, plot(fulltime,Sub_metering_1,       ### Set a blank pic
                  ylab="Energy sub metering",
                  xlab = NA,
                  type="n"))
  ### Add 3 lines
  with(hpc, lines(fulltime,Sub_metering_1,lwd=1.5, col="black"))
  with(hpc, lines(fulltime,Sub_metering_2,lwd=1.5, col="red"))
  with(hpc, lines(fulltime,Sub_metering_3,lwd=1.5, col="blue"))
  
  ### Add legend      
  legend("topright", pch = "-", cex= 0.8,
         legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
         col = c("black", "red", "blue"))
  
  
  ## Plot last pic(lower right)
  with(hpc, plot(fulltime, Global_reactive_power,  ### Set a blank pic
                  
                  xlab = "datetime",
                  type="n"))
  with(hpc, lines(fulltime,Global_reactive_power,lwd=1.5)) ### Add the line
  
  
  dev.off() ## Close device
  print("File < plot4.png > has been saved in your working directory ")
}
  
  
  
  