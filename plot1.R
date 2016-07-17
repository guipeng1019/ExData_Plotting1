plot1 <- function(con= "./household_power_consumption.txt"){

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
  png(file="plot1.png", width=480, height=480)   
  par("mfrow"=c(1,1))
  
  ## Plotting
  hist(hpc[["Global_active_power"]],    
       main="Global Active Power", 
       breaks = 20,  
       col ="red", 
       xlab = "Global Active Power (kilowatts)")
  dev.off() ##Close device
  print("File < plot1.png > has been saved in your working directory ")
}
