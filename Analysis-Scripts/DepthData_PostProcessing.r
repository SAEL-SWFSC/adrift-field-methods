# Processing Depth Data

# Load Packages
library(tidyverse)
library(chron)
library(lubridate)

# OVERVIEW
# 1. Read in raw depth data
# 2. Add column names and additional depth/time columns
# 3. Fill in columns to calculate depth and date/time
# 4. Remove unnecessary columns and export final data

# ADRIFT Sensus Depth Data
## 1. Read in raw depth data

# Set global variable for drift ID
ID <<- 'ADRIFT_067'

#set temporary working directory
setwd(paste('Z:\\METADATA\\ADRIFT\\',ID,'\\',ID,'_Depth_Sensus', sep = ""))

#Rename raw data
file.rename(paste(ID,'_Depth_Sensus.csv', sep = ""), paste(ID,'_Depth_Sensus_Raw.csv', sep = ""))

#Read in data
rawDepth <- read.csv(paste(ID,'_Depth_Sensus_Raw.csv', sep = ""), header = FALSE)


## 2. Add column names and additional depth/time columns
#Add other columns
rawDepth <- rawDepth %>%
  add_column(Depth_m = NA, Temp_C = NA, Start_Time_UTC = NA, Start_Date_UTC = NA, DateTime_UTC = NA)

#Add column names
colnames(rawDepth) <- c('Leg','Sensus ID','','Year','Month','Day','StartHr UTC','StartMin UTC','StartSec UTC','Elapsed Sec','mBar','degK','Depth_m','Temp_C','Start_Time_UTC','Start_Date_UTC','DateTime_UTC')


## 3. Fill in columns to calculate depth and date/time
# Set up progress bar
pb <- txtProgressBar(min = 0,      
                     max = nrow(rawDepth), 
                     style = 3,    
                     width = 50,   
                     char = "=")   

# Calculate depth using pressure (mBar)
for (i in 1:nrow(rawDepth)) {
 rawDepth$Depth_m[i] <- (rawDepth$mBar[i]-992)/103

 # Convert temperature from Kelvin to Celsius
 rawDepth$Temp_C[i] <- rawDepth$degK[i]-273.15

 # Add start date column
 rawDepth$Start_Date_UTC[i] <- paste(rawDepth$Year[i],"-",rawDepth$Month[i],"-",rawDepth$Day[i], sep = "")

 # Add start time column
 rawDepth$Start_Time_UTC[i] <- paste(rawDepth$`StartHr UTC`[i],":",rawDepth$`StartMin UTC`[i],":",rawDepth$`StartSec UTC`[i], sep = "")

 # Add date/time column
 rawDepth$DateTime_UTC[i] <- paste(rawDepth$Start_Date_UTC[i]," ",rawDepth$Start_Time_UTC[i])

 # Set date/time as a POSIXct object
 rawDepth$DateTime_UTC <- as.POSIXct(rawDepth$DateTime_UTC, format = "%Y-%m-%d %H:%M:%S")

 # Add elapsed seconds to start date/time
 rawDepth$DateTime_UTC[i] <- rawDepth$DateTime_UTC[1] + rawDepth$`Elapsed Sec`[i]

 # Sets the progress bar to the current state
 setTxtProgressBar(pb, i)
}

close(pb) # Close the connection

# Correct time zone as needed (7 or 8 hours)
# for (i in 1:nrow(rawDepth)) {
#  rawDepth$DateTime_UTC[i] <- rawDepth$DateTime_UTC[i]+(7*3600)
# }


## 4. Remove unnecessary columns and export final data
finalDepth <- subset(rawDepth, select = c("Sensus ID","DateTime_UTC","mBar","degK","Depth_m","Temp_C"))
write.csv(finalDepth, paste('Z:\\METADATA\\ADRIFT\\',ID,'\\',ID,'_Depth_Sensus\\',ID,'_Depth_Sensus.csv', sep = ""), row.names=FALSE)


# CCES/PASCAL OpenTag Depth Data

## 1. Read in raw depth data
# Set global variable for drift ID
ID <<- 'PASCAL_002'

#set temporary working directory
setwd(paste('Z:/METADATA/PASCAL/',ID,'/',ID,'_Depth_OpenTag', sep = ""))

#Rename raw data
#file.rename(paste(ID,'_OT_Depth.csv', sep = ""), paste(ID,'_OT_Depth_Raw.csv', sep = ""))

#Read in raw data
rawDepth <- read.csv(paste(ID,'_OT_Depth_Raw.csv', sep = ""), header = FALSE)


## 2. Add column names and additional depth/time columns
#Add other columns
rawDepth <- rawDepth %>%
  add_column(Depth_m = NA, DateTime_UTC = NA)

#Add column names
colnames(rawDepth) <- c('Num','FileName','FileDate','FileTime','Sample','TimeFromStart_s','Pressure','Temperature_c','Depth_m', 'DateTime_UTC')


## 3. Fill in columns to calculate depth and date
# Set up progress bar
pb <- txtProgressBar(min = 0,      
                     max = nrow(rawDepth), 
                     style = 3,    
                     width = 50,   
                     char = "=")   

# Calculate depth using pressure (mBar)
for (i in 1:nrow(rawDepth)) {
  rawDepth$Depth_m[i] <- (rawDepth$Pressure[i] - 1016)/100.5 
  
  # Sets the progress bar to the current state
  setTxtProgressBar(pb, i)
}

close(pb) # Close the connection


# Convert date and time into datetime object
  dates <- rawDepth$FileDate
  times <- rawDepth$FileTime
  x <- paste(dates, times)
  
# Set date/time as a POSIXct object
  rawDepth$DateTime_UTC <- as.POSIXct(x, format = "%m/%d/%Y %H:%M:%S")
  
# # Correct time zone as needed (-7 hours)
# for (i in 1:nrow(rawDepth)) {
#   rawDepth$DateTime_UTC[i] <- rawDepth$DateTime_UTC[i]-(7*3600)
# 
#   # Sets the progress bar to the current state
#   setTxtProgressBar(pb, i)  
# }
# 
# close(pb) # Close the connection  


## 4. Remove unnecessary columns and export final data
finalDepth <- subset(rawDepth, select = c("FileName","DateTime_UTC","Pressure","Depth_m","Temperature"))

write.csv(finalDepth, paste('Z:/METADATA/PASCAL/',ID,'/',ID,'_Depth_OpenTag/',ID,'_OT_Depth.csv', sep = ""), row.names=FALSE)


# CCES LOTEK Depth Data

## 1. Read in raw depth data

# Set global variable for drift ID
ID <<- 'CCES_014'

#set temporary working directory
setwd(paste('Z:/METADATA/CCES_2018/',ID,'/',ID,'_DEPTH/', sep = ""))

#Rename raw data
#file.rename(paste(ID,'_LAT_Depth.csv', sep = ""), paste(ID,'_LAT_Depth_Raw.csv', sep = ""))

#Read in raw data
rawDepth <- read.csv(paste(ID,'_LAT_Depth_Raw.csv', sep = ""), header = FALSE)

# WHEN WORKING WITH LOTEK DATA YOU MUST REMOVE THE FIRST THREE ROWS OF THE RAW DATA
rawDepth <- rawDepth[-c(1,2,3), ]


## 2. Add column names and additional depth/time columns
#Add other columns
rawDepth <- rawDepth %>%
  add_column(Depth_m = NA, DateTime_UTC = NA)

#Add column names
colnames(rawDepth) <- c('Rec','Date','Time','Pressure','IntTemp','Depth_m', 'DateTime_UTC')


## 3. Fill in columns to calculate depth and date
# Set up progress bar
pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = nrow(rawDepth), # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")   # Character used to create the bar

# Convert pressure from dBar to mBar for depth calculation
rawDepth$Pressure <- as.numeric(rawDepth$Pressure)*100

for (i in 1:nrow(rawDepth)) {
  rawDepth$Depth_m[i] <- (rawDepth$Pressure[i] - 1016)/100.5 
  
  # Sets the progress bar to the current state
  setTxtProgressBar(pb, i)
}

close(pb) # Close the connection

# Convert date and time into datetime object
  dates <- rawDepth$Date
  times <- rawDepth$Time
  x <- paste(dates, times)

# Set date/time as a POSIXct object  
  rawDepth$DateTime_UTC <- as.POSIXct(x, format = "%m/%d/%Y %H:%M:%S")
  
# # Correct time zone as needed (-7 hours)
# for (i in 1:nrow(rawDepth)) {
#   rawDepth$DateTime_UTC[i] <- rawDepth$DateTime_UTC[i]-(7*3600)
# 
#   # Sets the progress bar to the current state
#   setTxtProgressBar(pb, i)  
# }
# 
# close(pb) # Close the connection 


## 4. Remove unnecessary columns and export final data
finalDepth <- subset(rawDepth, select = c("Rec","DateTime_UTC","Pressure","Depth_m","IntTemp"))

write.csv(finalDepth, paste('Z:/METADATA/CCES_2018/',ID,'/',ID,'_DEPTH/',ID,'_LAT_Depth.csv', sep = ""), row.names=FALSE)
