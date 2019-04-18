library(jsonlite)

#Setting Working directory for files
setwd("C:\Users\PC\Documents\BABD - Projects, Hands On\IOT\ligthing-optimization-r")

#Reading files and cleansing
#JSON File with forecast
forecast <- fromJSON("forecast.json")
forecast <- forecast$list
forecast <- cbind(forecast,do.call("rbind", forecast$weather))

#Lighting information and removal of duplicates
light_z1 <- read.csv(file="babd_light_z1.csv", header=TRUE, sep="\t")
light_z2 <- read.csv(file="babd_light_z2.csv", header=TRUE, sep="\t")
light_z3 <- read.csv(file="babd_light_z3.csv", header=TRUE, sep="\t")
light_z3A <- read.csv(file="babd_light_z3A.csv", header=TRUE, sep="\t")
light_z1 <- light_z1[!duplicated(light_z1$timestamp),]
light_z2 <- light_z2[!duplicated(light_z2$timestamp),]
light_z3 <- light_z3[!duplicated(light_z3$timestamp),]
light_z3A <- light_z3A[!duplicated(light_z3A$timestamp),]

#Movement information and removal of duplicates
movement_z1 <- read.csv(file="babd_movement_z1.csv", header=TRUE, sep="\t")
movement_z2 <- read.csv(file="babd_movement_z2.csv", header=TRUE, sep="\t")
movement_z3 <- read.csv(file="babd_movement_z3.csv", header=TRUE, sep="\t")
movement_z3A <- read.csv(file="babd_movement_z3A.csv", header=TRUE, sep="\t")
movement_z1 <- movement_z1[!duplicated(movement_z1$timestamp),]
movement_z2 <- movement_z2[!duplicated(movement_z2$timestamp),]
movement_z3 <- movement_z3[!duplicated(movement_z3$timestamp),]
movement_z3A <- movement_z3A[!duplicated(movement_z3A$timestamp),]

#Select only columns of interest from each dataset and meger into one dataframe since
light_z1 <- light_z1[, c("timestamp","zone", "value")]
light_z2 <- light_z2[, c("timestamp","zone", "value")]
light_z3 <- light_z3[, c("timestamp","zone", "value")]
light_z3A <- light_z3A[, c("timestamp","zone", "value")]
light <- rbind(light_z1, light_z2, light_z3, light_z3A)

movement_z1 <- movement_z1[, c("timestamp","zone", "n_passages")]
movement_z2 <- movement_z2[, c("timestamp","zone", "n_passages")]
movement_z3 <- movement_z3[, c("timestamp","zone", "n_passages")]
movement_z3A <- movement_z3A[, c("timestamp","zone", "n_passages")]
movement <- rbind(movement_z1, movement_z2, movement_z3, movement_z3A)

forecast <- forecast[, c("dt_txt","clouds")]
names(forecast)[1] <- "timestamp"
names(forecast)[2] <- "clouds"

#Obtain Hours and date to group values from light and movement
light$Date <- as.Date(light$timestamp)
light$Time <- format(as.POSIXct(light$timestamp) ,format = "%H:%M:%S")
light$Hour <- format(as.POSIXct(light$Time, format = "%H:%M:%S"),"%H")
light$Day <- weekdays(light$Date)

30$Date <- as.Date(movement$timestamp)
movement$Time <- format(as.POSIXct(movement$timestamp) ,format = "%H:%M:%S")
movement$Hour <- format(as.POSIXct(movement$Time, format = "%H:%M:%S"),"%H")
movement$Day <- weekdays(movement$Date)

forecast$Date <- as.Date(forecast$timestamp)
forecast$Time <- format(as.POSIXct(forecast$timestamp) ,format = "%H:%M:%S")
forecast$Hour <- format(as.POSIXct(forecast$Time, format = "%H:%M:%S"),"%H")
forecast$Day <- weekdays(forecast$Date)

#Aggregate light information by hour using the mean
agg_light = aggregate(light$value,
                by = list(light$Hour),
                FUN = mean)

barplot(agg_light$x, main="Light", xlab="Hour",  
        ylab="Total", names.arg=agg_light$Group.1, 
        border="blue")

#Aggregate movement information by hour using the mean
agg_movement = aggregate(movement$n_passages,
                      by = list(movement$Hour),
                      FUN = mean)

barplot(agg_movement$x, main="Light", xlab="Hour",  
        ylab="Total", names.arg=agg_movement$Group.1, 
        border="blue")


#############################################################
# Forecasted_lux per hour
#############################################################

#TODO: Codigo ruben

##############################################################
# Optimization Code
##############################################################

# Optimize lighting function
# Will output a matrix of 63 * 4 with the values of the 4 control parameters that minimize cost.
#
optimizeLighting <- function(input){
  
  input <- data.frame("forecasted_lux" = 1:(13*5), "hour" = 1:(13*5) , "day" = 1:(13*5))
  timeVector = 1:nrow(input) # time in weeks.
  minTotalCost = 999999
  
  optimalSolutionForAutoCurtains = matrix(0L, nrow = length(timeVector), ncol = 4)
  for(t in timeVector){
    minCostForHour_t = 9999999999
    
    for(I in c(0, 0.5, 1)){
      for(B in c(0, 0.3, 0.5)){
        for(C in c(1, 0.7, 0.4)){
          costForHour_t = calculateCostForHour(t, I, B, C, 1)
          if(costForHour_t < minCostForHour_t){
            minCostForHour_t = costForHour_t
            optimalSolutionForAutoCurtains[t,]= c(I, B, C, 1)
          }
        }
      }
    }
  }
  
  return (optimalSolutionForAutoCurtains)
}

# Cost function
# L = 450*I + Forecasted_Lux*C
# CostLackComfort(L) = abs(L-450) if x < 450, 0 if x in [450, 650], abs(L-650) if x > 650
# Cost_Function = costLackComfort(L) + B*0.22 + {0|1}*0.022
calculateCostForHour <- function(t, I, B, C, autoCurtains){
  forecasted_lux = input[t, "forecasted_lux"]
  L = 450*I + forecasted_lux*C
  totalCostForHour = costLackOfComfort(L) + B*0.22 + autoCurtains*0.022
  return (totalCostForHour)
}

costLackOfComfort <- function(L){
  if(L < 450){
    return (450-L)
  }
  if(L > 650){
    return (650-L)
  }
  if(450 <= L && L <= 650){
    return (0)
  }
}

# Same for x = 0.








