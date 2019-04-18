# Optimizer module

#####################################################################################
# Main function
#####################################################################################
# 
# Optimize lighting function
# Will output a matrix of 63 * 4 with the values of the 4 control parameters that minimize cost.
#
optimizeLighting <- function(input){
  
  timeVector = 1:nrow(input) # time in weeks.
  
  #Optimal solution for auto curtains
  optimalSolutionForAutoCurtains = matrix(0L, nrow = length(timeVector), ncol = 5)
  for(t in timeVector){
    #Find min for each hour in the week.
    minCostForHour_t = 9999999999
    for(j in 1:3){
      electricityConsumedLed = c(0, 0.3, 0.5)
      intensityLed = c(0, 0.5, 1)
      B = electricityConsumedLed[j]
      I = intensityLed[j]
      for(C in c(1, 0.7, 0.4)){
        
        forecasted_lux = input[t, "forecasted_lux"]
        costForHour_t = calculateCostForHour(forecasted_lux, I, B, C, 1)
        
        if(costForHour_t < minCostForHour_t){
          minCostForHour_t = costForHour_t
          optimalSolutionForAutoCurtains[t,]= c(I, B, C, 1, minCostForHour_t)
        }
      }
    }
  }
  
  #optimalsolutionfor manual curtains
  optimalSolutionForManualCurtains = matrix(0L, nrow = length(timeVector), ncol = 5)
  for(C in c(1, 0.7, 0.4)){
    #Set one curtain value and find min for the whole week.
    for(t in timeVector){
      minCostForHour_t = 9999999999
      for(j in 1:3){
        electricityConsumedLed = c(0, 0.3, 0.5)
        intensityLed = c(0, 0.5, 1)
        B = electricityConsumedLed[j]
        I = intensityLed[j]
        
        forecasted_lux = input[t, "forecasted_lux"]
        costForHour_t = calculateCostForHour(forecasted_lux, I, B, C, 0)
        
        if(costForHour_t < minCostForHour_t){
          minCostForHour_t = costForHour_t
          optimalSolutionForManualCurtains[t,]= c(I, B, C, 0, minCostForHour_t)
        }
        
      }
    }
  }
  
  
  #Compare between auto curtains and manual curtains weekly cost and return cheapest option
  weekCostAuto = sum(optimalSolutionForAutoCurtains[,5])
  weekCostManual = sum(optimalSolutionForManualCurtains[,5])
  if( weekCostAuto < weekCostManual ){
    return (optimalSolutionForAutoCurtains)
  }
  else{
    return (optimalSolutionForManualCurtains)
  }
}

#####################################################################################
# Helper Functions
#####################################################################################

#
# Cost function
# L = 450*I + Forecasted_Lux*C
# CostLackComfort(L) = abs(L-450) if x < 450, 0 if x in [450, 650], abs(L-650) if x > 650
# Cost_Function = costLackComfort(L) + B*0.22 + {0|1}*0.022
#
calculateCostForHour <- function(forecasted_lux, I, B, C, autoCurtains){
  L = 450*I + forecasted_lux*C
  totalCostForHour = costLackOfComfort(L) + B*0.22 + autoCurtains*0.022
  return (totalCostForHour)
}


#
#Cost of lack comfort
#
costLackOfComfort <- function(L){
  if(L < 450){
    return (450-L)
  }
  if(L > 650){
    return (L-650)
  }
  if(450 <= L && L <= 650){
    return (0)
  }
}