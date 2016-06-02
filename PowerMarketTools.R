### Electricity Market Analysis And Modelling Functions. 
## Anthony Chuah, 2016-05-18

# Requires package: lubridate. 
# If you don't have them, the next few lines auto-installs for you. 
ListOfPackages <- c("lubridate")
ToInstall <- ListOfPackages[!(ListOfPackages %in% installed.packages()[,"Package"])]
if (length(ToInstall)) {
  print("You're missing required packages:")
  print(ToInstall)
  print("Installing them now.")
  install.packages(ToInstall)
}

# CountStarts counts the number of times a plant has started by looking at its production time series vector. 
CountStarts <- function(InputProduction) {
  NumPeriods <- length(InputProduction)
  
  # Initialize the vector counting the number of starts. It should show 1 if a period was a start, else show 0. 
  StartsVec <- rep(0,NumPeriods)
  for (i in 2:NumPeriods) {
    if (InputProduction[i] > 0 & InputProduction[i-1] == 0)
    StartsVec[i] <- 1
  }
  return(sum(StartsVec))
}

# Calculates the captured price of a power plant based on prices, plant load factors, and 
# time summary grouping (e.g. I want to summarise halfhourly prices into yearly prices). 
# For example, hour 1 groups halfhours 1 and 2, hour 2 groups halfhours 3 and 4, etc. 
# So halfhourly-hours will look like [1,1,2,2,3,3,...]. Halfhourly-years will have [2015,2015,...] 
# repeated 17520 times before moving on to 2016. 
CalculateCapPrice <- function(PriceSeries,LoadSeries,TimeSeries) {
  # Throw error if all 3 inputs did not have the same length. 
  if (!(length(PriceSeries) == length(LoadSeries) & length(LoadSeries) == length(TimeSeries))) {
    stop("All 3 inputs into the CalculateCapprice function must be of the same length.")
  }
  # Find time-grouped sum load factors. 
  TimeSumLF <- tapply(LoadSeries,TimeSeries,sum)
  
  # Find the time-grouped load-weighted load factors. 
  UniqueTime <- unique(TimeSeries)
  
  # Initialize an empty vector containing time-grouped load-weighted fraction-of-total load factors. 
  TimeLoadweightLF <- rep(0,length(TimeSeries))
  # Set a counter so that we do not have to repeatedly compare itime to UniqueTime to get the index of TimeSumLF relevant for each iteration. 
  # Very minimal improvement in solve time. 
  init_counter <- 1
  for (itime in UniqueTime) {
    TimeLoadweightLF[TimeSeries == itime] <- LoadSeries[TimeSeries == itime] / TimeSumLF[init_counter]
    init_counter <- init_counter + 1
  }
  
  # Multiply the time-grouped load-weighted fraction-of-total load factors. 
  TimeCapPrice <- TimeLoadweightLF * PriceSeries
  
  # Sum the TimeCapPrice by the time groups in order to get captured price. 
  CapPrice <- tapply(TimeCapPrice,TimeSeries,sum)
  # Return the result to the calling environment. 
  return(CapPrice)
}

# When simulating a market, how to take the best offers to fulfill demand. 
# For example: I need 2 GW of power, and I have several plants willing to provide their capacity at given prices. 
# I take all the cheapest possible plants to fulfill my 2 GW of power. 
# Function returns the quantities taken, and the order of the price-sorting. 
# Decreasing = FALSE means you take the lowest prices first. 
MarketClearance <- function(Bids,Quantities,Target,decreasing=FALSE) {
  # Bids should be a vector containing each player's bids. 
  # Quantities should be a vector containing each player's quantity offered/demanded. 
  # The ORDER of players should be identical in both vectors. 
  # Target is the total quantity the market needs to clear. 
  if (!decreasing) {
    # Sort bids so that the lowest bids are indexed first. 
    Sorting <- sort(Bids,decreasing=FALSE,index.return=TRUE)
  } else {
    # Sort bids so that the highest bids are indexed first. 
    Sorting <- sort(Bids,decreasing=TRUE,index.return=TRUE)
  }
  NumPlayers <- length(Bids)
  
  SortedBids <- Sorting$x
  SortedOrder <- Sorting$ix
  QtySortedByBids <- Quantities[SortedOrder]
  
  # Pro tip: to retrieve the unsorted array, do SortedArray[SortedOrder] <- SortedArray. 
  TakenQty <- 0
  TakenQuantities <- rep(0,NumPlayers)
  for (j in 1:NumPlayers) {
    # Go in order of the sorting, since that specifies first the most desirable buyers or sellers. 
    TakenQty <- TakenQty + QtySortedByBids[j]
    if (TakenQty > Target) {
      # Then we've found all the orders we want to accept.  
      # First account for the overshooting with the latest order taken. 
      Overshooting <- TakenQty - Target
      TakenQuantities[j] <- as.numeric(QtySortedByBids[j]) - Overshooting
      TakenQty <- TakenQty - Overshooting
      break
    } else {
      # Else just take all quantity at that bid. 
      TakenQuantities[j] <- as.numeric(QtySortedByBids[j])
    }
  }
  Output <- list()
  Output$results <- TakenQuantities
  Output$order <- SortedOrder
  return(Output)
}

# Calculates net fuel cost of a plant given fuel price per MWh of fuel energy content, 
# CO2 emissions cost per tonne, transport cost of fuel per MWh, CO2 intensity of fuel. 
# clean=TRUE gives clean fuel cost, clean=FALSE gives fuel cost excluding CO2 cost. 
GenerateNetFuelcost <- function(Fuelcost,CO2cost,Transport,CO2int,clean=TRUE) {
  # Take hourly fuel, co2 cost, scalar transport cost, co2 intensity as input. 
  # Vector vector scalar scalar. 
  # names(HourlyFuelprice) == "gas" gives index of gas column in HourlyFuelprice. 
  DirtyFuelcost <- Fuelcost + Transport
  if (clean) {
    CO2cost <- CO2cost * CO2int
    CleanFuelcost <- DirtyFuelcost + CO2cost
    return(CleanFuelcost)
  } else {
    return(DirtyFuelcost)
  }
}

# Calculate fuel spreads (dark spreads, spark spreads). 
# If you want dirty spreads instead of clean, set CO2int to 0. 
# Fuelcost: GBP/MWhElec, CO2cost: GBP/tonne, CO2int: tonne/MWhFuel
# Dependency: GenerateNetFuelcost function. 
CalculateFuelSpread <- function(PriceSeries,Fuelcost,CO2cost,CO2int,RepresentativeEff,clean=TRUE) {
  # RepresentativeEff should be 0.4913 in UK or Germany for gas, or 0.38 for coal, for example. 
  # First 3 inputs can be vectors of the same length or scalars. 
  if (clean) {
    CleanFuelcost <- GenerateNetFuelcost(Fuelcost,CO2cost,0,CO2int,clean=TRUE)
    CleanSpread <- PriceSeries - (CleanFuelcost / RepresentativeEff)
    return(CleanSpread)
  } else {
    DirtyFuelcost <- GenerateNetFuelcost(Fuelcost,CO2cost,0,CO2int,clean=FALSE)
    DirtySpread <- PriceSeries - (DirtyFuelcost / RepresentativeEff)
    return(DirtySpread)
  }
}

CalculateCapSpread <- function(TimeSeries,PriceSeries,Fuelcost,CO2cost,LoadSeries,Efficiency,CO2int,clean=TRUE) {
  # TimeSeries should be a vector containing the grouping-by-time you want. For example, getting annual captured spreads from 
  # halfhourly data would require a vector showing the year of every halfhour, which means 2015 would be repeated 17520 times, etc. 
  # PriceSeries,Fuelcost,CO2cost,LoadSeries are vectors of the same length as TimeSeries. 
  # Efficiency is treated as a scalar right now, but we could change the code to make it act as a vector in the future. 
  # CO2int is a scalar. clean is TRUE by default which means function will return captured clean spread of plant. 
  # Note: this is always load-weighted not time-weighted. 
  NumPeriods <- length(TimeSeries)
  RawSpread <- CalculateFuelSpread(PriceSeries,Fuelcost,CO2cost,CO2int,Efficiency,clean=clean)
  TimeSumLF <- tapply(LoadSeries,TimeSeries,sum)
  UniqueTime <- unique(TimeSeries)
  TimeLoadweightLF <- rep(0,length(TimeSeries))
  init_counter <- 1
  for (itime in UniqueTime) {
    TimeLoadweightLF[TimeSeries == itime] <- LoadSeries[TimeSeries == itime] / TimeSumLF[init_counter]
    init_counter <- init_counter + 1
  }
  TimeCapSpread <- TimeLoadweightLF * RawSpread
  CapSpread <- tapply(TimeCapSpread,TimeSeries,sum)
  return(CapSpread)
}

YearSeriesToLeap <- function(YearSeries) {
  # Function transforms a year series into a leap year series by inserting 
  # into 29 Feb the exactly 48 halfhours that are in 28 Feb. 
  
  # If modulo 365 does not return 0, then it's probably wrong input. 
  if (length(YearSeries) %% 365 != 0) {
    stop("YearSeries must be of length that is a multiple of 365.")
  }
  # Time unit is halfhourly, hourly, daily, etc. 
  NumPeriods <- length(YearSeries)
  PeriodsPerDay <- NumPeriods / 365
  NumPeriodsLeap <- NumPeriods + PeriodsPerDay
  # In a leap year, the first 31 + 28 days are the same. 
  # Then day number 60 is a repeat of day 59. 
  JanDays <- 31
  FebDays <- 28
  LeapDay <- JanDays + FebDays + 1
  
  # Find the indexes for each relevant stretch. 
  PeriodsUpTo28Feb <- 1:((LeapDay-1)*PeriodsPerDay)
  PeriodsIn28Feb <- ((LeapDay-2)*PeriodsPerDay+1):((LeapDay-1)*PeriodsPerDay)
  PeriodsIn29Feb <- ((LeapDay-1)*PeriodsPerDay+1):(LeapDay*PeriodsPerDay)
  Periods1MarToEndYear <- ((LeapDay-1)*PeriodsPerDay+1):NumPeriods
  Periods1MarToEndLeap <- (LeapDay*PeriodsPerDay+1):NumPeriodsLeap
  
  LeapSeries <- rep(0,17568)
  LeapSeries[PeriodsUpTo28Feb] <- YearSeries[PeriodsUpTo28Feb]
  LeapSeries[PeriodsIn29Feb] <- YearSeries[PeriodsIn28Feb]
  LeapSeries[Periods1MarToEndLeap] <- YearSeries[Periods1MarToEndYear]
  return(LeapSeries)
}