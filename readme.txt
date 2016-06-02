PowerMarketTools

PowerMarketTools contains convenience functions for electricity market analysis and modelling. 


1. CountStarts(InputProduction)

CountStarts takes a vector of plant production and counts the number of times it has started during that period. 


2. CalculateCapPrice(PriceSeries,LoadSeries,TimeSeries)

CalculateCapPrice takes price vector, load factor vector, and time series vector. All 3 vectors must be of equal length. 

PriceSeries is simply the electricity price series over time. LoadSeries is the load factor (or production) of the plant over time. 

The time series is a "grouping" vector. For example, the year labels of a halfhourly time series should look like [2015,2015,2015,...,2016,2016,...] such that 2015 was repeated 17520 times, the number of halfhours in that year. Basically, it labels the halfhourly series with the relevant information. This is a "year" grouping of halfhourly time series. 

The captured price is defined by the load-weighted average of electricity price, grouped by the time groupings specified. 

Function returns the captured price, of a length equal to the number of unique time labels in TimeSeries. 


3. MarketClearance(Bids,Quantities,Target,decreasing=FALSE)

MarketClearance takes a series of bids, and quantities offered, and a target amount the market requires. decreasing=FALSE specifies that we should take the lowest bids first (as in purchasing from sellers), whereas the converse would take the highest bids first (as in selling to buyers). 

Function returns a list containing $results and $order, the quantities taken from the participants and the order of participants' bids. 


4. GenerateNetFuelcost(Fuelcost,CO2cost,Transport,CO2int,clean=TRUE)

GenerateNetFuelcost takes as inputs vectors Fuelcost, CO2cost, and scalars Transport, CO2int. 

Calculates the clean or dirty fuel cost depending on clean=TRUE or clean=FALSE. 


5. CalculateCapSpread(PriceSeries,Fuelcost,CO2cost,CO2int,RepresentativeEff,clean=TRUE)

CalculateCapSpread basically does the same thing as CalculateCapPrice, except for the fuel spreads captured by the plant in question. 


6. YearSeriesToLeap(YearSeries)

YearSeriesToLeap takes a series of data within one year (e.g. 17520 halfhours in a year, or 8760 hours in a year) and modifies it for leap years by duplicating the data in 28 Feb and inserting into 29 Feb. This works for any granularity from daily and below, i.e. halfhourly, hourly, minutes, daily, etc. 