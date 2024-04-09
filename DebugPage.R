rawFidelityData <- read.csv("C:/Users/SUPERUSER/Desktop/Realized_Gain_Loss_Account_Z19598491.csv", na.strings = c("", "NA"))






## Remove wash sales from datasheet
removeWashSales <- subset(rawFidelityData, !grepl("Wash Sale", rawFidelityData$Date.Sold, ignore.case = TRUE))

## Remove random column formatting.. dollar signs, etc.
cleanedData <- removeWashSales %>%
  mutate(Symbol = gsub("\\(.*?\\)", "", Symbol.CUSIP.))%>%
  mutate(
    Cost.Basis = gsub("\\$", "", Cost.Basis, ),
    Proceeds = gsub("\\$", "", Proceeds),
    Short.Term.Gain.Loss = gsub("\\((.*?)\\)", "-\\1", Short.Term.Gain.Loss),
    Short.Term.Gain.Loss = gsub("\\$", "", Short.Term.Gain.Loss),
    Cost.Basis = as.numeric(Cost.Basis),
    Proceeds = as.numeric(Proceeds),
    Date.Acquired = as.Date(Date.Acquired, format = "%m/%d/%Y"),
    Date.Sold = as.Date(Date.Sold, format = "%m/%d/%Y")
  ) %>% 
  select(Symbol, everything(), -Symbol.CUSIP., -Security.Description, -Long.Term.Gain.Loss)









## Use an arbitrary amount of time (t + 6days) to determine if adding to a trade or new trade..
## If you buy the stock and buy it again within 6 days, it is considered an "add" and defined as the same trade.
addDates <- cleanedData %>% 
  group_by(Symbol) %>% 
  mutate(
    tradeStartDate = as.Date(min(Date.Acquired)),
    addWindow = floor_date(tradeStartDate, "day") + days(6),
    tradeEndDate = max(Date.Sold)
  ) %>% 
  select(Symbol, Quantity, Date.Acquired, Date.Sold, everything()) 

## Use dates to group trades
addStartDate <- addDates %>% 
  mutate(
    transactionType = ifelse(Date.Acquired <= addWindow & Date.Acquired + days(6) >= addWindow, "same_trade", "new_trade"),
    tradeStartDate2 = as.Date(ifelse(transactionType == "new_trade", Date.Acquired, tradeStartDate)),
    buyPrice = Cost.Basis/Quantity,
    sellPrice = Proceeds/Quantity
  ) %>% 
  arrange(Symbol, tradeStartDate2) %>% 
  group_by(Symbol) %>%
  ungroup() %>%
  select(Symbol, Quantity, tradeStartDate2, Date.Acquired, tradeEndDate, Date.Sold, Cost.Basis, Proceeds, buyPrice, sellPrice)

## Add Id's to grouped trades
addIds <- addStartDate %>% 
  mutate(
    Id = cumsum(c(1, diff(data.table::rleid(tradeStartDate2)) != 0))
  ) %>% 
  select(Id, everything())

## Calculate weighted averages for trades
addWeightAvg <- addIds %>%
  group_by(Id) %>% 
  mutate(
    weightAvgBuy = (sum(buyPrice*Quantity)/sum(Quantity)),
    weightAvgSell = (sum(sellPrice*Quantity)/sum(Quantity))
  ) %>% 
  select(-Cost.Basis, -Proceeds, -buyPrice, -sellPrice, -Date.Acquired, -Date.Sold, -Quantity)

## Calculate percent gain or loss per trade
addPercentChange <- addWeightAvg %>% 
  group_by(Id) %>% 
  mutate(
    percentChange = ((weightAvgSell - weightAvgBuy)/weightAvgBuy)*100,
    fourPercentLoss = weightAvgBuy*0.96
  ) %>% 
  distinct(Id, .keep_all = TRUE)









retrieveAndMerge <- function(symbol, fromDate, addPercentChange) {
  data <- getSymbols(symbol, src = "yahoo", from = fromDate, auto.assign = FALSE)
  yahoo_dates <- index(data)
  
  # Create a data frame with dates and the symbol
  yahoo_data <- data.frame(Date = yahoo_dates, Symbol = symbol, data)
  mergedTable <- merge(addPercentChange, yahoo_data, by = "Symbol", all.x = TRUE)
  
  column_name <- paste0(symbol, ".Low")
  
  # Find the index where the condition is met
  end_index <- which(yahoo_data[[column_name]] < addPercentChange$fourPercentLoss)[1]
  
  # If no such index is found, set end_date to the last date
  if (is.na(end_index)) {
    end_date <- max(yahoo_data$Date)
  } else {
    end_date <- yahoo_data$Date[end_index]
  }
  
  # Filter the data based on the dynamic end date
  mergedTable <- mergedTable %>%
    filter(Date <= end_date)
  
  return(mergedTable)
}






# Initialize a list to store start dates for each Id
startDates <- list()

# Loop through your table
for (i in 1:nrow(addPercentChange)) {
  symbol <- addPercentChange$Symbol[i]
  Id <- addPercentChange$Id[i]
  
  # Check if start date for this Id exists, if not, set it
  if (!(as.character(Id) %in% names(startDates))) {
    startDates[[as.character(Id)]] <- addPercentChange$tradeStartDate2[i]
  }
  
  fromDate <- startDates[[as.character(Id)]]
  
  # Retrieve and merge data for the current symbol
  mergedTable <- retrieveAndMerge(symbol, fromDate, addPercentChange[i, , drop = FALSE])
  
  # Assign the merged table to a variable with a unique name
  assign(paste("merged_", symbol, "_Id_", Id, sep = ""), mergedTable)
  
  # Update the start date if the next Id is different
  if (i < nrow(addPercentChange) && addPercentChange$Id[i+1] != Id) {
    nextId <- addPercentChange$Id[i+1]
    startDates[[as.character(nextId)]] <- addPercentChange$tradeStartDate2[i+1]
  }
}


# Accessing each merged table
for (symbol in addPercentChange$Symbol) {
  for (Id in unique(addPercentChange$Id[addPercentChange$Symbol == symbol])) {
    mergedTableName <- paste("merged_", symbol, "_Id_", Id, sep = "")
    print(paste("Merged table:", mergedTableName))
    print(head(get(mergedTableName)))
  }
}

