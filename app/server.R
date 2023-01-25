## Shiny server

# Install and load packages
#install.packages("quantmod")
#install.packages("plotly")
library(quantmod)
library(plotly)


# Calculate yearly return for the last ten or eleven years (or as much data as available if newer company/ETF, etc)
yearEND_df <- function(dataframe){
        
        unq_years <- unique(sub("-.+", "", dataframe$Date, perl = TRUE))
        final_df <- data.frame()
        
        for (i in 1:length(unq_years)){
                
                temp_df <- dataframe[grepl(unq_years[i], dataframe$Date),]
                temp_df <- temp_df[nrow(temp_df),]
                final_df <- rbind(final_df, temp_df)
                
        }
        
        # Will not include final year's data if month is not in at least December
        check <- final_df[nrow(final_df),]$Date[1]
        check <- sub("[0-9]{4}-", "", check, perl = TRUE)
        check <- sub("-.+", "", check, perl = TRUE)
        
        if (as.numeric(as.character(check)) < 12){
                
                final_df <- final_df[1:(nrow(final_df)-1),]
                pcntIncrease <- vector()
                for (i in 1:(nrow(final_df)-1)){
                        original <- final_df[i,4]
                        new <- final_df[i+1,4]
                        pcntChange <- new - original
                        pcntChange <- round(pcntChange / original * 100, digits = 2)
                        if (i == 1){
                                pcntIncrease <- append(pcntIncrease, "NA", after = length(pcntIncrease))   
                        }
                        pcntIncrease <- append(pcntIncrease, pcntChange, after = length(pcntIncrease))
                        
                        if (i == (nrow(final_df)-1)){
                                final_df <- cbind(final_df, pcntIncrease)
                                return(final_df[c(4,7,8)])
                        }
                }
                
        } else {
                pcntIncrease <- vector()
                for (i in 1:(nrow(final_df)-1)){
                        original <- final_df[i,4]
                        new <- final_df[i+1,4]
                        pcntChange <- new - original
                        pcntChange <- round(pcntChange / original * 100, digits = 2)
                        if (i == 1){
                                pcntIncrease <- append(pcntIncrease, "NA", after = length(pcntIncrease))   
                        }
                        pcntIncrease <- append(pcntIncrease, pcntChange, after = length(pcntIncrease))
                        
                        if (i == (nrow(final_df)-1)){
                                final_df <- cbind(final_df, pcntIncrease)
                                return(final_df[c(4,7,8)])
                        }
                }
        }
        
}


# Calculate the average rate of return for the ticker
calculateAnnualReturns_df <- function(dataset = "Input: 1 or 2", tickers = c("^IXIC", "^GSPC", "VGT", "MSFT", 6.1)){
        returns_df <- data.frame() # store the annual percent return
        raw_df <- data.frame() # store the historical price data in a data frame
       
        for (i in 1:length(tickers)){
                if (grepl("^[0-9]|[0-9]$", tickers[i], perl = TRUE)){
                       
                        returns_df <- rbind(returns_df, data.frame("Ticker" = paste("CUSTOM-", tickers[i], sep=""), "Range.of.Years.Measured" = "NA", "Annual.Percent.Return" = round(as.numeric(as.character(tickers[i])), digits = 2))) 
 
                } else {
                        data1_df <- data.frame()
                        while (nrow(data1_df) == 0){
                                try(
                                        data1_df <- data.frame(loadSymbols(tickers[i],auto.assign = FALSE))
                                        #returns df (MSFT.Open, MSFT.High, MSFT.Low, MSFT.Close, MSFT.Volume, MSFT.Adjusted)
                                , silent = TRUE)
                        }
                        data1_df$Date <- row.names(data1_df); row.names(data1_df) <- NULL
                        temp_df <- yearEND_df(data1_df) #calling another function
                        temp2_df <- temp_df
                        colnames(temp2_df)[1] <- "Close.Price"
                        raw_df <- rbind(raw_df, cbind(temp2_df, "Ticker" = tickers[i]))
                        
                        ticker <- sub("\\.Close", "", colnames(temp_df)[1], perl = TRUE)
                        temp_yrs <- sub("-.+", "", temp_df$Date, perl = TRUE)
                        temp_yrs <- paste(temp_yrs, collapse = "-")
                        temp_yrs <- sub("-.+-", "-", temp_yrs, perl = TRUE)
                        temp_df <- temp_df[temp_df$pcntIncrease!="NA",]
                        year_pcntAverage <- round(mean(as.numeric(as.character(temp_df$pcntIncrease))), digits = 2)
                        returns_df <- rbind(returns_df, data.frame("Ticker" = ticker, "Range.of.Years.Measured" = temp_yrs, "Annual.Percent.Return" = year_pcntAverage)) 
                        
                }
                
        }
        rownames(raw_df) <- NULL
        
        if (dataset == "1"){
                return(raw_df)
        } else if (dataset == "2"){
                return(returns_df)
        }
        
}


# Calculate the yearly future returns
FutureReturns_df <- function(principal, annual_addition, years, tickers){
        yrReturns_df <- data.frame() 
        returns_df <- calculateAnnualReturns_df("2", tickers)
        for (i in 1:nrow(returns_df)){
                
                interest_rate <- (as.numeric(as.character(returns_df$Annual.Percent.Return[i])) / 100)
                
                for (j in 0:years){
                        # Calculate the interest received each year
                        if (j == 0){
                                yrReturns_df <- rbind(yrReturns_df, data.frame("Ticker" = returns_df[i,1], "Year" = j, "Cumulative_withoutInterest" = principal, "Cumulative" = principal))
                                
                        } else if (j == 1) {
                                temp_df <- yrReturns_df[yrReturns_df$Ticker == returns_df[i,1],]
                                cumulative_premium <- as.numeric(as.character(temp_df$Cumulative_withoutInterest))[1] + annual_addition
                                cumulative_interest <- round(cumulative_premium * (1 + interest_rate), digits = 2)
                                yrReturns_df <- rbind(yrReturns_df, data.frame("Ticker" = returns_df[i,1], "Year" = j, "Cumulative_withoutInterest" = cumulative_premium, "Cumulative" = cumulative_interest))
                        } else {
                                temp_df <- yrReturns_df[yrReturns_df$Ticker == returns_df[i,1],]
                                cumulative_premium <- as.numeric(as.character(temp_df$Cumulative_withoutInterest[nrow(temp_df)])) + annual_addition
                                cumulative_interest <- round((temp_df$Cumulative[nrow(temp_df)] + annual_addition) * (1 + interest_rate), digits = 2)
                                yrReturns_df <- rbind(yrReturns_df, data.frame("Ticker" = returns_df[i,1], "Year" = j, "Cumulative_withoutInterest" = cumulative_premium, "Cumulative" = cumulative_interest))
                        }
                }
        }
        
        # Also include the cumulative returns
        temp <- yrReturns_df[yrReturns_df$Ticker==unique(yrReturns_df$Ticker)[1],]$Cumulative_withoutInterest # add cumulative premium to dataset
        temp2 <- yrReturns_df[yrReturns_df$Ticker==unique(yrReturns_df$Ticker)[1],]$Year # add cumulative premium to dataset
        yrReturns_df <- rbind(yrReturns_df, data.frame("Ticker" = "Cumulative Premium", "Year" = temp2, "Cumulative_withoutInterest" = temp, "Cumulative" = temp))
        
        return (yrReturns_df)
}


# Plot the data function
plot_compoundInt <- function(principal, annual_addition, years, tickers = c("^IXIC", "^GSPC", "VGT", "MSFT", 6.1)){
        tickers <- as.vector(strsplit(tickers, split=","))[[1]]
        temp_df <- FutureReturns_df(principal, annual_addition, years, tickers)
        
        # Plot the data
        p <- plot_ly(temp_df, x = ~Year, y = ~Cumulative, type = 'scatter', mode = 'lines', linetype = ~Ticker, line = list(color = 'ticker', width = 5.2)) %>%
                layout(title = 'Compounding Interest Analysis', xaxis = list(title = "Years in the Future", size = 7),
                       yaxis = list (title = "Total Value (USD$)", size = 7))
        p
}


# test print

shinyServer(
        function(input, output){
                options(warn = -1) # suppress warnings
                
                ##Plot compounding return
                output$CompInt.plot <- renderPlotly({plot_compoundInt(principal = as.numeric(as.character(input$principal)), annual_addition = as.numeric(as.character(input$annual_addition)), years = as.numeric(input$years), 
                                                                      tickers = input$ticker)})
                
                ##Show percent return
                output$CompInt.table <- DT::renderDataTable({calculateAnnualReturns_df(dataset = "2", tickers = as.vector(strsplit(input$ticker, split=","))[[1]])})
                
                
                ##Show percent return
                output$CompInt.table2 <- DT::renderDataTable({calculateAnnualReturns_df(dataset = "1", tickers = as.vector(strsplit(input$ticker, split=","))[[1]])})
                
                ##Show percent return
                output$CompInt.table3 <- DT::renderDataTable({FutureReturns_df(principal = as.numeric(as.character(input$principal)), annual_addition = as.numeric(as.character(input$annual_addition)), years = as.numeric(input$years), tickers = as.vector(strsplit(input$ticker, split=","))[[1]])})
                
                
                
                
                output$test <- renderText({class(input$years)})

        })