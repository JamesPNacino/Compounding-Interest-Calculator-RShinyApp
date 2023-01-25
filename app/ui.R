## Shiny UI
# Install and load packages
#install.packages("quantmod")
#install.packages("plotly")
library(quantmod)
library(plotly)


# Use a fluid Bootstrap layout
fluidPage(    
        
        # Give the page a title
        titlePanel("Investing Analysis"),
        
        # Generate a row with a sidebar
        sidebarLayout(fluid = FALSE,  
                
                # Define the sidebar layout
                sidebarPanel(
                        textInput(inputId = "ticker", label = "Stock/ETF/Index - Ticker(s):", value = paste(c("^IXIC", "GOOG", "MSFT", 6.1, "FTEC"), collapse=",")),
                        helpText("Input ETF/stock/or index (any ticker) or input a percent (integer ex, 6.1 is 6.1%), SEPARATE by commas. (e.g. ^IXIC is NASDAQ index and ^GSPC is SP500 index)"),
                        hr(),
                        
                        textInput(inputId = "principal", label = "Principal investment:", value = 1000),
                        helpText("Input initial/principal investment (1000 is equivalent to initial investment of $1000)"),
                        hr(),
                        
                        textInput(inputId = "annual_addition", label = "Annual Addition:", value = 3600),
                        helpText("Input annual addition to investment (3600 is equivalent to adding $300/month to your investment)"),
                        hr(),
                        
                        sliderInput(inputId = "years", label = "Years to invest:", min = 1, max = 80, value = 30, step = 1),
                        helpText("Input the number of years to grow your investment"),
                        hr(),
                        
                        submitButton(text = "Calculate"),
                        
                        width = 4
                ),
                
                
                
                # Create a spot for the barplot
                mainPanel(
                        tabsetPanel(

                                
                                tabPanel("Compounding Interest Analysis",
                                         
                                         tabsetPanel(

                                                 tabPanel("Analysis",
                                                          plotlyOutput("CompInt.plot", width = "950px", height = "550px"),
                                                          h4("The interest rate(s) in the plot above are based off of using the values below in the 'Annual.Percent.Return' column. The 'Annual.Percent.Return' column is calculated by averaging the yearly percent returns from all the years shown in the 'Range.of.Years.Measured' column. See the 'Data/Calculations' tab for further info."),
                                                          hr(),
                                                          DT::dataTableOutput("CompInt.table")),
                                                 
                                                 tabPanel("Data/Calculations",
                                                          h2("Historic Returns Table"),
                                                          p("This table shows the calculations of the historic yearly returns of the ticker(s). Used this table to consolidate the percent averages on the first tab"),
                                                          DT::dataTableOutput("CompInt.table2"),
                                                          hr(),
                                                          h2("Year by Year Interest Accumulation Table"),
                                                          p("This table shows the projections on how much interest is accumulated in the future. This table was used for the plot on first tab."),
                                                          DT::dataTableOutput("CompInt.table3"))))

                        ) 
                )
                
        )
)