## Lab 1 Script
## Applied Machine Learning

### PACKAGES ###
require(ggplot2)

### UTILITIES ###

# Wrapper for converting date 
to.POSIXct <- function(year, monthNumber){
    ## Function to create a POSIXct time series 
    ## object from a year.month format
    
    ## Create a character vector from the numeric input
    dateStr <- paste(as.character(year), "-",
                     as.character(monthNumber), "-",
                     "01", sep = "")
    ## Return the POSIXct time series object
    as.POSIXct( strptime(dateStr, "%Y-%m-%d"))
}

# Wrapper for factorising and ordering month colum.
order.month <- function(x){
    ## Function to make Month column an ordered factor.
    x <- substr(x, 1, 3) ## Use just the first three letters
    factor(x, 
           levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
           ordered = TRUE)
}

## Loading and Examining the data

# Load the dataset
df_ <- read.csv('cadairydata.csv')

# adjust date time columns


df_$dateTime <- to.POSIXct(df_$Year,df_$Month.Number)
dropCols <- c("X","Year.Month","Month.Number") # columns to drop.
df_$Month <- order.month(df_$Month)
dairy <- df_[,!names(df_) %in% dropCols]


head(dairy) # Check the data
plot(dairy) # Plot the data

# Check the date time class
class(dairy$dateTime)

# Examine the top 20 of the dateTime column
dairy$dateTime[1:20]

# Test arimthmetic op
dairy[dairy$dateTime > '2012-12-01',]


## Creating a time series plot.
ggplot(dairy,aes_string('dateTime','Milk.Prod')) + geom_line() + ggtitle('Time Series of Milk Production') + xlab('Time in years')

# For the most part of the plot, the milk production has increased year over year. However in 2009 there is a decline in milk production, due to the recession.
# Also, the time series exhibits a strong seasonal component, with an annual cycle.

### Statistical properties of the time series 
# Look at the milk production.
milkProd.ts <- dairy[,'Milk.Prod'] # Subset milk production.

milkProd.ts <- ts(milkProd.ts,start = 1995,frequency = 12) # Convert to time series.

acf(milkProd.ts, main = 'ACF of Milk Production')
# The values of the ACF at various lag values, decays slowly, indicating considerable serial correlation between the time series values at the various lags. THIS IS MOST LIKELY DUE TO A TREND.

pacf(milkProd.ts, main = 'PACF of Milk Production')


ggplot(dairy,aes(x=as.numeric(dairy$Milk.Prod))) + geom_histogram(bins = 40) + ggtitle("Histogram of Milk Production") + xlab("Milk Production")

# Seems to show considerable dispersion, again LIKELY BECAUSE A TREND.


## Icecream production.

# Does icecream production have a noticable seasonal component? Can you characterize the trend of ice cream production greater over time as strong or weak?

ggplot(dairy,aes_string(x='dateTime','Icecream.Prod')) + geom_line() + ggtitle('Time Series of Icecream Production') + xlab('Time in years')
# Definietly strong seasonal component. Trend of the icecream production seems to be relatively weak compared to milk production.

# Is the seasonal variation of icecream production noticable in the plot of ACF? Does the ACF plot indicate a strong trend component?
iceProd.ts <- dairy[,'Icecream.Prod']
iceProd.ts <- ts(iceProd.ts,start = 1995,frequency = 12)

acf(iceProd.ts,main='ACF of Icecream Production')
# The acf drops quickly, but seems to repeat over time. Indicating that there is a strong seasonal component indeed. 
pacf(iceProd.ts,main='PACF of Icecream Production')



#### Simple Moving Average Decomposition of the Time Series. ####

# Generally time series are decomposed into three components.

# Trend can be modelled by several methods, starting with by decomposing the time series using a simple moving average model.

# We wil develop a function which will use the moving window method to compute the average of the time series over the specified span (order of the operator).
# As the moving window operator moves over the data, it will average the values in the windows. 

movAvg <- function(df,col='Milk.Prod',span=12){
    t_ <- df[,col]
    
    end_ <- length(t_) - 1 
    out_ <- rep(0,length(t_))
    
    out_[1] <- temp[1]
    for(i in 1:end_){
        if(i - span <= 1) j <- 1
        else j <- j + 1
        out_[i +1] <- sum(t_[j:i])/(i-j+1)
    }
    out_
}


# Once the trend has been removed, the seasonal component must be modelled and removed. 
# The function below computes teh seasonal component as a function of the month of the year using a linear model.
# The 0 in the model supresses the intercept term. Since 12 monthly factors are used to model seasonal variation, the model would be over-determined if an intercept was included.

seasComp <- function(df, col='Milk.Prod'){
    df$y <- df[,col]
    fit = lm(y ~ 0 + Month,data = df)
    predict(fit,newdata=df)
}

# Using movAvg and seasComp you can decomplose the timeseries into its components.

# The function below, uses multiplicative decompostion of the time series.
# The model is transformed to a multiplicative model by taking the log of the time series values. The moving average is computed over a 12 month moving window.

multDecomp <- function(df,col='Milk.Prod',mulltiplicative = TRUE, span = 12){
    if(multiplicative){
        t_ <- log(df[,col])
        df[,col] <- t_
    } else {
        t_ <- df[,col]
    }
    
    
}























