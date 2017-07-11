library(gtrendsR)

##### Set up Google Trends
usr <- "<Google account email>"
psw <- "<Google account password>"
ch <- gconnect(usr, psw) 
# x <- gtrends(c("data is", "data are"), res="day")
x <- gtrends(query = 'corporate social responsibility',  
                res="week")

##### Get the trend into a dataframeag
trend <- x$trend

##### Make a date object from start
trend$date <- as.Date(trend$start)

##### Get a year column
trend$year <- format(trend$date, '%Y')

##### Rename interest
names(trend)[3] <- 'interest'

##### Group by year and replot
temp <- 
  trend %>%
  group_by(year) %>%
  summarise(avg = mean(interest))

## Make year numeric
temp$year <- as.numeric(temp$year)

# Plot
plot(temp$year, temp$avg)
