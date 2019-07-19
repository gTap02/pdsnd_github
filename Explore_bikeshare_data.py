
# coding: utf-8

# ### Explore Bike Share Data
#
# For this project, your goal is to ask and answer three questions about the available bikeshare data from Washington, Chicago, and New York.  This notebook can be submitted directly through the workspace when you are confident in your results.
#
# You will be graded against the project [Rubric](https://review.udacity.com/#!/rubrics/2508/view) by a mentor after you have submitted.  To get you started, you can use the template below, but feel free to be creative in your solutions!

# In[46]:

ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')
head(ny)
head(wash)
head(chi)


# ### Question 1
# **What are the popular times of travel in each of the three cities basis the most common month, the most common day and the most common hour?**

# Your solution code goes here

library(lubridate)
library(dplyr)
library(ggplot2)

# Most common month - Chicago
chi$month <- month(as.Date(chi$Start.Time), label = TRUE)
chiDF <- data.frame(table(chi$month))
names(chiDF) <- c("Month", "Chicago")
#chiDF <- chiDF[order(-chiDF$freq),]

# Most common month - NY
ny$month <- month(as.Date(ny$Start.Time), label = TRUE)
nyDF <- data.frame(table(ny$month))
names(nyDF) <- c("Month", "New.York")
#nyDF <- nyDF[order(-nyDF$freq),]

# Most common month - Chicago
wash$month <- month(as.Date(wash$Start.Time), label = TRUE)
washDF <- data.frame(table(wash$month))
names(washDF) <- c("Month", "Washington")
#washDF <- washDF[order(-washDF$freq),]

df <- merge(chiDF,nyDF)
DF <- merge(df, washDF)
DF <- DF[order(DF$Month),]
DF

ggplot(DF, aes(Month)) +
  geom_line(aes(y = Chicago, colour = "Chicago", group = 1)) +
  geom_line(aes(y = New.York, colour = "New York", group = 1)) +
  geom_line(aes(y = Washington, colour = "Washington", group = 1)) +
  labs(x = "Months", y = "Frequency", title = "The most common Month of travel")


# Most common day - Chicago
chi$day <- wday(as.Date(chi$Start.Time), label = TRUE)
chiDF <- data.frame(table(chi$day))
names(chiDF) <- c("Day", "Chicago")
#chiDF <- chiDF[order(-chiDF$freq),]

# Most common day - NY
ny$day <- wday(as.Date(ny$Start.Time), label = TRUE)
nyDF <- data.frame(table(ny$day))
names(nyDF) <- c("Day", "New.York")
#nyDF <- nyDF[order(-nyDF$freq),]

# Most common day - Chicago
wash$day <- wday(as.Date(wash$Start.Time), label = TRUE)
washDF <- data.frame(table(wash$day))
names(washDF) <- c("Day", "Washington")
#washDF <- washDF[order(-washDF$freq),]

df <- merge(chiDF,nyDF)
DF <- merge(df, washDF)
DF <- DF[order(DF$Day),]
DF

ggplot(DF, aes(Day)) +
  geom_line(aes(y = Chicago, colour = "Chicago", group = 1)) +
  geom_line(aes(y = New.York, colour = "New York", group = 1)) +
  geom_line(aes(y = Washington, colour = "Washington", group = 1)) +
  labs(x = "Days of the week", y = "Frequency", title = "The most common day of travel")



# In[53]:

# Most common hour - Chicago
chi$hour <- hour(ymd_hms(chi$Start.Time))
chiDF <- data.frame(table(chi$hour))
names(chiDF) <- c("Hour", "Chicago")
#chiDF <- chiDF[order(-chiDF$freq),]

# Most common hour - NY
ny$hour <- hour(ymd_hms(ny$Start.Time))
nyDF <- data.frame(table(ny$hour))
names(nyDF) <- c("Hour", "New.York")
#nyDF <- nyDF[order(-nyDF$freq),]

# Most common hour - Washington
wash$hour <- hour(ymd_hms(wash$Start.Time))
washDF <- data.frame(table(wash$hour))
names(washDF) <- c("Hour", "Washington")
#washDF <- washDF[order(-washDF$freq),]

#chiDF[1,]
#nyDF[1,]
#washDF[1,]

df <- merge(chiDF,nyDF)
DF <- merge(df, washDF)
DF <- DF[order(DF$Hour),]
DF

ggplot(DF, aes(Hour)) +
  geom_line(aes(y = Chicago, colour = "Chicago", group = 1)) +
  geom_line(aes(y = New.York, colour = "New York", group = 1)) +
  geom_line(aes(y = Washington, colour = "Washington", group = 1)) +
  labs(x = "Hours", y = "Frequency", title = "The most common Time of travel")


# **June is the most common month of travel for all the three cities, probably due to the fact that it is Summer.
# Chicago has Monday as the most common day of travel whereas, Wednesday is the most common day for NY & Washington.
# Chicago and New York has 17:00 hours as the most frequent hour of travel while Washington has 08:00 hours. Also, Chicago has quite less frequencies as compared to NY & Washington in all the three cases.**

# ### Question 2
#
# **What is the most common start station, end station and the most frequent combination of the start station and end station for the three cities?**

# In[60]:

# Your solution code goes here

# Most common start station : NY
nydf <- data.frame(table(ny$Start.Station))
names(nydf) <- c('station', 'freq')
nydf <- nydf[order(-nydf$freq),]

# Most common start station : Chicago
chidf <- data.frame(table(chi$Start.Station))
names(chidf) <- c('station', 'freq')
chidf <- chidf[order(-chidf$freq),]

# Most common start station : Washington
washdf <- data.frame(table(wash$Start.Station))
names(washdf) <- c('station', 'freq')
washdf <- washdf[order(-washdf$freq),]

rownames(nydf) <- NULL
rownames(chidf) <- NULL
rownames(washdf) <- NULL

tempdf <- rbind(nydf[1,], chidf[1,], washdf[1,])

tempdf <- cbind(tempdf, data.frame(c('NYC','Chicago','Washington')))
colnames(tempdf) <- c('Station', 'Frequency', 'City')

tempdf

# Most common end station : NY
nydf <- data.frame(table(ny$End.Station))
names(nydf) <- c("station", "freq")
nydf <- nydf[order(-nydf$freq),]


# Most common end station : Chicago
chidf <- data.frame(table(chi$End.Station))
names(chidf) <- c("station", "freq")
chidf <- chidf[order(-chidf$freq),]


# Most common end station : Washington
washdf <- data.frame(table(wash$End.Station))
names(washdf) <- c("station", "freq")
washdf <- washdf[order(-washdf$freq),]

nydf[1,]
chidf[1,]
washdf[1,]

rownames(nydf) <- NULL
rownames(chidf) <- NULL
rownames(washdf) <- NULL

tempdf <- rbind(nydf[1,], chidf[1,], washdf[1,])

tempdf <- cbind(tempdf, data.frame(c('NYC','Chicago','Washington')))
colnames(tempdf) <- c('Station', 'Frequency', 'City')

tempdf


# In[56]:

# Most frequent combination of start station and end station : NY
ny$SE <- paste(ny$Start.Station, ny$End.Station, sep = " _ ")
nydf <- data.frame(table(ny$SE))
names(nydf) <- c("station", "freq")
nydf <- nydf[order(-nydf$freq),]


# Most frequent combination of start station and end station : Chicago
chi$SE <- paste(chi$Start.Station, chi$End.Station, sep = " _ ")
chidf <- data.frame(table(chi$SE))
names(chidf) <- c("station", "freq")
chidf <- chidf[order(-chidf$freq),]


# Most frequent combination of start station and end station : Washington
wash$SE <- paste(wash$Start.Station, wash$End.Station, sep = " _ ")
washdf <- data.frame(table(wash$SE))
names(washdf) <- c("station", "freq")
washdf <- washdf[order(-washdf$freq),]

nydf[1,]
chidf[1,]
washdf[1,]


# **Although, the most common or popular start station and end station are same for each of the three cities, the most frequent combination of start station and end station are different and do not include either the start station or the end station in the combination.**

# ### Question 3
#
# **What is the total trip duration and average trip duration for each of the three cities?**

# In[57]:

# Your solution code goes here

# Total trip duration in hours : Chicago
c <- sum(chi$Trip.Duration)/3600

# Total trip duration in hours : NY
n <- sum(ny$Trip.Duration, na.rm = TRUE)/3600

# Total trip duration in hours : Washington
w <- sum(wash$Trip.Duration, na.rm = TRUE)/3600

print(paste("The total trip duration for Chicago is", c, "hours."))
print(paste("The total trip duration for New York is", n, "hours."))
print(paste("The total trip duration for Washington is", w, "hours."))


# In[58]:

# Average trip duration in minutes : Chicago
c <- mean(chi$Trip.Duration)/60

# Average trip duration in minutes : NY
n <- mean(ny$Trip.Duration, na.rm = TRUE)/60

# Average trip duration in minutes : Washington
w <- mean(wash$Trip.Duration, na.rm = TRUE)/60

print(paste("The average trip duration for Chicago is", c, "mins."))
print(paste("The average trip duration for New York is", n, "mins."))
print(paste("The average trip duration for Washington is", w, "mins."))


# **The total duration of trips in Washington is almost more than twice of the trip duration in New York and almost 14 times more than Chigaco. At the same time, the average trip duration of the three cities is quite comparable, from which we can comfortably conclude that the number of trips in Washington is proportionally greater than the other two cities.**

#
# ## Finishing Up
#
# > Congratulations!  You have reached the end of the Explore Bikeshare Data Project. You should be very proud of all you have accomplished!
#
# > **Tip**: Once you are satisfied with your work here, check over your report to make sure that it is satisfies all the areas of the [rubric](https://review.udacity.com/#!/rubrics/2508/view).
#
#
# ## Directions to Submit
#
# > Before you submit your project, you need to create a .html or .pdf version of this notebook in the workspace here. To do that, run the code cell below. If it worked correctly, you should get a return code of 0, and you should see the generated .html file in the workspace directory (click on the orange Jupyter icon in the upper left).
#
# > Alternatively, you can download this report as .html via the **File** > **Download as** submenu, and then manually upload it into the workspace directory by clicking on the orange Jupyter icon in the upper left, then using the Upload button.
#
# > Once you've done this, you can submit your project by clicking on the "Submit Project" button in the lower right here. This will create and submit a zip file with this .ipynb doc and the .html or .pdf version you created. Congratulations!

# In[59]:

system('python -m nbconvert Explore_bikeshare_data.ipynb')


# In[ ]:




# In[ ]:
