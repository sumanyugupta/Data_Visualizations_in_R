install_and_load <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("dplyr", "ggplot2", "lubridate", "gridExtra", "XML", "treemap")
install_and_load(packages)

# Code to tranform the XML into a dataframe is commented below.
# Source: http://www.ryanpraski.com/apple-health-data-how-to-export-analyze-visualize-guide/

# xml <- xmlParse("export.xml")
# df <- XML:::xmlAttrsToDataFrame(xml["//Record"])
# df$value <- as.numeric(as.character(df$value))
# df$endDate <-ymd_hms(df$endDate,tz="America/New_York")
# df$month<-format(df$endDate,"%m")
# df$year<-format(df$endDate,"%Y")
# df$date<-format(df$endDate,"%Y-%m-%d")
# df$dayofweek <-wday(df$endDate, label=TRUE, abbr=FALSE)
# df$hour <-format(df$endDate,"%H")

# For file storage purposes, the export.xml file has already been saved in RDS format.

df <- readRDS('data/healthData.RDS')

# Total number of steps per day in the specified date range
stepDF <- df %>%
  filter(type == 'HKQuantityTypeIdentifierStepCount') %>%
  filter(as.Date(date) > as.Date('2018-05-26')) %>%
  filter(as.Date(date) < as.Date('2018-06-10')) %>%
  group_by(date,dayofweek) %>%
  summarize(steps=sum(value)) %>%
  mutate(Week = ifelse(as.Date(date) < '2018-06-03', 1, 2)) %>%
  print (n=100)

stepDF$Week <- as.factor(stepDF$Week)

ggplot(stepDF, aes(x=dayofweek, y=steps, color=Week, group=Week)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_brewer() +
  theme_bw() +  
  theme(panel.grid.major = element_blank()) +
  labs(title = 'Daily Steps Walked (05/27 to 06/09)', x = 'Day of Week', y = 'Number of Steps')

###########################################################
#### Graph Exercise Minutes ####
minsDF <- df %>% 
  filter(type == 'HKQuantityTypeIdentifierAppleExerciseTime') %>%
  filter(as.Date(date) > as.Date('2018-05-26')) %>%
  filter(as.Date(date) < as.Date('2018-06-10')) %>%
  group_by(date,dayofweek) %>%
  summarize(mins=sum(value)) %>%
  mutate(Week = ifelse(as.Date(date) < '2018-06-03', 1, 2)) %>%
  print (n=100)
  
minsDF$Week <- as.factor(minsDF$Week)

ggplot(minsDF, aes(x=dayofweek, y=mins, color=Week, group=Week)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks=seq(0, 160, 20)) +
  scale_fill_brewer() +
  theme_bw() +  
  theme(panel.grid.major = element_blank()) +
  labs(title = 'Daily Minutes Exercised (05/27 to 06/09)', x = 'Day of Week', y = 'Minutes Exercised')

###########################################################
#### Graph Calories Burned ####
calsDF <- df %>% 
  filter(type == 'HKQuantityTypeIdentifierActiveEnergyBurned') %>%
  filter(as.Date(date) > as.Date('2018-05-26')) %>%
  filter(as.Date(date) < as.Date('2018-06-17')) %>%
  group_by(date,dayofweek) %>%
  summarize(cals=sum(value)) %>%
  mutate(Week=ifelse(as.Date(date) > as.Date("2018-06-09"), 3,
    ifelse(as.Date(date) < as.Date("2018-06-03"), 1, 2))) %>%
  print (n=100)

for (i in 1:length(calsDF$date)) {
  if (calsDF$cals[i] > 1000) {
    calsDF$Activity[i] <- 'Played tennis'
  }
  else if ((calsDF$cals[i] > 990 && calsDF$cals[i] < 1000) || calsDF$cals[i] == 728.055)  {
    calsDF$Activity[i] <- 'Mowed the lawn'
  }
  else {
    calsDF$Activity[i] <- 'No workout'
  }
}

calsDF$Week <- as.factor(calsDF$Week)

ggplot(calsDF, aes(x=dayofweek, y=cals, color=Week, group=Week)) +
  geom_line() +
  geom_point(aes(shape=Activity, size = 2, alpha=0.5)) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_brewer() +
  theme_bw() +  
  theme(panel.grid.major = element_blank()) +
  guides(size=FALSE, alpha=FALSE) +
  labs(title = 'Daily Calories Burned (05/27 to 06/16)', 
    x = 'Day of Week', y = 'Calories Burned')

meanCals <- df %>% 
  filter(type == 'HKQuantityTypeIdentifierActiveEnergyBurned') %>%
  filter(as.Date(date) > as.Date('2018-05-26')) %>%
  filter(as.Date(date) < as.Date('2018-06-18')) %>%
  group_by(date,dayofweek) %>%
  summarize(cals=sum(value)) %>%
  mutate(Week=ifelse(as.Date(date) > as.Date("2018-06-09"), 3,
                     ifelse(as.Date(date) < as.Date("2018-06-03"), 1, 2))) %>%
  group_by(dayofweek) %>%
  summarize(meanCals = mean(cals)) %>%
  rename(Day=dayofweek, MeanCalories=meanCals)

tbl <- tableGrob(meanCals, rows=NULL, theme=ttheme_minimal())
grid.arrange(tbl)

##################################################################

# Create heatmap of steps per hour per day
df %>%
  filter(type == 'HKQuantityTypeIdentifierStepCount') %>%
  filter(as.Date(date) > as.Date('2018-05-26')) %>%
  filter(as.Date(date) < as.Date('2018-06-17')) %>%
  group_by(date,dayofweek,hour) %>% 
  summarize(Steps=sum(value)) %>% 
  group_by(hour,dayofweek) %>%
  summarize(Steps=mean(Steps)) %>%
  arrange(desc(Steps)) %>%
  print (n=200) %>%
  ggplot(aes(x=dayofweek, y=hour, fill=Steps)) + 
  geom_tile() + 
  scale_fill_continuous(labels = scales::comma, low = 'white', high = 'red') +
  theme_bw() + 
  theme(panel.grid.major = element_blank()) +
  labs(title = 'Average Steps Walked per Hour (05/27 to 06/16)', x = 'Day of Week', y = 'Hour (Military Time)')

# Save as RDS to reduce initial data manipulation whenever RStudio is restarted.
saveRDS(df, 'healthData.RDS')

##########################################################################

# Examine heartbeat data

heartDF <- df %>% 
  filter(type == 'HKQuantityTypeIdentifierWalkingHeartRateAverage') %>%
  group_by(startDate,dayofweek) %>%
  mutate(Week = ifelse(as.Date(date) < '2018-06-03', 1, 2)) %>%
  print (n=100)

##########################################################################

snap <- read.csv("data/Snapchat_Data.csv")
snap <- snap %>%
  select(-c(X, X.1)) %>%
  mutate(Value=1) %>%
  mutate(TStamp=as.POSIXct(paste(Date, TimeSent))) %>%
  mutate(Week=ifelse(as.Date(Date) > as.Date("2018-06-09"), 3,
                     ifelse(as.Date(Date) < as.Date("2018-06-03"), 1, 2))) %>%
  mutate(Day=weekdays(TStamp)) %>%
  mutate(ActualHour=substr(TStamp, 12, 13))

for (i in 1:length(snap$TStamp)) {
  currTime <- substr(snap$TStamp[i], 12, 19)
  if (currTime >= "00:00:00" && currTime < "04:00:00") {
    snap$NewHour[i] = 1
    snap$HourLabel[i] = "00:00 - 03:59 AM"
  }
  else if (currTime >= "04:00:00" && currTime < "08:00:00") {
    snap$NewHour[i] = 2
    snap$HourLabel[i] = "04:00 - 07:59 AM"
  }
  else if (currTime >= "08:00:00" && currTime < "12:00:00") {
    snap$NewHour[i] = 3
    snap$HourLabel[i] = "08:00 - 11:59 AM"
  }
  else if (currTime >= "12:00:00" && currTime < "16:00:00") {
    snap$NewHour[i] = 4
    snap$HourLabel[i] = "12:00 - 03:59 PM"
  }
  else if (currTime >= "16:00:00" && currTime < "20:00:00") {
    snap$NewHour[i] = 5
    snap$HourLabel[i] = "04:00 - 07:59 PM"
  }
  else if (currTime >= "20:00:00" && currTime < "24:00:00") {
    snap$NewHour[i] = 6
    snap$HourLabel[i] = "08:00 - 11:59 PM"
  }
}

snap$Week <- as.factor(snap$Week)

snap %>%
  group_by(Week, ActualHour, Day) %>%
  summarize(Snaps=sum(Value)) %>%
  arrange(desc(Snaps)) %>%
  rename(HourOfDay=ActualHour) %>%
  print (n=500)

timeSnaps <- snap %>%
  group_by(Week, HourLabel, Day) %>%
  summarize(Snaps=sum(Value)) %>%
  print (n=100)

timeSnaps$Day <- factor(timeSnaps$Day, levels=c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'))
timeSnaps$HourLabel <- factor(timeSnaps$HourLabel, levels=c('00:00 - 03:59 AM', '04:00 - 07:59 AM', '08:00 - 11:59 AM', '12:00 - 03:59 PM', '04:00 - 07:59 PM', '08:00 - 11:59 PM'))


ggplot(timeSnaps, aes(x = HourLabel, y = Snaps, fill=Week)) +
  geom_bar(position='dodge', stat='identity') +
  facet_grid(.~Day) +
  scale_fill_brewer() +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank() , axis.text.x = element_text(angle = 90)) +
  labs(title = 'Breakdown of Total Snapchats Per Day (05/27 to 06/16)', x = 'Time of Day', y = 'Total Snapchats')





snap %>%
  group_by(ActualHour, Day, Week) %>%
  summarize(snaps=sum(Value)) %>%
  arrange(desc(snaps)) %>%
  print (n=500)

snap %>% 
  filter(Sender == 'Sumanyu') %>%
  group_by(Sender, Receiver) %>%
  summarize(snaps=sum(Value)) %>%
  arrange(desc(snaps))

# Bar chart of snaps sent by me to all people
snap %>% 
  group_by(Sender, Receiver, Affiliation) %>%
  filter(Sender == 'Sumanyu') %>%
  summarize(snaps=sum(Value)) %>%
  ggplot(aes(x=reorder(Receiver, snaps), y=snaps, fill = Affiliation)) +
  geom_bar(stat='identity') +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  theme(panel.grid.major = element_blank()) +
  labs(title = paste0('Snapchats Sent by Sumanyu (05/27 to 06/16)'), x = 'Recipient', y = 'Number of Snapchats') +
  coord_flip()

sent_by_me <- snap %>% 
  group_by(Affiliation) %>%
  filter(Sender == 'Sumanyu') %>%
  summarize(Snapchats=sum(Value)) %>%
  arrange(desc(Snapchats))

# Table of snapchats sent by me grouped by Affiliation.

tbl <- tableGrob(sent_by_me, rows=NULL, theme=ttheme_minimal())
grid.arrange(tbl)

snap %>%
  group_by(Date) %>%
  summarize(snaps = sum(Value)) %>%
  arrange(desc(snaps))
#############################################################
snap$NewDate <- format(as.Date(snap$Date), "%m-%d")

snap %>% 
  group_by(NewDate) %>% 
  summarize(snaps=sum(Value)) %>% 
  arrange(desc(snaps))%>% 
  print (n=100) %>%
  ggplot(aes(x=NewDate, y=snaps)) +
  geom_line(group=1) +
  geom_point() +
  geom_vline(xintercept = 8, linetype="dashed") +
  geom_vline(xintercept = 15, linetype="dashed") +
  geom_hline(yintercept = 45.38095, linetype="dotted") +
  annotate("text", x=0.6, y = 48, label=paste0('Average\nsnapchats (45)'), angle=90, size=3.5) +
  annotate("rect", fill = "yellow", alpha = 0.1, 
           xmin = 0, xmax = 8,
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "blue", alpha = 0.1, 
           xmin = 8, xmax = 15,
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "green", alpha = 0.1, 
           xmin = 15, xmax = 22,
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.1, 
           xmin = 7, xmax = 9,
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.1, 
           xmin = 14, xmax = 16,
           ymin = -Inf, ymax = Inf) +
  annotate("text", x=3, y = 69, label='Week 1') +
  annotate("text", x=11, y = 69, label='Week 2') +
  annotate("text", x=18, y = 69, label='Week 3') +
  annotate("text", x=8, y = 60, label='Weekend', size=3.5) +
  annotate("text", x=15, y = 60, label='Weekend', size=3.5) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank()) +
  labs(title = 'Breakdown of Total Snapchats Per Day (05/27 to 06/16)', x = 'Date', y = 'Total Snapchats')


####################################################

sumSent <- snap %>% 
  group_by(Medium, Affiliation) %>%
  filter(Sender == 'Sumanyu') %>%
  summarize(snaps=sum(Value)) %>%
  mutate(Sender='Sumanyu') %>%
  mutate(NumLabel=paste0(Medium, ' (', snaps, ')')) %>%
  mutate(AffLabels=ifelse(
    Affiliation == 'HighSchool', paste0('High School\nTotal: 211'), ifelse(Affiliation == 'Work', paste0('Work\nTotal: 159'), paste0('College\nTotal: 84')))) %>%
  arrange(desc(snaps))
  
treemap(sumSent, index=c("AffLabels","NumLabel"), vSize="snaps", type="index",
        palette = "Set1",
        border.col=c("grey20","grey80"),
        bg.labels=c("transparent"),
        align.labels=list(
          c("center", "center"), 
          c("center", "bottom")
        ),
        title=paste0("Breakdown of Snapchats Sent by Sumanyu"),
        fontsize.title=14,
        fontsize.labels=c(13,11),
        fontcolor.labels = c("grey90", "white"),
        border.lwds=c(4,2))

