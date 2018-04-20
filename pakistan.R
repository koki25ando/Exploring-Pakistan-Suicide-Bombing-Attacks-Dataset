
library(data.table)
library(tidyverse)
library(readr)
library(stringr)
library(plotly)
library(scales)

pakistan10 <- read.csv("https://s3-ap-southeast-2.amazonaws.com/koki25ando/PakistanSuicideAttacks+Ver+6+(10-October-2017).csv", stringsAsFactors=FALSE, fileEncoding="latin1")
pakistan11 <- read.csv("https://s3-ap-southeast-2.amazonaws.com/koki25ando/PakistanSuicideAttacks+Ver+11+(30-November-2017).csv", stringsAsFactors=FALSE, fileEncoding="latin1")

pakistan <- bind_rows(pakistan10, pakistan11)
pakistan$Longitude <- as.numeric(pakistan$Longitude)
pakistan$Location.Sensitivity <- as.factor(pakistan$Location.Sensitivity)
pakistan$Injured.Max <- as.numeric(pakistan$Injured.Max)

pakistan$Target.Type <- str_replace(pakistan$Target.Type, pattern = "civilian", replacement = "Civilian")
pakistan$Target.Type <- str_replace(pakistan$Target.Type, pattern = "foreigner", replacement = "Foreigner")
pakistan$Target.Type <- str_replace(pakistan$Target.Type, pattern = "police", replacement = "Police")
pakistan$Target.Type <- str_replace(pakistan$Target.Type, pattern = "Government official", replacement = "Government Official")
pakistan$Target.Type <- str_replace(pakistan$Target.Type, pattern = "religious", replacement = "Religious")


pakistan <- 
  pakistan %>% 
  select(-S.) %>% 
  separate(Date, sep = "-", into = c("Day", "Date"), extra = "merge")
pakistan$Date <- 
  pakistan$Date %>% 
  str_replace(" ", "-")
pakistan$Date <- pakistan$Date %>% as.Date("%b-%d-%Y")

pakistan$Day <- as.factor(pakistan$Day)

pakistan <- 
  pakistan %>% 
  mutate(Year = year(Date))

pakistan %>% head()


world.map <- map_data ("world")
world.map <- world.map %>% filter(region == "Pakistan")
ggplot() + 
  geom_map(data=world.map, map=world.map,
           aes(x=long, y=lat, group=group, map_id=region),
           fill="white", colour="black") + 
  geom_point(data = pakistan, aes(x = Longitude, y = Latitude), colour = "red") + 
  labs(x = "Longitude", y = "Latitude", 
       title = "Pakistan", subtitle ="Where all the attacks happen?")

pp <- ggplot(as.data.frame(table(pakistan$Province)) %>% 
               arrange(desc(Freq)),
             aes(reorder(Var1, -Freq), Freq, fill = Var1)) + 
  geom_bar(stat = "identity") + 
  labs(x="Province", title = "Top 10 Provinces that experienced most Suicide Bombing Attacks")
ggplotly(pp)


kpk <- pakistan %>% 
  filter(Province == "KPK")
bp <- kpk %>% 
  ggplot(aes(x = Year, fill = Location.Category)) + 
  geom_bar() +
  labs(title = "Where did Suicide Bombing Attacks in KPK province from 2004 to 2017 happen?", 
       subtitle = "Where did they happen?") +
  scale_fill_discrete(name="Location")
ggplotly(bp)

ggplot() + 
  geom_map(data=world.map, map=world.map,
           aes(x=long, y=lat, group=group, map_id=region),
           fill="white", colour="black") + 
  geom_point(data = kpk, aes(x = Longitude, y = Latitude, colour = Injured.Min, size = Killed.Max)) + 
  labs(x = "Longitude", y = "Latitude", size = "Killed", title = "Suicide Bombing Attacks in KPK Province") + 
  scale_colour_gradient(low = "yellow", high = "red", name = "Injured") + 
  xlim(67,74) + 
  ylim(30,35.5)

bar.plot <- kpk %>% 
  ggplot(aes(x = Year, fill = Target.Type)) + 
  geom_bar() +
  labs(title = "Who were the targets in PKP?", fill = "Target Type")
ggplotly(bar.plot)

city.p <- ggplot(as.data.frame(table(kpk$City)) %>% 
                   arrange(desc(Freq)) %>% head(10),
                 aes(reorder(Var1, -Freq), Freq, fill = Var1)) + 
  geom_bar(stat = "identity") + 
  labs(x="City", title = "Which city has experienced the most Attacks?")
ggplotly(city.p)

peshawar <- kpk %>% filter(City == "Peshawar")
peshawar.p <- peshawar %>% 
ggplot(aes(Day, fill = Location.Category)) + 
geom_bar() + 
scale_x_discrete(limits=c("Sunday", "Monday", "Tuesday", 
"Wednesday", "Thursday", "Friday", "Saturday")) + 
labs(title = "When did attacks happen?")
ggplotly(peshawar.p)

peshawar <- 
peshawar %>% 
mutate(Month = month(Date))
peshawar$Month <- as.factor(peshawar$Month)

peshawar.month <- peshawar %>% 
ggplot(aes(Month)) + 
geom_bar() +
scale_x_discrete(labels = c('Jan','Feb','Mar', 'Apr','May','Jun',
'Jul','Aug','Sep','Oct','Nov','Dec')) + 
labs(title = "When did attacks happen?")
ggplotly(peshawar.month)

plot_ly(as.data.frame(table(peshawar$Target.Type))[-1,], 
labels = ~Var1, values = ~Freq, type = 'pie') %>%
layout(title = 'Who were most targeted in Peshawar?',
xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
