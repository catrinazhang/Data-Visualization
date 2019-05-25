library(leaflet)
library(shiny)
library(tidyverse)
library(dplyr)
library(ggmap)
library(writexl)
library(readxl)
library(shiny)
library(shinydashboard)
library(MASS)
library(readr)
library(scales)
library(maps)
library(plotly)
library(sp)
library(rgdal)
library(data.table)
library(fmsb)

#rsconnect::setAccountInfo(name='catrina', token='6A713A5CE48306E0A123BB47A650512C', secret='uiyyPAAFihVhc9AWznjYSEDK+lkswTMBpAUHOdyC')

#setwd("C:/Users/catri/Documents/SpeedDating")

select <- dplyr::select
map <- read_csv("MAP.csv")
geocodes <- read_excel("geocodes.xlsx")
geomus <- read_excel("geomus.xlsx")
features <- read_excel("feature_importance.xlsx")
map1 <- map %>% 
  filter(!is.na(map$from))

map2 <- as.data.frame(map1)
map4 <- map2 %>% select(iid, gender, race, from, match)

map5 <- map4 %>% 
  group_by(from) %>% 
  summarise(count = n(), rate = percent(mean(match)))

#geocodes <- geocode(map5$from, output = "latlon" , source = "google")

map3 <- cbind(map5, geocodes) 

mus <- map2 %>% select(iid, from, zipcode, match)

mus1 <- mus %>% 
  filter(floor(log10(zipcode))+1 == 5) %>% 
  filter(from == "USA")

mus3 <- mus1 %>% 
  group_by(zipcode) %>% 
  summarise(count = n(), rate = percent(mean(match)), rate2 = mean(match))

#mus3$address <- paste(mus3$zipcode, "USA")

#geomus <- geocode(mus3$address, output = "latlon" , source = "google")

mus2 <- cbind(mus3, geomus)
mus2 <- mus2 %>% 
  subset(!(zipcode %in% c(96797, 96701, 96822)))

ip <- read_excel("ip.xlsx")

ip$total <- rowSums(ip[,c("attractive", "ambitious", "sincere", "intelligent", "fun", "interests")])

ip <- ip %>% 
  filter(!total == 0)
ip$attractive <- round(ip$attractive/ip$total*100, digits = 2)
ip$ambitious <- round(ip$ambitious/ip$total*100, digits = 2)
ip$sincere <- round(ip$sincere/ip$total*100, digits = 2)
ip$intelligent <- round(ip$intelligent/ip$total*100, digits = 2)
ip$fun <- round(ip$fun/ip$total*100, digits = 2)
ip$interests <- round(ip$interests/ip$total*100, digits = 2)

ip <- ip[,-10]

# iphead <- head(ip,100)
# 
# ip2 <- ip %>% 
#   gather(attributes, rating, attractive:interests)

# ip <- map %>% 
#   select(gender, age, goal, attr1_1, sinc1_1, intel1_1, fun1_1, amb1_1,	shar1_1)
# 
# sum(is.na(ip$gender))
# 
# ip$asum <- rowSums(ip[,c("attr1_1", "sinc1_1", "intel1_1", "fun1_1", "amb1_1", "shar1_1")])
# 
# ip <- ip %>% 
#   filter(!is.na(ip$age)) %>% 
#   filter(!is.na(ip$goal)) %>% 
#   filter(!asum == 0)
# 
# ip$attr1_1 <- round(ip$attr1_1/at11$asum*100, digits = 2)
# ip$sinc1_1 <- round(ip$sinc1_1/at11$asum*100, digits = 2)
# ip$intel1_1 <- round(ip$intel1_1/ip$asum*100, digits = 2)
# ip$fun1_1 <- round(ip$fun1_1/ip$asum*100, digits = 2)
# ip$amb1_1 <- round(ip$amb1_1/ip$asum*100, digits = 2)
# ip$shar1_1 <- round(ip$shar1_1/ip$asum*100, digits = 2)
# 
# ip$gender <- ifelse(ip$gender == 0, "F", "M")
# 
# ip$goal <- ifelse(ip$goal %in% c(1,2,5,6), "Casual", "Serious")
# 
# ip <- ip[,-10]
# 
# colnames(ip) <- c("gender", "age", "goal", "attractive", "sincere", "intelligent", "fun", "ambitious", "interests")

career= read.csv("heatmapdata.csv")

career$career2 <- ifelse(career$career_c == 1, "lawyer", ifelse(career$career_c == 2, "academic",
                  ifelse(career$career_c == 3, "psychologist", ifelse(career$career_c == 4,
                  "medicine", ifelse(career$career_c == 5, "engineer", ifelse(career$career_c == 6, "arts",
                  ifelse(career$career_c == 7, "banking", ifelse(career$career_c == 8, "real estate",
                  ifelse(career$career_c == 9, "humanitarian", ifelse(career$career_c == 10, "undecided",
                  ifelse(career$career_c == 11, "social work", ifelse(career$career_c == 12, "speech pathology",
                  ifelse(career$career_c == 13, "politics", ifelse(career$career_c == 14, "sports",
                  ifelse(career$career_c == 15, "other", ifelse(career$career_c == 16, "journalism", "architecture"))))))))))))))))
  
 
career$career_c = career$career2

temp = career$pid
mylist = list()
for (i in 1:length(temp)){
  mylist[i]=head(career[career$iid == temp[i],]$career_c,1)}

career2 <- career
# Add the coded field of partner
career2$career_partner <- unlist(mylist)

group <- career2 %>% 
  group_by(career_c, career_partner) %>% 
  count() %>% 
  filter(! (career_c %in% c("speech pathology", "other", "journalism", "real estate", "politics"))) %>% 
  filter(! (career_partner %in% c("speech pathology", "other", "journalism", "real estate", "politics"))) %>% 
  filter(n >= 10)

career10 <- inner_join(group, career2, c("career_c" = "career_c", "career_partner" = "career_partner"))
career2 = career10
# Explore...
career3 <- career2 %>%
  group_by(career_c, career_partner) %>%
  summarise(match_rate = mean(match)) %>% 
  filter(!is.na(career_c) & !is.na(career_partner))

comp <- map %>% 
  select(gender, attr3_1, intel3_1, sinc3_1, fun3_1, amb3_1, attr_o, intel_o, sinc_o, fun_o, amb_o)

comp <- na.omit(comp)

# comp$total1 <- rowSums(comp[,c("attr3_1", "sinc3_1", "intel3_1", "fun3_1", "amb3_1")])
# comp$total2 <- rowSums(comp[,c("attr5_1", "sinc5_1", "intel5_1", "fun5_1", "amb5_1")])
# 
# comp$attr1_1 <- round(comp$attr1_1/comp$total1*100, digits = 2)
# comp$sinc1_1 <- round(comp$sinc1_1/comp$total1*100, digits = 2)
# comp$intel1_1 <- round(comp$intel1_1/comp$total1*100, digits = 2)
# comp$fun1_1 <- round(comp$fun1_1/comp$total1*100, digits = 2)
# comp$amb1_1 <- round(comp$amb1_1/comp$total1*100, digits = 2)
# 
# comp$attr2_1 <- round(comp$attr2_1/comp$total2*100, digits = 2)
# comp$sinc2_1 <- round(comp$sinc2_1/comp$total2*100, digits = 2)
# comp$intel2_1 <- round(comp$intel2_1/comp$total2*100, digits = 2)
# comp$fun2_1 <- round(comp$fun2_1/comp$total2*100, digits = 2)
# comp$amb2_1 <- round(comp$amb2_1/comp$total2*100, digits = 2)

comp1 <- comp[,1:6]
comp2 <- comp[,c(1,7:11)]

comp1_1 <- comp1 %>% 
  gather(attributes1, self_rating, attr3_1:amb3_1)

comp2_1 <- comp2 %>% 
  gather(attributes2, date_rating, attr_o:amb_o)

comps <- cbind(comp1_1, comp2_1)

comps$attributes1 <- ifelse(comps$attributes1 == "attr3_1", "attractiveness", 
                      ifelse(comps$attributes1 == "intel3_1", "intelligence",
                      ifelse(comps$attributes1 == "sinc3_1", "sincerity",
                      ifelse(comps$attributes1 == "fun3_1", "fun", "ambition"))))

comps$attributes2 <- ifelse(comps$attributes2 == "attr_o", "attractiveness", 
                            ifelse(comps$attributes2 == "intel_o", "intelligence",
                                   ifelse(comps$attributes2 == "sinc_o", "sincerity",
                                          ifelse(comps$attributes2 == "fun_o", "fun", "ambition"))))

comps$gender = ifelse(comps$gender == 0, "F", "M")
comps$date_rating = ifelse(comps$date_rating > 10, 10, comps$date_rating)
comps <- comps[,-4]


##MAPS
df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv')

countrydata <- map
countrydata2 <- countrydata[!is.na(countrydata$from),]
#write.csv(countrydata2,file="Recoded_without_blank.csv")

countrynumber <- countrydata %>%
  group_by(from,iid)%>%
  summarise(number = n())

countrynumber2  <- na.omit(countrynumber)
countrynumber3 <- countrynumber2 %>%
  group_by(from) %>%
  summarise(number = n())

colnames(countrynumber3) = c("COUNTRY", "number") 
countrynumber3$density = log(countrynumber3$number)

countrynumber3 <- countrynumber3 %>%
  mutate(COUNTRY=replace(COUNTRY, COUNTRY == "UK", "United Kingdom"), 
         COUNTRY=replace(COUNTRY, COUNTRY == "USA", "United States"),
         COUNTRY=replace(COUNTRY, COUNTRY == "South Korea"|COUNTRY == "Korea", "Korea, South"))

country4 <- left_join(countrynumber3, df)
country4 <- country4[,-4]

##successMAP

successdata = read.csv("Recoded_without_blank.csv", stringsAsFactors = FALSE)
successdata2 <- successdata %>%
  group_by(from)%>%
  summarise(rate = mean(match))

colnames(successdata2) = c("COUNTRY", "rate") 
successdata2 <- successdata2 %>%
  mutate(COUNTRY=replace(COUNTRY, COUNTRY == "UK", "United Kingdom"), 
         COUNTRY=replace(COUNTRY, COUNTRY == "USA", "United States"),
         COUNTRY=replace(COUNTRY, COUNTRY == "South Korea"|COUNTRY == "Korea", "Korea, South"))

success4 <- left_join(successdata2, df)
success4 <- success4[,-3]

l <- list(color = toRGB("grey"), width = 0.5)
g <- list(
  showframe = FALSE,
  showcoastlines = TRUE,
  projection = list(type = 'Natural earth'),
  showland = TRUE,
  showsubunits = TRUE,
  landcolor = toRGB("gray95"),
  subunitcolor = toRGB("gray85"),
  countrycolor = toRGB("gray65"),
  countrywidth = 0.5,
  subunitwidth = 0.5
)

##PIE CHART
Racedata7 = read.csv("racedata7.csv")

##VIOLIN
agedf <- map %>%
  filter(!is.na(age)) %>%
  select(iid, gender, age)
agedf <- filter(agedf, age < max(age))
agedf <- unique(agedf, by = iid)
agedf1 <- agedf %>%
  group_by(age) %>%
  count(gender,age)
agedf1$age <- factor(agedf1$age)
agedf1$gender <- factor(agedf1$gender)

agedf$gender <- factor(agedf$gender)
agedf$gender1 <- ifelse(agedf$gender == 1, "male","female")
agedf<-agedf[,-2]
names(agedf) <- c("iid","age","gender")
v <- ggplot(agedf, aes(x=gender, y=age, color = gender, fill = gender)) +
  geom_violin()+ geom_boxplot(width=0.05, color = "black")+ 
  labs(x = "gender", y = "age") + theme_bw() + theme(legend.position = "none")
v

## GOALS

Goaldata = map[,c("iid","gender","goal")]
Goaldata2 = unique(Goaldata,by=iid)
colnames(Goaldata2) = c("iid","gender_c","goal_c")
Goaldata2$gender = ifelse(Goaldata2$gender_c==1,"Male","Female")
#Goaldata3 = drop.na(Goaldata2)
Goaldata3 = Goaldata2 %>%
  filter(!is.na(goal_c))
goalcode = read.csv("goal code.csv")
Goaldata4 = left_join(Goaldata3,goalcode)


Goaldata5 = Goaldata4 %>%
  group_by(gender,goal) %>%
  summarise(count=n())

Goaldata5 = na.omit(Goaldata5)

setDT(Goaldata5)

Goaldata5[,order:=order(count),by=gender]

Goaldata5$order = c(1,5,2,4,3,6,1,3,2,5,4,6)

##radar

sp1<- map %>%
  filter(wave %in% c(1:5, 10:21) ) %>%
  group_by(gender) %>%
  select(iid, gender, attr1_1, sinc1_1, intel1_1, fun1_1, amb1_1, shar1_1) %>% 
  unique()
sp1<- sp1 %>% 
  filter(!is.na(attr1_1))
sp1[is.na(sp1)] <- 0
sp1$total1_1 <- rowSums(sp1[,c("attr1_1", "sinc1_1", "intel1_1", "fun1_1", "amb1_1", "shar1_1")])
sp1$shar1_1 <- round(sp1$shar1_1/sp1$total1_1*100, digits = 2)
sp1$attr1_1 <- round(sp1$attr1_1/sp1$total1_1*100, digits = 2)
sp1$sinc1_1 <- round(sp1$sinc1_1/sp1$total1_1*100, digits = 2)
sp1$fun1_1 <- round(sp1$fun1_1/sp1$total1_1*100, digits = 2)
sp1$amb1_1 <- round(sp1$amb1_1/sp1$total1_1*100, digits = 2)
sp1$intel1_1 <- round(sp1$intel1_1/sp1$total1_1*100, digits = 2)
sp1$total <- 100

sp1df <- sp1 %>%
  group_by(gender) %>%
  summarise(Attractive = mean(attr1_1), Sincere = mean(sinc1_1), Intelligent = mean(intel1_1), Fun = mean(fun1_1), Ambitious = mean(amb1_1), Interest = mean(shar1_1))


# What do you think the opposite sex looks for in a date?
sp2<- map %>%
  filter(wave %in% c(1:5, 10:21) ) %>%
  group_by(gender) %>%
  select(iid, gender, attr2_1, sinc2_1, intel2_1, fun2_1, amb2_1, shar2_1) %>% 
  unique()
sp2<- sp2 %>% 
  filter(!is.na(attr2_1))
sp2<- sp2 %>% 
  filter(!is.nan(attr2_1))
sp2[is.na(sp2)] <- 0
sp2$total2_1 <- rowSums(sp2[,c("attr2_1", "sinc2_1", "intel2_1", "fun2_1", "amb2_1", "shar2_1")])
sp2$shar2_1 <- round(sp2$shar2_1/sp2$total2_1*100, digits = 2)
sp2$attr2_1 <- round(sp2$attr2_1/sp2$total2_1*100, digits = 2)
sp2$sinc2_1 <- round(sp2$sinc2_1/sp2$total2_1*100, digits = 2)
sp2$fun2_1 <- round(sp2$fun2_1/sp2$total2_1*100, digits = 2)
sp2$amb2_1 <- round(sp2$amb2_1/sp2$total2_1*100, digits = 2)
sp2$intel2_1 <- round(sp2$intel2_1/sp2$total2_1*100, digits = 2)
sp2$total <- 100

sp2df <- sp2 %>%
  group_by(gender) %>%
  summarise(Attractive = mean(attr2_1), Sincere = mean(sinc2_1), Intelligent = mean(intel2_1), Fun = mean(fun2_1), Ambitious = mean(amb2_1), Interest = mean(shar2_1))

men <- rbind(rep(40,7),rep(0, 7),sp1df[2,],sp2df[1,]) %>%
  select(-gender)

women <- rbind(rep(40,7),rep(0, 7),sp1df[1,],sp2df[2,])  %>%
  select(-gender)


p <- plot_ly(
  type = 'scatterpolar',
  fill = 'toself'
) %>%
  add_trace(
    r = c(29.42, 16.04, 19.38, 17.36, 7.69, 10.13, 29.42),
    theta = c('attractive','sincere','intelligent', 'fun', 'ambitious', 'interest', 'attractive'),
    name = 'What Men Thinks'
  ) %>%
  add_trace(
    r = c(38.66, 10.34, 11.94, 18.96, 8.11, 11.99, 29.42),
    theta = c('attractive','sincere','intelligent', 'fun', 'ambitious', 'interest', 'attractive'),
    name = 'What Women Thinks'
  ) %>%
  layout(
    polar = list(
      radialaxis = list(
        visible = F,
        range = c(0,40)
      )
    )
  )
p
