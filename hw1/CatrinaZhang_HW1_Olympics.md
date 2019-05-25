---
title: "Homework 1 Olympics"
author: "Catrina Zhang"
date: 2019-02-22
output: 
  html_document:
    keep_md: true
---



## Assignment Setup
In this section, I will be loading the package and data required for the assignment.

```r
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(rvest)
library(stringr)
library(plotly)
library(DT)

setwd("~/")
noc_region=read_csv("noc_regions.csv")
gdp_pop=read_csv("gdp_pop.csv")
a_e=read_csv("athletes_and_events.csv")
```
## Q1 Medal Counts over Time
a) Here I combined noc region, GDP and athletes and events tables. I have identified serveral countries that have different NOCs, I combined URS and EUN into RUS (Russia) and SAA, FRG, and GDR as GER (Germany). I also relabeled SIN as SGP (Singapore) and ANZ as AUS (Australia) to make them consistent for joining, and relabelled the region corresponding to HKG as Hong Kong (previously China). 

```r
noc_region <- noc_region %>% 
  mutate(NOC=replace(NOC, NOC == "URS"|NOC == "EUN", "RUS"), 
         NOC=replace(NOC, NOC == "SAA"|NOC == "FRG"|NOC == "GDR", "GER"),
         NOC=replace(NOC, NOC == "SIN", "SGP"),
         NOC=replace(NOC, NOC == "ANZ", "AUS"),
         region=replace(region, NOC == "HKG", "Hong Kong")) %>% 
  unique()

a_e <- a_e %>%
  mutate(NOC=replace(NOC, NOC == "URS"|NOC == "EUN", "RUS"), 
         NOC=replace(NOC, NOC == "SAA"|NOC == "FRG"|NOC == "GDR", "GER")) %>% 
  unique()

joined = full_join(noc_region, gdp_pop, by = c('NOC'='Code'))
joined <- joined %>% 
  select(NOC, region, Population, `GDP per Capita`) %>% 
  distinct()

colnames(joined) <- c("NOC", "Region", "Population", "GDPPerCapita")

olympics <- inner_join(x = a_e, y = joined, by = "NOC")
```

b) Here I created a summary of how many Summer Games each country competed in and the number of medals of each type was won, selecting the top 10 NOCs in terms of number of Games participated in for the table so as to not overwhelm the reader. 

```r
participation <- olympics[!is.na(olympics$Medal), ]

participationSummer <- participation[which(participation$Season=="Summer"),]
participationSummerAll <- olympics[which(olympics$Season=="Summer"),]

participationByCountry <- data.frame(table(unique(data.frame(participationSummerAll$NOC,participationSummerAll$Year))$participationSummerAll.NOC))

colnames(participationByCountry) <- c("NOC", "NumberOfGames")

NOCMedals <- participationSummer %>% 
  select(NOC, Year, Medal)

NOCMedals2 <- NOCMedals %>%
  group_by(NOC,Medal) %>%
summarize(medals = length(Medal))
NOCMedals2 <- NOCMedals2 %>% 
  spread(Medal, medals)

gamesMedals <- left_join(x = participationByCountry, y = NOCMedals2, by = "NOC") %>% 
  arrange(desc(NumberOfGames)) %>% 
  head(10)

gamesMedals
```

```
##    NOC NumberOfGames Bronze Gold Silver
## 1  FRA            29    587  463    567
## 2  GBR            29    620  635    729
## 3  GRE            29     84   62    109
## 4  ITA            29    454  518    474
## 5  SUI            29    139   99    178
## 6  AUT            28     53   29     88
## 7  DEN            28    177  179    236
## 8  SWE            28    358  354    396
## 9  USA            28   1197 2472   1333
## 10 AUS            27    510  342    452
```
b cont) I also created two visualizations, graph A showing the total medal count split by type of medal for the top 10 medal-winning NOCs, as well as graph B showing an over-time comparison of total medals won by the top 10 medal-winning NOCs, split by sex, from 1980 to 2016. I would recommend graph A to the reader as it provides a very informative view of the total number of medals won by the top 10 medal-winning NOCs, as well as the medal split by type. While graph B is informative an we can clearly see a trend of females winning more medals in more recent years, it does not show which NOCs are the major contributors to this phenomenon. 

```r
contingency <- data.frame(table(participation$NOC))
contingencySummer <- data.frame(table(participationSummer$NOC))
subsetContingency <- (head((contingencySummer[order(-contingencySummer$Freq),]), n = 10))[,1]

object <- data.frame(table(participationSummer$NOC, participationSummer$Medal))
colnames(object) <- c("NOC", "medal", "count")

temps <- filter(object, NOC %in% subsetContingency)

medalsByColor <-
  ggplot(data = temps, aes(x=reorder(NOC, desc(count)), y=count, fill = medal)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("Gold" = "gold", "Silver" = "#a09a95", "Bronze" = "#dd842c")) +
  xlab("NOC") +
  ylab("Numbers of Medals") +
  ggtitle("Top 10 Medal Winning NOCs, Summer Games")

medalsByColor
```

![](figures/Q1_b_cont-1.png)<!-- -->

```r
object2 <- data.frame(table(participationSummer$NOC, participationSummer$Year))
temps4 <- filter(object2, Var1 %in% subsetContingency, Var2 %in% c(1980:2016))

participationTop10 <- filter(participationSummer, NOC %in% subsetContingency)

medalsOverTime <- data.frame(table(participationTop10$Year, participationTop10$Sex))

medalsSummary <- medalsOverTime %>% 
  mutate(Var1 = as.numeric(as.character(Var1))) %>% 
  filter(Var1 > 1979)

colnames(medalsSummary) <- c("Year", "Sex", "Count")

medalGenderPlot <-
  ggplot(data = medalsSummary, aes(x=Year, y=Count, group = Sex, color = Sex)) +
  geom_line() + 
  geom_point() +
  xlab("Year") +
  ylab("Total Numbers of Medals") +
  ggtitle("Total Medals for Top 10 Medal Winning NOCs, Summer Games") + 
  scale_x_continuous(breaks = seq(1980, 2016, by = 4))

medalGenderPlot
```

![](figures/Q1_b_cont-2.png)<!-- -->

##Q2 Medal Count Adjusted by Population and GDP
Here I created 3 graphs, based on aggregate (Gold, Silver, and Bronze) medals for the top 10 medal winning NOCs. Note that this is based on both Summer and Winter Games, whereas Q1 is only Summer.
graph A shows the total number of medals won by the top 10 medal-winning NOCs
graph B is adjusted by population, we can see that SWE and HUN really stood out because these are small countries with a small population, whereas USA came out at the bottom since it has a huge population compared with most of the other countries on the list
graph C is adjusted by GDP, we can see that RUS leads in this graph as it is the only developing country on the list with relativey low GDP.

I did not use a multiplier/scaling factor for GDP or population because I only want to show the relative standing for each country. 

```r
joined2 = merge(contingency, joined, by.x ='Var1', by.y = 'NOC')

medalsPopGDP <- joined2 %>% 
  na.omit() %>% 
  mutate(populationM = Freq/Population,
         GDPM = Freq/GDPPerCapita) %>% 
  arrange(desc(Freq)) %>% 
  head(10)

medalPlot <-
  ggplot(data = medalsPopGDP, aes(x=reorder(Var1, desc(Freq)), y=Freq, fill = Var1)) +
  geom_bar(stat = "identity") + 
  labs(title = "Top 10 Medal Winning NOCs",
       x = "NOC", y = "Medals") +
  theme(legend.position = "none")

medalPlot3 <-
  ggplot(data = medalsPopGDP, aes(x=reorder(Var1, desc(Freq)), y=populationM, fill = Var1)) +
  geom_bar(stat = "identity") + 
  labs(title = "Top 10 Medal Winning NOCs",
       subtitle = "Adjusted by Population",
       x = "NOC", y = "Medals") +
  theme(legend.position = "none")
  
medalPlot4 <-
  ggplot(data = medalsPopGDP, aes(x=reorder(Var1, desc(Freq)), y=GDPM, fill = Var1)) +
  geom_bar(stat = "identity") + 
  labs(title = "Top 10 Medal Winning NOCs",
       subtitle = "Adjusted by GDP",
       x = "NOC", y = "Medals") +
  theme(legend.position = "none")

grid.arrange(medalPlot, medalPlot3, medalPlot4, nrow = 3)
```

![](figures/Q2-1.png)<!-- -->

##Q3 Host Country Advantage
To perform the data cleaning, I manually changed the names of countries in order to match up with country names in the csv files.

Here I created a plot showing each NOC that has hosted the Olympics from from 1980 to 2016, the blue point shows the average number of medals won per Game when the NOC is hosting the Olympics, and the red point shows the average number of medals won per Game when the NOC is not hosting. I only included medal information from 1980 to 2016 since the number of medals won in the very distant past (e.g. 1896) is less relevant as a basic of comparison since both the number of participants and disciplines are very different from today. I also only included medals won during Summer Games to be consistent with the Wikipedia table. 

It looks like there is a host country advantage considering every single NOC received more medals on average during Games when they were host than when they were not. 


```r
wiki_hosts <- read_html("https://en.wikipedia.org/wiki/Summer_Olympic_Games")
hosts <- html_table(html_nodes(wiki_hosts, "table")[[8]], fill=TRUE)
hosts <- hosts[-1,1:3]
hosts$city <- str_split_fixed(hosts$Host, n=2, ",")[,1]
hosts$country <- str_split_fixed(hosts$Host, n=2, ",")[,2]
hosts$country <-trimws(hosts$country,"l")

hosts <- hosts[-c(6,12,13),]
hosts <- hosts %>% 
  mutate(country=replace(country, country == "California, United States"|country == "California, United States[30]"|country == "United States", "USA"), 
         country=replace(country, country == "France[30]", "France"),
         country=replace(country, country == "United Kingdom", "UK"),
         country=replace(country, country == "Soviet Union", "Russia"),
         country=replace(country, country == "West Germany", "Germany")) %>% 
  left_join(noc_region, by = c('country'='region')) 

countryMedal <- data.frame(table(participationSummer$Year, participationSummer$NOC)) %>% 
  mutate(Var1 = as.numeric(as.character(Var1)), host = "No")

for (i in 1:nrow(hosts)){
  countryMedal <- countryMedal %>% 
    mutate(host = replace(host, Var1 == hosts$Year[i] & Var2 == hosts$NOC[i], "Yes"))
}

colnames(countryMedal) <- c("Year", "NOC", "Count", "Host")

countryMedalSummary <- countryMedal %>% 
  filter(Year > 1979) %>% 
  group_by(NOC, Host) %>% 
  summarize(average = mean(Count)) %>% 
  ungroup() %>% 
  mutate(NOC = factor(NOC), Host = factor(Host)) %>% 
  filter(NOC %in% hosts$NOC[22:33])

hostPlot <-
  ggplot(data = countryMedalSummary, mapping = aes(x=average, y = NOC, colour = Host)) +
  geom_point(size = 3) +
  xlab("Average Number of Medals") +
  ylab("NOC") +
  ggtitle("Host Country Advantage")

hostPlot
```

![](figures/Q3-1.png)<!-- -->

##Q4 Most Successful Athletes
a) Here I am showing the top 10 "most successful" athletes of all time, which I have defined as having the highest total number of medals won. We can see that Michael Phelps came up on top with 28 medals won, and that 7 out of the top 10 most successful athletes are males. 

```r
successPre <- 
  participation %>%
  group_by(Name) %>%
  mutate(totalMedals = length(Medal)) %>%
  arrange(desc(totalMedals)) %>%
  ungroup() %>%
  select(Sport, NOC, Name, Sex, totalMedals) %>%
  unique()
  
successA <- successPre %>% 
  arrange(desc(totalMedals)) %>% 
  head(10)

athletesPlot <-
  ggplot(data = successA, aes(x=reorder(Name, totalMedals), y=totalMedals)) +
  geom_bar(stat = "identity", aes(fill = Sex)) + 
  xlab("Name") +
  ylab("Medals Won") +
  ggtitle("Top 10 Most Successful Athletes") +
  scale_y_continuous(breaks = seq(0, 30, by = 2)) +
  coord_flip()

athletesPlot
```

![](figures/Q4_a-1.png)<!-- -->

b) Here I am showing the most successful athletes in Badminton, by NOC. We can see that all of the top 10 athletes are from Asian countries, with most being from CHN (China) or KOR (Korea). 

```r
badminton <- successPre %>% 
  filter(Sport == "Badminton") %>% 
  arrange(desc(totalMedals)) %>% 
  head(10)

badmintonPlot <-
  ggplot(data = badminton, aes(x=reorder(Name, totalMedals), y=totalMedals, color = NOC)) +
  geom_bar(stat = "identity", aes(fill = NOC)) + 
  xlab("Name") +
  ylab("Medals Won") +
  ggtitle("Successful Athletes in Badminton by NOC based on Total Medals") +
  scale_y_continuous(breaks = seq(0, 8, by = 1)) +
  coord_flip()

badmintonPlot
```

![](figures/Q4_b-1.png)<!-- -->

##Q5 Interactivity
Here I made two interactive plots, graph A is the host country advantage, graph B is the number of medals won by type for the top 10 medal-winning NOCs.

I chose to show the host country advantage plot so the user can see the exact number of average medals won (since the scale is in increments of 100). The total medals won by medal type plot is chosen so the user can hover over specific medal colors and see more details. 

```r
ggplotly(hostPlot)
```

<!--html_preserve--><div id="htmlwidget-08a23f366a163232f06d" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-08a23f366a163232f06d">{"x":{"data":[{"x":[87.8888888888889,41.1111111111111,80.5555555555556,40.4444444444444,65,5.88888888888889,49.5555555555556,156.444444444444,220.625],"y":[1,2,3,4,5,6,7,8,9],"text":["average:  87.888889<br />NOC: AUS<br />Host: No","average:  41.111111<br />NOC: BRA<br />Host: No","average:  80.555556<br />NOC: CHN<br />Host: No","average:  40.444444<br />NOC: ESP<br />Host: No","average:  65.000000<br />NOC: GBR<br />Host: No","average:   5.888889<br />NOC: GRE<br />Host: No","average:  49.555556<br />NOC: KOR<br />Host: No","average: 156.444444<br />NOC: RUS<br />Host: No","average: 220.625000<br />NOC: USA<br />Host: No"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(248,118,109,1)","opacity":1,"size":11.3385826771654,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(248,118,109,1)"}},"hoveron":"points","name":"No","legendgroup":"No","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[183,50,184,69,126,31,77,442,305.5],"y":[1,2,3,4,5,6,7,8,9],"text":["average: 183.000000<br />NOC: AUS<br />Host: Yes","average:  50.000000<br />NOC: BRA<br />Host: Yes","average: 184.000000<br />NOC: CHN<br />Host: Yes","average:  69.000000<br />NOC: ESP<br />Host: Yes","average: 126.000000<br />NOC: GBR<br />Host: Yes","average:  31.000000<br />NOC: GRE<br />Host: Yes","average:  77.000000<br />NOC: KOR<br />Host: Yes","average: 442.000000<br />NOC: RUS<br />Host: Yes","average: 305.500000<br />NOC: USA<br />Host: Yes"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,191,196,1)","opacity":1,"size":11.3385826771654,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,191,196,1)"}},"hoveron":"points","name":"Yes","legendgroup":"Yes","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":43.7625570776256,"r":7.30593607305936,"b":40.1826484018265,"l":43.1050228310502},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"title":{"text":"Host Country Advantage","font":{"color":"rgba(0,0,0,1)","family":"","size":17.5342465753425},"x":0,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-15.9166666666667,463.805555555556],"tickmode":"array","ticktext":["0","100","200","300","400"],"tickvals":[-1.77635683940025e-015,100,200,300,400],"categoryorder":"array","categoryarray":["0","100","200","300","400"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"Average Number of Medals","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,9.6],"tickmode":"array","ticktext":["AUS","BRA","CHN","ESP","GBR","GRE","KOR","RUS","USA"],"tickvals":[1,2,3,4,5,6,7,8,9],"categoryorder":"array","categoryarray":["AUS","BRA","CHN","ESP","GBR","GRE","KOR","RUS","USA"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"NOC","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895},"y":0.93503937007874},"annotations":[{"text":"Host","x":1.02,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"left","yanchor":"bottom","legendTitle":true}],"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","showSendToCloud":false},"source":"A","attrs":{"41a05d554897":{"x":{},"y":{},"colour":{},"type":"scatter"}},"cur_data":"41a05d554897","visdat":{"41a05d554897":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

```r
ggplotly(medalsByColor)
```

<!--html_preserve--><div id="htmlwidget-b512106554821502dc69" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-b512106554821502dc69">{"x":{"data":[{"orientation":"v","width":[0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.899999999999999,0.899999999999999,0.899999999999999],"base":[3805,2194,2062,1364,1030,992,794,760,750,547],"x":[1,2,3,4,5,6,7,8,9,10],"y":[1197,994,1064,620,587,454,510,363,358,371],"text":["reorder(NOC, desc(count)): USA<br />count: 1197<br />medal: Bronze","reorder(NOC, desc(count)): RUS<br />count:  994<br />medal: Bronze","reorder(NOC, desc(count)): GER<br />count: 1064<br />medal: Bronze","reorder(NOC, desc(count)): GBR<br />count:  620<br />medal: Bronze","reorder(NOC, desc(count)): FRA<br />count:  587<br />medal: Bronze","reorder(NOC, desc(count)): ITA<br />count:  454<br />medal: Bronze","reorder(NOC, desc(count)): AUS<br />count:  510<br />medal: Bronze","reorder(NOC, desc(count)): HUN<br />count:  363<br />medal: Bronze","reorder(NOC, desc(count)): SWE<br />count:  358<br />medal: Bronze","reorder(NOC, desc(count)): NED<br />count:  371<br />medal: Bronze"],"type":"bar","marker":{"autocolorscale":false,"color":"rgba(221,132,44,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Bronze","legendgroup":"Bronze","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":[0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.899999999999999,0.899999999999999,0.899999999999999],"base":[1333,974,987,729,567,474,452,328,396,302],"x":[1,2,3,4,5,6,7,8,9,10],"y":[2472,1220,1075,635,463,518,342,432,354,245],"text":["reorder(NOC, desc(count)): USA<br />count: 2472<br />medal: Gold","reorder(NOC, desc(count)): RUS<br />count: 1220<br />medal: Gold","reorder(NOC, desc(count)): GER<br />count: 1075<br />medal: Gold","reorder(NOC, desc(count)): GBR<br />count:  635<br />medal: Gold","reorder(NOC, desc(count)): FRA<br />count:  463<br />medal: Gold","reorder(NOC, desc(count)): ITA<br />count:  518<br />medal: Gold","reorder(NOC, desc(count)): AUS<br />count:  342<br />medal: Gold","reorder(NOC, desc(count)): HUN<br />count:  432<br />medal: Gold","reorder(NOC, desc(count)): SWE<br />count:  354<br />medal: Gold","reorder(NOC, desc(count)): NED<br />count:  245<br />medal: Gold"],"type":"bar","marker":{"autocolorscale":false,"color":"rgba(255,215,0,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Gold","legendgroup":"Gold","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":[0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.899999999999999,0.899999999999999,0.899999999999999],"base":[0,0,0,0,0,0,0,0,0,0],"x":[1,2,3,4,5,6,7,8,9,10],"y":[1333,974,987,729,567,474,452,328,396,302],"text":["reorder(NOC, desc(count)): USA<br />count: 1333<br />medal: Silver","reorder(NOC, desc(count)): RUS<br />count:  974<br />medal: Silver","reorder(NOC, desc(count)): GER<br />count:  987<br />medal: Silver","reorder(NOC, desc(count)): GBR<br />count:  729<br />medal: Silver","reorder(NOC, desc(count)): FRA<br />count:  567<br />medal: Silver","reorder(NOC, desc(count)): ITA<br />count:  474<br />medal: Silver","reorder(NOC, desc(count)): AUS<br />count:  452<br />medal: Silver","reorder(NOC, desc(count)): HUN<br />count:  328<br />medal: Silver","reorder(NOC, desc(count)): SWE<br />count:  396<br />medal: Silver","reorder(NOC, desc(count)): NED<br />count:  302<br />medal: Silver"],"type":"bar","marker":{"autocolorscale":false,"color":"rgba(160,154,149,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Silver","legendgroup":"Silver","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":43.7625570776256,"r":7.30593607305936,"b":40.1826484018265,"l":48.9497716894977},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"title":{"text":"Top 10 Medal Winning NOCs, Summer Games","font":{"color":"rgba(0,0,0,1)","family":"","size":17.5342465753425},"x":0,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,10.6],"tickmode":"array","ticktext":["USA","RUS","GER","GBR","FRA","ITA","AUS","HUN","SWE","NED"],"tickvals":[1,2,3,4,5,6,7,8,9,10],"categoryorder":"array","categoryarray":["USA","RUS","GER","GBR","FRA","ITA","AUS","HUN","SWE","NED"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"NOC","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-250.1,5252.1],"tickmode":"array","ticktext":["0","1000","2000","3000","4000","5000"],"tickvals":[2.8421709430404e-014,1000,2000,3000,4000,5000],"categoryorder":"array","categoryarray":["0","1000","2000","3000","4000","5000"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"Numbers of Medals","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895},"y":0.93503937007874},"annotations":[{"text":"medal","x":1.02,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"left","yanchor":"bottom","legendTitle":true}],"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","showSendToCloud":false},"source":"A","attrs":{"41a0913235a":{"x":{},"y":{},"fill":{},"type":"bar"}},"cur_data":"41a0913235a","visdat":{"41a0913235a":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Q6 Data Table 
Here I made a data table of the top 100 most successful athletes, ranked by number of medals won. You can search for the athlete, or sort by sport, NOC, name, sex and total medals won. I chose this data table because it's simple to understand and provides a lot of useful information. 

```r
datatable(head(successPre, 100), options = list(
  order = list(5,'desc')
))
```

<!--html_preserve--><div id="htmlwidget-ed83ffca0802582178cd" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-ed83ffca0802582178cd">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100"],["Swimming","Gymnastics","Gymnastics","Biathlon","Fencing","Gymnastics","Gymnastics","Swimming","Canoeing","Gymnastics","Swimming","Gymnastics","Athletics","Swimming","Swimming","Swimming","Gymnastics","Gymnastics","Shooting","Biathlon","Swimming","Swimming","Gymnastics","Cross Country Skiing","Cross Country Skiing","Gymnastics","Athletics","Fencing","Swimming","Gymnastics","Athletics","Gymnastics","Gymnastics","Gymnastics","Cross Country Skiing","Swimming","Archery","Equestrianism","Short Track Speed Skating","Swimming","Gymnastics","Biathlon","Athletics","Fencing","Cross Country Skiing","Swimming","Gymnastics","Athletics","Speed Skating","Gymnastics","Athletics","Shooting","Swimming","Gymnastics","Gymnastics","Gymnastics","Equestrianism","Fencing","Swimming","Gymnastics","Cross Country Skiing","Alpine Skiing","Swimming","Short Track Speed Skating","Short Track Speed Skating","Athletics","Athletics","Shooting","Fencing","Swimming","Swimming","Fencing","Swimming","Speed Skating","Biathlon","Swimming","Canoeing","Biathlon","Equestrianism","Gymnastics","Canoeing","Cross Country Skiing","Swimming","Gymnastics","Swimming","Gymnastics","Gymnastics","Speed Skating","Short Track Speed Skating","Rowing","Shooting","Swimming","Athletics","Diving","Swimming","Swimming","Swimming","Swimming","Fencing","Cycling"],["USA","RUS","RUS","NOR","ITA","JPN","RUS","USA","GER","JPN","USA","RUS","FIN","USA","USA","USA","TCH","RUS","USA","RUS","RUS","USA","RUS","ITA","NOR","RUS","USA","HUN","USA","HUN","USA","JPN","RUS","BLR","RUS","GER","BEL","GER","CHN","USA","ROU","GER","USA","ITA","SWE","AUS","JPN","JAM","GER","FIN","USA","SWE","AUS","RUS","JPN","RUS","NED","ITA","HUN","RUS","RUS","NOR","USA","KOR","RUS","JAM","JAM","SWE","FRA","USA","NED","FRA","GER","GER","GER","AUS","SWE","GER","GER","HUN","HUN","RUS","USA","SUI","GER","SUI","RUS","GER","USA","ROU","NOR","AUS","FIN","RUS","USA","USA","GBR","AUS","ITA","GBR"],["Michael Fred Phelps, II","Larysa Semenivna Latynina (Diriy-)","Nikolay Yefimovich Andrianov","Ole Einar Bjrndalen","Edoardo Mangiarotti","Takashi Ono","Borys Anfiyanovych Shakhlin","Natalie Anne Coughlin (-Hall)","Birgit Fischer-Schmidt","Sawao Kato","Ryan Steven Lochte","Aleksey Yuryevich Nemov","Paavo Johannes Nurmi","Jennifer Elisabeth \"Jenny\" Thompson (-Cumpelik)","Dara Grace Torres (-Hoffman, -Minas)","Matthew Nicholas \"Matt\" Biondi","Vra slavsk (-Odloilov)","Viktor Ivanovych Chukarin","Carl Townsend Osburn","Aleksandr Vladimirovich Popov","Aleksandr Vladimirovich Popov","Mark Andrew Spitz","Polina Hryhorivna Astakhova","Stefania Belmondo","Marit Bjrgen","Aleksandr Nikolayevich Dityatin","Raymond Clarence \"Ray\" Ewry","Aladr Gerevich (-Gerei)","Gary Wayne Hall, Jr.","gnes Keleti-Srkny (Klein)","Frederick Carlton \"Carl\" Lewis","Akinori Nakayama","Vitaly Venediktovich Shcherbo","Vitaly Venediktovich Shcherbo","Raisa Petrovna Smetanina","Franziska van Almsick","Gerard Theodor Hubert Van Innis","Isabelle Regina Werth","Yang Yang","Shirley Frances Babashoff","Nadia Elena Comneci (-Conner)","Ursula \"Uschi\" Disl","Allyson Michelle Felix","Giulio Gaudini","Edy Sixten Jernberg","Leisel Marie Jones","Eizo Kenmotsu","Merlene Joyce Ottey-Page","Claudia Pechstein","Heikki Ilmari Savolainen","Martin Joseph Sheridan","Alfred Gomer Albert \"Alf\" Swahn","Ian James Thorpe","Yury Yevlampiyevich Titov","Mitsuo Tsukahara","Lyudmila Ivanovna Turishcheva (-Borzova)","Theodora Elisabeth Gerarda \"Anky\" van Grunsven","Maria Valentina Vezzali","Zoltn Imre dn von Halmay","Mikhail Yakovlevich Voronin","Lyubov Ivanovna Yegorova","Kjetil Andr Aamodt","Nathan Ghar-Jun Adrian","Viktor An","Viktor An","Usain St. Leo Bolt","Veronica Angella Campbell-Brown","Gustaf Vilhelm Carlberg","Philippe Louis Eugne Cattiau","Charles Meldrum \"Charlie\" Daniels","Inge de Bruijn","Roger Franois Ducret","Kornelia Ender (-Matthes, -Grummt)","Karin Enke-Kania (-Busch-, -Richter)","Sven Fischer","Dawn Lorraine Fraser","Gert Fridolf Fredriksson","Ricco Gro","Reiner Klimke","Margit Korondi (Kronstein-, -Plachy, -Szalay)","Katalin Kovcs","Galina Alekseyevna Kulakova","Jason Edward Lezak","Eugen Mack","Roland Matthes","Georg \"Georges\" Miez","Sofiya Ivanovna Muratova (Poduzdova-)","Gunda Niemann-Stirnemann-Kleemann","Apolo Anton Ohno","Elisabeta Oleniuc-Lip","Otto Martin Olsen","Susan \"Susie\" O'Neill","Viljo Eino \"Ville\" Ritola (Koukkari-)","Dmitry Ivanovich Sautin","Allison Rodgers Schmitt","Donald Arthur \"Don\" Schollander","Henry Taylor","Petria Ann Thomas (-Jones)","Giovanna Trillini","Bradley Marc Wiggins"],["M","F","M","M","M","M","M","F","F","M","M","M","M","F","F","M","F","M","M","M","M","M","F","F","F","M","M","M","M","F","M","M","M","M","F","F","M","F","F","F","F","F","F","M","M","F","M","F","F","M","M","M","M","M","M","F","F","F","M","M","F","M","M","M","M","M","F","M","M","M","F","M","F","F","M","F","M","M","M","F","F","F","M","M","M","M","F","F","M","F","M","F","M","M","F","M","M","F","F","M"],[28,18,15,13,13,13,13,12,12,12,12,12,12,12,12,11,11,11,11,11,11,11,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>Sport<\/th>\n      <th>NOC<\/th>\n      <th>Name<\/th>\n      <th>Sex<\/th>\n      <th>totalMedals<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"order":[5,"desc"],"columnDefs":[{"className":"dt-right","targets":5},{"orderable":false,"targets":0}],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->
