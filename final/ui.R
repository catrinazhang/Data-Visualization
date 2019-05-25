ui <- dashboardPage(skin = "blue",
  dashboardHeader(title = 'How to Win Your Love: A Speed Dating Experiment', titleWidth = 500
  ),
  
  dashboardSidebar(hr(),
                   sidebarMenu(id ="tabs",
                               menuItem("Introduction",tabName = "introduction", icon = icon("info-circle")),
                               menuItem("Descriptive Statistics", tabName = "ds", icon = icon("book")),
                               menuItem("Hometowns",tabName = "maps",icon = icon("map-marker-alt"),
                                        menuSubItem("World View",tabName = "world",icon = icon("globe")),
                                        menuSubItem("US View", tabName = "usa", icon = icon("map"))),
                               menuItem("What Are They Looking For",tabName = "looking",icon = icon("binoculars")),
                               menuItem("Ratings: Ours and Theirs", tabName = "princess", icon = icon("flask")),
                               menuItem("Finding A Match",tabName = "find",icon = icon("search"),
                                        menuSubItem("Overview", tabName = "overview", icon = icon("eye")),
                                        menuSubItem("Career", tabName = "career", icon = icon("briefcase"))),
                               
      
                               menuItem("How Do You Stack Up?",tabName = "match",icon = icon("heart"))
                   )),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "introduction",
              fluidRow(
                column(12, align = "center",
                       titlePanel(HTML("&hearts;<font face='brush script MT'><font size='35'>&hearts; Speed Dating Experiment &hearts;</font></font>&hearts;")))
              ),
              fluidRow(
                column(12, align = "center",
                       HTML("<font face = 'verdana'>Luqi Chen, Weiting Gu, Zaiyi Liu, Catrina Zhang</font>"))
                
              ),
              br(),
              fluidRow(
                column(10, align = "center",
                img(src = "bobamy2.png", width = "115%", height = "80%"))
              ),
              br(),
              fluidRow(
                box(width = 6, title = "The Experiment", 
                    column(8, align = "center", img(src = "bag.jpg", width = "120%")), br(), br(),
                    column(12, br(), "The experiment was a series of 4-minute speed dates between two individuals of opposite gender. All participants were Columbia University graduate and professional school students. In total there were 21 separate speed dating events (waves) and each individual meets up to 22 others during each event (rounds). After each round, participants have to determine whether or not they are interested in meeting each other again. If both people 'accept', then each is subsequently provided with the other's contact information.")),
                box(width = 6,  title = "The Data",
                    column(8, align = "center", img(src = "columbia.jpg", width = "80%")), br(), br(),
                    column(12, br(), "The dataset was compiled by two Columbia Business School professors, Ray Fisman and Sheena Iyengar. It contains 8,280 rows and 195 columns. Each row represents one speed date, and the columns contain features including gender, race, age, and answers to the survey questions. The main variable of interest is the Yes decision of one participant with respect to another."))
      )),
      tabItem(tabName = "ds",
              fluidRow(
                box(width = 6, title = "Age Distribution Of Participants", img(src = "violins.png", width = "90%"), br(),
                    "The violin plot shows the distribution of age of the participants. Men are on the right and women on the left. The distribution is rather symmetric. Most of the male and female participants were in their early to late twenties, which accords with the fact that the subjects were all students at Columbia University's graduate and professional schools."),
                box(width = 6, title = "Race Distribution Of Participants", plotlyOutput("plotleerace", height= 350), 
                    "The pie chart shows the distribution or race of the participants. European/ Caucasian-American and Asian-American make up most of the sample.")),
              fluidRow(
                box(width = 6, title ="Goals Of The Event", img(src = "goal.png", width = "100%"), br(), br(),
                    "The bar plots shows that the goals for speed dating are similar for male and female, most of them think of it as a fun night or chance to meet new people"),
                box(width = 6, title ="Correlation of Attributes", plotlyOutput("plotleecorr", height = 400))
              )
              ),
      
      tabItem(tabName = "princess",
              
              fluidPage(
                titlePanel("How People Rated Themselves vs. How Their Dates Rated Them"),
                sidebarLayout(
                  sidebarPanel(
                    "Participants rated themselves on these attributes and were then rated by their dates (both on a scale of 1-10). This plot explores whether people tend to be overly confident or overly conservative about how they stack up. Select different genders and attributes to compare!", br(), br(), 
                    radioButtons("attInput3", "Select Gender",
                                 choices = c("All", "M", "F")),
                    
                    radioButtons("attInput", "Select Attribute",
                                 choices = c("intelligence", "attractiveness", "sincerity", "fun", "ambition"))
            
                  ),
                  mainPanel(
                    plotlyOutput("scat", height = 438)
                  )
                )
              )),
      
      
      tabItem(tabName = "looking",
              fluidRow(
                box(width = 6, title = "Attributes Men Think Are Important For A Potential Date", plotlyOutput("plotleeRadarM", height= 350), br(),
                                   "1) Attributes of ambition, fun, and shared interests have similar weights of importance from both the female perspective and the male perspective", br(), br(), 
"2)	Women seem to put more importance on the attributes of sincerity and intelligence and less importance on the attractiveness attribute than men thought", br(), br(),
                                   "3) Men also thought women would put the most importance on the attractiveness attribute, just as women did to men"),
                box(width = 6, title = "Attributes Women Think Are Important For A Potential Date", plotlyOutput("plotleeRadarF", height= 350), br(),
                    "1) Similar to the first plot, ambition, fun, and shared interests have similar weights of importance from both the male perspective and the female perspective", br(), br(),
                    "2) Men tend to value the attributes of sincerity and intelligence more and attractiveness less than women thought", br(), br(),
                    "3)	Compared with the first plot, this time the importance women think men would attach to attractive is greater than men think women do"
                )
              ),
br()
              
      ),
      tabItem(tabName = "find",
              "testing"),
      tabItem(tabName = "overview",
              fluidRow(
                box(width = 10, plotlyOutput("plotleefeature", height= 400), br(), br(),
                    "Talk is cheap! Let's look at some attributes from a classification model and find out what people are picking on when making the ultimate decision whether (s)he is a match or no. Surprisingly, order matters. Maybe it's not all that cliche when people say 'love at first sight'. Being Attractiveness, your anticipation of how you date likes you, shared interests, sincerity, fun, and ambition also stand out among hundreds of other things. Of course, it would be interesting to also look at difference by gender as well, which we will leave for future research.")
              )),
      tabItem(tabName = "career",
              fluidRow(
                box(width = 11,
                    plotlyOutput("plotlee", height = 500), br(),
                    "Are people from certain careers more likely to be matched with each other? Participants select from a list of 16 career choices (including 'other' and 'undecided'). Careers with less than 100 participants were removed and at least 10 dates must have occurred between 'career' and 'partner's career' for that combination to be included in the plot.  The highest match rate occurs between two engineers, followed by psychologist/humanitarian and psychologist/arts. Hover cursor over tiles to view details."))
              ),
      tabItem(tabName = "maps",
              "sup"
      ),
      tabItem(tabName = "world",
              fluidRow(
                box(width = 11,title = "Density Of Participants By Country",plotlyOutput("plotleemap"),
                    "** Density is calculated based on taking the logarithm of the number of participants")
              ),
              fluidRow(
                box(width = 11,title = "Success Rate by Country",plotlyOutput("plotleesuccess"))
              )
              ),
      tabItem(tabName = "usa",
              leafletOutput("usmap", height = 600)
      ),


      tabItem("match",
              fluidPage(
                titlePanel("What's Your Ideal Date Looking For?"),
                fluidRow(
                  box(width = 12, img(src = "bobamy.png", width = "90%"))),
                sidebarLayout(
                  sidebarPanel(
                    #sliderInput("ageInput", "Your Age", 0, 100, c(0, 100), post = " years"),
                    sliderInput("targetAge", "Looking For Age", 18, 37, c(20, 25), post = " years"),
                    radioButtons("sexInput", "Looking for Gender",
                                 choices = c("M", "F")),
                    radioButtons("goalInput", "You Are Looking For",
                                 choices = c("Casual", "Serious"))
                  ),
                  mainPanel(
                    plotOutput("coolplot", height = 305),
                    br(), br(),
                    tableOutput("results")
                  )
                )
              ))
        
)
    
)
  
) 
