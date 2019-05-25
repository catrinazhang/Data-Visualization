server <- function(input, output, session){
  
  data <- reactive({
    x <- df
  })
  
  output$mymap <- renderLeaflet({
    df <- data()
    
    m <- leaflet(data=map3) %>% 
      addTiles() %>% 
      addMarkers(lng = ~lon,
                 lat = ~lat,
                 popup = paste("Country:", map3$from, "<br",
                               "# of Dates:", map3$count, "<br>",
                               "# of Dates:", map3$count, "<br>",
                               "Success Rate:", map3$rate))
    m
  })
  
  # output$usmap <- renderLeaflet({
  #   df <- data()
  #   
  #   mus <- leaflet(data = mus2) %>% 
  #     addTiles() %>% 
  #     addMarkers(lng = ~lon,
  #                lat = ~lat,
  #                popup = paste("Zipcode:", mus2$zipcode, "<br",
  #                              "# of Dates:", mus2$count, "<br>",
  #                              "# of Dates:", mus2$count, "<br>",
  #                              "Success Rate:", mus2$rate))
  #   mus
  # })
  
  output$usmap <- renderLeaflet({
    df <- data()
    content <- paste("Zipcode:", mus2$zipcode, "<br",
            "# of Dates:", mus2$count, "<br>",
            "# of Dates:", mus2$count, "<br>",
            "Success Rate:", mus2$rate)
    
    mus2$ratecat <- ifelse(mus2$rate2 < 0.1, "<10%", ifelse(mus2$rate2 < 0.2, "10%-20%",
                           ifelse(mus2$rate2 < 0.3, "20%-30%", ifelse(mus2$rate2 < 0.4, "30%-40%",
                            ifelse(mus2$rate2 < 0.5, "40%-50%", ">50%")))))
    
    palette = colorFactor("Dark2", domain = mus2$ratecat) # Grab a palette
    colorscheme = palette(mus2$ratecat)
  
    mus <- leaflet(data = mus2) %>% 
      addTiles()
    popmus <- mus %>% addCircles(lng = ~lon, lat = ~lat, weight = 8, color = colorscheme, popup = content) %>% 
      addLegend(pal = palette, values = ~mus2$ratecat, title = "Match Rate")
    
    cluster <- popmus %>% addCircleMarkers(color = colorscheme, 
                                                   popup = content,
                                                   clusterOptions = markerClusterOptions())
    
    cluster
  })
  
  output$coolplot <- renderPlot({
    filtered <-
      ip %>%
      filter(age >= input$targetAge[1],
             age <= input$targetAge[2],
             gender == input$sexInput,
             goal == input$goalInput
      )
    f2 <- filtered %>% 
      gather(attributes, rating, attractive:interests)
    
    f3 <- f2 %>% 
      group_by(age, gender, goal, attributes) %>% 
      summarise(value = mean(rating)) 
    
    f4 <- na.omit(f3)
    
    ggplot(data = f4, aes(x = attributes, y = value, fill = attributes)) +
    geom_bar(stat = "identity") + 
    labs(title = "Relative Attribute Importance",
           x = "Attributes", y = "Relative Importance") +
    theme(legend.position = "none") +
    theme(axis.ticks.y = element_blank(),
          axis.text.y=element_blank()) 
  })
  
  output$plotlee <- renderPlotly({
    df <- data()
    
    df2 <- ggplot(data = career3, aes(career_c, career_partner)) + 
              geom_tile(aes(fill = match_rate), colour = "white") + 
              scale_fill_gradient2(low= "white", mid = "steelblue1", high="black") + 
      labs(title = "Matches By Career",
           subtitle = "Hover cursor over tiles to view details",
           x = "Career", y = "Partner's Career") +
              theme_bw() +
      labs(fill = "Match Rate")
              # theme(axis.text.y = element_blank(),
              #       axis.ticks.y = element_blank(),
              #       axis.text.x = element_blank(),
              #       axis.ticks.x = element_blank())
    
    df3 <- ggplotly(df2)
    df3
  })
    
    output$coolplot <- renderPlot({
      
      filtered <-
        ip %>%
        filter(age >= input$targetAge[1],
               age <= input$targetAge[2],
               gender == input$sexInput,
               goal == input$goalInput
        )
      f2 <- filtered %>% 
        gather(attributes, rating, attractive:interests)
      
      f3 <- f2 %>% 
        group_by(attributes) %>% 
        summarise(value = mean(rating)) 
      
      f4 <- na.omit(f3)
      
      ggplot(data = f4, aes(x = attributes, y = value, fill = attributes)) +
        geom_bar(stat = "identity") + 
        labs(title = "Relative Attribute Importance (Distributed Out of 100)",
             x = "Attributes", y = "Importance (out of 100)") +
        theme(legend.position = "none") +
        scale_fill_brewer(palette="Paired")
        # theme(axis.ticks.y = element_blank(),
        #       axis.text.y=element_blank()) 
    })
    
    output$scat <- renderPlotly({
      attInput2 = ifelse(input$attInput == "attractiveness", "attractiveness", 
                         ifelse(input$attInput == "intelligence", "intelligence",
                                ifelse(input$attInput == "sincerity", "sincerity",
                                ifelse(input$attInput == "fun", "fun", "ambition"))))
      
      if (input$attInput3 == "All") {
        filtered1 <- comps[which(comps$attributes1 == input$attInput & comps$gender %in% c("M","F") & comps$attributes2 == attInput2),]
      } else {
        filtered1 <- comps[which(comps$attributes1 == input$attInput & comps$gender == input$attInput3 & comps$attributes2 == attInput2),]
      }
      #filtered1 <- comps[which(comps$attributes1 == input$attInput & comps$gender == input$attInput3 & comps$attributes2 == attInput2),]
      
      filtered2 <- na.omit(filtered1)
      
      gg <- ggplot(aes(x=date_rating, y=self_rating),data=filtered2) +
        geom_jitter(alpha = 0.6, aes(color = self_rating)) +
        xlab("How people were rated by their date") +
        ylab("How people rated themselves") + theme(legend.position = "none") 
      
      
      # x <- list(
      #   title = "Points the selected gender thinks the opposite sex allocates to attribute"
      # )
      # y <- list(
      #   title = "Points the selected gender allocates to attribute"
      # )
      # 
      # pp <- plot_ly(data = filtered2, x = ~orating, 
      #               y = ~srating,
      #               text = ~paste("self: ", srating, '<br>other:', orating),
      #         mode = "markers", opacity = 0.8, color = ~srating, size = 1) %>% 
      #   layout(xaxis = x, yaxis = y) 
      # 
      # pp
    })
    
    output$plotleecorr <- renderPlotly({
      df <- data()
      
      corr=map[,c('attr_o','sinc_o','intel_o','fun_o','amb_o','shar_o','prob','like')]
      
      row.has.na <- apply(corr, 1, function(x){any(is.na(x))})
      corr <- corr[!row.has.na,]
      names(corr) = c("Attractive","Sincere","Intelligent","Fun","Ambitious","Shared Interets","Prob of (s)he saying yes","Like score")
      
      #jpeg("rplot.jpg")
      cor_tb= cor(corr)
      
      get_upper_tri <- function(cormat){
        cormat[upper.tri(cormat)]<- NA
        return(cormat)
      }
      
      cor_upper= get_upper_tri(cor_tb)
      
      p <- plot_ly(
        x =  colnames(corr), y =  colnames(corr),
        z = cor_upper, type = "heatmap",colors = "Blues"
        
      ) %>%
        layout(
          font = list(size = 10))
      p
    })
    
    output$plotleefeature <- renderPlotly({
      df <- data()
      
      g = ggplot(data=features, aes(x =Label, y= score),size=10 ) +
        geom_bar(stat="identity", fill = "steelblue", width = .5)+ ggtitle("What Attributes Matter in Dating") + ylab("Importance") + 
        coord_flip() + theme(text = element_text(size = 10)) +
        theme(axis.title.y = element_blank())
      
      Label = features$Label
      score = features$score
      
      dp <- ggplotly(g)
        
      dp
    })
    
    output$plotleemap <- renderPlotly({
      df <- data()
      
      p <- plot_geo(country4) %>%
        add_trace(
          z = ~density, color = ~density, colors = 'Blues',
          text = ~COUNTRY, locations = ~CODE, marker = list(line = l)
        ) %>%
        colorbar(title = 'Participant Density') %>%
        layout(
          geo = g
        )
      p
    })
    
    output$plotleesuccess <- renderPlotly({
      df <- data()
      
      p2 <- plot_geo(success4) %>%
        add_trace(
          z = ~rate, color = ~rate, colors = 'Reds',
          text = ~COUNTRY, locations = ~CODE, marker = list(line = l)
        ) %>%
        colorbar(title = 'Success Rates') %>%
        layout(
          geo = g
        )
      p2
    })
    
    output$plotleerace <- renderPlotly({
      df <- data()
      
      colors = c("steelblue", "deepskyblue", "turquoise1", "darkblue", "blue", "cyan1", "blue","white")
      
      p3 <- plot_ly(Racedata7, labels = ~race, values = ~n, type = 'pie',
                    textposition = 'inside',
                    textinfo = 'label+percent',
                    marker = list(colors = colors,
                                  line = list(color = '#FFFFFF', width = 1)),
                    showlegend = FALSE) %>%
        layout(
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      p3
    })
    
    output$plotleeRadarM <- renderPlotly({
      p <- plot_ly(
        type = 'scatterpolar',
        fill = 'toself'
      ) %>%
        add_trace(
          r = c(29.42, 16.04, 19.38, 17.36, 7.69, 10.13, 29.42),
          theta = c('attractiveness','sincerity','intelligence', 'fun', 'ambition', 'shared interests', 'attractiveness'),
          name = 'What Men Thinks'
        ) %>%
        add_trace(
          r = c(38.66, 10.34, 11.94, 18.96, 8.11, 11.99, 29.42),
          theta = c('attractiveness','sincerity','intelligence', 'fun', 'ambition', 'shared interests', 'attractiveness'),
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
    })
    
    output$plotleeRadarF <- renderPlotly({F
      p <- plot_ly(
        type = 'scatterpolar',
        fill = 'toself'
      ) %>%
        add_trace(
          r = c(26.98, 14.71, 16.21, 17.98, 13.81, 10.30, 26.98),
          theta = c('attractiveness','sincerity','intelligence', 'fun', 'ambition', 'shared interests', 'attractiveness'),
          name = 'What Men Thinks'
        ) %>%
        
        add_trace(
          r = c(18.77, 18.21, 21.55,17.25, 12.02, 12.20, 18.77),
          theta = c('attractiveness','sincerity','intelligence', 'fun', 'ambition', 'shared interests', 'attractiveness'),
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
    })
    
    
  
}
