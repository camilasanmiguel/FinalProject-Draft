# the ui and the server
library(readr)


# THE UI
ui <- fluidPage(
  # app title
  navbarPage(
    title = "The Cartel Violence Crisis in Mexico: Public Perceptions of Safety and Border Apprehensions",
    theme = shinytheme("sandstone")),
  
  # first tab (overview)
  
  tabPanel("Overview",
           htmlOutput("summary")
  ),
  # second tab (tab for visualizations of homicides, secuestros, and feminicides)
  tabPanel("Violence Over Time: Homicides and Abductions",
           htmlOutput("summary_b")
  ),
  # third tab (perceptions of insecurity)
  tabPanel(
    title = "Perceptions of Insecurity",
    sidebarLayout(
      sidebarPanel(
        selectInput("state",
                    choices = c("Mexico as a whole" = "Estados Unidos Mexicanos",
                                "Aguascalientes", "Baja California", "Baja California Sur", 
                                "Campeche", "Coahuila de Zaragoza", "Colima", "Chiapas", 
                                "Chihuahua", "Ciudad de México", "Durango", "Guanajuato", 
                                "Guerrero", "Hidalgo", "Jalisco", "México", "Michoacán de Ocampo", 
                                "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla", "Querétaro", 
                                "Quintana Roo", "San Luis Potosí", "Sinaloa", "Sonora", "Tabasco", 
                                "Tamaulipas", "Tlaxcala", "Veracruz de Ignacio de la Llave", 
                                "Yucatán", "Zacatecas"),
                    selected = "All Services",
                    multiple = FALSE),
        h6("Source: INEGI")),
      mainPanel(
        tabsetPanel(
          tabPanel(
            title = "Percentage of citizens over 18 who feel unsafe in their state and country",
            plotlyOutput("plot_perception"))
        )
      )
    )
  ),
  #   fourth tab panel (MAP TAB I GUESS)
  tabPanel(
    title = "Map",
    sidebarLayout(
      sidebarPanel(
        selectInput("Type of violent crime",
                    choices = c("Intentional Homicides",
                                "Abduction",
                                "Feminicides",
                                "Extortion"),
                    selected = "Intentional Homicides",
                    multiple = TRUE),
        h6("Source: CONAPO"))),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Interactive Map of Cartel-Related Crime in Mexico by State",
          plotlyOutput("map_cartel_homicides")
        )
      )
    )
  )
  # FIFTH TAB ON BP APPREHENSIONS
  tabPanel(
    title = "Border Patrol Apprehensions on the Southwest U.S. Border by Year",
    sidebarLayout(
      sidebarPanel(
        selectInput("group",
                    choices = c("Family Units", "Unaccompanied Children", "Single Adults"),
                    selected = "Unaccompanied Children",
                    multiple = FALSE),
        h6("Source: Border Patrol and UMich data people"))),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Border Patrol Apprehensions on the Southwest Border Each Year, By Group",
          plotlyOutput("plot_bp"))
      )
    )
  ),
  tabPanel(
    title = "About",
    htmlOutput("about_me")
  )
)
# i just finished going through and finding exactly
# where i was missing a parentheses and that took so stupidly long im embarrassed.       

############################################
## MY OUTPUTS... IM KIND OF SCARED RIGHT NOW 
############################################

server <- function(input, output) { 
  
  output$summary <- renderUI({
    str1 <- ("There are many reasons around cartel violence and its effects 
                         that result in rises of illegal immigration, particularly in women and children.")
    str2 <- paste("Violence is an intrinsic feature of the trade in illicit drugs. Traffickers use it to settle disputes, 
                                              and a credible threat of violence maintains employee discipline and a semblance of order with suppliers, 
                                              creditors, and buyers. This type of drug trafficking-related violence has occurred routinely and 
                                              intermittently in U.S. cities since the early 1980s.")
    str3 <- paste("The violence now associated with drug trafficking 
                                              organizations (DTOs) in Mexico is of an entirely different scale. In Mexico, the violence is not only 
                                              associated with resolving disputes or maintaining discipline but also has been directed toward the 
                                              government, political candidates, and the media. The excesses of some of 
                                              Mexican cartels' displays of violence through abductions, tortures, and executions are considered exceptional by the typical standards of organized crime.")
    str4 <- paste("Overall, the Latin America region has a significantly higher homicide levels than 
                                            other regions worldwide. According to the U.N.’s Global Study on Homicide 
                                            published in July 2019, with 13% of the world’s population in 2017, Latin America 
                                            had 37% of the world’s intentional homicides.")
    str5 <- ("Data Sources")
    str6 <- paste("Kaplan, Jacob. U.S. Customs and Border Protection Statistics and Summaries. Ann Arbor, MI: 
                                            Inter-university Consortium for Political and Social Research distributor, 2019-04-30. 
                                            https://doi.org/10.3886/E109522V2 Kaplan, Jacob. U.S. Customs and Border Protection Statistics 
                                              and Summaries: family_child_total_monthly_2000_2018.zip. Ann Arbor, MI: Inter-university Consortium 
                                              for Political and Social Research distributor, 2019-04-30. https://doi.org/10.3886/E109522V2-19923")
    str7 <- paste("INEGI, Mexican government agency that performs surveys")
    str8 <- paste("ok")
    str9 <- paste("ok")
    str10 <- paste("ok")
    str11 <- paste("test")
    str12 <- paste("test")
    str13 <- paste("Source: test")
    
    HTML(paste(h1(str1), p(str2), p(str3), p(str4), p(str5), p(str6), p(str7), p(str8), p(str9), p(str10), p(str11), p(str12), h6(str13)))
  })
  
  #### OUTPUT FOR SECOND TAB I THINK #####
  
  output$summary_b <- renderUI({
    str1 <- ("Violence Over Time: Homicides and Abductions")
    str2 <- paste("text")
    str3 <- paste("text")
    str4 <- paste("text")
    str5 <- paste("Data From: CONAPO, Mexican government agency on crime and law enforcement")
    str6 <- paste("All crime data used is taken from the publicly available compilation spreadsheet updated regularly with data by each state's government.")
    
    HTML(paste(h2(str1), p(str2), p(str3), p(str4), h2(str5), p(str6)))
  })
  
  ### THE OUTPUT FOR THE PERCEPTION PLOT        
  output$plot_perception <- renderPlotly({
    
    plot_perception <- perception %>%
      filter(state == input$state) %>%
      ggplot(aes(x = year, y = percent)) + 
      geom_point() + 
      geom_line() +
      labs(title = "Percentage of citizens over 18 who feel unsafe in their state and country",
           subtitle = "Over time, the drug war and its effects have brought the vast 
                         majority of Mexicans, especially in the states most affected by cartel violence, to feel unsafe",
           subtitle = "Data courtesy of Mexican government agency INEGI") + 
      ylab("percent of people who feel unsafe") + 
      xlab("state") + 
      scale_x_continuous(breaks = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)) +
      theme(text = element_text(family = "Times New Roman", size = 14), panel.background = element_blank()) + 
      theme(legend.title=element_blank()) 
    
    ggplotly(plot_perception)
  })
  
  ### MAKING A HOVER, INTERACTIVE MAP ON CARTEL VIOLENCE (first homicides)
  
  output$map_cartel_homicides <- renderPlotly({
    
    map_cartel_homicides <- map_cartel_data_1
    
    
    ggplotly(map_cartel_homicides)
  })
  
  
  ### THE THIRD PLOTS FOR THE BORDER PATROL APPREHENSIONS VISUALIZATIONS
  
  output$plot_bp <- renderPlotly({
    
    plot_bp <- NEWBPDATAFRAME %>%
      ggplot(aes(x = Year, y = Value, color = Sector, group = Sector)) + 
      geom_point() + 
      geom_line() + 
      labs(title = "US Exports to China") + 
      ylab("Millions of USD") + 
      xlab("Year") + 
      theme(text = element_text(family = "Times New Roman", size = 14), panel.background = element_blank()) + 
      theme(legend.title=element_blank()) 
    
    ggplotly(plot_bp)
    
  })
  
  #### OUTPUT FOR INFO TAB #####
  
  output$about_me <- renderUI({
    str1 <- paste("My name is Camila Sanmiguel and I am a sophomore at Harvard College studying
                              History & Literature, passionate about data science and Latin American history.")
    str2 <- paste("Find my website here and CV here.")
    str3 <- paste("The code for this site can be found on this Github.")
    
    HTML(paste(h2(str1), p(str2), p(str3)))
  })
} 




###########################################################################
## Run the application...im scared im scared...this is really the existence i live
##    maybe it works maybe it doesn't only god and preceptor could guess .... ##
# im starting to get existential now
###########################################################################

shinyApp(ui = ui, server = server)
