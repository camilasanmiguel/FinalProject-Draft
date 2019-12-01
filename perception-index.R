perception_2000 <- perception %>%
  # filters for Latin American countries
  
  filter(state == "Estados Unidos Mexicanos") %>%
  # set the X axis to the countries and y axis to data...
  
  ggplot(aes(x = as.numeric(X2000))) +
  geom_col() +
  # sets the limits on the y axis for the bar plot
  
  scale_y_continuous(limits = c(0, 100)) +
  # appropriate titles and lables for the graph
  
  labs(title = "Latin American Countries' Corruption Perceptions Index", 
       subtitle = "Measures the degree that corruption is percieved, with higher values representing less corruption",
       caption = "Source: Transparency International") +
  xlab(NULL) +
  ylab("Corruption Perceptions Index") 

# creates the rds file for each individual graph that is called in the shiny app

write_rds(perception_2000, "./perception_2000.rds")
