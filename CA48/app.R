#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(janitor)
library(leaflet)
library(gt)
library(broom)

count_reports <- read_csv("raw-data/Count Reports for AC - Sheet1.csv")[-2,]
contest_table <- read_csv("raw-data/contest_table.txt")

count_reports <- count_reports %>% clean_names() %>% na.omit("Description") %>%
  mutate(description = as.numeric(description)) %>%
  mutate(precinct_name = description %% 100000)

contest_table <- contest_table %>% clean_names() %>%
  mutate(precinct_name = as.numeric(precinct_name)) %>%
  mutate(total_votes = absentee_votes + early_votes + election_votes) %>%
  mutate(vote_proportion = total_votes/ballots)

join_table <-  left_join(contest_table, count_reports, by = "precinct_name") 

full_results <- join_table %>%
  filter(contest_title == "UNITED STATES REPRESENTATIVE 48th District") %>%
  filter(ad != "NA") %>%
  filter(candidate_name == "HARLEY ROUDA") %>%
  mutate(turnout = ballots/reg_voters,
         viet_share = alyssa_vietnamese_an/reg_voters,
         latino_share = alyssa_latino_an/reg_voters,
         high_viet = ifelse(viet_share > .5, 1, 0),
         high_latino = ifelse(latino_share > .5, 1, 0),
         viet_candidate = ifelse(ad == 72 || sd == 34, 1, 0)
         )
  

# Define UI for application that draws a histogram
ui <- navbarPage("CA-48 Midterms",
  
  tabPanel("Background",
           h1("Information About this Election"),
           p("Orange County is often considered the prototype of 
white, suburban conservativism. Richard Nixon was born in Yorba Linda. 
             Ronald Reagan referred to the county as \"where good conservatives go to die\".
The Real Housewives are still shooting there, and the people in Arrested Development 
(the banana stand is a bona fide Newport Beach institution) are as advertised.
The area just to the south of Los Angeles - the boundary is referred to as the 
\"Orange Curtain\" - voted Republican in every presidential election 
since 1936, up until 2016. Then in 2018, all four Congressional seats held by 
Republicans in Orange County flipped to the Democratic column, 
leaving the county with an all-blue delegation and leaving many onlookers shocked.
To those in the area, this change was a marker of a long process of 
diversification in the area, with massive growth coming in 
Hispanic-majority cities like Santa and Costa Mesa as well as 
Asian-majority cities like Westminster and Irvine.
"),
           p("The goal for this project is to look at the voting patterns by demographic group 
in California's 48th Congressional District. This seat is currently held by 
Democrat Harley Rouda, who unseated Republican Dana Rohrabacher in the 
2018 midterm elections. (Full disclosure: I worked for the Democratic Congressional 
Campaign Committee on Rep. Rouda's race in 2018.) This seat also overlaps with the 
state's three State Senate seats and ten State Assembly seats that were all up in 2018. 
Additionally, officeholders in all statewide positions were elected in 2018. 
"),
           p("Political scientists have studied whether the race of candidates impacts the 
turnout of voters of the same race. In particular, Bernard Fraga has shown that the race of 
candidates does not affect voter turnout by race, but the concentration of voters of the 
same race in a voter's area does. This project will look at Vietnamese- and 
Asian-American candidates that ran in 2018, and determine whether race played a factor 
in turnout and vote share for those candidates.
")),
  
  tabPanel("Charts", 
  selectInput("lat_ad", label = "Assembly District", choices = full_results$ad),
  selectInput("viet_c", label = "City", choices = full_results$city),
  
  # Application title
  titlePanel("2018 Midterm Elections in California's 48 Congressional District"),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("districtPlot"),
    plotOutput("lat_districtPlot")
  )),
  
  tabPanel("Statistical Analysis",
           gt_output("stats_gt"),
           hr(),
           gt_output("turnout_minority"),
           hr(),
           gt_output("dem_72"),
           hr(),
           p("The findings here are pretty interesting - while Vietnamese voters
             seem to be about as likely as the district as a whole to vote for 
             the Democrat, Harley Rouda, Latino voters are much more likely to 
             vote for the Democrat.")),
  
  tabPanel("Motivation",
           h1("A Four Sentence Summary"),
           p("2018 was a landmark election year, as Democrats retook the majority in the 
             House of Representatives in large part due to high support in the suburbs.
             One of the seats claimed by the Democrats was California's 48th, situated 
             in the heart of Orange County, where voters ousted the 30-year 
             incumbent Dana Rohrabacher for the freshman representative Harley Rouda.
             But CA-48, while having many suburban white constituent cities, also has
             large populations of Latino and Vietnamese voters. In this project, I aim to
             model the effect that these voters had on this election."),
           p("More to come."))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  output$districtPlot <- renderPlot({
    full_results %>% filter(city == input$viet_c,
                            candidate_name == "HARLEY ROUDA") %>%
      ggplot(aes(x = vote_proportion, y = alyssa_vietnamese_an, color = ad)) + geom_point() +
      xlab("Vote Proportion") +
      ylab("Vietnamese Voters") +
      ggtitle("Proportion of Vote for Harley Rouda (D) in
              California's 48th Congressional District by Precinct",
              subtitle = "Sorted by City, 
              Measured Against Number of Vietnamese Voters")
    
  })
  
  output$lat_districtPlot <- renderPlot({
    full_results %>% filter(ad == input$lat_ad,
                            candidate_name == "HARLEY ROUDA") %>%
      ggplot(aes(x = vote_proportion, y = alyssa_latino_an, color = city)) + geom_point() +
      xlab("Vote Proportion") +
      ylab("Vietnamese Voters") +
      ggtitle("Proportion of Latino Vote for Harley Rouda (D) in
              California's 48th Congressional District by Precinct",
              subtitle = "Sorted by Assembly District, 
              Measured Against Number of Latino Voters")
    
  })
  
  output$stats_gt <- render_gt({
    full_results %>%
      lm(vote_proportion ~ high_viet + high_latino, data = ., weights = reg_voters) %>%
      tidy(conf.int = TRUE) %>%
      select(term, estimate, conf.low, conf.high)  %>%
      mutate_if(is.numeric, round, 3) %>%
      gt()
  })

  
  output$turnout_minority <- render_gt({
    full_results %>%
    lm(turnout ~ high_viet + high_latino, data = ., weights = reg_voters) %>%
    tidy(conf.int = TRUE) %>%
    select(term, estimate, conf.low, conf.high)  %>%
    mutate_if(is.numeric, round, 3) %>%
    gt() %>%
    tab_header("Vote Proportion for Democrat by Race of Voters",
            subtitle = "A \" high \" Proportion is 50% ")
  })
  
  output$viet_cand <- render_gt({
    full_results %>%
      filter(viet_candidate == 1) %>%
      lm(turnout ~ high_viet + high_latino, data = ., weights = reg_voters) %>%
      tidy(conf.int = TRUE) %>%
      select(term, estimate, conf.low, conf.high)  %>%
      mutate_if(is.numeric, round, 3) %>%
      gt() %>%
      tab_header("Voter Turnout by Race of Voters",
              subtitle = "A \" high \" Proportion is 50% ")
  })
  
  output$dem_72 <- render_gt({
    dem_72 <- join_table %>%
      filter(contest_id == 1920,
             choice_party == "DEM") %>%
      summarize(voteshare = sum(total_votes)/sum(ballots)) %>%
      pull(voteshare)
    
    dem_34 <- join_table %>%
      filter(contest_id == 1914,
             choice_party == "DEM") %>%
      summarize(voteshare = sum(total_votes)/sum(ballots)) %>%
      pull(voteshare)
    
    dem_48 <- join_table %>%
      filter(contest_id == 1911,
             choice_party == "DEM") %>%
      summarize(voteshare = sum(total_votes)/sum(ballots)) %>%
      pull(voteshare)
    
    join_table %>%
      filter(ad == 72,
             choice_party == "DEM",
             contest_id %in% c(1911, 1920)) %>%
      mutate(diff = ifelse(contest_id == 1920, 
                              vote_proportion - dem_72, vote_proportion - dem_48),
             viet_share = alyssa_vietnamese_an/reg_voters,
             high_viet = ifelse(viet_share > .5, 1, 0)) %>%
      pivot_wider(names_from = contest_id, values_from = diff, names_prefix = "x") %>%
      group_by(precinct_name, high_viet, total_votes) %>%
      mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
      summarize(races_diff = sum(x1911) - sum(x1920)) %>%
      lm(races_diff ~ high_viet, data = ., weights = total_votes) %>%
      tidy(conf.int = TRUE) %>%
      select(term, estimate, conf.low, conf.high)  %>%
      mutate_if(is.numeric, round, 3) %>%
      gt() %>%
      tab_header("Are Vietnamese Voters More Likely to Choose a Vietnamese Candidate?",
                 subtitle = "Survey Says No")
      
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


#tidycensus 
#nhgis
#missouri geographic mapping