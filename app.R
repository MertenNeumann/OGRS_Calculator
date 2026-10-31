##############
###Packages###
##############
library("haven")
library("shiny")
library("tidyverse")
library("ggplot2")
library("readxl")
library("ggpubr")

###################
###OGRS function###
###################

# OGRS 3 - Calculator
# by Merten Neumann
#
# based on
#
# Breiling, L., Dahle, K.-P., Oberlader, V. & Rettenberger, M. (2022). 
#   Die deutsche Version der Offender Group Reconviction Scale, Version 3 (OGRS 3) 
#   (Berichte und Materialien (BM-Online), Bd. 30). Wiesbaden: Kriminologische Zentralstelle.
#
# Howard, P., Francis, B., Soothill, K. & Humphreys, L. (2009). 
#   OGRS 3: The revised offender group reconviction scale. London: Ministry of Justice.

ogrs <- function(Data = NA, Item1 = c("m", "f"), 
                 Item2 = NA, Item3 = NA, Item4 = NA, 
                 Item5 = NA, Item6 = c("Violence", "Robbery", "Public_order", "Sexual_no_child",
                                       "Sexual_child", "Soliciting_Prostitution", "Burglary_domestic",
                                       "Burglary_other", "Theft_non_motor", "Handling_Stolen_Goods",
                                       "Fraud_Forgery", "Absconding_Bail_Offences", "Taking_Driving", "Theft_from_Vehicle",
                                       "Other_motoring", "Drink_Driving", "Criminal_Damage", "Drug_import_export_production",
                                       "Drug_possession_supply", "Other"), Shiny_Progress = FALSE) {
  
  comp_ogrs <- function(Item1 = c("m", "f"), 
                        Item2 = NA, Item3 = NA, Item4 = NA, 
                        Item5 = NA, Item6 = c("Violence", "Robbery", "Public_order", "Sexual_no_child",
                                              "Sexual_child", "Soliciting_Prostitution", "Burglary_domestic",
                                              "Burglary_other", "Theft_non_motor", "Handling_Stolen_Goods",
                                              "Fraud_Forgery", "Absconding_Bail_Offences", "Taking_Driving", "Theft_from_Vehicle",
                                              "Other_motoring", "Drink_Driving", "Criminal_Damage", "Drug_import_export_production",
                                              "Drug_possession_supply", "Other")) {
    
    require(tidyverse)
    
    #Compute Copas Rate
    Copas_rate <- log((1 + Item4)/(10 + (Item2 - Item5)))
    
    #Weights for one and 2 years
    A1 <- 1.402562384
    A2 <- 2.121705678
    
    #Copas Rate with corresponding weight
    B1 <- Copas_rate * 1.251124464
    
    #Weights for sanctioning history (only for convictions)
    B2 <- ifelse(Item4 == 0, 0.126142106, 0.463062792)
    
    #Weights based on age and sex
    B3 <- case_when(Item1 == "m" & Item3 >= 10 & Item3 < 12 ~ 0,
                    Item1 == "m" & Item3 >= 12 & Item3 < 14 ~ 0.083922902,
                    Item1 == "m" & Item3 >= 14 & Item3 < 16 ~ 0.075775765,
                    Item1 == "m" & Item3 >= 16 & Item3 < 18 ~ (-0.061594199),
                    Item1 == "m" & Item3 >= 18 & Item3 < 21 ~ (-0.625103618),
                    Item1 == "m" & Item3 >= 21 & Item3 < 25 ~ (-1.051515067),
                    Item1 == "m" & Item3 >= 25 & Item3 < 30 ~ (-1.166679288),
                    Item1 == "m" & Item3 >= 30 & Item3 < 35 ~ (-1.325976554),
                    Item1 == "m" & Item3 >= 35 & Item3 < 40 ~ (-1.368045933),
                    Item1 == "m" & Item3 >= 40 & Item3 < 50 ~ (-1.499690953),
                    Item1 == "m" & Item3 >= 50 ~ (-2.025261458),
                    
                    Item1 == "f" & Item3 >= 10 & Item3 < 12 ~ (-0.785038489),
                    Item1 == "f" & Item3 >= 12 & Item3 < 14 ~ (-0.613852078),
                    Item1 == "f" & Item3 >= 14 & Item3 < 16 ~ (-0.669521331),
                    Item1 == "f" & Item3 >= 16 & Item3 < 18 ~ (-0.959179629),
                    Item1 == "f" & Item3 >= 18 & Item3 < 21 ~ (-0.897480934),
                    Item1 == "f" & Item3 >= 21 & Item3 < 25 ~ (-1.028488454),
                    Item1 == "f" & Item3 >= 25 & Item3 < 30 ~ (-1.052777806),
                    Item1 == "f" & Item3 >= 30 & Item3 < 35 ~ (-1.129127959),
                    Item1 == "f" & Item3 >= 35 & Item3 < 40 ~ (-1.42187494),
                    Item1 == "f" & Item3 >= 40 & Item3 < 50 ~ (-1.524652221),
                    Item1 == "f" & Item3 >= 50 ~ (-2.44983716))
    
    #Weights based on current offence
    B4 <- case_when(Item6 == "Violence" ~ (0),
                    Item6 == "Robbery" ~ (-0.634795912),
                    Item6 == "Public_order" ~ (0.181917975),
                    Item6 == "Sexual_no_child" ~ (0.003276327),
                    Item6 == "Sexual_child" ~ (-0.653434071),
                    Item6 == "Soliciting_Prostitution" ~ (0.760608858),
                    Item6 == "Burglary_domestic" ~ (-0.12394352),
                    Item6 == "Burglary_other" ~ (0.240604429),
                    Item6 == "Theft_non_motor" ~ (0.661244321),
                    Item6 == "Handling_Stolen_Goods" ~ (0.351866973),
                    Item6 == "Fraud_Forgery" ~ (0.159910192),
                    Item6 == "Absconding_Bail_Offences" ~ (0.733378677),
                    Item6 == "Taking_Driving" ~ (0.380059431),
                    Item6 == "Theft_from_Vehicle" ~ (0.427225615),
                    Item6 == "Other_motoring" ~ (0.262228428),
                    Item6 == "Drink_Driving" ~ (-0.121439408),
                    Item6 == "Criminal_Damage" ~ (0.204960477),
                    Item6 == "Drug_import_export_production" ~ (-0.795556373),
                    Item6 == "Drug_possession_supply" ~ (0.077165871),
                    Item6 == "Other" ~ (-0.060667525))
    
    #Compute probabilities
    z1 <- A1 + B1 + B2 + B3 + B4
    z2 <- A2 + B1 + B2 + B3 + B4
    
    Prob1 <- exp(z1)/(1 + exp(z1))
    Prob2 <- exp(z2)/(1 + exp(z2))
    
    Out_vec <- as.numeric(c(Prob1, Prob2, Copas_rate))
    names(Out_vec) <- c("Prob_one_year", "Prob_two_years", "Copas_rate")
    
    Out_vec
    
  }
  
  if(all(is.na(Data))){
    
    Out <- comp_ogrs(Item1 = Item1, Item2 = Item2, Item3 = Item3, Item4 = Item4, Item5 = Item5, Item6 = Item6)
    
  }else{
    
    Out <- matrix(data = rep(NA, times = 3 * nrow(Data)), nrow = nrow(Data), ncol = 3)
    
    if(Shiny_Progress){
      
      withProgress(message = "Computing", value = 0, {
        
        N <- nrow(Data)
        Inc_idx <- ceiling(N/100)
        
        for(i in 1:N) {
          
          Out[i,] <- comp_ogrs(Item1 = Data[i, 1], Item2 = Data[i, 2], Item3 = Data[i, 3], Item4 = Data[i, 4], Item5 = Data[i, 5], Item6 = Data[i, 6])
          
          if(i %% Inc_idx == 0) {setProgress(value = i/N, detail = paste(as.character(round((i/N) * 100, digits = 0)), "%"))}
          
        }
      })
      
    }else{
      
      for(i in 1:nrow(Data)) {
        
        Out[i,] <- comp_ogrs(Item1 = Data[i, 1], Item2 = Data[i, 2], Item3 = Data[i, 3], Item4 = Data[i, 4], Item5 = Data[i, 5], Item6 = Data[i, 6])
        
      }
    }
    
    colnames(Out) <- c("Prob_one_year", "Prob_two_years", "Copas_rate")
    
    Out <- as.data.frame(Out)
    
  }
  
  Out
  
}

#Lookup Item 1
Lookup_item1 <- c("m", "f")
names(Lookup_item1) <- c("Male", "Female")


#Lookups Item 6
Lookup_item6 <- c("Violence", "Robbery", "Public_order", "Sexual_no_child",
                  "Sexual_child", "Soliciting_Prostitution", "Burglary_domestic",
                  "Burglary_other", "Theft_non_motor", "Handling_Stolen_Goods",
                  "Fraud_Forgery", "Absconding_Bail_Offences", "Taking_Driving", "Theft_from_Vehicle",
                  "Other_motoring", "Drink_Driving", "Criminal_Damage", "Drug_import_export_production",
                  "Drug_possession_supply", "Other")
names(Lookup_item6) <- c("Violence", "Robbery", "Public order", "Sexual (not against child)",
                         "Sexual (against child)", "Soliciting or prostitution", "Burglary (domestic)",
                         "Burglary (other)", "Theft (non-motor)", "Handling stolen goods", "Fraud and forgery",
                         "Absconding or bail offences", "Taking & driving away and related offences",
                         "Theft from vehicles", "Other motoring", "Drink driving", "Criminal damage",
                         "Drug import/export/production", "Drug possession/supply", "Other offence")

Lookup_item6_2 <- c("Violence", "Robbery", "Public order", "Sexual (not against child)",
                           "Sexual (against child)", "Soliciting or prostitution", "Burglary (domestic)",
                           "Burglary (other)", "Theft (non-motor)", "Handling stolen goods", "Fraud and forgery",
                           "Absconding or bail offences", "Taking & driving away and related offences",
                           "Theft from vehicles", "Other motoring", "Drink driving", "Criminal damage",
                           "Drug import/export/production", "Drug possession/supply", "Other offence")
names(Lookup_item6_2) <- c("Violence", "Robbery", "Public_order", "Sexual_no_child",
                  "Sexual_child", "Soliciting_Prostitution", "Burglary_domestic",
                  "Burglary_other", "Theft_non_motor", "Handling_Stolen_Goods",
                  "Fraud_Forgery", "Absconding_Bail_Offences", "Taking_Driving", "Theft_from_Vehicle",
                  "Other_motoring", "Drink_Driving", "Criminal_Damage", "Drug_import_export_production",
                  "Drug_possession_supply", "Other")



#Lookup Item names
Lookup_names <- c("Age at the time of the last conviction", "Age at start of the at-risk period",
                  "Number of previous convictions", "Age at the time of the first conviction")
names(Lookup_names) <- c("Item2", "Item3", "Item4", "Item5")



####################
###User Interface###
####################
ui <- navbarPage(
  
  #Application title
  "OGRS 3 - Calculator",
  
  #Position of Navbar
  position = "fixed-top",
  collapsible = TRUE,
  
    
    #Tab for calculating OGRS scores
    tabPanel(title = "Calculate", 
             
             div(style = "height:80px"),
             
             #Sidebar for single Calculation
               sidebarPanel(
                 
                 #Sidebar Title
                 h2("Single calculation"),
                 
                 p("Here you can compute the OGRS 3 scores for a single case."),
                 
                 #Item inputs          
                 selectInput("item1", label = "Item 1 - Sex", choices = list("Male", "Female")),
                 numericInput("item2", label = "Item 2 - Age at the time of the last conviction", value = 20),
                 numericInput("item3", label = "Item 3 -  Age at start of the at-risk period", value = 20),
                 numericInput("item4", label = "Item 4 - Number of previous convictions", value = 0),
                 numericInput("item5", label = "Item 5 - Age at the time of the first conviction", value = 20),
                 selectInput("item6", label = "Item 6 - Principal current offence", choices = list("Violence", "Robbery", "Public order", "Sexual (not against child)",
                                                                                                   "Sexual (against child)", "Soliciting or prostitution", "Burglary (domestic)",
                                                                                                   "Burglary (other)", "Theft (non-motor)", "Handling stolen goods", "Fraud and forgery",
                                                                                                   "Absconding or bail offences", "Taking & driving away and related offences",
                                                                                                   "Theft from vehicles", "Other motoring", "Drink driving", "Criminal damage",
                                                                                                   "Drug import/export/production", "Drug possession/supply", "Other offence")),
                 #Results
                 h3("Results"),
                 textOutput("risk_text1"),
                 textOutput("risk_text2"),
                 textOutput("risk_text3"),
                 
                 div(style = "height:25px")),
               
               #Main panel for calculation on dataset
               mainPanel(
                 
                 #Main panel title
                 h2("Calculation on dataset"),
                 
                 p("This section provides the possibility to calculate OGRS 3 scores for a whole dataset in one go."),
                 
                 #Instructions       
                 p("The data must contain information about the following six items of the OGRS 3:"),
                 p("1. Sex"),
                 p("2. Age at the time of the last conviction"),
                 p("3. Age at start of the at-risk period"),
                 p("4. Number of previous convictions"),
                 p("5. Age at the time of the first conviction"),
                 p("6. Principal current offence"),
                 
                 p("Follow the instructions below to get your OGRS 3 scores."),
                 
                 #Upload  
                 h3("1. Data Upload"),
                 
                 p("Select the format of your data using the drop-down menu. The following formats are supported: ",
                   a(".xlsx (Excel)", href = "https://www.rdocumentation.org/packages/readxl/versions/0.1.1/topics/read_excel"), ", ",
                   a(".csv", href = "https://www.rdocumentation.org/packages/readr/versions/2.1.3/topics/read_delim"), ", ",
                   a(".csv (European)", href = "https://www.rdocumentation.org/packages/readr/versions/2.1.3/topics/read_delim"), ", ",
                   a(".txt", href = "https://www.rdocumentation.org/packages/readr/versions/2.1.3/topics/read_delim"), ", ",
                   a(".sav (SPSS)", href = "https://www.rdocumentation.org/packages/haven/versions/2.5.1/topics/read_spss"), "."),
                 
                 selectInput("data_format", label = "Choose your data format", choices = list("xlsx (Excel)", "csv", "csv (European)", "txt", "sav (SPSS)")),
                 
                 p("Use this button to upload your data."),
                 fileInput("ogrs_input", label = "Upload dataset", accept = c(".xlsx", ".csv", ".txt", ".sav")),
                 
                 #Variable names
                 h3("2. Choose variable names"),
                 
                 p("Here, you can choose which variables from your dataset are to be used for the calculation. Please write the exact variable name under each OGRS 3 item."),
                 
                 div(style = "height:30px"),
                 
                 splitLayout(cellWidths = c("50%", "50%"),
                             textInput("nameitem1", "Item 1 - Sex", value = "Item1"),
                             textInput("nameitem2", "Item 2 - Age at the time of the last conviction", value = "Item2")),
                 
                 splitLayout(cellWidths = c("50%", "50%"),
                             textInput("nameitem3", "Item 3 - Age at start of the at-risk period", value = "Item3"),
                             textInput("nameitem4", "Item 4 - Number of previous convictions", value = "Item4")),
                 
                 splitLayout(cellWidths = c("50%", "50%"),
                             textInput("nameitem5", "Item 5 - Age at the time of the first conviction", value = "Item5"),
                             textInput("nameitem6", "Item 6 - Principal current offence", value = "Item6")),
                 
                 #Category Labels
                 h3("3. Choose category labels"),
                 
                 p("The items 1 and 6 of the OGRS 3 are categorical. Please use the following text inputs to
                    specify the category labels you used in your data."),
                 p("CAUTION: You need to use the numeric values if you upload a .sav-file and 
                    your categorical variables are labelled numerics in SPSS"),
                 
                 div(style = "height:20px"),
                 
                 h4("Item 1 - Sex"),
                 
                 splitLayout(cellWidths = c("50%", "50%"),
                             textInput("labelitem1_1", "Male", value = "Male"),
                             textInput("labelitem1_2", "Female", value = "Female")),
                 
                 h4("Item 6 - Principal current offence"),
                 
                 splitLayout(cellWidths = c("50%", "50%"),
                             textInput("labelitem6_1", "Violence", value = "Violence"),
                             textInput("labelitem6_2", "Robbery", value = "Robbery")),
                 
                 splitLayout(cellWidths = c("50%", "50%"),
                             textInput("labelitem6_3", "Public order", value = "Public order"),
                             textInput("labelitem6_4", "Sexual (not against child)", value = "Sexual (not against child)")),
                 
                 splitLayout(cellWidths = c("50%", "50%"),
                             textInput("labelitem6_5", "Sexual (against child)", value = "Sexual (against child)"),
                             textInput("labelitem6_6", "Soliciting or prostitution", value = "Soliciting or prostitution")),
                 
                 splitLayout(cellWidths = c("50%", "50%"),
                             textInput("labelitem6_7", "Burglary (domestic)", value = "Burglary (domestic)"),
                             textInput("labelitem6_8", "Burglary (other)", value = "Burglary (other)")),
                 
                 splitLayout(cellWidths = c("50%", "50%"),
                             textInput("labelitem6_9", "Theft (non-motor)", value = "Theft (non-motor)"),
                             textInput("labelitem6_10", "Handling stolen goods", value = "Handling stolen goods")),
                 
                 splitLayout(cellWidths = c("50%", "50%"),
                             textInput("labelitem6_11", "Fraud and forgery", value = "Fraud and forgery"),
                             textInput("labelitem6_12", "Absconding or bail offences", value = "Absconding or bail offences")),
                 
                 splitLayout(cellWidths = c("50%", "50%"),
                             textInput("labelitem6_13", "Taking & driving away and related offences", value = "Taking & driving away and related offences"),
                             textInput("labelitem6_14", "Theft from vehicles", value = "Theft from vehicles")),
                 
                 splitLayout(cellWidths = c("50%", "50%"),
                             textInput("labelitem6_15", "Other motoring", value = "Other motoring"),
                             textInput("labelitem6_16", "Drink driving", value = "Drink driving")),
                 
                 splitLayout(cellWidths = c("50%", "50%"),
                             textInput("labelitem6_17", "Criminal damage", value = "Criminal damage"),
                             textInput("labelitem6_18", "Drug import/export/production", value = "Drug import/export/production")),
                 
                 splitLayout(cellWidths = c("50%", "50%"),
                             textInput("labelitem6_19", "Drug possession/supply", value = "Drug possession/supply"),
                             textInput("labelitem6_20", "Other offence", value = "Other offence")),
                 
                 #Compute Scores
                 h3("4. Compute scores"),
                 
                 p("Use this button to compute the risk scores. This may take a moment (depending on the size of your data). 
                   Observe the progress bar in the lower right-hand corner to check on the progress of the calculation."),
                 
                 actionButton("calc", "Calculate OGRS 3 scores"),
                 
                 div(style = "height:30px"),
                 
                 #Diagnostics
                 h3("5. Check the distributions"),
                 
                 p("The plot shows the distribution of the chosen output within your sample. Use it to check if the computation was successful."),
                 
                 radioButtons("risk_idx", "Which output?", 
                              choices = list("One year" = "Prob_one_year", "Two years" = "Prob_two_years", "Copas rate" = "Copas_rate")),
                 plotOutput("Distplot"),
                 
                 #Download
                 h3("6. Download"),
                 
                 div(style = "border-style: solid; border-color: black; border-width: 2px; padding: 10px; border-radius: 8px;",
                     p("This button can be used to save the computed OGRS 3 scores as a .csv-file (European)."),
                     downloadButton("downloadData", "Download")),
                 
                 div(style = "height:30px"))),
               
    #Tab to explore OGRS distributions           
    tabPanel(title = "Explore",
             
             div(style = "height:70px"),
             
             p("This section can be used to explore the impact of different input variables on the distribution of OGRS 3 scores. Use the sidebar
               on the left side to customize your plots."),
             
               #Sidebar to choose inputs
               sidebarPanel(
                 style = "height: 90vh; overflow-y: auto;",
                 
                 h2("Specify the inputs"),
                 
                 #Item on the x-axis
                 h3("Which item should be displayed on the x-axis?"),
                 
                 radioButtons("inputX", "Choose one continuous item", choices = list(
                   "Item 2" = "Item2",
                   "Item 3" = "Item3",
                   "Item 4" = "Item4",
                   "Item 5" = "Item5"),
                   selected = "Item4"),
                 
                 #Fixed inputs
                 h3("Which other inputs should be used for calculation?"),
                 
                  checkboxGroupInput("checkItem1", "Item 1 - Sex", choices = list(
                               "Male" = "m",
                               "Female" = "f"),
                               selected = "m"), 
                 
                 sliderInput("slideItem2", "Item 2 - Age at the time of the last conviction", min = 14, max = 90, value = 20),
                 sliderInput("slideItem3", "Item 3 - Age at start of the at-risk period", min = 14, max = 90, value = 20),
                 sliderInput("slideItem4", "Item 4 - Number of previous convictions", min = 0, max = 50, value = 0),
                 sliderInput("slideItem5", "Item 5 - Age at the time of the first conviction", min = 14, max = 90, value = 20),
                 
                  includeScript(path = "js4checkbox.js"),
                  checkboxGroupInput("checkItem6", "Item 6 - Principal current offence", choices = list(
                               "Violence" = "Violence", 
                               "Robbery" = "Robbery", 
                               "Public order" = "Public_order", 
                               "Sexual (not against child)" = "Sexual_no_child",
                               "Sexual (against child)" = "Sexual_child", 
                               "Soliciting or prostitution" = "Soliciting_Prostitution", 
                               "Burglary (domestic)" = "Burglary_domestic",
                               "Burglary (other)" = "Burglary_other", 
                               "Theft (non-motor)" = "Theft_non_motor", 
                               "Handling stolen goods" = "Handling_Stolen_Goods", 
                               "Fraud and forgery" = "Fraud_Forgery",
                               "Absconding or bail offences" = "Absconding_Bail_Offences", 
                               "Taking & driving away and related offences" = "Taking_Driving",
                               "Theft from vehicles" = "Theft_from_Vehicle", 
                               "Other motoring" = "Other_motoring", 
                               "Drink driving" = "Drink_Driving", 
                               "Criminal damage" = "Criminal_Damage",
                               "Drug import/export/production" = "Drug_import_export_production", 
                               "Drug possession/supply" = "Drug_possession_supply", 
                               "Other offence" = "Other"),
                               selected = "Violence")),
               
               #Main panel for the plot
               mainPanel(plotOutput("Explore_Plot"))),
    
    #Tab for explanations about the page
    tabPanel(title = "About",
             
             div(style = "height:70px"),
             
             p("This OGRS 3 calculator is mainly based on the German manual by", 
               a("Breiling, Dahle, Oberlader and Rettenberger (2022).", href = "https://www.krimz.de/publikationen/bm-online/bm-online-30.html"),  
               "The only difference to the original version described by", 
               a("Howard, Francis, Soothill and Humphreys (2009)", href = "https://www.researchgate.net/publication/258365273_OGRS3_the_revised_Offender_Group_Reconviction_Score"), 
               "concerns the sanctions. As most research in Germany is conducted based on official criminal records, 
                it is common practice to only use registered convictions. Thus, this calculator does not provide the option to include other sanctioning occasions."),
             p("Please do not upload more data than necessary and adhere to existing data protection regulations.
               This app is running on shinyapps.io. More information regarding this cloud service can be found", 
                   a("here.", href = "https://docs.rstudio.com/shinyapps.io/security-and-compliance.html")),
             p("The R-function that is used to calculate the OGRS 3 scores in this app is accessible on  ", a("github.", href = "https://github.com/MertenNeumann/OGRS_Calculator")),
               div(style = "height:30px"),
              p("Have Fun!"),
              p("Merten Neumann"),
              p(a("Researchgate", href = "https://www.researchgate.net/profile/Merten-Neumann"), ",",
                a("Scholar", href = "https://scholar.google.com/citations?user=QjfiK8gAAAAJ&hl=de"))
  
 )
)

##################
###Server logic###
##################

server <- function(input, output) {
  
  #Outputs single calculation
  output$risk_text1 <- renderText({
    
    item1_clean <- unname(Lookup_item1[input$item1])
    
    item6_clean <- unname(Lookup_item6[input$item6])
    
    Help1 <- ogrs(Data = NA, Item1 = item1_clean, Item2 = input$item2, Item3 = input$item3, Item4 = input$item4, Item5 = input$item5, Item6 = item6_clean)
    
    paste(c("One year = ", round(Help1[1] * 100, digits = 2), "%"), collapse = "")

    
  })
  
  output$risk_text2 <- renderText({
    
    item1_clean <- unname(Lookup_item1[input$item1])
    
    item6_clean <- unname(Lookup_item6[input$item6])
    
    Help2 <- ogrs(Data = NA, Item1 = item1_clean, Item2 = input$item2, Item3 = input$item3, Item4 = input$item4, Item5 = input$item5, Item6 = item6_clean)
    
    paste(c("Two years = ", round(Help2[2] * 100, digits = 2), "%"), collapse = "")

    
  })
  
  output$risk_text3 <- renderText({
    
    item1_clean <- unname(Lookup_item1[input$item1])
    
    item6_clean <- unname(Lookup_item6[input$item6])
    
    Help3 <- ogrs(Data = NA, Item1 = item1_clean, Item2 = input$item2, Item3 = input$item3, Item4 = input$item4, Item5 = input$item5, Item6 = item6_clean)
    
    paste(c("Copas rate = ", round(Help3[3], digits = 2)), collapse = "")

  })
    
  #Compute OGRS on dataset
  Dist_risk <- eventReactive(input$calc, {
    
    req(input$ogrs_input)
    
    Var_idx <- c(input$nameitem1, input$nameitem2, input$nameitem3, input$nameitem4, input$nameitem5, input$nameitem6)
    
    #xlsx
    if(input$data_format == "xlsx (Excel)") {
      
      ogrsData_raw <- read_excel(input$ogrs_input$datapath) %>% 
        dplyr::select(all_of(Var_idx)) %>% 
        as.data.frame()
      
      names(ogrsData_raw) <- c("Item1", "Item2", "Item3", "Item4", "Item5", "Item6")
      
    }
    
    #csv
    if(input$data_format == "csv") {
      
      ogrsData_raw <- read_csv(input$ogrs_input$datapath) %>% 
        dplyr::select(all_of(Var_idx)) %>% 
        as.data.frame()
      
      names(ogrsData_raw) <- c("Item1", "Item2", "Item3", "Item4", "Item5", "Item6")
      
    }
    
    #csv2
    if(input$data_format == "csv (European)") {
      
      ogrsData_raw <- read_csv2(input$ogrs_input$datapath) %>% 
        dplyr::select(all_of(Var_idx)) %>% 
        as.data.frame()
      
      names(ogrsData_raw) <- c("Item1", "Item2", "Item3", "Item4", "Item5", "Item6")
      
    }
    
    #txt
    if(input$data_format == "txt") {
      
      ogrsData_raw <- read_delim(input$ogrs_input$datapath) %>% 
        dplyr::select(all_of(Var_idx)) %>% 
        as.data.frame()
      
      names(ogrsData_raw) <- c("Item1", "Item2", "Item3", "Item4", "Item5", "Item6")
      
    }
    
    #sav  
    if(input$data_format == "sav (SPSS)") {
      
      ogrsData_raw <- read_sav(input$ogrs_input$datapath) %>% 
        dplyr::select(all_of(Var_idx)) %>% 
        as.data.frame()
      
      names(ogrsData_raw) <- c("Item1", "Item2", "Item3", "Item4", "Item5", "Item6")
      
    }
    
    #Change labels
    ogrsData <- ogrsData_raw %>% 
      mutate(Item1 = as.character(Item1)) %>% 
      mutate(Item1 = case_when(Item1 == input$labelitem1_1 ~ "m", Item1 == input$labelitem1_2 ~ "f")) %>%
      mutate(Item6 = as.character(Item6)) %>% 
      mutate(Item6 = case_when(Item6 == input$labelitem6_1 ~ "Violence",
                               Item6 == input$labelitem6_2 ~ "Robbery",
                               Item6 == input$labelitem6_3 ~ "Public_order",
                               Item6 == input$labelitem6_4 ~ "Sexual_no_child",
                               Item6 == input$labelitem6_5 ~ "Sexual_child",
                               Item6 == input$labelitem6_6 ~ "Soliciting_Prostitution",
                               Item6 == input$labelitem6_7 ~ "Burglary_domestic",
                               Item6 == input$labelitem6_8 ~ "Burglary_other",
                               Item6 == input$labelitem6_9 ~ "Theft_non_motor",
                               Item6 == input$labelitem6_10 ~ "Handling_Stolen_Goods",
                               Item6 == input$labelitem6_11 ~ "Fraud_Forgery",
                               Item6 == input$labelitem6_12 ~ "Absconding_Bail_Offences",
                               Item6 == input$labelitem6_13 ~ "Taking_Driving",
                               Item6 == input$labelitem6_14 ~ "Theft_from_Vehicle",
                               Item6 == input$labelitem6_15 ~ "Other_motoring",
                               Item6 == input$labelitem6_16 ~ "Drink_Driving",
                               Item6 == input$labelitem6_17 ~ "Criminal_Damage",
                               Item6 == input$labelitem6_18 ~ "Drug_import_export_production",
                               Item6 == input$labelitem6_19 ~ "Drug_possession_supply",
                               Item6 == input$labelitem6_20 ~ "Other"))
    
    #Compute scores
    ogrsData_out <- ogrs(Data = ogrsData, Shiny_Progress = TRUE)
    
    ogrsData_out
    
    
  })
  
  #Display distribution of computed scores
  output$Distplot <- renderPlot({
    
    Data_Plot <- Dist_risk()
    Column_Plot <- input$risk_idx
    
    ggplot(data = Data_Plot, aes_string(x = Column_Plot)) +
      geom_histogram(aes(y = ..density..), fill = "lightgrey", color = "black") +
      geom_density(color = "red") +
      annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1, size = 7, 
               label = paste0("N = ", nrow(Data_Plot), "\n",
                              "NA = ", sum(is.na(Data_Plot[, Column_Plot])), " (", round((sum(is.na(Data_Plot[, Column_Plot]))/nrow(Data_Plot)) * 100, digits = 1), "%)", "\n",
                              "M = ", round(mean(Data_Plot[, Column_Plot], na.rm = TRUE), digits = 2), "\n",
                              "Med = ", round(median(Data_Plot[, Column_Plot], na.rm = TRUE), digits = 2), "\n",
                              "SD = ", round(sd(Data_Plot[, Column_Plot], na.rm = TRUE), digits = 2))) +
      xlab(input$risk_idx) +
      theme_classic(base_size = 18)
    
  })
  
  #Download dataset
  output$downloadData <- downloadHandler(
    filename = "OGRS_Output.csv",
    content = function(file) {
      
      req(input$ogrs_input)
      
      Var_idx <- c(input$nameitem1, input$nameitem2, input$nameitem3, input$nameitem4, input$nameitem5, input$nameitem6)
      
      Data_Download <- Dist_risk()
      
      write.csv2(Data_Download, file, row.names = FALSE)
      
    })
  
  #Produce plots to investigate OGRS scores
  output$Explore_Plot <- renderPlot({
    
    plotInputList <- list("Item1" = input$checkItem1, "Item2" = input$slideItem2, "Item3" = input$slideItem3, 
                          "Item4" = input$slideItem4, "Item5" = input$slideItem5, "Item6" = input$checkItem6)
    
    #Multiple Groups Item 1 & Item 6
    if(length(input$checkItem1) > 1 & length(input$checkItem6) > 1) {
      
      if(input$inputX == "Item2" | input$inputX == "Item3" | input$inputX == "Item5"){
        
        plotInputList[[input$inputX]] <- c(14:90)
        
      }
      if(input$inputX == "Item4"){
        
        plotInputList[[input$inputX]] <- c(0:50)
        
      }
      
      PlotData_base <- expand.grid(plotInputList)
      
      PlotData_ogrs <- ogrs(Data = PlotData_base, Shiny_Progress = TRUE)
      
      PlotData <- cbind(PlotData_base, PlotData_ogrs)
      ##############
      
      Exp_Plot1 <- ggplot(data = PlotData, aes_string(x = input$inputX, y = "Copas_rate")) +
        geom_line(size = 0.75) +
        ylab("Copas rate") +
        xlab(Lookup_names[input$inputX]) +
        theme_classic(base_size = 18)
      
      Exp_Plot2 <- ggplot(data = PlotData, aes_string(x = input$inputX, y = "Prob_one_year")) +
        geom_line(aes(color = Item6), size = 0.75) +
        facet_grid(rows = vars(Item1), labeller = function(variable, value) {return(list("m" = "Male", "f" = "Female")[value])}) +
        ylab("Predicted probability (One year)") +
        xlab(Lookup_names[input$inputX]) +
        scale_color_brewer(palette = "Dark2", name = NULL, labels = Lookup_item6_2[input$checkItem6]) +
        theme_classic(base_size = 18)
      
    }
    
    #Multiple Groups Item 1
    if(length(input$checkItem1) > 1 & length(input$checkItem6) == 1) {
      
      if(input$inputX == "Item2" | input$inputX == "Item3" | input$inputX == "Item5"){
        
        plotInputList[[input$inputX]] <- c(14:90)
        
      }
      if(input$inputX == "Item4"){
        
        plotInputList[[input$inputX]] <- c(0:50)
        
      }
      
      PlotData_base <- expand.grid(plotInputList)
      
      PlotData_ogrs <- ogrs(Data = PlotData_base, Shiny_Progress = TRUE)
      
      PlotData <- cbind(PlotData_base, PlotData_ogrs)
      ##############
      
      Exp_Plot1 <- ggplot(data = PlotData, aes_string(x = input$inputX, y = "Copas_rate")) +
        geom_line(size = 0.75) +
        ylab("Copas rate") +
        xlab(Lookup_names[input$inputX]) +
        theme_classic(base_size = 18)
      
      Exp_Plot2 <- ggplot(data = PlotData, aes_string(x = input$inputX, y = "Prob_one_year")) +
        geom_line(aes(color = Item1), size = 0.75) +
        ylab("Predicted probability (One year)") +
        xlab(Lookup_names[input$inputX]) +
        scale_color_brewer(palette = "Dark2", name = NULL, labels = c("Male", "Female")) +
        theme_classic(base_size = 18)
      
    }
    
    #Multiple Groups Item 6
    if(length(input$checkItem1) == 1 & length(input$checkItem6) > 1) {
      
      if(input$inputX == "Item2" | input$inputX == "Item3" | input$inputX == "Item5"){
        
        plotInputList[[input$inputX]] <- c(14:90)
        
      }
      if(input$inputX == "Item4"){
        
        plotInputList[[input$inputX]] <- c(0:50)
        
      }
      
      PlotData_base <- expand.grid(plotInputList)
      
      PlotData_ogrs <- ogrs(Data = PlotData_base, Shiny_Progress = TRUE)
      
      PlotData <- cbind(PlotData_base, PlotData_ogrs)
      ##############
      
      Exp_Plot1 <- ggplot(data = PlotData, aes_string(x = input$inputX, y = "Copas_rate")) +
        geom_line(size = 0.75) +
        ylab("Copas rate") +
        xlab(Lookup_names[input$inputX]) +
        theme_classic(base_size = 18)
      
      Exp_Plot2 <- ggplot(data = PlotData, aes_string(x = input$inputX, y = "Prob_one_year")) +
        geom_line(aes(color = Item6), size = 0.75) +
        ylab("Predicted probability (One year)") +
        xlab(Lookup_names[input$inputX]) +
        scale_color_brewer(palette = "Dark2", name = NULL, labels = Lookup_item6_2[input$checkItem6]) +
        theme_classic(base_size = 18)
      
    }
    
    #One group
    if(length(input$checkItem1) == 1 & length(input$checkItem6) == 1) {
      
      if(input$inputX == "Item2" | input$inputX == "Item3" | input$inputX == "Item5"){
        
        plotInputList[[input$inputX]] <- c(14:90)
        
      }
      if(input$inputX == "Item4"){
        
        plotInputList[[input$inputX]] <- c(0:50)
        
      }
      
      PlotData_base <- expand.grid(plotInputList)
      
      PlotData_ogrs <- ogrs(Data = PlotData_base, Shiny_Progress = TRUE)
      
      PlotData <- cbind(PlotData_base, PlotData_ogrs)
      ##############
      
      Exp_Plot1 <- ggplot(data = PlotData, aes_string(x = input$inputX, y = "Copas_rate")) +
        geom_line(size = 0.75) +
        ylab("Copas rate") +
        xlab(Lookup_names[input$inputX]) +
        theme_classic(base_size = 18)
      
      Exp_Plot2 <- ggplot(data = PlotData, aes_string(x = input$inputX, y = "Prob_one_year")) +
        geom_line(size = 0.75) +
        ylab("Predicted probability (One year)") +
        xlab(Lookup_names[input$inputX]) +
        theme_classic(base_size = 18)
      
    }
    
    Exp_Plot <- ggarrange(Exp_Plot1, Exp_Plot2, nrow = 2, heights = c(1, 1.5))
    
    Exp_Plot
    
  }, height = 700)
}

#########################
###Run the application###
#########################

shinyApp(ui = ui, server = server)
