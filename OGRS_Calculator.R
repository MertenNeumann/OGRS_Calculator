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
                                       "Drug_possession_supply", "Other")) {
  
  #Calculation for single case
  comp_ogrs <- function(Item1 = c("m", "f"), 
                        Item2 = NA, Item3 = NA, Item4 = NA, 
                        Item5 = NA, Item6 = c("Violence", "Robbery", "Public_order", "Sexual_no_child",
                                              "Sexual_child", "Soliciting_Prostitution", "Burglary_domestic",
                                              "Burglary_other", "Theft_non_motor", "Handling_Stolen_Goods",
                                              "Fraud_Forgery", "Absconding_Bail_Offences", "Taking_Driving", "Theft_from_Vehicle",
                                              "Other_motoring", "Drink_Driving", "Criminal_Damage", "Drug_import_export_production",
                                              "Drug_possession_supply", "Other")) {
    
    #Package
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
    
    #Output as named vector
    Out_vec <- as.numeric(c(Prob1, Prob2, Copas_rate))
    names(Out_vec) <- c("Prob_one_year", "Prob_two_years", "Copas_rate")
    
    Out_vec
    
  }
  
  #Single computation if Data is NA
  if(all(is.na(Data))){
    
    Out <- comp_ogrs(Item1 = Item1, Item2 = Item2, Item3 = Item3, Item4 = Item4, Item5 = Item5, Item6 = Item6)
    
  }else{
    
    #Prepare output with three columns
    Out <- matrix(data = rep(NA, times = 3 * nrow(Data)), nrow = nrow(Data), ncol = 3)
    
    #Loop for all cases in the data input
    for(i in 1:nrow(Data)) {
      
      Out[i,] <- comp_ogrs(Item1 = Data[i, 1], Item2 = Data[i, 2], Item3 = Data[i, 3], Item4 = Data[i, 4], Item5 = Data[i, 5], Item6 = Data[i, 6])
      
    }
    
    #Output as data frame
    colnames(Out) <- c("Prob_one_year", "Prob_two_years", "Copas_rate")
    
    Out <- as.data.frame(Out)
    
  }
  
  Out
  
}
