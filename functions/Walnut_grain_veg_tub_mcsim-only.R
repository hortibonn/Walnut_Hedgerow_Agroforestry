ontogenic_growth_gompertz <- function (max_harvest,
                                       time_to_first_yield_estimate,
                                       time_to_second_yield_estimate,
                                       first_yield_estimate_percent,
                                       second_yield_estimate_percent,
                                       n_years,
                                       var_CV = 0,
                                       no_yield_before_first_estimate = TRUE) 
{
  a = max_harvest
  t1 = time_to_first_yield_estimate
  t2 = time_to_second_yield_estimate
  p2 = second_yield_estimate_percent/100
  if (p2 > 0.999)
    p2 <- 0.999
  if (p2 < 0.001)
    p2 <- 0.001
  if (first_yield_estimate_percent > 99)
    first_yield_estimate_percent <- 99
  if (first_yield_estimate_percent < 1)
    first_yield_estimate_percent <- 1
  p1 = p2*(first_yield_estimate_percent/100)
  if (t1 == t2)
    t2 <- t1 + 1
  c <- sum(log(log(p2)/log(p1))/(t1 - t2)) # why to use sum()? The function only has one element, the output without function sum() is the same
  b <- (-log(p1)/exp(-c * t1))
  gompertz <- function(x) {
    a * exp(-b * exp(-c * x))
  }
  yield_n_years_ideal <- gompertz(1:n_years)
  yield_n_years_real <- unlist(lapply(yield_n_years_ideal,
                                      vv, var_CV = var_CV, n = 1))
  if (no_yield_before_first_estimate & t1 > 1) {
    yield_n_years_real[1:min(c(n_years, t1 - 1))] <- 0
  }
  return(yield_n_years_real)
}

Walnut_grain_veg_tub <- function(
                                 x
                                 ,varnames
                                 # ,crop_rotation
                                 ) {
  
  # list2env(as.list(x), envir = environment())
  # crop_rotation <- strsplit(name["crop_rotation"], ";", fixed = TRUE)[[1]]
  
  #message("Starting DA function...")
  # Get_crop_indices <- function(Crop_rotation, Crop_name, n_years_c) {
  #   Rotation_length <- length(Crop_rotation)
  #   Full_rotation <- rep(Crop_rotation, length.out = n_years_c)
  #   
  #   # Find positions where the specified crop occurs
  #   Crop_indices <- which(Full_rotation == Crop_name)
  #   return(Crop_indices)
  # }

  # helper: where does a crop occur in the repeated rotation?
  # get_indices <- function(rot, crop, n_years) {
  #   full <- rep(rot, length.out = n_years)
  #   which(full == crop)
  # }
  
  
  # Key variables for multiple crop rotation/AF-scenarios ###############################################################
  #Initial creation of the basic variables happens here, before conditional functions,
  #crop indices etc. are then added in the conditional functions, based on the modeled scenario
  Arable_area_AF1 <- field_area_c - AF1_tree_row_area_c - AF1_hedgerow_area_c
  

  # Annual production of each arable crop across a continuous time series (i.e.: as if no crop rotation existed)
  Leek_yield <-vv(leek_yield_p, var_CV_p, n = n_years_c) #[t/ha]
  Maize_yield <-vv(maize_yield_p, var_CV_p, n = n_years_c)
  Carrot_yield <-vv(carrot_yield_p, var_CV_p, n = n_years_c)
  Celeriac_yield <-vv(celeriac_yield_p, var_CV_p, n = n_years_c)
  Potato_yield <-vv(potato_yield_p, var_CV_p, n = n_years_c)
  Wheat_yield <-vv(wheat_yield_p, var_CV_p, n = n_years_c)
  Beans_yield <-vv(beans_yield_p, var_CV_p, n = n_years_c)
  
  arable_crops_yield_disturbance <- chance_event(annual_crop_risk_t, value_if = 1, value_if_not = 0, n = n_years_c)
  
  # Farm-gate price's time series of each arable crop
  Leek_price <- vv(leek_value_p, var_CV_p, n_years_c)
  Maize_price <- vv(maize_value_p, var_CV_p, n_years_c)
  Carrot_price <- vv(carrot_value_p, var_CV_p, n_years_c)
  Celeriac_price <- vv(celeriac_value_p, var_CV_p, n_years_c)
  Potato_price <- vv(potato_value_p, var_CV_p, n_years_c)
  Wheat_price <- vv(wheat_value_p, var_CV_p, n_years_c)
  Beans_price <- vv(beans_value_p, var_CV_p, n_years_c)
  
  # Costs of production of each arable crop across a continuous time series (i.e.: as if no crop rotation existed)
  # cost of labor time
  Labour_costs <- vv(labour_costs_p, var_CV = var_CV_p, n_years_c) #[EUROS/hour]
  # cost of labor employed in arable crop production [EUROS/ha]
  Leek_labour_cost <- vv(leek_labour_p, var_CV = var_CV_p, n = n_years_c) * Labour_costs
  Maize_labour_cost <- vv(maize_labour_p, var_CV = var_CV_p, n = n_years_c) * Labour_costs
  Carrot_labour_cost <- vv(carrot_labour_p, var_CV = var_CV_p, n = n_years_c)* Labour_costs
  Celeriac_labour_cost <- vv(celeriac_labour_p, var_CV = var_CV_p, n = n_years_c) * Labour_costs
  Potato_labour_cost <- vv(potato_labour_p, var_CV = var_CV_p, n = n_years_c) * Labour_costs
  Wheat_labour_cost <- vv(wheat_labour_p, var_CV = var_CV_p, n = n_years_c) * Labour_costs
  Beans_labour_cost <- vv(beans_labour_p, var_CV = var_CV_p, n = n_years_c) * Labour_costs
  
  # cost of material inputs used in arable crop production [EUROS]
  Leek_management_cost <- vv(leek_management_p, var_CV = var_CV_p, n = n_years_c)
  Maize_management_cost <- vv(maize_management_p, var_CV = var_CV_p, n = n_years_c)
  Carrot_management_cost <- vv(carrot_management_p, var_CV = var_CV_p, n = n_years_c)
  Celeriac_management_cost <- vv(celeriac_management_p, var_CV = var_CV_p, n = n_years_c)
  Potato_management_cost <- vv(potato_management_p, var_CV = var_CV_p, n = n_years_c)
  Wheat_management_cost <- vv(wheat_management_p, var_CV = var_CV_p, n = n_years_c)
  Beans_management_cost <- vv(beans_management_p, var_CV = var_CV_p, n = n_years_c)
  
  # Crop indices (serve to determine in which years each arable crop is cultivated)
  
  # Leek_indices1    <- get_indices(crop_rotation, "leek",    n_years_c)
  # Maize_indices1   <- get_indices(crop_rotation, "maize",   n_years_c)
  # Carrot_indices1  <- get_indices(crop_rotation, "carrot",  n_years_c)
  # Celeriac_indices1<- get_indices(crop_rotation, "celeriac",n_years_c)
  # Potato_indices1  <- get_indices(crop_rotation, "potato",  n_years_c)
  # Wheat_indices1   <- get_indices(crop_rotation, "wheat",   n_years_c)
  # Beans_indices1   <- get_indices(crop_rotation, "beans",   n_years_c)
  
  Leek_indices1 <- seq(from = 1, to = n_years_c, by = 6)
  Maize_indices1 <- seq(from = 2, to = n_years_c, by = 6)
  Carrot_indices1 <- seq(from = 3, to = n_years_c, by = 12)
  Celeriac_indices1 <- seq(from = 3+6, to = n_years_c, by = 12)
  Potato_indices1 <- seq(from = 4, to = n_years_c, by = 6)
  Wheat_indices1 <- seq(from = 5, to = n_years_c, by = 6)
  Beans_indices1 <- seq(from = 6, to = n_years_c, by = 6)
  
  # Crop indices (serve to determine in which years each arable crop is cultivated)
  # Leek_indices1 <- Get_crop_indices(treeless_system_crop_rotation_1_c, "Leek", n_years_c)
  #  Maize_indices1 <- Get_crop_indices(treeless_system_crop_rotation_1_c, "Maize", n_years_c)
  #  Carrot_indices1 <- Get_crop_indices(treeless_system_crop_rotation_1_c, "Carrot", n_years_c)
  #  Celeriac_indices1 <- Get_crop_indices(treeless_system_crop_rotation_1_c, "Celeriac", n_years_c)
  #  Potato_indices1 <- Get_crop_indices(treeless_system_crop_rotation_1_c, "Potato", n_years_c)
  #  Wheat_indices1 <- Get_crop_indices(treeless_system_crop_rotation_1_c, "Wheat", n_years_c)
  #  Beans_indices1 <- Get_crop_indices(treeless_system_crop_rotation_1_c, "Beans", n_years_c)

  #Treeless system ###############################################################
  if(treeless_system_crop_rotation_1_c ==1){  
    # Benefits of arable crop production
    ## Crop-specific time series of arable crops' yield
    Leek_yield_CR1 <- rep(0, n_years_c)
    Maize_yield_CR1 <- rep(0, n_years_c)
    Carrot_yield_CR1 <- rep(0, n_years_c)
    Celeriac_yield_CR1 <- rep(0, n_years_c)
    Potato_yield_CR1 <- rep(0, n_years_c)
    Wheat_yield_CR1 <- rep(0, n_years_c)
    Beans_yield_CR1 <- rep(0, n_years_c)
    
    Leek_yield_CR1[Leek_indices1] <- Leek_yield[Leek_indices1] * field_area_c
    Maize_yield_CR1[Maize_indices1] <- Maize_yield[Maize_indices1] * field_area_c
    Carrot_yield_CR1[Carrot_indices1] <- Carrot_yield[Carrot_indices1] * field_area_c
    Celeriac_yield_CR1[Celeriac_indices1] <- Celeriac_yield[Celeriac_indices1] * field_area_c
    Potato_yield_CR1[Potato_indices1] <- Potato_yield[Potato_indices1] * field_area_c
    Wheat_yield_CR1[Wheat_indices1] <- Wheat_yield[Wheat_indices1] * field_area_c
    Beans_yield_CR1[Beans_indices1] <- Beans_yield[Beans_indices1] * field_area_c
    
    Leek_yield_CR1 <- Leek_yield_CR1*(1-arable_crops_yield_disturbance*yield_red_annual_crop_risk_t)
    Maize_yield_CR1 <- Maize_yield_CR1*(1-arable_crops_yield_disturbance*yield_red_annual_crop_risk_t)
    Carrot_yield_CR1 <- Carrot_yield_CR1*(1-arable_crops_yield_disturbance*yield_red_annual_crop_risk_t)
    Celeriac_yield_CR1 <- Celeriac_yield_CR1*(1-arable_crops_yield_disturbance*yield_red_annual_crop_risk_t)
    Potato_yield_CR1 <- Potato_yield_CR1*(1-arable_crops_yield_disturbance*yield_red_annual_crop_risk_t)
    Wheat_yield_CR1 <- Wheat_yield_CR1*(1-arable_crops_yield_disturbance*yield_red_annual_crop_risk_t)
    Beans_yield_CR1 <- Beans_yield_CR1*(1-arable_crops_yield_disturbance*yield_red_annual_crop_risk_t)
    
    ## Crop-specific time series of the income perceived from the sales arable crops' yield 
    Leek_benefit_CR1 <- Leek_price * Leek_yield_CR1
    Maize_benefit_CR1 <- Maize_price * Maize_yield_CR1
    Carrot_benefit_CR1 <- Carrot_price * Carrot_yield_CR1
    Celeriac_benefit_CR1 <- Celeriac_price * Celeriac_yield_CR1
    Potato_benefit_CR1 <- Potato_price * Potato_yield_CR1
    Wheat_benefit_CR1 <- Wheat_price * Wheat_yield_CR1
    Beans_benefit_CR1 <- Beans_price * Beans_yield_CR1
    
    ## Time series of the annual income perceived from the sales of all arable crops' in the treeless system
    Total_benefit_treeless_CR1 <- Leek_benefit_CR1 + Maize_benefit_CR1 + Carrot_benefit_CR1 + Celeriac_benefit_CR1 + Potato_benefit_CR1 + Wheat_benefit_CR1 + Beans_benefit_CR1
    
    # Costs of production of each arable crop, taking into account the crop rotation cycle
    ## cost of labor employed in arable crop production [EUROS]
    Leek_labour_cost_CR1 <- rep(0, n_years_c)
    Maize_labour_cost_CR1 <- rep(0, n_years_c)
    Carrot_labour_cost_CR1 <- rep(0, n_years_c)
    Celeriac_labour_cost_CR1 <- rep(0, n_years_c)
    Potato_labour_cost_CR1 <- rep(0, n_years_c)
    Wheat_labour_cost_CR1 <- rep(0, n_years_c)
    Beans_labour_cost_CR1 <- rep(0, n_years_c)
    Leek_grazing_labour_cost_CR1 <- rep(0, n_years_c)
    
    Leek_labour_cost_CR1[Leek_indices1] <- Leek_labour_cost[Leek_indices1] * field_area_c
    Maize_labour_cost_CR1[Maize_indices1] <- Maize_labour_cost[Maize_indices1] * field_area_c
    Carrot_labour_cost_CR1[Carrot_indices1] <- Carrot_labour_cost[Carrot_indices1] * field_area_c
    Celeriac_labour_cost_CR1[Celeriac_indices1] <- Celeriac_labour_cost[Celeriac_indices1] * field_area_c
    Potato_labour_cost_CR1[Potato_indices1] <- Potato_labour_cost[Potato_indices1] * field_area_c
    Wheat_labour_cost_CR1[Wheat_indices1] <- Wheat_labour_cost[Wheat_indices1] * field_area_c
    Beans_labour_cost_CR1[Beans_indices1] <- Beans_labour_cost[Beans_indices1] * field_area_c
    
    ## cost of material inputs used in arable crop production [EUROS], #includes: seed, insurance, fixed+variable machine cost
    Leek_management_cost_CR1 <- rep(0, n_years_c) 
    Maize_management_cost_CR1 <- rep(0, n_years_c)
    Carrot_management_cost_CR1 <- rep(0, n_years_c)
    Celeriac_management_cost_CR1 <- rep(0, n_years_c)
    Potato_management_cost_CR1 <- rep(0, n_years_c)
    Wheat_management_cost_CR1 <- rep(0, n_years_c)
    Beans_management_cost_CR1 <- rep(0, n_years_c)
    
    Leek_management_cost_CR1[Leek_indices1] <- Leek_management_cost[Leek_indices1] * field_area_c
    Maize_management_cost_CR1[Maize_indices1] <- Maize_management_cost[Maize_indices1] * field_area_c
    Carrot_management_cost_CR1[Carrot_indices1] <- Carrot_management_cost[Carrot_indices1] * field_area_c
    Celeriac_management_cost_CR1[Celeriac_indices1] <- Celeriac_management_cost[Celeriac_indices1] * field_area_c
    Potato_management_cost_CR1[Potato_indices1] <- Potato_management_cost[Potato_indices1] * field_area_c
    Wheat_management_cost_CR1[Wheat_indices1] <- Wheat_management_cost[Wheat_indices1] * field_area_c
    Beans_management_cost_CR1[Beans_indices1] <- Beans_management_cost[Beans_indices1] * field_area_c
    
    ## Total costs of production of each arable crop, taking into account the crop rotation cycle
    Total_Leek_cost_CR1 <- Leek_management_cost_CR1 + Leek_labour_cost_CR1
    Total_Maize_cost_CR1 <- Maize_management_cost_CR1 + Maize_labour_cost_CR1
    Total_Carrot_cost_CR1 <- Carrot_management_cost_CR1 + Carrot_labour_cost_CR1
    Total_Celeriac_cost_CR1 <- Celeriac_management_cost_CR1 + Celeriac_labour_cost_CR1
    Total_Potato_cost_CR1 <- Potato_management_cost_CR1 + Potato_labour_cost_CR1
    Total_Wheat_cost_CR1 <- Wheat_management_cost_CR1 + Wheat_labour_cost_CR1
    Total_Beans_cost_CR1 <- Beans_management_cost_CR1 + Beans_labour_cost_CR1
    
    ## Time series of the annual costs incurred by the production of all arable crops' in the treeless system
    Total_cost_treeless_CR1 <- Total_Leek_cost_CR1 + Total_Maize_cost_CR1 + Total_Carrot_cost_CR1 + Total_Celeriac_cost_CR1 + Total_Potato_cost_CR1 + Total_Wheat_cost_CR1 + Total_Beans_cost_CR1
    
    # Profits of the treeless system (i.e.: the difference between the income and the costs):
    Treeless_bottom_line_benefit <- Total_benefit_treeless_CR1 - Total_cost_treeless_CR1
  }#will only be calculated, if User decides to check the box "Crop Rotation 1"
  
  if(AF1_system_crop_rotation_1_c == 1){
    # Agroforestry system #######################################################
    ## ---- Costs of the AF system ----
    ### ---- Establishment costs ----
    AF1_planning_cost <- rep(0, n_years_c) #FE;Invoice of service provider (planners/consultants), planning the AF system + measuring tree strips using GPS[EURO]
    AF1_tree_cost <- rep(0, n_years_c) # the expenditure on purchasing trees
    AF1_protection_material_cost <- rep(0, n_years_c) #FE; Material cost of tree protection mesh [EURO]
    AF1_field_prep_cost <- rep(0, n_years_c) #cost for subsoiling and harrowing [EURO/ha]
    AF1_tree_planting_cost <- rep(0, n_years_c) #FE; Labour cost for planting one tree [EURO] -
    AF1_weed_protect_cost <- rep(0, n_years_c) #Material cost of weed suppressing fleece [EURO]
    #AF1_compost_cost <- rep(0, n_years_c) #FE; Cost of compost used during planting [EURO]
    #AF1_irrigation_system_cost <- rep(0, n_years_c) #FE; Material and labour cost of installing a drip irrigation system in the tree rows [EURO]
    #AF1_irrigation_planting_cost <- rep(0, n_years_c) #FE; Cost for watering in newly planted trees [EURO]
    
    AF1_planning_cost[1] <- planning_consulting_p + (farmer_planning_time_p * Labour_costs[1])
    #Field prep
    AF1_tree_cost[1] <- walnut_tree_price_p * AF1_num_trees_c + hedgerow_stems_price_p
    AF1_protection_material_cost[1] <-  tree_planting_associated_material_p
    AF1_field_prep_cost[1] <- woody_strip_field_prep_p * field_area_c * Labour_costs[1]
    AF1_tree_planting_cost[1] <- ((tree_planting_p * AF1_num_trees_c) + hedgerow_planting_time_p) * Labour_costs[1]
    #AF1_compost_cost[1] <- compost_planting_tree_p * compost_price_p * AF1_num_trees_c
    #AF1_irrigation_system_cost[1] <- irrigation_sys_install_p
    #AF1_irrigation_planting_cost[1] <- irrigation_planting_tree_p * water_price_p * AF1_num_trees_c
    
    AF1_total_planting_cost <- rep(0, n_years_c)
    AF1_total_planting_cost <- AF1_tree_cost + AF1_protection_material_cost + AF1_field_prep_cost + AF1_tree_planting_cost + AF1_weed_protect_cost
    
    AF1_total_investment_cost <- AF1_planning_cost + AF1_total_planting_cost #Investment cost of AF system implementation
    
    ### ---- Running costs ----
    AF1_maintenance_cost <- rep(0, n_years_c)
    AF1_maintenance_cost[2:n_years_c] <- vv(woody_strip_undergrowth_maintenance_time_p, var_CV_p, n_years_c-1) * field_area_c * Labour_costs[2:n_years_c]
    AF1_maintenance_cost[c(2,3,4,5,6,8)] <- AF1_maintenance_cost[c(2,3,4,5,6,8)] + pruning_time_earlier_p * AF1_num_trees_c * Labour_costs[c(2,3,4,5,6,8)]
    AF1_maintenance_cost[c(10,12)] <- AF1_maintenance_cost[c(10,12)] + pruning_time_later_p * AF1_num_trees_c * Labour_costs[c(10,12)]
    
    AF1_subsidy_application <- rep(0, n_years_c) #FE; Time (regarded as labour cost) spent for application of Eco Scheme subsidy [EURO]
    AF1_subsidy_application <- vv(subsidy_application_p, var_CV_p, n_years_c) * Labour_costs #application subsidy has to be repeated annually 
    
    AF1_annual_irrigation <- rep(0, n_years_c) #FE; Cost of annual irrigation of tree rows [EURO]
    #AF1_annual_irrigation[1:3] <- vv(irrigation_123_p, var_CV = var_CV_p, 3)
    #AF1_annual_irrigation[4:n_years_c] <- vv(irrigation_annual, var_CV = var_CV_p, length(4:n_years_c))
    #AF1_annual_irrigation_cost <- AF1_annual_irrigation * water_price_p
    
    AF1_timber_harvest_cost <- rep(0, n_years_c)
    if (exists("Timber_harvest_indices")) {
      AF1_timber_harvest_cost[Timber_harvest_indices] <- ((stump_removal_time_p*AF1_num_trees_c + stump_chopping_time_p*field_area_c) + tim_harv_time_p*growing_timber_volume[Timber_harvest_indices]) * Labour_costs[Timber_harvest_indices]
    }
    
    ## ---- Trees' impact on arable crops' yield ----
    # proportion of the achievable yield that is not attained due to reduced resources available to alley crops caused by the presence by trees 
    yield_reduc_max_p <- ifelse(yield_reduc_max_p < 0.1, 0.1, yield_reduc_max_p)
    AF1_perc_yield_reduction <- gompertz_yield(
      max_harvest = yield_reduc_max_p,
      time_to_first_yield_estimate = time_to_first_reduction_c,
      time_to_second_yield_estimate = time_to_second_reduction_c,
      first_yield_estimate_percent = perc_max_first_reduction_p,
      second_yield_estimate_percent = perc_max_second_reduction_p,
      n_years = n_years_c,
      var_CV = var_CV_p,
      no_yield_before_first_estimate = FALSE)
    AF1_perc_yield_reduction <- rep(0, n_years_c)
    
    # proportion of the impact of severe disturbance situations on alley crop yields that is avoided due to the higher stability of the agroforestry, as compared to the trelees, system
    AF1_disturb_red <- gompertz_yield(max_harvest = max_AF_disturb_red_t,
                                      time_to_first_yield_estimate = time_first_disturb_red_est_c,
                                      time_to_second_yield_estimate = time_second_disturb_red_est_c,
                                      first_yield_estimate_percent = first_AF_disturb_red_per_p,
                                      second_yield_estimate_percent = second_AF_disturb_red_per_p,
                                      n_years = n_years_c,
                                      var_CV = var_CV_p,
                                      no_yield_before_first_estimate = FALSE)
    AF1_disturb_red[arable_crops_yield_disturbance == 0] <- 0          
    
    ## ---- Arable crops' in the AF system ----
    # Benefits of arable crop production
    ## Crop-specific time series of arable crops' yield
    AF1_Leek_yield_CR1 <- rep(0, n_years_c)
    AF1_Maize_yield_CR1 <- rep(0, n_years_c)
    AF1_Carrot_yield_CR1 <- rep(0, n_years_c)
    AF1_Celeriac_yield_CR1 <- rep(0, n_years_c)
    AF1_Potato_yield_CR1 <- rep(0, n_years_c)
    AF1_Wheat_yield_CR1 <- rep(0, n_years_c)
    AF1_Beans_yield_CR1 <- rep(0, n_years_c)
    
    Leek_Arable_area_AF1 <- rep(Arable_area_AF1, n_years_c) #leek cultivation is affected by additional area restriction, due to the configuration of the harvest machinery
    Leek_Arable_area_AF1[AF1_leek_operational_yield_red_start_c:n_years_c] <- Leek_Arable_area_AF1[AF1_leek_operational_yield_red_start_c:n_years_c] * (1 - AF1_leek_operational_yield_red_c)
    
    AF1_Leek_yield_CR1[Leek_indices1] <- Leek_yield[Leek_indices1] * Leek_Arable_area_AF1[Leek_indices1]
    AF1_Maize_yield_CR1[Maize_indices1] <- Maize_yield[Maize_indices1] * Arable_area_AF1
    AF1_Carrot_yield_CR1[Carrot_indices1] <- Carrot_yield[Carrot_indices1] * Arable_area_AF1
    AF1_Celeriac_yield_CR1[Celeriac_indices1] <- Celeriac_yield[Celeriac_indices1] * Arable_area_AF1
    AF1_Potato_yield_CR1[Potato_indices1] <- Potato_yield[Potato_indices1] * Arable_area_AF1
    AF1_Wheat_yield_CR1[Wheat_indices1] <- Wheat_yield[Wheat_indices1] * Arable_area_AF1
    AF1_Beans_yield_CR1[Beans_indices1] <- Beans_yield[Beans_indices1] * Arable_area_AF1
    
    AF1_Leek_yield_CR1 <- AF1_Leek_yield_CR1*(1-(arable_crops_yield_disturbance*yield_red_annual_crop_risk_t*(1-AF1_disturb_red)))
    AF1_Maize_yield_CR1 <- AF1_Maize_yield_CR1*(1-(arable_crops_yield_disturbance*yield_red_annual_crop_risk_t*(1-AF1_disturb_red)))
    AF1_Carrot_yield_CR1 <- AF1_Carrot_yield_CR1*(1-(arable_crops_yield_disturbance*yield_red_annual_crop_risk_t*(1-AF1_disturb_red)))
    AF1_Celeriac_yield_CR1 <- AF1_Celeriac_yield_CR1*(1-(arable_crops_yield_disturbance*yield_red_annual_crop_risk_t*(1-AF1_disturb_red)))
    AF1_Potato_yield_CR1 <- AF1_Potato_yield_CR1*(1-(arable_crops_yield_disturbance*yield_red_annual_crop_risk_t*(1-AF1_disturb_red)))
    AF1_Wheat_yield_CR1 <- AF1_Wheat_yield_CR1*(1-(arable_crops_yield_disturbance*yield_red_annual_crop_risk_t*(1-AF1_disturb_red)))
    AF1_Beans_yield_CR1 <- AF1_Beans_yield_CR1*(1-(arable_crops_yield_disturbance*yield_red_annual_crop_risk_t*(1-AF1_disturb_red)))
    
    AF1_Leek_yield_CR1 <- AF1_Leek_yield_CR1 * (1 - AF1_perc_yield_reduction)
    AF1_Maize_yield_CR1 <- AF1_Maize_yield_CR1 * (1 - AF1_perc_yield_reduction)
    AF1_Carrot_yield_CR1 <- AF1_Carrot_yield_CR1 * (1 - AF1_perc_yield_reduction)
    AF1_Celeriac_yield_CR1 <- AF1_Celeriac_yield_CR1 * (1 - AF1_perc_yield_reduction)
    AF1_Potato_yield_CR1 <- AF1_Potato_yield_CR1 * (1 - AF1_perc_yield_reduction)
    AF1_Wheat_yield_CR1 <- AF1_Wheat_yield_CR1 * (1 - AF1_perc_yield_reduction)
    AF1_Beans_yield_CR1 <- AF1_Beans_yield_CR1 * (1 - AF1_perc_yield_reduction)
    
    ## Crop-specific time series of the income perceived from the sales arable crops' yield 
    AF1_Leek_benefit_CR1 <- Leek_price * AF1_Leek_yield_CR1
    AF1_Maize_benefit_CR1 <- Maize_price * AF1_Maize_yield_CR1
    AF1_Carrot_benefit_CR1 <- Carrot_price * AF1_Carrot_yield_CR1
    AF1_Celeriac_benefit_CR1 <- Celeriac_price * AF1_Celeriac_yield_CR1
    AF1_Potato_benefit_CR1 <- Potato_price * AF1_Potato_yield_CR1
    AF1_Wheat_benefit_CR1 <- Wheat_price * AF1_Wheat_yield_CR1
    AF1_Beans_benefit_CR1 <- Beans_price * AF1_Beans_yield_CR1
    
    # Costs of production of each arable crop, taking into account the crop rotation cycle
    ## cost of labor employed in arable crop production [EUROS]
    AF1_Leek_labour_cost_CR1 <- rep(0, n_years_c)
    AF1_Maize_labour_cost_CR1 <- rep(0, n_years_c)
    AF1_Carrot_labour_cost_CR1 <- rep(0, n_years_c)
    AF1_Celeriac_labour_cost_CR1 <- rep(0, n_years_c)
    AF1_Potato_labour_cost_CR1 <- rep(0, n_years_c)
    AF1_Wheat_labour_cost_CR1 <- rep(0, n_years_c)
    AF1_Beans_labour_cost_CR1 <- rep(0, n_years_c)
    
    AF1_Leek_labour_cost_CR1[Leek_indices1] <- Leek_labour_cost[Leek_indices1] * Arable_area_AF1 * af1_added_management_time_factor_p
    AF1_Maize_labour_cost_CR1[Maize_indices1] <- Maize_labour_cost[Maize_indices1] * Arable_area_AF1 * af1_added_management_time_factor_p
    AF1_Carrot_labour_cost_CR1[Carrot_indices1] <- Carrot_labour_cost[Carrot_indices1] * Arable_area_AF1 * af1_added_management_time_factor_p
    AF1_Celeriac_labour_cost_CR1[Celeriac_indices1] <- Celeriac_labour_cost[Celeriac_indices1] * Arable_area_AF1 * af1_added_management_time_factor_p
    AF1_Potato_labour_cost_CR1[Potato_indices1] <- Potato_labour_cost[Potato_indices1] * Arable_area_AF1 * af1_added_management_time_factor_p
    AF1_Wheat_labour_cost_CR1[Wheat_indices1] <- Wheat_labour_cost[Wheat_indices1] * Arable_area_AF1 * af1_added_management_time_factor_p
    AF1_Beans_labour_cost_CR1[Beans_indices1] <- Beans_labour_cost[Beans_indices1] * Arable_area_AF1 * af1_added_management_time_factor_p
    
    ## cost of material inputs used in arable crop production [EUROS], #includes: seed, insurance, fixed+variable machine cost
    AF1_Leek_management_cost_CR1 <- rep(0, n_years_c) #includes: seed, insurance, fixed+variable machine cost (Values from GER available)
    AF1_Maize_management_cost_CR1 <- rep(0, n_years_c)
    AF1_Carrot_management_cost_CR1 <- rep(0, n_years_c)
    AF1_Celeriac_management_cost_CR1 <- rep(0, n_years_c)
    AF1_Potato_management_cost_CR1 <- rep(0, n_years_c)
    AF1_Wheat_management_cost_CR1 <- rep(0, n_years_c)
    AF1_Beans_management_cost_CR1 <- rep(0, n_years_c)
    
    AF1_Leek_management_cost_CR1[Leek_indices1] <- Leek_management_cost[Leek_indices1] * Arable_area_AF1
    AF1_Maize_management_cost_CR1[Maize_indices1] <- Maize_management_cost[Maize_indices1] * Arable_area_AF1
    AF1_Carrot_management_cost_CR1[Carrot_indices1] <- Carrot_management_cost[Carrot_indices1] * Arable_area_AF1
    AF1_Celeriac_management_cost_CR1[Celeriac_indices1] <- Celeriac_management_cost[Celeriac_indices1] * Arable_area_AF1
    AF1_Potato_management_cost_CR1[Potato_indices1] <- Potato_management_cost[Potato_indices1] * Arable_area_AF1
    AF1_Wheat_management_cost_CR1[Wheat_indices1] <- Wheat_management_cost[Wheat_indices1] * Arable_area_AF1
    AF1_Beans_management_cost_CR1[Beans_indices1] <- Beans_management_cost[Beans_indices1] * Arable_area_AF1
    
    ## Total costs of production of each arable crop, taking into account the crop rotation cycle
    AF1_total_Leek_cost_CR1 <- AF1_Leek_management_cost_CR1 + AF1_Leek_labour_cost_CR1
    AF1_total_Maize_cost_CR1 <- AF1_Maize_management_cost_CR1 + AF1_Maize_labour_cost_CR1
    AF1_total_Carrot_cost_CR1 <- AF1_Carrot_management_cost_CR1 + AF1_Carrot_labour_cost_CR1
    AF1_total_Celeriac_cost_CR1 <- AF1_Celeriac_management_cost_CR1 + AF1_Celeriac_labour_cost_CR1
    AF1_total_Potato_cost_CR1 <- AF1_Potato_management_cost_CR1 + AF1_Potato_labour_cost_CR1
    AF1_total_Wheat_cost_CR1 <- AF1_Wheat_management_cost_CR1 + AF1_Wheat_labour_cost_CR1
    AF1_total_Beans_cost_CR1 <- AF1_Beans_management_cost_CR1 + AF1_Beans_labour_cost_CR1
    
    AF1_arable_cost_CR1 <- AF1_total_Leek_cost_CR1 + AF1_total_Maize_cost_CR1 + AF1_total_Carrot_cost_CR1 + AF1_total_Celeriac_cost_CR1 + AF1_total_Potato_cost_CR1  + AF1_total_Wheat_cost_CR1 + AF1_total_Beans_cost_CR1
    
    ## ---- Woody stand growth ----
    ### ---- Volume dynamics (only aboveground wood) ----
    rotation_length <- ifelse(round(rotation_length_p)<1, 1, round(rotation_length_p))
    planting_age <- ifelse(round(planting_age_p)<1, 1, round(planting_age_p))
    
    stand_growth_curve <- ontogenic_growth_gompertz(max_harvest = max_volume_p,
                                                    time_to_first_yield_estimate = time_first_volume_est_c,
                                                    time_to_second_yield_estimate = time_sec_volume_est_c,
                                                    first_yield_estimate_percent = first_vol_rel_to_sec_perc_p,
                                                    second_yield_estimate_percent = sec_volume_perc_p,
                                                    n_years = ifelse(rotation_length > n_years_c, rotation_length*2, n_years_c*2),
                                                    no_yield_before_first_estimate = FALSE
    )
    
    if (rotation_length > n_years_c){
      growing_timber_volume <- stand_growth_curve[planting_age: 
                                                    (n_years_c+planting_age-1)] * field_area_c
    } else {
      growing_timber_volume <- stand_growth_curve[planting_age: 
                                                    (rotation_length+planting_age-1)] * field_area_c
    }
    
    ### ---- Biomass dynamics ----
    #### ---- Aboveground perennial biomass (AGB) ----
    # Calculate biomass based on wood density values, and repeat the vector as many times as necessary to cover the whole simulation period
    AGB <- growing_timber_volume * wood_density_p
    
    #### ---- Belowground perennial biomass (AGB) ----
    # Simulate belowground biomass values as a ratio of AGB
    
    if (rotation_length > n_years_c){
      root_to_shoot_ratio <- vv(root_to_shoot_ratio_initial_p, 
                                cv_root_to_shoot_ratio_p,
                                n = rotation_length*2,
                                relative_trend = trend_root_to_shoot_ratio_n,
                                lower_limit = root_to_shoot_ratio_mature_p)
      BGB <- AGB * root_to_shoot_ratio[planting_age: 
                                         (n_years_c+planting_age-1)]
      # ensure that perennial belowground biomass does not decrease over the growth development period of trees
      BGB <- cummax(BGB)
      root_to_shoot_ratio <- rep_len(root_to_shoot_ratio[planting_age:
                                                           (n_years_c+planting_age-1)],
                                     n_years_c)
    } else {
      root_to_shoot_ratio <- vv(root_to_shoot_ratio_initial_p, 
                                cv_root_to_shoot_ratio_p,
                                n = n_years_c*2,
                                relative_trend = trend_root_to_shoot_ratio_n,
                                lower_limit = root_to_shoot_ratio_mature_p)
      BGB <- AGB * root_to_shoot_ratio[planting_age:
                                         (rotation_length+planting_age-1)]
      # ensure that perennial belowground biomass does not decrease over the growth development period of trees
      BGB <- cummax(BGB)
      root_to_shoot_ratio <- rep_len(root_to_shoot_ratio[planting_age:
                                                           (rotation_length+planting_age-1)],
                                     n_years_c)
    }
    growing_timber_volume <- rep_len(growing_timber_volume, n_years_c)
    AGB <- rep_len(AGB, n_years_c)
    BGB <- rep_len(BGB, n_years_c)
    
    ### ---- Carbon sequestration ----
    #### ---- C-sequestration in woody biomass ----
    # Calculate the carbon content of woody biomass (and its interannual change) based on carbon density values
    AGBcarbon <- AGB * carbon_density_p
    AGBcarbon_change <- AGBcarbon
    AGBcarbon_change[2:length(AGBcarbon_change)] <- diff(AGBcarbon)
    
    BGBcarbon <- BGB * carbon_density_p
    BGBcarbon_change <- BGBcarbon
    BGBcarbon_change[2:n_years_c] <- diff(BGBcarbon)
    
    #### ---- C-sequestration in the soil ----
    ##### ---- Soil C-sequestration under the woody strips -----
    #if (rotation_length < n_years_c){
    soil_C_stock <- rep(0, n_years_c)
    rel_soc_change <- rep(0, n_years_c)
    interannual_rel_soc_change <- rep(0, n_years_c)
    chance_soil_loss_resolved <- rep(0, n_years_c)
    #} else {
    #  soil_C_stock <- rep(0, rotation_length)
    #  rel_soc_change <- rep(0, rotation_length)
    #  interannual_rel_soc_change <- rep(0, rotation_length)
    #  chance_soil_loss_resolved <- rep(0, rotation_length)
    #}
    
    first_growth_years <- seq(from = 1, to = n_years_c, by = rotation_length)
    
    if (rotation_length > n_years_c){
      end_cycle_soc_dyn <- n_years_c
    } else {
      end_cycle_soc_dyn <- seq(from = rotation_length, to = n_years_c, by = rotation_length)
    }
    if (!n_years_c %in% end_cycle_soc_dyn){
      end_cycle_soc_dyn <- c(end_cycle_soc_dyn, n_years_c)
    }
    
    chance_soil_loss_resolved[ceiling(first_year_soil_loss_resolved_p)] <- 1
    if (rotation_length < n_years_c){
      for (i in 2:length(end_cycle_soc_dyn)){
        chance_soil_loss_resolved[first_growth_years[i]:end_cycle_soc_dyn[i]] <- chance_event(1/rotation_length,
                                                                                              value_if = 1, 
                                                                                              value_if_not = 0, 
                                                                                              n = length(first_growth_years[i]:end_cycle_soc_dyn[i]))
      }
    }
    
    start_soc_recovery <- rep(0, length(end_cycle_soc_dyn))
    for (i in 1:length(end_cycle_soc_dyn)){
      # Find the first year in which chance == 1 for each cycle of SOC dynamics
      start_soc_recovery[i] <- which(chance_soil_loss_resolved == 1)[which(chance_soil_loss_resolved == 1) %in% first_growth_years[i]:end_cycle_soc_dyn[i]][1]
    }
    time_period_length <- ifelse(rotation_length < n_years_c, rotation_length, n_years_c)
    
    ####response functions for 100 cm profile depths (The centennial legacy of land-use change on organic carbon stocks)
    rel_soc_accrual <- accrual_roof_p * (1-exp(-soc_accrual_constant_p*1:n_years_c))
    rel_soc_loss <- -loss_floor_p * (1-exp(-soc_loss_constant_p*1:n_years_c))
    achievable_soc_stock <- initial_soil_C_stock_cropland_p*(1+accrual_roof_p/100)
    
    interannual_rel_soc_loss <- c(rel_soc_loss[1], diff(rel_soc_loss))
    
    for (i in 1:length(end_cycle_soc_dyn)) {
      # Determine the reference point: start_soc_recovery[i+1] or first_growth_years[i+1] if NA
      recovery_start <- ifelse(is.na(start_soc_recovery[i]), end_cycle_soc_dyn[i]+1, start_soc_recovery[i])
      for (j in first_growth_years[i]:end_cycle_soc_dyn[i]) {
        if (recovery_start == n_years_c+1){
          interannual_rel_soc_change[j] <- interannual_rel_soc_loss[j - time_period_length*(i-1)]
          rel_soc_change[1:(recovery_start-1)] <- cumsum(interannual_rel_soc_change[1:(recovery_start-1)])
          # Recalculate the soil carbon stock
          soil_C_stock[1:(recovery_start-1)] <- 
            initial_soil_C_stock_cropland_p * (1 + rel_soc_change[1:(recovery_start-1)] / 100)
          
          # Step 0: 
        } else if (recovery_start == 1) {
          rel_soc_change[1:time_period_length] <- rel_soc_accrual[1:time_period_length]
          soil_C_stock[1:time_period_length] <- initial_soil_C_stock_cropland_p + initial_soil_C_stock_cropland_p*rel_soc_change[1:time_period_length]/100
          interannual_rel_soc_change[1:time_period_length] <- c(rel_soc_change[1], diff(rel_soc_change[1:time_period_length]))
          
          # Step 0.5: 
        } else if (recovery_start < end_cycle_soc_dyn[i] & j < recovery_start) {
          #interannual_rel_soc_change[j] <- interannual_rel_soc_loss[j - recovery_start + 1]
          interannual_rel_soc_change[j] <- interannual_rel_soc_loss[j]
          
          
          # Step 1: While j < recovery_start
        } else if (j < recovery_start) {
          #interannual_rel_soc_change[j] <- interannual_rel_soc_loss[j - recovery_start + 1]
          interannual_rel_soc_change[j] <- interannual_rel_soc_loss[j - time_period_length*(i-1)]
          
          # Step 2: When j == recovery_start
        } else if (j == recovery_start) {
          # Simulate carbon stock's dynamics along the time period covered by the previous iteration
          rel_soc_change[1:(recovery_start-1)] <- cumsum(interannual_rel_soc_change[1:(recovery_start-1)])
          #rel_soc_change[recovery_start:end_cycle_soc_dyn[i+1]] <- 
          #cumsum(interannual_rel_soc_change)[recovery_start:end_cycle_soc_dyn[i+1]]
          
          soil_C_stock[1:(recovery_start-1)] <- 
            initial_soil_C_stock_cropland_p * (1 + rel_soc_change[1:(recovery_start-1)] / 100)
          
          # Reset the relative value of the achievable SOC stock
          # Compute new value for accrual roof
          new_value_accrual_roof_p <- accrual_roof_p * 
            (achievable_soc_stock - soil_C_stock[recovery_start-1]) / 
            (achievable_soc_stock - initial_soil_C_stock_cropland_p)
          
          # Simulate carbon stock's dynamics along the time period covered by the current iteration
          current_baseline_year_accrual_function <- ifelse(recovery_start == 1, recovery_start, recovery_start-1)
          current_value_accrual_roof_p <- ifelse(recovery_start == 1, accrual_roof_p, new_value_accrual_roof_p)
          
          soil_C_stock[recovery_start:end_cycle_soc_dyn[i]] <- 
            soil_C_stock[current_baseline_year_accrual_function] + soil_C_stock[current_baseline_year_accrual_function]*
            current_value_accrual_roof_p * (1 - exp(-soc_accrual_constant_p * seq_along(recovery_start:end_cycle_soc_dyn[i]))) /100 
          
          rel_soc_change[1:end_cycle_soc_dyn[i]] <- 100*(soil_C_stock/initial_soil_C_stock_cropland_p-1)[1:end_cycle_soc_dyn[i]]
          interannual_rel_soc_change <- c(rel_soc_change[1], diff(rel_soc_change))
        }
      }
    }
    rel_soc_change_ws <- rel_soc_change
    soil_C_stock_ws <- soil_C_stock
    annual_soil_carbon_change_ws <- c(soil_C_stock_ws[1] - initial_soil_C_stock_cropland_p, 
                                      diff(soil_C_stock_ws))
    
    ##### ---- Soil C-sequestration under the alleys -----
    annual_soil_carbon_change_ca <- rep(0, n_years_c)
    treeless_production_CR1 <- Leek_yield_CR1 + Maize_yield_CR1 + Carrot_yield_CR1 + Celeriac_yield_CR1 + Potato_yield_CR1 + Wheat_yield_CR1 + Beans_yield_CR1
    AF1_alley_production_CR1 <- AF1_Leek_yield_CR1 + AF1_Maize_yield_CR1 + AF1_Carrot_yield_CR1 + AF1_Celeriac_yield_CR1 + AF1_Potato_yield_CR1 + AF1_Wheat_yield_CR1 + AF1_Beans_yield_CR1
    alley_vs_treeless_production_ratio <- AF1_alley_production_CR1 / treeless_production_CR1
    
    treeless_soc_change <- vv(var_mean = baseline_soc_change_treeless_p, var_CV = 0, n = n_years_c, 
                              relative_trend = -treeless_soc_change_decrease_constant_p, lower_limit = 0)
    
    annual_soil_carbon_change_ca <- annual_soil_carbon_change_ws * alley_stock_rel_ws_t + ((((1-alley_vs_treeless_production_ratio) * treeless_soc_change) - treeless_soc_change) * soc_tradeoff_alley_t)
    
    soil_C_stock_ca <- initial_soil_C_stock_cropland_p + cumsum(annual_soil_carbon_change_ca)
    
    soil_C_stock_ca[soil_C_stock_ca > initial_soil_C_stock_cropland_p*(1+max_alley_soc_rel_advantage_c)] <- initial_soil_C_stock_cropland_p*(1+max_alley_soc_rel_advantage_c)
    
    annual_soil_carbon_change_ca <- c(soil_C_stock_ca[1] - initial_soil_C_stock_cropland_p, 
                                      diff(soil_C_stock_ca))
    
    ##### ---- Total soil C-sequestration -----
    AF1_annual_soil_carbon_change_CR1 <- annual_soil_carbon_change_ws * (AF1_tree_row_area_c+AF1_hedgerow_area_c)/field_area_c + annual_soil_carbon_change_ca * Arable_area_AF1/field_area_c # [t C ha]
    AF1_soil_C_stock_CR1 <- soil_C_stock_ws * (AF1_tree_row_area_c+AF1_hedgerow_area_c)/field_area_c + soil_C_stock_ca * Arable_area_AF1/field_area_c # [t C ha]
    
    #### ---- Whole-ecosystem's additional carbon sequestration derived from the agroforestry intervention ----
    AF1_annual_carbon_change_CR1 <- AGBcarbon_change + BGBcarbon_change + (AF1_annual_soil_carbon_change_CR1*field_area_c)  # tones over the whole intervention's area
    AF1_cum_additional_carbon_change_CR1 <- cumsum(AF1_annual_carbon_change_CR1)  # tones over the whole intervention's area
    AF1_ecosystem_carbon_stock_CR1 <- AGB + BGB + cumsum(AF1_annual_carbon_change_CR1)  # tones over the whole intervention's area
    
    ## ---- Wood products ----
    AF1_total_wood_income <- rep(0, n_years_c)  # Initialize a vector of zeros for each year
    
    ### ---- Quality categories quantification ----
    #pruning
    pruning_biomass_yield <- rep(0, n_years_c) # the yield of low value wood (in kg/ha)
    sawnwood_yield <- rep(0, n_years_c) # the yield of wood (in m3/ha) that will be sold as sawnwood
    veneer_yield <- rep(0, n_years_c) # the yield of wood (in m3/ha) that will be sold as veneer
    
    #final_harvest_benefit <- (pruning_biomass_yield + sawnwood_final_yield + veneer_yield) * final_harvest # income from sales of the final harves
    pruning_biomass_yield[c(2,3,4,5,6,8,10,12)] <- growing_timber_volume[c(2,3,4,5,6,8,10,12)] * pruning_ratio_p * wood_density_p
    
    ### ---- Sales of woody biomass ----
    # Create vector that defines the timing of clear-cut operations
    # Create a vector containing the years in which clear-cutting operations occur...
    if (rotation_length <= n_years_c) {
      #... only if, at least, the first clear cutting operation is conducted during the assessed time frame
      Timber_harvest_indices <- seq(from = rotation_length, to = n_years_c, by = rotation_length)
    }
    
    #final harvest
    if (exists("Timber_harvest_indices")) {
      pruning_biomass_yield[Timber_harvest_indices] <- growing_timber_volume[Timber_harvest_indices] * (1-proportion_durable_product_t) * wood_density_p
      sawnwood_yield[Timber_harvest_indices] <- growing_timber_volume[Timber_harvest_indices] * proportion_durable_product_t*(1-veneer_proportion_t)
      veneer_yield[Timber_harvest_indices] <- growing_timber_volume[Timber_harvest_indices] * proportion_durable_product_t*veneer_proportion_t
    }
    
    pew_income <- pruning_biomass_yield * vv(pew_value_p, var_CV_p, n_years_c) #  the income from all sales of low quality wood
    sawnwood_income <- sawnwood_yield * vv(sawnwood_value_p, var_CV_p, n_years_c) #  the income from all sales of low quality wood
    veneer_income <- veneer_yield * vv(veneer_value_p, var_CV_p, n_years_c) #  the income from all sales of veneer
    AF1_total_wood_income <- pew_income + sawnwood_income + veneer_income
    
    ## ---- Non-wood tree products ----
    ### ---- Yield of non-wood tree products ----
    walnuts_yield <- gompertz_yield(max_harvest = max_walnuts_yield_p,
                                    time_to_first_yield_estimate = time_first_walnuts_est_c,
                                    time_to_second_yield_estimate = time_sec_walnuts_est_c,
                                    first_yield_estimate_percent = first_walnuts_perc_p,
                                    second_yield_estimate_percent = sec_walnuts_perc_p,
                                    n_years= n_years_c,
                                    var_CV= CV_walnuts_yield_p,
                                    no_yield_before_first_estimate = TRUE)
    alive_trees <- AF1_num_trees_c # - cumsum(dead_tree_units)
    tot_walnuts_yield <- walnuts_yield * alive_trees
    tot_walnuts_yield <- tot_walnuts_yield[1:rotation_length]
    tot_walnuts_yield <- rep_len(tot_walnuts_yield, n_years_c)
    tot_walnuts_yield <- chance_event(nuts_risk_t, tot_walnuts_yield, tot_walnuts_yield*(1-yield_red_nuts_risk_t))
    
    AF1_nuts_harvest_cost <- rep(0, n_years_c)
    AF1_nuts_harvest_cost[tot_walnuts_yield > 0] <- (walnut_harv_costs_p + walnut_postharv_costs_p) * field_area_c
    
    ### ---- Sales of non-wood tree products ----
    AF1_nuts_benefit <- tot_walnuts_yield * vv(walnuts_value_p, var_CV_p, n_years_c)
    
    ## ---- Sum of the sales of all tree products ----
    AF1_tree_benefit <- AF1_total_wood_income + AF1_nuts_benefit
    
    ## ---- Sum of the sales of all tree products ----
    AF1_tree_benefit <- AF1_total_wood_income + AF1_nuts_benefit
    
    ## ---- Value of ecosystem services (ES) ----
    ### ---- Carbon marketing ----
    
    AF1_co2_benefit <- vv(AF1_annual_carbon_change_CR1*co2_price_p*3.67, var_CV_p, n = n_years_c)
    
    # Value of ES whose provision is not marketed, or marketed through targeted schemes (e.g.: carbon markets)
    AF1_GW_benefit <- rep(0, n_years_c)
    AF1_erosion_control_benefit <- rep(0, n_years_c)
    
    ### ---- Groundwater storage ----
    NMES_indices <- seq(from = 5, to = n_years_c)
    AF1_GW_benefit[NMES_indices] <-
      vv(pc_ground_water_recharge_c, var_CV_p, length(NMES_indices)) * field_area_c
    
    ### ---- Erosion control ----
    AF1_erosion_control_benefit[NMES_indices] <- (vv(AF1_soil_loss_water_p, var_CV_p, length(NMES_indices)) + vv(AF1_soil_loss_wind_p, var_CV_p, length(NMES_indices))) * vv(pc_soil_loss_c, var_CV_p, length(NMES_indices)) * field_area_c  
    
    AF1_Nonmarket_ES_benefit <- AF1_co2_benefit + AF1_GW_benefit + AF1_erosion_control_benefit 
    
    ## ---- External funds' support ----
    # check if support with percentage of investment cost is selected by the user
    if (selected_percentage_c == 1) {
      AF1_percentage_funding <- sum(AF1_percentage_values_c * AF1_total_investment_cost)
      AF1_total_one_time_funding <- AF1_total_one_time_funding_c + AF1_percentage_funding
    } else {
      AF1_total_one_time_funding <- AF1_total_one_time_funding_c
    }
    # Subsidy in AF system
    # AF1_subsidy[1:n_years_c] <- AF1_subsidy * AF1_tree_row_area_c
    AF1_total_annual_funding <- rep(AF1_total_annual_funding_c, n_years_c)
    
    AF1_farm_benefit <- AF1_tree_benefit + AF1_total_annual_funding
    AF1_farm_benefit[2] <- AF1_farm_benefit[2] + AF1_total_one_time_funding
    AF1_ES_benefit <- AF1_farm_benefit + AF1_Nonmarket_ES_benefit
    
    ## Additional time required to establish and maintain the system, and its worth
    establishment_additional_time <- rep(0, n_years_c)
    establishment_additional_time[1] <- farmer_planning_time_p + (tree_planting_p * AF1_num_trees_c)
    
    annual_additional_time <- rep(0, n_years_c)
    annual_additional_time <- vv(subsidy_application_p, var_CV_p, n_years_c)
    annual_additional_time[2:n_years_c] <- annual_additional_time[2:n_years_c] + vv(woody_strip_undergrowth_maintenance_time_p, var_CV_p, n_years_c-1)*field_area_c
    annual_additional_time[c(2,3,4,5,6,8)] <- annual_additional_time[c(2,3,4,5,6,8)] + pruning_time_earlier_p*AF1_num_trees_c
    annual_additional_time[c(10,12)] <- annual_additional_time[c(10,12)] + pruning_time_later_p*AF1_num_trees_c
    
    total_additional_time <- establishment_additional_time + annual_additional_time
    
    self_fulfilling_time <- rep(0, n_years_c)
    self_fulfilling_time[total_additional_time < labor_time_threshold_p] <- total_additional_time[total_additional_time < labor_time_threshold_p] * sense_of_purpose_t
    self_fulfilling_time[total_additional_time >= labor_time_threshold_p] <- labor_time_threshold_p * sense_of_purpose_t
    
    worth_self_fulfilling_time <- self_fulfilling_time * Labour_costs
    
    ## Time series of the annual income perceived from the management of the agroforestry system
    AF1_total_benefit_CR1 <- AF1_ES_benefit + AF1_Leek_benefit_CR1 + AF1_Maize_benefit_CR1 + AF1_Carrot_benefit_CR1 + AF1_Celeriac_benefit_CR1 + AF1_Potato_benefit_CR1 + AF1_Wheat_benefit_CR1 + AF1_Beans_benefit_CR1
    ## Time series of costs associated with the integration of the woody strips which occur in other years different to the year of establishment
    AF1_total_running_cost <- AF1_subsidy_application + AF1_nuts_harvest_cost + AF1_maintenance_cost + AF1_timber_harvest_cost #+ AF1_annual_irrigation_cost
    ## Time series of costs associated with the integration of the woody strips
    AF1_total_treerow_cost <- AF1_total_investment_cost + AF1_total_running_cost
    ## Time series of the annual costs incurred by the production of all arable crops' in the agroforestry system
    AF1_total_cost_CR1 <- AF1_total_treerow_cost + AF1_total_Leek_cost_CR1 + AF1_total_Maize_cost_CR1 + AF1_total_Carrot_cost_CR1 + AF1_total_Celeriac_cost_CR1 + AF1_total_Potato_cost_CR1  + AF1_total_Wheat_cost_CR1 + AF1_total_Beans_cost_CR1
    ## Profits of the agroforestry system (i.e.: the difference between the income and the costs):
    AF1_bottom_line_benefit <- AF1_total_benefit_CR1 + worth_self_fulfilling_time - AF1_total_cost_CR1
    
  }#Will only be calculated if user checks the box "Crop rotation 1" for AF 1
  
  ## ---- Net Present Value (NPV) and cashflows ----
  #Treeless system
  Treeless_NPV <- discount(Treeless_bottom_line_benefit, discount_rate = discount_rate_p,
                           calculate_NPV = TRUE) #NVP of monoculture arable system 
  Treeless_discounted_cash_flow <- discount(Treeless_bottom_line_benefit, discount_rate = discount_rate_p,
                                            calculate_NPV = FALSE) #Cash flow of monoculture system
  Treeless_cum_discounted_cash_flow <- cumsum(Treeless_discounted_cash_flow) #Cumulative cash flow of monoculture system
  
  #AF System 1
  AF1_NPV <- discount(AF1_bottom_line_benefit, discount_rate=discount_rate_p,
                      calculate_NPV = TRUE)#NVP of AF system
  AF1_discounted_cash_flow <- discount(AF1_bottom_line_benefit,discount_rate=discount_rate_p,
                                       calculate_NPV = FALSE)#Cash flow of AF system
  AF1_cum_discounted_cash_flow <- cumsum(AF1_discounted_cash_flow) #Cumulative cash flow of AF system
  
  AF1_decision_benefit <- AF1_bottom_line_benefit - Treeless_bottom_line_benefit
  AF1_NPV_decision <- discount(AF1_decision_benefit, discount_rate = discount_rate_p,
                               calculate_NPV = TRUE)
  AF1_decision_discounted_benefit <- discount(AF1_decision_benefit, discount_rate = discount_rate_p,
                                              calculate_NPV = FALSE)
  
  ## ---- Choose output variables ----
  return(list(
    NPV_Agroforestry_System1 = AF1_NPV,
    NPV_Treeless_System = Treeless_NPV,
    NPV_decision_AF1 = AF1_NPV_decision,
    Cashflow_AF1 = AF1_bottom_line_benefit,
    CumCashflow_AF1 = cumsum(AF1_bottom_line_benefit),
    Discounted_Cashflow_AF1 = AF1_discounted_cash_flow,
    Discounted_Cum_Cashflow_AF1 = AF1_cum_discounted_cash_flow,
    Discounted_Cashflow_treeless = Treeless_discounted_cash_flow,
    Discounted_Cum_Cashflow_treeless = Treeless_cum_discounted_cash_flow,
    Cashflow_AF1_decision = AF1_decision_benefit,
    Cum_Cashflow_AF1_decision = cumsum(AF1_decision_benefit),
    Discounted_Cashflow_AF1_decision = AF1_decision_discounted_benefit,
    Discounted_Cum_Cashflow_AF1_decision = cumsum(AF1_decision_discounted_benefit),
    
    implementation_labor = establishment_additional_time,
    additional_yearly_labor = annual_additional_time,
    total_additional_labor = total_additional_time,
    
    annual_soc_change = AF1_annual_soil_carbon_change_CR1,
    cum_annual_soc_change = cumsum(AF1_annual_soil_carbon_change_CR1),
    soc_stock = AF1_soil_C_stock_CR1,
    
    annual_carbon_change = AF1_annual_carbon_change_CR1,
    cum_annual_carbon_change = AF1_cum_additional_carbon_change_CR1,
    carbon_stock = AF1_ecosystem_carbon_stock_CR1,
    
    volume = growing_timber_volume,
    
    AGB_carbon_stock = AGBcarbon, 
    AGB_carbon_change = AGBcarbon_change,
    BGB_carbon_stock = BGBcarbon, 
    BGB_carbon_change = BGBcarbon_change,
    
    maintenance_funding = AF1_total_annual_funding,
    establishment_funding = AF1_total_one_time_funding,
    ES_value = AF1_Nonmarket_ES_benefit,
    initial_planting_cost = AF1_total_planting_cost,
    
    leek_CR = sum(Leek_benefit_CR1 - Total_Leek_cost_CR1),
    maize_CR = sum(Maize_benefit_CR1 - Total_Maize_cost_CR1),
    carrot_CR = sum(Carrot_benefit_CR1 - Total_Carrot_cost_CR1),
    celeriac_CR = sum(Celeriac_benefit_CR1 - Total_Celeriac_cost_CR1),
    potato_CR = sum(Potato_benefit_CR1 - Total_Potato_cost_CR1),
    wheat_CR = sum(Wheat_benefit_CR1 - Total_Wheat_cost_CR1),
    beans_CR = sum(Beans_benefit_CR1 - Total_Beans_cost_CR1),
    
    leek_AF = sum(AF1_Leek_benefit_CR1 - AF1_total_Leek_cost_CR1),
    maize_AF = sum(AF1_Maize_benefit_CR1 - AF1_total_Maize_cost_CR1),
    carrot_AF = sum(AF1_Carrot_benefit_CR1 - AF1_total_Carrot_cost_CR1),
    celeriac_AF = sum(AF1_Celeriac_benefit_CR1 - AF1_total_Celeriac_cost_CR1),
    potato_AF = sum(AF1_Potato_benefit_CR1 - AF1_total_Potato_cost_CR1),
    wheat_AF = sum(AF1_Wheat_benefit_CR1 - AF1_total_Wheat_cost_CR1),
    beans_AF = sum(AF1_Beans_benefit_CR1 - AF1_total_Beans_cost_CR1),
    trees_AF = sum(AF1_ES_benefit - AF1_total_treerow_cost),
    
    nuts_costs = sum(AF1_nuts_harvest_cost),
    wood_harvest_costs = sum(AF1_timber_harvest_cost),
    application_cost = sum(AF1_subsidy_application),
    strips_maintenance = sum(AF1_maintenance_cost),
    planting_cost = sum(AF1_total_planting_cost),
    planing_costs = sum(AF1_planning_cost),
    nut_sales = sum(AF1_nuts_benefit),
    wood_sales = sum(AF1_total_wood_income)
  ))
}


