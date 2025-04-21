### UI ######

ui <- fluidPage(
  theme = bs_theme(version = 5,
                   bootswatch = 'flatly',
                   base_font = font_google("Roboto")), 
  # tags$img(src = "data/images/ReFOREST_logo_horizontal.jpg"),
  # Include custom CSS for the scroll bar and accordion styling
  tags$head(
    tags$style(HTML("
      /* Custom scrollbar styling */
      ::-webkit-scrollbar {
        width: 10px;}
      ::-webkit-scrollbar-track {
        background: #f1f1f1;}
      ::-webkit-scrollbar-thumb {
        background: skyblue;
      }
      ::-webkit-scrollbar-thumb:hover {
        background: linear-gradient(lightblue, skyblue, dodgerblue, blue, darkblue);}
      /* For Firefox */
      body {
        scrollbar-width: thin;
        scrollbar-color: skyblue #f1f1f1;}
        
      /* Header styling */
      .app-header {
      /*background: linear-gradient(135deg, #1E90FF, #00FA9A, #FFD700); */ /* light bue to light green to gold */
     /* background: linear-gradient(135deg, #32CD32, #228B22, #006400);  LimeGreen to ForestGreen to DarkGreen */
        background-color:#228B22;
        color: white;
        padding: 15px 0;
        position: relative;
      }
      .app-title {
        font-size: 48px;
        font-weight: bold;
        text-align: center;
        margin: 0;
      }

      /* Accordion header styling */
      .accordion-button {
        background-color: #007BFF;
        color: white;
      }
      .accordion-button:not(.collapsed) {
        background-color: #0056b3;
      }
      .accordion-button:hover {
        background-color: #0056b3;
      }
      .accordion-button:focus {
        box-shadow: none;
      }
      .my-btn {
        display: block;
        text-align: center;
        background: #238a21; 
    /*  rgb(241 119 65 / 75%); */
    /*  width: 50%; */
    /*  height: 100%; */
        color: white;
    /*  transform: translateX(-50%); */
    /*  left: 50%; */
    /*  font-size: 3rem; */
        border-radius: 20px;
        position: relative;
        margin-top: 25px;
        margin-bottom: 25px;
      }
     "))
  ),
  # Responsive Layout with Animated Width
  # layout_column_wrap(
  #   width = "200px",
  #   anim_width("100%", "67%"),  # Smooth resizing transition
  # Application title
  titlePanel(
    div(class = "app-header",
        # Logos column
        fluidRow(column(width = 2,
                        align = "right",
                        tags$a(href = "https://agroreforest.eu/", # Add URL here
                               tags$img(src = "ReFOREST_logo_horizontal_transparent.png", 
                                        style = "max-width: 100%; height: auto;"
                                        #height = "70px"
                               ),
                               target="_blank")),
                 column(width = 8,
                        align = "center",
                        h2(class = "app-title",
                           "Holistic Decision Analysis for an Agrisilvicultural Agroforestry System")),
                 column(width = 2,
                        align = "left",
                        tags$a(href = "https://www.gartenbauwissenschaften.uni-bonn.de/", # Add URL here
                               tags$img(src = "UniBonnHortiBonn_logo_transparent.png",
                                        style = "max-width: 100%; height: auto;"
                                        #height = "100px"
                               ),
                               target="_blank")),
                 windowTitle = "MyPage")
    )),
  sidebarLayout(
    sidebarPanel(width = 4,
                 # style = "height: 100vh; overflow-y: auto",
                 style = "height: 100%; overflow-y: auto",
                 # Collapsible sections
                 accordion(
                   id = "collapseSidebar",
                   open = FALSE,
                   # Basic Information Panel
                   accordion_panel(
                     title = "Basic Information",
                     icon = icon("info-circle"),
                     
                     ########### save load delete logic ##############
                     
                     # Admin user selection UI
                     uiOutput("admin_user_select"),
                     
                     textInput("project_name", "Project Name:", value = ""),
                     actionButton("save", "Save Settings"),
                     
                     # column(width = 6,
                     #        textInput("version", label = "Version", value = "1")),
                     
                     selectInput("version_select", "Select Version to Load:", choices = NULL),
                     actionButton("load", "Load Selected Version"),
                     
                     selectInput("delete_version_select", "Select Version to Delete:", choices = NULL),
                     actionButton("delete", "Delete Selected Version"),
                     
                     ########### save load delete logic ##############
                     
                     # dateInput("date", label = "Date input", value = Sys.Date()), # format(Sys.time(), "%Y-%m-%d_%H-%M" for naming the downloaded file
                     # 
                     # downloadButton("save_data", "Save project data"), # !! enable save local machine and our server 
                     br(),
                     br(),
                     actionButton("run_simulation", "Run Model", icon = icon("play"), class = "btn-primary"),
                     textOutput("validation_message") # To display validation messages
                   ),
                   ### Data entry by users ######
                   # Basic Farm and Decision Parameters Panel
                   accordion_panel(
                     title = "Basic Farm and Decision Parameters",
                     icon = icon("tractor"),
                     # System modulators and risks
                     h4("System Modulators"),
                     numericInput("num_simulations_c", "Number of Simulations",
                                  min = 10, max = 50000, value = 100),
                     numericInput("n_years_c", "Simulation period in years",
                                  min = 1, max = 100, value = 40),
                     sliderInput("discount_rate_p", "Discount rate (%)",
                                 min = 1, max = 50, value = c(1.8,5), step = 1),
                     sliderInput("var_CV_p", "Coefficient of Variation",
                                 min = 1, max = 50, value = c(5,10), step = 1),
                     br(),
                     h4("Farm Details"),
                     # Dropdown menu of countries, state and funding schemes
                     selectInput("country", "Select Country:", choices = names(country_states), selected = NULL),
                     uiOutput("state_ui"),
                     uiOutput("funding_one_ui"),
                     uiOutput("funding_yearly_ui"),
                     numericInput("annual_external_support_c", "Annual private support [GBP/ha]",
                                  min = 1, max = 5000, value = 50),
                     numericInput("onetime_external_support_c", "One-time private support [GBP]",
                                  min = 1, max = 1000000, value = 500),
                     
                     numericInput("arable_area_c", "Size of the farm [ha]", min = 0.001, max = 1000, value = 14.9),
                     sliderInput("subsidy_application_p", "Time spent on application for subsidies [h]",
                                 min = 1, max = 500, value = c(10,40)),
                   ),
                   accordion_panel(
                     title = "Existing System",
                     icon = icon("tractor"),
                     # h4("Existing System"),
                     selectInput(
                       inputId = "treeless_crop_rotation", #"crop_rotation", 
                       label = "Choose a crop rotation scheme:", 
                       choices = c("Crop Rotation 1: 3 years of Herbal Ley followed by Winter wheat" = "rotation_1", 
                                   "Crop Rotation 2: 2 yeras of Herbal Ley followed by rotation with Winter wheat, Spring barley, summer beans and Winter oats" = "rotation_2"),
                       selected = NULL
                     ),
                     # Checkbox to introduce animals with numbers
                     # checkboxInput("include_animals_c", "Introduce Animals", FALSE),
                     # conditionalPanel(
                     #   condition = "input.include_animals_c == true",
                     #   numericInput("treeless_number_of_animals_c", "Number of Animals:", value = 1, min = 1)
                     # Checkbox to introduce animals with grazing %
                     checkboxInput("treeless_include_animals_c", "Introduce Animals", FALSE),
                     conditionalPanel(
                       condition = "input.treeless_include_animals_c == true",
                       selectInput("treeless_animal_type", "Select Animal",
                                   choices = c("cattle", "sheep"), #"Goats", "Chickens", "Turkeys"),
                                   selected = NULL,
                                   multiple = TRUE),
                       uiOutput("treeless_grazing_intensity"), # Dynamic UI for slider inputs (one per selected animal)
                       textOutput("treeless_grazing_warning"), # Display a warning message if the total exceeds 1
                     ),
                   ),
                   accordion_panel(
                     title = "Agroforestry Systems",
                     icon = icon("tree"),
                     h4("General preparation"),
                     sliderInput("tree_planting_p", "Cost of digging tree well & planting tree [GBP/tree]",
                                 min = 1, max = 5000, value = c(10,50)),
                     sliderInput("farmer_planning_time_p", "Time spent planning by farmer [h]",
                                 min = 0, max = 1000, value = c(10,40)),
                     sliderInput("planning_consulting_p", "Total payment of hired planner/consultant [GBP]",
                                 min = 0, max = 10000, value = c(50,1000)),
                     sliderInput("weed_protection_p", "Cost of controlling weeds during establishment [GBP]",
                                 min = 0, max = 5000, value = c(15,25)),
                     br(),
                     h4("Design 1"),
                     numericInput("AF1_tree_row_area_c", "Total area of tree rows [ha]",
                                  min = 0.001, max = 100, value = 0.8),
                     numericInput("AF1_num_trees_c", "Number of trees",
                                  min = 0, max = 10000, value = 5926),
                     sliderInput("AF1_plant_protection_p", "Cost of fencing [GBP/tree]",
                                 min = 0, max = 10, value = c(0.843,1.022)),
                     sliderInput("SRC_cutting_price_p", "Price of willow cuttings [GBP]",
                                 min = 0, max = 10, value = c(0.1,0.6)),
                     sliderInput("SRC_field_prep_p", "Subsoiling and harrowing cost [h/ha]",
                                 min = 1, max = 10, value = c(2.6,3.8)),
                     sliderInput("SRC_planting_p", "Cost of planting cuttings mechanically [h/ha]",
                                 min = 1, max = 15, value = c(4,8)),
                     sliderInput("SRC_machine_rent_p", "Rent of planting machine [GBP]",
                                 min = 1, max = 500, value = c(150,300)),
                     sliderInput("harvest_interval_SRC_p", "Number of years between SRC harvest [ha]",
                                 min = 1, max = 60, value = c(3,5)),
                     sliderInput("af1_added_management_time_factor_p", "Extra time for managing AF vs. Baseline [%]",
                                 min = 1, max = 10, value = c(1.05,1.2)),
                     sliderInput("AF1_soil_loss_water_p", "Soil loss due to water [tons/ha/year]",
                                 min = 1, max = 10, value = c(2,4)),
                     sliderInput("AF1_soil_loss_wind_p", "Soil loss due to wind [tons/ha/year]",
                                 min = 1, max = 10, value = c(2,4)),
                     sliderInput("af1_less_grazing_management_time_factor_p", "Reduced livestock mgmt from AF providing partial natural paddock [%]",
                                 min = 0, max = 1, value = c(0.8,0.9)),
                     sliderInput("AF1_woody_benefit_windreduc_p", " Stress Reduction and Livestock Performance Enhancement [%]",
                                 min = 0, max = 1, value = c(0.01,0.02)),
                     selectInput(
                       inputId = "AF1_crop_rotation", #"crop_rotation", 
                       label = "Choose a crop rotation scheme:", 
                       choices = c("Crop Rotation 1: 3 years of Herbal Ley followed by Winter wheat" = "rotation_1", 
                                   "Crop Rotation 2: 2 yeras of Herbal Ley followed by rotation with Winter wheat, Spring barley, summer beans and Winter oats" = "rotation_2"),
                       selected = NULL
                     ),
                     # Checkbox to introduce animals with numbers
                     # checkboxInput("AF1_include_animals_c", "Introduce Animals", FALSE),
                     # conditionalPanel(
                     #   condition = "input.AF1_include_animals_c == true",
                     #   numericInput("AF1_number_of_animals_c", "Number of Animals:", value = 1, min = 1)
                     
                     # Checkbox to introduce animals with grazing %
                     checkboxInput("AF1_include_animals_c", "Introduce Animals", FALSE),
                     conditionalPanel(
                       condition = "input.AF1_include_animals_c == true",
                       selectInput("AF1_animal_type", "Select Animal",
                                   choices = c("cattle", "sheep"), #"Goats", "Chickens", "Turkeys"),
                                   selected = NULL,
                                   multiple = TRUE),
                       uiOutput("AF1_grazing_intensity"), # Dynamic UI for slider inputs (one per selected animal)
                       textOutput("AF1_grazing_warning"), # Display a warning message if the total exceeds 1
                     ),
                     br(),
                     h4("Design 2"),
                     numericInput("AF2_tree_row_area_c", "Total area of tree rows [ha]",
                                  min = 0.001, max = 100, value = 0.45),
                     numericInput("num_oak_trees_c", "Number of oak trees",
                                  min = 0, max = 50, value = 30),
                     sliderInput("oak_tree_cost_p", "Price of an oak sapling [GBP]",
                                 min = 0., max = 500, value = c(1.77,3.49)),
                     numericInput("num_birch_trees_c", "Number of birch trees",
                                  min = 0, max = 5000, value = 30),
                     sliderInput("birch_tree_cost_p", "Price of a birch sapling [GBP]",
                                 min = 0., max = 10, value = c(1.77,2.19)),
                     numericInput("num_rowan_trees_c", "Number of rowan trees",
                                  min = 0, max = 100, value = 30),
                     sliderInput("rowan_tree_cost_p", "Price of a rowan sapling [GBP]",
                                 min = 0., max = 500, value = c(1.77,2.49)),
                     numericInput("num_hazel_trees_c", "Number of hazel trees",
                                  min = 0, max = 50, value = 30),
                     sliderInput("hazel_tree_cost_p", "Price of a hazel sapling [GBP]",
                                 min = 0., max = 15, value = c(1.77,2.6)),
                     numericInput("num_damson_trees_c", "Number of damson trees",
                                  min = 0, max = 100, value = 30),
                     sliderInput("damson_tree_cost_p", "Price of a damson sapling [GBP]",
                                 min = 0., max = 50, value = c(15,21.6)),
                     numericInput("num_bcherry_trees_c", "Number of bird cherry trees",
                                  min = 0, max = 100, value = 30),
                     sliderInput("bcherry_tree_cost_p", "Price of a bird cherry sapling [GBP]",
                                 min = 0., max = 10, value = c(1.77,1.99)),
                     numericInput("AF2_num_shrubs_c", "Number of Shrubs",
                                  min = 0, max = 4000, value = 900),
                     sliderInput("shrub_price_p", "Price per shrub [GBP/shrub]",
                                 min = 0., max = 10, value = c(0.2,0.3)),
                     sliderInput("AF2_plant_protection_p", "Cost of fencing [GBP/tree]",
                                 min = 1, max = 50, value = c(11.39,14.81)),
                     sliderInput("af2_added_management_time_factor_p", "Extra time for managing AF vs. Baseline [%]",
                                 min = 1, max = 10, value = c(1.05,1.2)),
                     sliderInput("AF2_soil_loss_water_p", "soil loss due to water [tons/ha/year]",
                                 min = 1, max = 50, value = c(2,4)),
                     sliderInput("AF2_soil_loss_wind_p", "soil loss due to wind [tons/ha/year]",
                                 min = 1, max = 50, value = c(2,4)),
                     sliderInput("af2_less_grazing_management_time_factor_p", "Reduced livestock mgmt from AF providing partial natural paddock [%]",
                                 min = 0, max = 1, value = c(0.8,0.9)),
                     sliderInput("AF2_woody_benefit_windreduc_p", " Stress Reduction and Livestock Performance Enhancement [%]",
                                 min = 0, max = 1, value = c(0.03,0.055)),
                     
                     selectInput(
                       inputId = "AF2_crop_rotation", #"crop_rotation", 
                       label = "Choose a crop rotation scheme:", 
                       choices = c("Crop Rotation 1: 3 years of Herbal Ley followed by Winter wheat" = "rotation_1", 
                                   "Crop Rotation 2: 2 yeras of Herbal Ley followed by rotation with Winter wheat, Spring barley, summer beans and Winter oats" = "rotation_2"),
                       selected = NULL
                     ),
                     # Checkbox to introduce animals with number of animals
                     # checkboxInput("AF2_include_animals_c", "Introduce Animals", FALSE),
                     # conditionalPanel(
                     #   condition = "input.AF2_include_animals_c == true",
                     #   numericInput("AF2_number_of_animals_c", "Number of Animals:", value = 1, min = 1)
                     # ),
                     
                     # Checkbox to introduce animals with grazing %
                     checkboxInput("AF2_include_animals_c", "Introduce Animals", FALSE),
                     conditionalPanel(
                       condition = "input.AF2_include_animals_c == true",
                       selectInput("AF2_animal_type", "Select Animal",
                                   choices = c("cattle", "sheep"), #"Goats", "Chickens", "Turkeys"),
                                   selected = NULL,
                                   multiple = TRUE),
                       uiOutput("AF2_grazing_intensity"), # Dynamic UI for slider inputs (one per selected animal)
                       textOutput("AF2_grazing_warning"), # Display a warning message if the total exceeds 1
                     ),
                   ),
                   accordion_panel(
                     title = "Annual Crops",
                     icon = icon("pagelines"),
                     #h4("Annual Crops"),
                     h5 ("Wheat"),
                     sliderInput("winter_wheat_yield_p", "Yield [t/ha]",
                                 min = 0, max = 50, value = c(2,8)),
                     sliderInput("winter_wheat_value_p", "Sale price [GBP/t]",
                                 min = 0, max = 1000, value = c(450,520)),
                     h5 ("Barley"),
                     sliderInput("spring_barley_yield_p", "Yield [t/ha]",
                                 min = 0, max = 50, value = c(1.5,5.5)),
                     sliderInput("spring_barley_value_p", "Sale price [GBP/t]",
                                 min = 0, max = 1000, value = c(350,470)),
                     h5 ("Herbal Ley"),
                     sliderInput("herbal_ley_yield_p", "Yield [t/ha]",
                                 min = 0, max = 100, value = c(5,12)),
                     sliderInput("herbal_ley_value_p", "Sale price [GBP/t]",
                                 min = 0, max = 100, value = c(5,10)),
                     # sliderInput("herbal_effect_p", "Beneficial effect on subsequent crops [%]",
                     #             min = 0, max = 10000, value = c(1,5)),
                     h5 ("Summer Beans"),
                     sliderInput("summer_beans_yield_p", "Yield [t/ha]",
                                 min = 0, max = 1000, value = c(1.5,4.5)),
                     sliderInput("summer_beans_value_p", "Sale price [GBP/t]",
                                 min = 0, max = 1000, value = c(500,600)),
                     h5 ("Winter Oats"),
                     sliderInput("winter_oats_yield_p", "Yield [t/ha]",
                                 min = 0, max = 50, value = c(1.5,6)),
                     sliderInput("winter_oats_value_p", "Sale price [GBP/t]",
                                 min = 0, max = 100, value = c(5,10)),
                     h5 ("Winter Cover Crops"),
                     sliderInput("winter_cover_crop_yield_p", "Yield [t/ha]",
                                 min = 0, max = 100, value = c(3,6)),
                   ),
                   accordion_panel(
                     title = "Animals",
                     icon = icon("cow"), 
                     #h4("Animals"),
                     sliderInput("grazing_efficiency_p", "Trampling selective grazing & areas not consumed [%]",
                                 min = 0, max = 1, value = c(0.5,0.8)),
                     sliderInput("herbal_grazing_labour_p", "labour to accommodate livestock grazing [h/ha over a year]",
                                 min = 1, max = 50, value = c(2,5)),
                     sliderInput("beef_value_p", "Price of selling live cows [GBP/Kg]",
                                 min = 1, max = 50, value = c(4.2,5.8)),
                     sliderInput("lamb_value_p", "Price of selling sheep [GBP/Kg]",
                                 min = 1, max = 50, value = c(7.3,8.7)),
                   ),
                   accordion_panel(
                     title = "Benefits of AF",
                     icon = icon("envira"),
                     #h4("Benefits & Drawbacks of AF"),
                     # sliderInput("Nonmarket_ES_value_p", "Monetary value of ES in AF system GBP/ ha/ y",
                     #             min = 1, max = 10000, value = c(600,960)),
                     sliderInput("C_price_p", "Carbon Price [GBP/t C]",
                                 min = 1, max = 500, value = c(20,30)),
                     sliderInput("winter_grazing_effect_p", "Positive yield effect or fertilization effect because of winter grazing [%]",
                                 min = 1, max = 100, value = c(1.05,1.15)),
                     # sliderInput("per_market_price_p", "% fluctuation in market price of all products [%]",
                     #             min = 1, max = 100, value = c(1,5)),
                   ),
                   accordion_panel(
                     title = "Need clarification",
                     icon = icon("question-circle"),
                     #h4("Need clarification"),
                     sliderInput("irrigation_sys_install_p", "?Cost of installing irrigation system[GBP]",
                                 min = 1, max = 10000, value = c(1000,3000)),
                     sliderInput("irrigation_planting_shrub_p", "?Water used soon after planting shrub[l/shrub]",
                                 min = 1, max = 50, value = c(1,1.5)),
                     sliderInput("irrigation_planting_tree_p", "?Water used soon after planting tree[l/tree]",
                                 min = 1, max = 90, value = c(10,20)),
                     # sliderInput("irrigation_123_p", "?Water use in the frist three years of tree establishment[l/tree]",
                     #             min = 1, max = 10000, value = c(1,5)),
                     # sliderInput("irrigation_annual_p", "?Annual water use after the first three years[l/tree]",
                     #             min = 1, max = 10000, value = c(1,5)),
                     sliderInput("compost_planting_shrub_p", "?Compost used at the time of planting shrubs [l/shrub]",
                                 min = 0.1, max = 50, value = c(0.5,1)),
                     sliderInput("compost_planting_tree_p", "?Compost used at the time of planting tree [l/tree]",
                                 min = 0.1, max = 70, value = c(10,20)),
                     sliderInput("compost_price_p", "?Price of compost [GBP/l]",
                                 min = 0.1, max = 50, value = c(0.1,0.25)),
                     sliderInput("rowan_yield_max_p", "? max yield of Rowan [kg/tree]",
                                 min = 1, max = 100, value = c(10,40)),
                     sliderInput("rowan_value_p", "?Price of Rowan[GBP/kg]",
                                 min = 0, max = 10, value = c(0.5,0.75)),
                     sliderInput("hazel_yield_max_p", "?Max yield of Hazel [kg/tree]",
                                 min = 1, max = 50, value = c(3,12)),
                     sliderInput("hazel_value_p", "?Price of Hazel [GBP/kg]",
                                 min = 0, max = 20, value = c(0.89,3.08)),
                     sliderInput("damson_yield_max_p", "?Max yield of Damson [kg/tree]",
                                 min = 1, max = 100, value = c(11.35,27.24)),
                     sliderInput("damson_value_p", "?Price of Damson [GBP/kg]",
                                 min = 0, max = 20, value = c(0.88,3.3)),
                     sliderInput("tree_yield_max_p", "?Max. volume of SRC tree growth [t/ha*yr]",
                                 min = 1, max = 50, value = c(7,12)),
                     sliderInput("biomass_timber_price_p", "?Price of selling one tonne of SRC willow [GBP/t]",
                                 min = 1, max = 1000, value = c(80,260)),
                   ),
                   br(),
                   br(),
                   actionButton("run_simulation", "Run Model", icon = icon("play"), class = "btn-primary"),
                 )
    ),
    # define the content of the main panel of the UI
    mainPanel(
      width = 8,
      # Display plots of DA
      fluidRow(
        column(width = 4),
        #textOutput("display_version_1"),
        # Added a button to open the URL
        # actionButton("open_url", "Click here for latest info on Sustainable Farming Incentive"), 
        column(width = 4,
               tags$a("Click here for latest info on Sustainable Farming Incentive",
                      href = "https://www.gov.uk/government/publications/sustainable-farming-incentive-scheme-expanded-offer-for-2024",
                      target="_blank",
                      class = "my-btn")),
        column(width = 4)
      ),
      h5('Probabilistic outcome distributions from Monte Carlo simulation for baseline and the decision intervention.'),
      plotOutput("distPlot"),
      p(
        'The graphs above compare the net present value (NPV) distributions
        between baseline and intervention options, expressed in GBP/ha across
        the simulation period. The x-axis displays NPV values, representing the
        sum of discounted cash flows, while the y-axis presents the different
        scenarios being compared.'
      ),       # p('The graph above provides a visual comparison of the outcome distributions 
      #   for the baseline and intervention options in terms of net present value 
      #   (NPV in GBP/ha) over the simulated period. The x-axis represents the NPV, 
      #   calculated as the sum of discounted cash flows for each simulation year. 
      #   The y-axis shows the probability of occurrence, indicating the likelihood 
      #   of different NPV values. A higher value on the y-axis corresponds to a 
      #   greater probability of the associated NPV on the x-axis.'),
      downloadButton("save_plot1", "Save Plot"),
      br(), # blank line
      br(), # blank line
      # h5('2. Probabilistic outcome of the decision in terms of NPV over the simulation period.'),
      # plotOutput("distPlot2"),
      # p(
      #   'The graph above illustrates the outcome in terms of NPV in GBP/ha) over 
      #   the simulated years, comparing the baseline scenario with the intervention. 
      #   It highlights the differences in net cash flows between the two optioons. 
      #   The right-skewness of the graph suggests that the intervention is generally 
      #   favorable. However, since the distribution includes both positive and 
      #   negative values, there remains a non-zero probability that the intervention 
      #   may not always yield a more favorable outcome than the baseline. The 
      #   integrated box plot shows that while the interquartile range (IQR) is mostly positive, 
      #   it does include some negative values. The vertical line within the box plot represents the median NPV.'),
      # downloadButton("save_plot2", "Save Plot"), 
      # br(), # blank line
      # br(), # blank line
      h5('Probabilistic outcome of the decisions'),
      plotOutput("distPlot3"),
      p('The graph above illustrates NPV outcomes (GBP/ha) over the simulation
         period, directly comparing intervention scenarios against the baseline. 
        The x-axis shows the NPV differential, where negative values indicate 
        the baseline outperforming interventions and positive values show 
        interventions outperforming the baseline. The y-axis represents 
        probability density, with higher values indicating greater likelihood 
        of achieving the corresponding NPV difference.'),
      downloadButton("save_plot3", "Save Plot") 
    )
  )
)
#)
# Define Server
server <- function(input, output, session) {
  #### Conditional Reactive variables based on user selection ####
  # Funding ReactiveVaules ####
  
  # Ensure reactive values are used correctly
  tidy_funding_data <- reactive({
    if (exists("funding_data")) {
      return(isolate(funding_data()))
    }
    return(list())
  })
  
  funding_data_reactive <- reactive({ return (funding_data) })
  
  # Fix UI rendering to use tagList instead of list
  output$state_ui <- renderUI({
    if (is.null(input$country)) return(NULL)
    req(input$country)
    tagList(
      selectInput("state", "Select State:", choices = country_states[[input$country]], selected = NULL)
    )
  })
  # Show funding schemes dropdown after state selection
  # One-time funding Schemes
  output$funding_one_ui <- renderUI({
    req(input$country, input$state)
    country <- input$country
    state <- input$state
    
    if (!is.null(funding_data_reactive()[[country]]) && !is.null(funding_data_reactive() [[country]][[state]])) {
      state_data <- funding_data_reactive() [[country]][[state]]
      return(tagList(
        selectInput("funding_one_ui", "Select One-time Funding Scheme(s):", 
                    choices = if (!is.null(state_data$one_time_funding_schemes)) state_data$one_time_funding_schemes else list(),
                    multiple = TRUE)
      ))
    }
    return(NULL)
  })
  # Annual funding Schemes
  output$funding_yearly_ui <- renderUI({
    req(input$state, input$country)
    country <- input$country
    state <- input$state
    
    if (!is.null(funding_data_reactive() [[country]]) && !is.null(funding_data_reactive() [[country]][[state]])) {
      state_data <- funding_data_reactive()[[country]][[state]]
      return(tagList(
        selectInput("funding_yearly_ui", "Select Annual Funding Scheme(s):", 
                    choices = if (!is.null(state_data$annual_funding_schemes)) state_data$annual_funding_schemes else list(),
                    multiple = FALSE)
      ))
    }
    return(NULL)
  })
  
  # Ensure function names are not overwritten
  if (!exists("safe_list")) {
    safe_list <- list
  }
  
  ########### save load delete logic ##############
  
  # Determine file directory based on OS
  file_dir <- if (Sys.info()[["sysname"]] == "Windows") {
    "user-states/Mindrum/"  # Use Windows path for local testing
  } else {
    "/srv/shiny-app-data/user-states/Mindrum/"  # Linux path for server
  }
  
  # Get user ID or set default for testing
  user_id <- session$user %||% "local_test_user"
  # user_id <- session$user %||% "Adrian"
  is_admin <- user_id %in% admin_users
  
  # Output to determine if user is admin
  output$isAdmin <- reactive(is_admin)
  outputOptions(output, "isAdmin", suspendWhenHidden = FALSE)
  
  # Admin user selection UI
  output$admin_user_select <- renderUI({
    if (is_admin) {
      user_folders <- list.dirs(file_dir, full.names = FALSE, recursive = FALSE)
      if (length(user_folders) == 0) {
        user_folders <- user_id
      }
      div(
        class = "admin-panel",
        h4("Admin Panel"),
        selectInput("admin_selected_user", "Select User Folder:", choices = user_folders, selected = user_id)
      )
    }
  })
  # Utility function to ensure directory exists
  ensureDirectoryExists <- function(dir) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }
  }
  # Reactive expression for current user directory
  current_user_dir <- reactive({
    user_folder <- if (is_admin && !is.null(input$admin_selected_user)) {
      input$admin_selected_user
    } else {
      user_id
    }
    dir <- file.path(file_dir, user_folder)
    ensureDirectoryExists(dir)
    dir
  })
  
  # Reactive value to store versions
  versions <- reactiveVal()
  
  # Function to update version selections
  updateVersionSelections <- function() {
    vers <- basename(list.files(current_user_dir(), full.names = TRUE, pattern = "csv"))
    versions(vers)
    updateSelectInput(session, "version_select", choices = versions())
    updateSelectInput(session, "delete_version_select", choices = versions())
  }
  
  # Update versions when current_user_dir changes
  observe({
    updateVersionSelections()
  })
  
  # Save settings
  observeEvent(input$save, {
    files <- list.files(current_user_dir(), full.names = TRUE, pattern = "\\.csv$")
    
    if (length(files) >= max_files) {
      showNotification("Error: Maximum number of files reached.", type = "error", duration = 5)
      return()
    }
    
    if (input$project_name == "") {
      showNotification("Error: Please enter a project name before saving.", type = "error", duration = 5)
      return()
    }
    
    timestamp <- format(Sys.time(), "%y%m%d_%H%M%S")
    safe_project_name <- gsub("[^A-Za-z0-9_]+", "_", input$project_name)
    file_name <- paste0(timestamp, "_", safe_project_name, ".csv")
    file_path <- file.path(current_user_dir(), file_name)
    
    # Exclude non-relevant inputs
    exclude_inputs <- c("collapseSidebar", "save", "load", "delete", "confirm_delete", 
                        "admin_selected_user", "project_name", "version_select", "delete_version_select")
    
    # Define variables based on input names
    variables <- setdiff(names(input)[grepl("(_c$|_p$|_t$)", names(input))], exclude_inputs)
    
    # Extract lower and upper values
    lower_values <- sapply(variables, function(var) {
      value <- input[[var]]
      if (length(value) == 1) {
        as.numeric(value)
      } else {
        as.numeric(value[1])
      }
    })
    
    upper_values <- sapply(variables, function(var) {
      value <- input[[var]]
      if (length(value) == 1) {
        as.numeric(value)
      } else {
        as.numeric(value[2])
      }
    })
    # Determine distributions
    distributions <- sapply(variables, function(var) {
      if (grepl("_c$", var)) {
        "const"  # Constant distribution
      } else if (grepl("_p$", var)) {
        "posnorm"  # Positive normal distribution
      } else {
        "tnorm_0_1"  # Default truncated normal
      }
    })
    # Create the data frame
    df <- data.frame(
      variable = variables,
      lower = lower_values,
      upper = upper_values,
      distribution = distributions,
      stringsAsFactors = FALSE
    )
    # Write to CSV
    write.csv(df, file_path, row.names = FALSE)
    showNotification("Settings saved successfully.", type = "message", duration = 5)
    # Refresh version selections
    updateVersionSelections()
  })
  
  # Load settings
  observeEvent(input$load, {
    selected_file <- file.path(current_user_dir(), input$version_select)
    if (file.exists(selected_file)) {
      df <- read.csv(selected_file, stringsAsFactors = FALSE)
      # Update inputs
      for (i in 1:nrow(df)) {
        var <- df$variable[i]
        lower <- df$lower[i]
        upper <- df$upper[i]
        
        if (var %in% names(input)) {
          # Determine the type of input and update accordingly
          if (length(input[[var]]) == 1) {
            # Numeric or text input
            if (is.numeric(input[[var]])) {
              updateNumericInput(session, var, value = lower)
            } else if (is.character(input[[var]])) {
              updateTextInput(session, var, value = as.character(lower))
            }
          } else if (length(input[[var]]) == 2) {
            # Slider input with range
            updateSliderInput(session, var, value = c(lower, upper))
          }
        }
      }
      showNotification("Settings loaded successfully.", type = "message", duration = 5)
    } else {
      showNotification("Error: File could not be loaded.", type = "error", duration = 5)
    }
  })
  # Delete settings with confirmation
  observeEvent(input$delete, {
    req(input$delete_version_select)
    showModal(modalDialog(
      title = "Confirm Deletion",
      paste("Are you sure you want to delete the file:", input$delete_version_select, "?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete", "Delete", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_delete, {
    removeModal()
    file_to_delete <- file.path(current_user_dir(), input$delete_version_select)
    if (file.exists(file_to_delete)) {
      file.remove(file_to_delete)
      showNotification("File deleted successfully.", type = "message", duration = 5)
      # Refresh version selections
      updateVersionSelections()
    } else {
      showNotification("Error: File could not be deleted.", type = "error", duration = 5)
    }
  })
  ## End of save load delete logic ####
  
  ## Collect all data ####
  #### Data analysts' csv file ####
  # Read variables from an internal CSV file
  # Validate CSV file before reading
  manual_variables <- reactive({
    message("Accessing analyst estimates...")
    csv_path <- here("manual_variables_AF.csv")
    if (!file.exists(csv_path)) {
      stop("Error: CSV file not found at ", csv_path)
    }
    manual_data <- read.csv(csv_path, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    
    required_columns <- c("variable", "lower", "upper", "distribution")
    if (!all(required_columns %in% names(manual_data))) {
      stop("Error: CSV must contain 'variable', 'lower', 'upper', and 'distribution' columns.")
    }
    manual_data <- manual_data[required_columns]
    manual_data$lower <- as.numeric(manual_data$lower)
    manual_data$upper <- as.numeric(manual_data$upper)
    return(manual_data)
  })
  #### End of Data analyst' csv file ####
  
  # Crop rotation estimates####
  crop_estimates <- reactive({
    message("Accessing crop estimates...")
    treeless_system_crop_rotation_1_c <- if (input$treeless_crop_rotation == "rotation_1") 1 else 0
    treeless_system_crop_rotation_2_c <- if (input$treeless_crop_rotation == "rotation_2") 1 else 0 
    AF1_system_crop_rotation_1_c <- if (input$AF1_crop_rotation == "rotation_1") 1 else 0
    AF1_system_crop_rotation_2_c <- if (input$AF1_crop_rotation == "rotation_2") 1 else 0
    AF2_system_crop_rotation_1_c <- if (input$AF2_crop_rotation == "rotation_1") 1 else 0
    AF2_system_crop_rotation_2_c <- if (input$AF2_crop_rotation == "rotation_2") 1 else 0
    
    crop_parameters <- c("treeless_system_crop_rotation_1_c", "treeless_system_crop_rotation_2_c", "AF1_system_crop_rotation_1_c", "AF1_system_crop_rotation_2_c", "AF2_system_crop_rotation_1_c", "AF2_system_crop_rotation_2_c")
    selected_values_crops <- c(treeless_system_crop_rotation_1_c, treeless_system_crop_rotation_2_c, AF1_system_crop_rotation_1_c, AF1_system_crop_rotation_2_c, AF2_system_crop_rotation_1_c, AF2_system_crop_rotation_2_c)
    
    crop_data <- data.frame(
      variable = crop_parameters,
      lower = selected_values_crops,
      upper = selected_values_crops,
      distribution = rep("const", length(crop_parameters)),
      stringsAsFactors = FALSE
    )
  })
  #### End of Crop estimates ####
  
  #### Funding estimates ####
  funding_variables <- reactive({
    message("Accessing funding estimates...")
    country <- input$country
    state   <- input$state
    
    # Default funding values (0 when no country/state is selected)
    selected_percentage_c <- 0
    AF1_total_one_time_funding_c  <- 0
    AF1_total_annual_funding_c    <- 0
    AF1_percentage_values_c       <- 0
    AF2_total_one_time_funding_c  <- 0
    AF2_total_annual_funding_c    <- 0
    AF2_percentage_values_c       <- 0
    
    # Validate area calculations (default to 0 if inputs are missing)
    AF1_area <- max(0, as.numeric(input$arable_area_c) - as.numeric(input$AF1_tree_row_area_c))
    AF2_area <- max(0, as.numeric(input$arable_area_c) - as.numeric(input$AF2_tree_row_area_c))
    
    # Default funding values
    AF1_one_funding <- 0
    AF1_annual_funding <- 0
    AF2_one_funding <- 0
    AF2_annual_funding <- 0
    
    # Only process funding calculations if both country and state are selected
    if (!is.null(country) && !is.null(state) && country != "None" && state != "None") {
      if (!is.null(funding_data_reactive()[[country]]) &&
          !is.null(funding_data_reactive()[[country]][[state]])) {
        
        state_data <- funding_data_reactive()[[country]][[state]]
        
        # ---- AF1 One-Time Funding ----
        AF1_selected_per_ha <- intersect(input$funding_one_ui, state_data$funding_onetime_per_ha_schemes)
        if (length(AF1_selected_per_ha) > 0) {
          AF1_per_ha_values <- state_data$funding_onetime_values_named[AF1_selected_per_ha]
          AF1_one_funding <- sum(AF1_per_ha_values) * AF1_area
        }
        
        AF1_selected_per_tree <- intersect(input$funding_one_ui, state_data$funding_onetime_per_tree_schemes)
        if (length(AF1_selected_per_tree) > 0) {
          AF1_per_tree_values <- state_data$funding_onetime_values_named[AF1_selected_per_tree]
          AF1_one_funding <- AF1_one_funding + sum(AF1_per_tree_values) * input$AF1_num_trees_c
        }
        
        AF1_selected_percentage <- intersect(input$funding_one_ui, state_data$funding_onetime_percentage_schemes)
        if (length(AF1_selected_percentage) > 0) {
          selected_percentage_c <- 1
          AF1_percentage_values_c <- sum(state_data$funding_onetime_values_named[AF1_selected_percentage])
        }
        
        AF1_one_funding <- AF1_one_funding + input$onetime_external_support_c
        AF1_total_one_time_funding_c <- AF1_one_funding
        
        # ---- AF1 Annual Funding ----
        AF1_selected_annual <- intersect(input$funding_yearly_ui, state_data$annual_funding_schemes)
        if (length(AF1_selected_annual) > 0) {
          AF1_annual_values <- state_data$funding_yearly_values_named[AF1_selected_annual]
          AF1_annual_funding <- sum(AF1_annual_values) * AF1_area
        }
        
        AF1_annual_funding <- AF1_annual_funding + input$annual_external_support_c * AF1_area
        AF1_total_annual_funding_c <- AF1_annual_funding
        
        # ---- AF2 One-Time Funding ----
        AF2_selected_per_ha <- intersect(input$funding_one_ui, state_data$funding_onetime_per_ha_schemes)
        if (length(AF2_selected_per_ha) > 0) {
          AF2_per_ha_values <- state_data$funding_onetime_values_named[AF2_selected_per_ha]
          AF2_one_funding <- sum(AF2_per_ha_values) * AF2_area
        }
        
        AF2_selected_per_tree <- intersect(input$funding_one_ui, state_data$funding_onetime_per_tree_schemes)
        if (length(AF2_selected_per_tree) > 0) {
          AF2_per_tree_values <- state_data$funding_onetime_values_named[AF2_selected_per_tree]
          total_AF2_trees <- sum(input$num_oak_trees_c, input$num_birch_trees_c,
                                 input$num_rowan_trees_c, input$num_hazel_trees_c,
                                 input$num_damson_trees_c, input$num_bcherry_trees_c)
          AF2_one_funding <- sum(AF2_per_tree_values) * total_AF2_trees
        }
        
        AF2_selected_percentage <- intersect(input$funding_one_ui, state_data$funding_onetime_percentage_schemes)
        if (length(AF2_selected_percentage) > 0) {
          selected_percentage_c <- 1
          AF2_percentage_values_c <- sum(state_data$funding_onetime_values_named[AF2_selected_percentage])
        }
        
        AF2_one_funding <- AF2_one_funding + input$onetime_external_support_c
        AF2_total_one_time_funding_c <- AF2_one_funding
        
        # ---- AF2 Annual Funding ----
        AF2_selected_annual <- intersect(input$funding_yearly_ui, state_data$annual_funding_schemes)
        if (length(AF2_selected_annual) > 0) {
          AF2_annual_values <- state_data$funding_yearly_values_named[AF2_selected_annual]
          AF2_annual_funding <- sum(AF2_annual_values) * AF2_area
        }
        
        AF2_annual_funding <- AF2_annual_funding + input$annual_external_support_c * AF2_area
        AF2_total_annual_funding_c <- AF2_annual_funding
      }
    }
    
    # ---- Store Results in Data Frame ----
    funding_parameters <- c(
      "AF1_total_annual_funding_c", "AF2_total_annual_funding_c",
      "AF1_total_one_time_funding_c", "AF2_total_one_time_funding_c",
      "AF2_percentage_values_c", "AF1_percentage_values_c",
      "selected_percentage_c")
    selected_values_funding <- c(
      AF1_total_annual_funding_c, AF2_total_annual_funding_c,
      AF1_total_one_time_funding_c, AF2_total_one_time_funding_c,
      AF2_percentage_values_c, AF1_percentage_values_c,
      selected_percentage_c)
    funding_variables_df <- data.frame(
      variable = funding_parameters,
      lower = selected_values_funding,
      upper = selected_values_funding,
      distribution = rep("const", length(funding_parameters)),
      stringsAsFactors = FALSE
    )
    message("Funding Variables Updated: ")
    #print(funding_variables_df)
    return(funding_variables_df)
  })
  #### End of funding estimates ####
  
  ######## Reactive UI data #####
  ui_estimates <- reactive({
    message("Accessing user input estimates from te interface...")
    exclude_inputs <- c("collapseSidebar", "save", "load", "delete", "confirm_delete", 
                        "admin_selected_user", "project_name", "version_select", "delete_version_select")
    
    variables <- setdiff(names(input)[grepl("(_c$|_p$|_t$)", names(input))], exclude_inputs)
    req(variables) # Ensure variables exist
    
    lower_values <- sapply(variables, function(var) {
      value <- input[[var]]
      if (length(value) == 1) as.numeric(value) else as.numeric(value[1])
    })
    upper_values <- sapply(variables, function(var) {
      value <- input[[var]]
      if (length(value) == 1) as.numeric(value) else as.numeric(value[2])
    })
    distributions <- sapply(variables, function(var) {
      if (grepl("_c$", var)) "const" else if (grepl("_p$", var)) "posnorm" else "tnorm_0_1"
    })
    data.frame(
      variable = variables,
      lower = lower_values,
      upper = upper_values,
      distribution = distributions,
      stringsAsFactors = FALSE)
  })
  # Observing ui_estimates
  observe({
    req(ui_estimates()) # Validate reactive
    estimates <- ui_estimates()
  })
  
  # Merge dynamic data from the UI for funding, animals, crops and others with manual data from CSV
  input_estimates <- reactive({
    df1 <- ui_estimates()  # get ui variables
    df2 <- manual_variables()  # get analyst variables
    df3 <- animal_estimates() # get animal variables
    df4 <- crop_estimates() # get crop variables
    df5 <- funding_variables() # get funding variables
    
    req(df1, df2, df3, df4, df5)  # Ensure they are not NULL
    print(names(df1))
    print(names(df2))
    print(names(df3))
    print(names(df4))
    print(names(df5))
    df_combined <- rbind(df1, df3, df4, df5, df2)
    # Convert to numeric where needed
    df_combined$lower <- as.numeric(df_combined$lower)
    df_combined$upper <- as.numeric(df_combined$upper)
    
    if (any(is.na(df_combined$lower)) || any(is.na(df_combined$upper))) {
      stop("Error: NA values found in 'lower' or 'upper' columns.")
    }
    write.csv(df_combined, "my_data.csv", row.names = FALSE, fileEncoding = "UTF-8")  
    # #manual_data <- read.csv(csv_path, stringsAsFactors = FALSE)
    # input_file <- read.csv("my_data.csv", stringsAsFactors = FALSE)
    message("All input estimates successfully generated.")
    #print(as.estimate(df_combined))
    rownames(df_combined) <- 1:nrow(df_combined)
    df_combined <- df_combined[df_combined[1] != "0",] ### remove line with "0; 0; const" variable
    saveRDS(df_combined,"data.rds")
    return(df_combined)
  })
  ### End of gathering all inputs ####
  
  # Display validation message
  # output$validation_message <- renderText({
  #   validate_input()
  # })
  # output$display_version_1 <- renderText({paste("This is version:", input$version)})
  # Open the URL in a browser when the button is clicked
  # observeEvent(input$open_url, {
  #   url <- "https://www.gov.uk/government/publications/sustainable-farming-incentive-scheme-expanded-offer-for-2024"
  #   utils::browseURL(url) })
  