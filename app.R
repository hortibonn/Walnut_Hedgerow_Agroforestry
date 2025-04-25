# ---- Install Libraries ----
if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}
library(shiny)

if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}
library(readxl)

if (!requireNamespace("bslib", quietly = TRUE)) {
  install.packages("bslib")
}
library(bslib)

if (!requireNamespace("shinythemes", quietly = TRUE)) {
  install.packages("shinythemes")
}
library(shinythemes)

if (!requireNamespace("shinyWidgets", quietly = TRUE)) {
  install.packages("shinyWidgets")
}
library(shinyWidgets)

if (!requireNamespace("decisionSupport", quietly = TRUE)) {
  install.packages("decisionSupport")
}
library(decisionSupport)

if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
library(tidyverse)

if (!requireNamespace("readr", quietly = TRUE)) {
  install.packages("readr")
}
library(readr)  # For reading and writing CSV files

if (!requireNamespace("ggridges", quietly = TRUE)) {
  install.packages("ggridges")
}
library(ggridges)

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)
if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
}
library(here)

source("functions/dynamic-helper.R")
source("functions/Walnut_grain_veg_tub_mcsim-only.R")
source("functions/funding_server.R")
file_path_vars <- "data/Walnut_grain_veg_tub.xlsx"
sheet_meta <- readxl::read_excel(file_path_vars, sheet = "sheet_names",
                                 col_types = c("text", "text"))
sheet_names <- sheet_meta$sheet_names
sheet_icons <- setNames(sheet_meta$icon, sheet_meta$sheet_names)


#-----------------------------------------------------------------------------#
# ---- Pre-requisites to UI ---------------------------------------------------
#-----------------------------------------------------------------------------#

#-----------------------------------------------------------------------------#
## ---- Countries and states --------------------------------------------------
#-----------------------------------------------------------------------------#

# country_states <-
#   list(
#     "None" = c("none"),
#     "Belgium" = c("Flanders", "Wallonia", "Brussels"),
#     "Bulgaria" = c("Blagoevgrad", "Burgas", "Varna"),
#     "Czech Republic" = c("Prague", "Central Bohemian", "South Bohemian", "Plzeň",
#                          "Karlovy Vary", "Ústí nad Labem", "Liberec", "Hradec Králové",
#                          "Pardubice", "Vysočina", "South Moravian", "Olomouc",
#                          "Zlín", "Moravian-Silesian"),
#     "Denmark" = c("North Jutland", "Central Jutland", "Southern Denmark"),
#     "England" = c("Bedfordshire", "Berkshire", "Bristol", "Buckinghamshire",
#                   "Cambridgeshire", "Cheshire", "Cornwall", "Cumbria", "Derbyshire",
#                   "Devon", "Dorset", "Durham", "East Sussex", "Essex", "Gloucestershire",
#                   "Greater London", "Greater Manchester", "Hampshire", "Herefordshire",
#                   "Hertfordshire", "Isle of Wight", "Kent", "Lancashire",
#                   "Leicestershire", "Lincolnshire", "Merseyside", "Norfolk", "North Yorkshire",
#                   "Northamptonshire", "Northumberland", "Nottinghamshire", "Oxfordshire",
#                   "Rutland", "Shropshire", "Somerset", "South Yorkshire", "Staffordshire",
#                   "Suffolk", "Surrey", "Tyne and Wear", "Warwickshire", "West Midlands",
#                   "West Sussex", "West Yorkshire", "Wiltshire", "Worcestershire"),
#     "France" = c("Auvergne-Rhône-Alpes", "Bourgogne-Franche-Comté", "Brittany", 
#                  "Centre-Val de Loire", "Corsica", "French Guiana", "Grand Est", 
#                  "Hauts-de-France", "Île-de-France", "Martinique", "Mayotte", "Normandy",
#                  "Nouvelle-Aquitaine", "Occitanie", "Pays de la Loire", 
#                  "Provence-Alpes-Côte d'Azur", "Réunion"),
#     "Germany" = c("Baden-Württemberg", "Bavaria", "Berlin", "Brandenburg", "Bremen",
#                   "Hamburg", "Hesse", "Lower Saxony", "Mecklenburg-Vorpommern",
#                   "North Rhine-Westphalia", "Rhineland-Palatinate", "Saarland",
#                   "Saxony", "Saxony-Anhalt", "Schleswig-Holstein", "Thuringia"),
#     "Hungary" = c("Budapest", "Pest", "Csongrád"),
#     "Italy" = c("Abruzzo", "Aosta Valley", "Apulia", "Basilicata", "Calabria", "Campania",
#                 "Emilia-Romagna", "Friuli-Venezia Giulia", "Lazio", "Liguria", "Lombardy",
#                 "Marche", "Molise", "Piedmont", "Sardinia", "Sicily", "Trentino-South Tyrol",
#                 "Tuscany", "Umbria", "Veneto"),
#     "Northern Ireland" = c("Belfast", "Derry", "Lisburn"),
#     "Portugal" = c("Açores", "Alentejo", "Algarve", "Centro", "Grande Lisboa", "Madeira", 
#                    "Norte", "Península de Setúbal", "Oeste e Vale do Tejo"),
#     "Scotland" = c("Edinburgh", "Glasgow", "Aberdeen"),
#     "Spain" = c("Andalucía", "Aragón", "Asturias", "Baleares", "Canarias", "Cantabria", 
#                 "Castilla-La Mancha", "Castilla y León", "Cataluña", "Extremadura", "Galicia", 
#                 "Madrid", "Murcia", "Navarra", "País Vasco", "La Rioja", "Comunidad Valenciana"),
#     "Wales" = c("Cardiff", "Swansea", "Newport")
#   )
# 
# #-----------------------------------------------------------------------------#
# ## ---- Funding data per country and state ------------------------------------
# #-----------------------------------------------------------------------------#
# # For demonstration purposes, I'll add example funding schemes for a few countries and states
# funding_data <- 
#   list(
#     "None" = list(
#       "none" = list(
#         one_time_funding_schemes = c(),
#         funding_onetime_values_named = setNames(c(),
#                                                 c()),
#         funding_onetime_per_ha_schemes = c(),
#         funding_onetime_per_tree_schemes = c(),
#         funding_onetime_percentage_schemes = c(),
#         annual_funding_schemes = c(),
#         funding_yearly_values_named = setNames(c(),c())
#       )
#     ),
#     "England" = list(
#       "Northumberland" = list(
#         # one-time
#         one_time_funding_schemes = c(
#           "Countryside Stewardship - PA4 Agroforestry [GBP/ha]",
#           "Countryside Stewardship - AF Woodland trees [GBP/tree]",
#           "Countryside Stewardship - AF Fruit trees [GBP/tree]",
#           "Tree guard (TE6) [GBP/guard/tree]",
#           "Species Diversity Bonus (if more than 5 diff. species) [GBP/tree]",
#           "Woodland Trust - MOREwoods Scheme (min. 0.5 ha woodland with 500+ trees) [% of initial costs]",
#           "Woodland Trust - MOREwoods Scheme with contractor (min. 1 ha woodland) [% of initial costs]"
#         ),
#         funding_onetime_values_named = setNames(c(1268.08, 5.40, 17.83, 3.95, 1.16, 0.75, 0.6),
#                                                 c("Countryside Stewardship - PA4 Agroforestry [GBP/ha]",
#                                                   "Countryside Stewardship - AF Woodland trees [GBP/tree]",
#                                                   "Countryside Stewardship - AF Fruit trees [GBP/tree]",
#                                                   "Tree guard (TE6) [GBP/guard/tree]",
#                                                   "Species Diversity Bonus (if more than 5 diff. species) [GBP/tree]",
#                                                   "Woodland Trust - MOREwoods Scheme (min. 0.5 ha woodland with 500+ trees) [% of initial costs]",
#                                                   "Woodland Trust - MOREwoods Scheme with contractor (min. 1 ha woodland) [% of initial costs]"
#                                                 ) 
#         ),
#         funding_onetime_per_ha_schemes = c("Countryside Stewardship - PA4 Agroforestry [GBP/ha]"),
#         funding_onetime_per_tree_schemes = c("Countryside Stewardship - AF Woodland trees [GBP/tree]",
#                                              "Countryside Stewardship - AF Fruit trees [GBP/tree]",
#                                              "Tree guard (TE6) [GBP/guard/tree]",
#                                              "Species Diversity Bonus (if more than 5 diff. species) [GBP/tree]"
#         ),
#         funding_onetime_percentage_schemes = c("Woodland Trust - MOREwoods Scheme (min. 0.5 ha woodland with 500+ trees) [% of initial costs]",
#                                                "Woodland Trust - MOREwoods Scheme with contractor (min. 1 ha woodland) [% of initial costs]"
#         ),
#         #annual
#         annual_funding_schemes = c("SFI Premium payment - low density AF on less sensitive land [GBP/ha/year]",
#                                    "SFI Premium payment - low density AF on more sensitive land [GBP/ha/year]",
#                                    "SFI Premium payment - medium density in-field AF [GBP/ha/year]",
#                                    "SFI Premium payment - high density in-field AF [GBP/ha/year]"
#         ),
#         funding_yearly_values_named = setNames(c(385, 385, 595, 849),
#                                                c("SFI Premium payment - low density AF on less sensitive land [GBP/ha/year]",
#                                                  "SFI Premium payment - low density AF on more sensitive land [GBP/ha/year]",
#                                                  "SFI Premium payment - medium density in-field AF [GBP/ha/year]",
#                                                  "SFI Premium payment - high density in-field AF [GBP/ha/year]"
#                                                ) 
#         )
#       ),
#       # not real subsidy schemes!!!
#       "Kent" = list(
#         one_time_funding_schemes = c("Kent Scheme - Tree Planting [GBP/tree]",
#                                      "Kent Scheme - Land Preparation [GBP/ha]"),
#         funding_onetime_values_named = setNames(c(10, 500),
#                                                 c("Kent Scheme - Tree Planting [GBP/tree]",
#                                                   "Kent Scheme - Land Preparation [GBP/ha]")),
#         funding_onetime_per_ha_schemes = c("Kent Scheme - Land Preparation [GBP/ha]"),
#         funding_onetime_per_tree_schemes = c("Kent Scheme - Tree Planting [GBP/tree]"),
#         funding_onetime_percentage_schemes = c(),
#         annual_funding_schemes = c("Kent Annual Support [GBP/ha/year]" ),
#         funding_yearly_values_named = setNames(c(100),c("Kent Annual Support [GBP/ha/year]"))
#       )
#     ),
#     # Add more countries and states with their funding schemes here - T6.3
#     "Germany" = list(
#       "Bavaria" = list(one_time_funding_schemes = c("Bavaria Start-up Aid [EURO/ha]",
#                                                     "Bavaria Tree Grant [EURO/tree]" ),
#                        funding_onetime_values_named = setNames(c(800, 15),
#                                                                c( "Bavaria Start-up Aid [EURO/ha]",
#                                                                   "Bavaria Tree Grant [EURO/tree]")),
#                        funding_onetime_per_ha_schemes = c("Bavaria Start-up Aid [EURO/ha]"),
#                        funding_onetime_per_tree_schemes = c("Bavaria Tree Grant [EURO/tree]"),
#                        funding_onetime_percentage_schemes = c(),
#                        annual_funding_schemes = c("Bavaria Annual Payment [EURO/ha/year]"),
#                        funding_yearly_values_named = setNames(c(200),c("Bavaria Annual Payment [EURO/ha/year]") )
#       )
#     ),
#     "Belgium" = list(
#       "Flanders" = list(one_time_funding_schemes = c(
#         "CAP establishment support [% of total establishment costs]"
#       ),
#       funding_onetime_values_named = setNames(c(0.75),
#                                               c("CAP establishment support [% of total establishment costs]"
#                                               )
#       ),
#       funding_onetime_percentage_schemes = c("CAP establishment support [% of total establishment costs]"),
#       annual_funding_schemes = c("Flanders Annual Payment [EURO/ha/year]"),
#       funding_yearly_values_named = setNames(c(378),c("Flanders Annual Payment [EURO/ha/year]"))
#       )
#     )
#     
#   )

#-----------------------------------------------------------------------------#
# ---- Main UI function (skeleton) --------------------------------------------
#-----------------------------------------------------------------------------#
ui <- fluidPage(
  
  theme = bs_theme(version = 5,
                   bootswatch = 'flatly',
                   base_font = font_google("Roboto")), 
  
  titlePanel(
    tags$div(
      style = "display:flex; align-items:center;justify-content:space-between;
      width: 100% !important; margin: 20px; padding: 0 15px;
      box-sizing: border-box; background-color: #f2f2f2;",
      
      # tags$a(href = "https://www.uni-bonn.de", target = "_blank",
      tags$img(src = "UniBonnHortiBonn_logo_transparent.png", height = "100px",
               style = "margin-left: auto; max-width: 20%; height: auto; cursor: pointer;"),
      # ),
      
      tags$h2(tags$div("Decision Analysis:"),
              tags$div("conversion of treeless cropland into alley-cropping"),
              style = "text-align: center; flex-grow: 1;"),
      
      # tags$a(href = "https://www.uni-bonn.de", target = "_blank",
      tags$img(src = "ReFOREST_logo_horizontal_transparent.png", height = "100px",
               style = "margin-right: auto; max-width: 30%; height: auto; cursor: pointer;")
      # ),
    ),
  ),
  
  sidebarLayout(
    sidebarPanel(width = 4,
                 style = "height: 100%; overflow-y: auto",
                 
                 accordion(
                   id = "collapseSidebar",
                   open = FALSE,
                   
                   br(),
                   div(
                     class = "text-center",
                     actionButton("run_simulation", "Run Model",
                                  icon = icon("play"), class = "btn-primary")
                   ),
                   br(),
                   accordion_panel(
                     title = "Expertise categories",
                     icon = icon("clipboard-question"),
                     tagList(
                       tags$h5(
                         "Expertise categories",
                         tags$span(
                           icon("circle-question"),
                           title = "Select your main expertise to modify only the variables suited to such expertise. Ultimately, we are all experts to a higher or lower degree in all categories. You too :) 
                           
                           But we recommend that the first time you use this interface you select only one or two categories in order to get familiar with it. The simulations will still run with the default values for those expertise categories that you do not select.
                           
                           If you do not check any box, you will see all the variables of the model (i.e.: all expertise categories).
                           
                           If you are the decision maker, e.g.: you are a farmer thinking about implementing agroforestry in treeles land, please check the Decision Maker box to set basic variables that serve to quantify your interests and motivations",
                           style = "cursor: help; margin-left: 8px;"
                         )
                       ),
                       uiOutput("category_filter_ui")
                     )
                   ),
                   br(),
                   
                   
                   
                   # accordion_panel(
                   #   title = "Save/Load Project",
                   #   icon = icon("floppy-disk"),
                   #   textInput("project_name", "Project Name:", value = ""),
                   #   actionButton("save", "Save Settings")
                   # ),
                   
                   
                   # --------------------------------------------------------------------
                   #  *NEW* Funding schemes – pulled from Excel by the module  -----------
                   # --------------------------------------------------------------------
                   accordion_panel(
                     title = "Funding schemes", icon = icon("euro-sign"),
                     
                     # ↓↓↓ the next line replaces ~100 lines of hand‑built inputs
                     create_funding_ui("funding")
                   ),
                   # --------------------------------------------------------------------
                   # Old Funding schemes – pulled from Excel by the module  -----------
                   # --------------------------------------------------------------------
                   
                   #   # Dropdown menu of countries, state and funding schemes
                   #   selectInput("country", "Select Country:", choices = names(country_states), selected = NULL),
                   #   uiOutput("state_ui"),
                   #   uiOutput("funding_one_ui"),
                   #   uiOutput("funding_yearly_ui"),
                   #   numericInput("annual_external_support_c", "Annual private support [GBP/ha]",
                   #                min = 1, max = 5000, value = 50),
                   #   numericInput("onetime_external_support_c", "One-time private support [GBP]",
                   #                min = 1, max = 1000000, value = 500)
                   # ),
                   uiOutput("dynamic_element_ui")
                   
                 )
                 
    ),
    mainPanel(width = 8, 
              plotOutput("plot1_ui"),
              plotOutput("plot2_ui"),
              plotOutput("plot3_ui"),
              plotOutput("plot4_ui"),
              plotOutput("plot5_ui"),
              plotOutput("plot6_ui"),
              plotOutput("plot7_ui"),
              plotOutput("plot8_ui"),
              plotOutput("plot9_ui")
    )
  )
  
)

#-----------------------------------------------------------------------------#
# ---- Server logic -----------------------------------------------------------
#-----------------------------------------------------------------------------#

server <- function(input, output, session) {
  
  # -------------------------------------------------------------------------
  # ---- Dynamic funding module ---------------------------------------------
  # -------------------------------------------------------------------------
  funding <- funding_server("funding")   # returns a list of reactives
  
  # Helper for safe extraction from named vector ----------------------------
  safe_get <- function(vec, name) {
    if (is.null(vec) || length(vec) == 0 || is.na(vec[name])) return(0)
    if (! name %in% names(vec)) return(0)
    as.numeric(vec[name])
  }
  
  # -------------------------------------------------------------------------
  # ---- Wrapper → *exact* variables the walnut model expects ---------------
  # -------------------------------------------------------------------------
  funding_variables <- reactive({
    sel <- funding$category_totals()        # named vector per category (gov)
    
    # private inputs ---------------------------------------------------------
    onetime_private <- funding$onetime_private_input()
    annual_private  <- funding$annual_private_input()
    
    # area / trees already entered elsewhere in UI --------------------------
    AF1_area  <- max(0, as.numeric(input$arable_area_c) - as.numeric(input$AF1_tree_row_area_c))
    AF2_area  <- max(0, as.numeric(input$arable_area_c) - as.numeric(input$AF2_tree_row_area_c))
    AF1_trees <- as.numeric(input$AF1_num_trees_c)
    AF2_trees <- sum(as.numeric(input$num_oak_trees_c), as.numeric(input$num_birch_trees_c),
                     as.numeric(input$num_rowan_trees_c), as.numeric(input$num_hazel_trees_c),
                     as.numeric(input$num_damson_trees_c), as.numeric(input$num_bcherry_trees_c))
    
    # funding amounts per category -----------------------------------------
    per_ha      <- safe_get(sel, "funding_onetime_per_ha_schemes_c")
    per_tree    <- safe_get(sel, "funding_onetime_per_tree_schemes_c")
    annual_ha   <- safe_get(sel, "annual_funding_schemes_c")
    perc_incost <- safe_get(sel, "funding_onetime_percentage_incost_schemes_c")
    perc_cons   <- safe_get(sel, "funding_onetime_percentage_consult_schemes_c")
    
    # Compute totals --------------------------------------------------------
    AF1_one_time   <- per_ha   * AF1_area  + per_tree * AF1_trees + onetime_private
    AF2_one_time   <- per_ha   * AF2_area  + per_tree * AF2_trees + onetime_private
    AF1_annual     <- annual_ha * AF1_area + annual_private * AF1_area
    AF2_annual     <- annual_ha * AF2_area + annual_private * AF2_area
    
    AF1_perc <- perc_incost   # tie whichever category you decide to AF1
    AF2_perc <- perc_cons     # … and AF2
    any_perc <- as.numeric((AF1_perc + AF2_perc) > 0)
    
    data.frame(
      variable = c("AF1_total_annual_funding_c", "AF2_total_annual_funding_c",
                   "AF1_total_one_time_funding_c", "AF2_total_one_time_funding_c",
                   "AF2_percentage_values_c", "AF1_percentage_values_c",
                   "selected_percentage_c"),
      lower = c(AF1_annual, AF2_annual, AF1_one_time, AF2_one_time,
                AF2_perc, AF1_perc, any_perc),
      upper = c(AF1_annual, AF2_annual, AF1_one_time, AF2_one_time,
                AF2_perc, AF1_perc, any_perc),
      distribution = "const",
      stringsAsFactors = FALSE
    )
  })
  
  
  # -------------------------------------------------------------------------
  # ---- 2.3  *Old* funding machinery – retained for reference -------------------
  # -------------------------------------------------------------------------
  # # Ensure reactive values are used correctly
  # tidy_funding_data <- reactive({
  #   if (exists("funding_data")) {
  #     return(isolate(funding_data()))
  #   }
  #   return(list())
  # })
  # 
  # funding_data_reactive <- reactive({ return (funding_data) })
  # 
  # # Fix UI rendering to use tagList instead of list
  # output$state_ui <- renderUI({
  #   if (is.null(input$country)) return(NULL)
  #   req(input$country)
  #   tagList(
  #     selectInput("state", "Select State:", choices = country_states[[input$country]], selected = NULL)
  #   )
  # })
  # # Show funding schemes dropdown after state selection
  # # One-time funding Schemes
  # output$funding_one_ui <- renderUI({
  #   req(input$country, input$state)
  #   country <- input$country
  #   state <- input$state
  #   
  #   if (!is.null(funding_data_reactive()[[country]]) && !is.null(funding_data_reactive() [[country]][[state]])) {
  #     state_data <- funding_data_reactive() [[country]][[state]]
  #     return(tagList(
  #       selectInput("funding_one_ui", "Select One-time Funding Scheme(s):", 
  #                   choices = if (!is.null(state_data$one_time_funding_schemes)) state_data$one_time_funding_schemes else list(),
  #                   multiple = TRUE)
  #     ))
  #   }
  #   return(NULL)
  # })
  # # Annual funding Schemes
  # output$funding_yearly_ui <- renderUI({
  #   req(input$state, input$country)
  #   country <- input$country
  #   state <- input$state
  #   
  #   if (!is.null(funding_data_reactive() [[country]]) && !is.null(funding_data_reactive() [[country]][[state]])) {
  #     state_data <- funding_data_reactive()[[country]][[state]]
  #     return(tagList(
  #       selectInput("funding_yearly_ui", "Select Annual Funding Scheme(s):", 
  #                   choices = if (!is.null(state_data$annual_funding_schemes)) state_data$annual_funding_schemes else list(),
  #                   multiple = FALSE)
  #     ))
  #   }
  #   return(NULL)
  # })
  # 
  # #### Funding estimates ####
  # funding_variables <- reactive({
  #   message("Accessing funding estimates...")
  #   country <- input$country
  #   state   <- input$state
  #   
  #   # Default funding values (0 when no country/state is selected)
  #   selected_percentage_c <- 0
  #   AF1_total_one_time_funding_c  <- 0
  #   AF1_total_annual_funding_c    <- 0
  #   AF1_percentage_values_c       <- 0
  #   AF2_total_one_time_funding_c  <- 0
  #   AF2_total_annual_funding_c    <- 0
  #   AF2_percentage_values_c       <- 0
  #   
  #   # Validate area calculations (default to 0 if inputs are missing)
  #   AF1_area <- max(0, as.numeric(input$arable_area_c) - as.numeric(input$AF1_tree_row_area_c))
  #   AF2_area <- max(0, as.numeric(input$arable_area_c) - as.numeric(input$AF2_tree_row_area_c))
  #   
  #   # Default funding values
  #   AF1_one_funding <- 0
  #   AF1_annual_funding <- 0
  #   AF2_one_funding <- 0
  #   AF2_annual_funding <- 0
  #   
  #   # Only process funding calculations if both country and state are selected
  #   if (!is.null(country) && !is.null(state) && country != "None" && state != "None") {
  #     if (!is.null(funding_data_reactive()[[country]]) &&
  #         !is.null(funding_data_reactive()[[country]][[state]])) {
  #       
  #       state_data <- funding_data_reactive()[[country]][[state]]
  #       
  #       # ---- AF1 One-Time Funding ----
  #       AF1_selected_per_ha <- intersect(input$funding_one_ui, state_data$funding_onetime_per_ha_schemes)
  #       if (length(AF1_selected_per_ha) > 0) {
  #         AF1_per_ha_values <- state_data$funding_onetime_values_named[AF1_selected_per_ha]
  #         AF1_one_funding <- sum(AF1_per_ha_values) * AF1_area
  #       }
  #       
  #       AF1_selected_per_tree <- intersect(input$funding_one_ui, state_data$funding_onetime_per_tree_schemes)
  #       if (length(AF1_selected_per_tree) > 0) {
  #         AF1_per_tree_values <- state_data$funding_onetime_values_named[AF1_selected_per_tree]
  #         AF1_one_funding <- AF1_one_funding + sum(AF1_per_tree_values) * input$AF1_num_trees_c
  #       }
  #       
  #       AF1_selected_percentage <- intersect(input$funding_one_ui, state_data$funding_onetime_percentage_schemes)
  #       if (length(AF1_selected_percentage) > 0) {
  #         selected_percentage_c <- 1
  #         AF1_percentage_values_c <- sum(state_data$funding_onetime_values_named[AF1_selected_percentage])
  #       }
  #       
  #       AF1_one_funding <- AF1_one_funding + input$onetime_external_support_c
  #       AF1_total_one_time_funding_c <- AF1_one_funding
  #       
  #       # ---- AF1 Annual Funding ----
  #       AF1_selected_annual <- intersect(input$funding_yearly_ui, state_data$annual_funding_schemes)
  #       if (length(AF1_selected_annual) > 0) {
  #         AF1_annual_values <- state_data$funding_yearly_values_named[AF1_selected_annual]
  #         AF1_annual_funding <- sum(AF1_annual_values) * field_area_c
  #       }
  #       
  #       AF1_total_annual_funding_c <- AF1_annual_funding + input$annual_external_support_c * field_area_c
  #       
  #       # ---- AF2 One-Time Funding ----
  #       AF2_selected_per_ha <- intersect(input$funding_one_ui, state_data$funding_onetime_per_ha_schemes)
  #       if (length(AF2_selected_per_ha) > 0) {
  #         AF2_per_ha_values <- state_data$funding_onetime_values_named[AF2_selected_per_ha]
  #         AF2_one_funding <- sum(AF2_per_ha_values) * AF2_area
  #       }
  #       
  #       AF2_selected_per_tree <- intersect(input$funding_one_ui, state_data$funding_onetime_per_tree_schemes)
  #       if (length(AF2_selected_per_tree) > 0) {
  #         AF2_per_tree_values <- state_data$funding_onetime_values_named[AF2_selected_per_tree]
  #         total_AF2_trees <- sum(input$num_oak_trees_c, input$num_birch_trees_c,
  #                                input$num_rowan_trees_c, input$num_hazel_trees_c,
  #                                input$num_damson_trees_c, input$num_bcherry_trees_c)
  #         AF2_one_funding <- sum(AF2_per_tree_values) * total_AF2_trees
  #       }
  #       
  #       AF2_selected_percentage <- intersect(input$funding_one_ui, state_data$funding_onetime_percentage_schemes)
  #       if (length(AF2_selected_percentage) > 0) {
  #         selected_percentage_c <- 1
  #         AF2_percentage_values_c <- sum(state_data$funding_onetime_values_named[AF2_selected_percentage])
  #       }
  #       
  #       AF2_one_funding <- AF2_one_funding + input$onetime_external_support_c
  #       AF2_total_one_time_funding_c <- AF2_one_funding
  #       
  #       # ---- AF2 Annual Funding ----
  #       AF2_selected_annual <- intersect(input$funding_yearly_ui, state_data$annual_funding_schemes)
  #       if (length(AF2_selected_annual) > 0) {
  #         AF2_annual_values <- state_data$funding_yearly_values_named[AF2_selected_annual]
  #         AF2_annual_funding <- sum(AF2_annual_values) * AF2_area
  #       }
  #       
  #       AF2_annual_funding <- AF2_annual_funding + input$annual_external_support_c * AF2_area
  #       AF2_total_annual_funding_c <- AF2_annual_funding
  #     }
  #   }
  #   
  #   # ---- Store Results in Data Frame ----
  #   funding_parameters <- c(
  #     "AF1_total_annual_funding_c", "AF2_total_annual_funding_c",
  #     "AF1_total_one_time_funding_c", "AF2_total_one_time_funding_c",
  #     "AF2_percentage_values_c", "AF1_percentage_values_c",
  #     "selected_percentage_c")
  #   selected_values_funding <- c(
  #     AF1_total_annual_funding_c, AF2_total_annual_funding_c,
  #     AF1_total_one_time_funding_c, AF2_total_one_time_funding_c,
  #     AF2_percentage_values_c, AF1_percentage_values_c,
  #     selected_percentage_c)
  #   funding_variables_df <- data.frame(
  #     variable = funding_parameters,
  #     lower = selected_values_funding,
  #     upper = selected_values_funding,
  #     distribution = rep("const", length(funding_parameters)),
  #     stringsAsFactors = FALSE
  #   )
  #   message("Funding Variables Updated: ")
  #   #print(funding_variables_df)
  #   return(funding_variables_df)
  # })
  # #### End of funding estimates ####
  
  
  # helper that sanitises category names into safe IDs
  sanitize <- function(x) gsub("[^A-Za-z0-9]", "_", x)
  
  #  all categories across every sheet  ----
  categories <- reactive({
    cats <- unique(unlist(lapply(excelData(), function(df) df$Expertise)))
    cats <- cats[!is.na(cats) & cats != ""]
    trimws(unique(unlist(strsplit(cats, ";"))))
  })
  
  # filter bar UI  ----
  output$category_filter_ui <- renderUI({
    if (length(categories()) == 0) return(NULL)
    tagList(
      lapply(categories(), function(cat){
        checkboxInput(
          paste0("cat_", sanitize_id(cat)), cat, value = FALSE)
      }),
      tags$hr()
    )
  })
  
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
  
  #-----------------------------------------------------------------------------#
  ## ---- dynamic UI ------------------------------------------------------------
  #-----------------------------------------------------------------------------#
  
  excelData <- reactive({
    sheet_number <- seq_along(sheet_names)+1
    all_sheets <- lapply(sheet_number, function(sht) {
      readxl::read_excel(file_path_vars, sheet = sht,
                         ,col_types = c("text", "numeric", "numeric", "text", "text", "text", "text", "guess", "guess", "text")
      )
    })
    names(all_sheets) <- sheet_names
    all_sheets
  })
  
  
  # ------ util: turns a category vector into a JS condition --------------------
  panel_condition <- function(cat_vec) {
    cat_vec <- trimws(cat_vec)
    cat_vec <- cat_vec[cat_vec != "" & !is.na(cat_vec)]
    if (length(cat_vec) == 0) return("true")   # always show
    ids <- sprintf("input['cat_%s']", sanitize_id(cat_vec))
    show_all <- paste0(
      "Object.keys(input).filter(k=>k.startsWith('cat_')).",
      "every(k=>input[k]===false)"
    )
    sprintf("(%s) || (%s)", show_all, paste(ids, collapse = " || "))
  }
  
  
  output$dynamic_element_ui <- renderUI({
    
    data_list   <- excelData()
    sheet_names <- names(data_list)
    
    # build one accordion panel per sheet --------------------------------------
    panels <- lapply(seq_along(data_list), function(j) {
      
      sheet <- data_list[[j]]
      
      cats <- unique(trimws(unlist(strsplit(sheet$Expertise %||% "", ";|,"))))
      cats <- cats[cats != ""]
      
      ui_elems <- lapply(seq_len(nrow(sheet)), function(i) {
        create_ui_element(sheet[i, ])
      })
      
      cond <- panel_condition(cats)   # JS condition built earlier
      
      conditionalPanel(
        condition = cond,             # << wrap entire panel
        accordion_panel(
          title = sheet_names[j],
          icon  = icon(sheet_icons[[ sheet_names[j] ]] %||% "circle-dot"),
          tagList(ui_elems)
        )
      )
    })
    
    tagList(panels)   # render the list
  })
  
  #-----------------------------------------------------------------------------#
  ## ---- Monte Carlo Simulation ------------------------------------------------
  #-----------------------------------------------------------------------------#
  
  mcSimulation_results <- eventReactive(input$run_simulation, {
    
    message("Accessing user input estimates from the interface...")
    
    # ---- 1. Gather current widget values --------------------------------------
    exclude_inputs <- c("collapseSidebar", "save", "load", "delete",
                        "confirm_delete", "admin_selected_user",
                        "project_name", "version_select", "delete_version_select")
    
    variables <- setdiff(
      names(input)[grepl("(_c$|_p$|_t$|_n$|_cond$)", names(input))],
      exclude_inputs
    )
    
    lower_values <- sapply(variables, function(v) {
      val <- input[[v]]
      if (length(val) == 1) as.numeric(val) else as.numeric(val[1])
    })
    upper_values <- sapply(variables, function(v) {
      val <- input[[v]]
      if (length(val) == 1) as.numeric(val) else as.numeric(val[2])
    })
    
    # ---- 2. Re-read Excel (keeps original bounds & distributions) -------------
    all_sheets <- excelData()            # list of data-frames
    input_file <- bind_rows(all_sheets)  # one big table
    
    # Overwrite lower/upper with current UI inputs
    input_file <- input_file %>%
      left_join(
        tibble(variable = variables,
               lower    = lower_values,
               upper    = upper_values),
        by = "variable",
        suffix = c("", ".new")
      ) %>%
      mutate(
        lower = coalesce(lower.new, lower),
        upper = coalesce(upper.new, upper)
      ) %>%
      select(-ends_with(".new"))
    
    # ---- 3. Append funding scalars -------------------------------------------
    fund_df <- funding_variables()
    fund_df$lower[is.na(fund_df$lower)] <- 0
    fund_df$upper[is.na(fund_df$upper)] <- 0
    
    input_file <- bind_rows(
      input_file %>% filter(!variable %in% fund_df$variable),
      fund_df
    )
    
    # ---- 4. Save UI snapshot (optional) ---------------------------------------
    saveRDS(list(sheet_names, input_file), "data/Walnut_grain_veg_tub_ui_updated.RDS")
    
    # ---- 5. FINAL clean-up: keep only numeric rows ----------------------------
    input_file <- input_file %>%
      filter(
        !is.na(lower), !is.na(upper),
        is.finite(lower), is.finite(upper)
      )
    
    # ---- 6. Run Monte-Carlo ---------------------------------------------------
    decisionSupport::mcSimulation(
      estimate          = decisionSupport::as.estimate(input_file),
      model_function    = Walnut_grain_veg_tub,
      numberOfModelRuns = input$num_simulations_c,
      functionSyntax    = "plainNames"
    )
  })
  
  
  observeEvent(mcSimulation_results(), {
    
    mc_data <- mcSimulation_results()
    
    #-----------------------------------------------------------------------------#
    ## ---- generating plots ------------------------------------------------------
    #-----------------------------------------------------------------------------#
    plot1 <- 
      decisionSupport::plot_distributions(mcSimulation_object = mc_data, 
                                          vars = c("NPV_Agroforestry_System1", "NPV_Treeless_System"),
                                          method = 'smooth_simple_overlay',
                                          old_names = c("NPV_Agroforestry_System1", "NPV_Treeless_System"),
                                          new_names = c("Agroforestry intervention", "Cultivation without integrating trees"),
                                          x_axis_name = "NPV (€)",
                                          y_axis_name = "Probability")+
      ggtitle("Net Present Value of a farming decision")+
      #ggtitle("Carbon stock of in the aboveground biomass of a clear-cut agroforestry plot")+
      theme(plot.title = element_text(hjust = 0.5),
            #axis.text.y=element_blank(),
            #axis.text.x=element_blank(),
            #axis.line = element_line(arrow = arrow(type="open")),
            #panel.border = element_blank(),
            legend.position="bottom")
    #xlim(0, 1e7)
    
    
    plot2 <- 
      decisionSupport::plot_distributions(mcSimulation_object = mc_data, 
                                          vars = c("NPV_decision_AF1"),
                                          method = 'smooth_simple_overlay',
                                          old_names = c("NPV_decision_AF1"),
                                          new_names = c("Agroforestry intervention"),
                                          x_axis_name = "NPV (€)",
                                          y_axis_name = "Probability")+
      ggtitle("Net Present Value of the decision to undertake the agroforestry intervention")+
      #ggtitle("Carbon stock of in the aboveground biomass of a clear-cut agroforestry plot")+
      theme(plot.title = element_text(hjust = 0.5),
            #axis.text.y=element_blank(),
            #axis.text.x=element_blank(),
            #axis.line = element_line(arrow = arrow(type="open")),
            #panel.border = element_blank(),
            legend.position="none"
      )
    #xlim(0, 1e7)
    
    plot3 <- 
      decisionSupport::plot_distributions(mcSimulation_object = mc_data, 
                                          vars = c("planing_costs", "planting_cost", "strips_maintenance", "application_cost"),
                                          method = 'boxplot',
                                          old_names = c("planing_costs", "planting_cost", "strips_maintenance","application_cost"),
                                          new_names = c("Planning and design", "Planting", "Maintenance", "Bureaucratic work time"),                                          
                                          x_axis_name = "sum (€) over the whole simulation period",
                                          y_axis_name = "Cost categories")+
      ggtitle("Costs of an agroforestry intervention (€)")+
      theme(plot.title = element_text(hjust = 0.5))
    
    plot4 <- 
      decisionSupport::plot_cashflow(mcSimulation_object = mc_data, 
                                     cashflow_var_name = "Cashflow_AF1", 
                                     x_axis_name = "Timeline of the intervention (years)",
                                     y_axis_name = "Cashflow (€)")+
      ggtitle("Cashflow of the agroforestry intervention")+
      theme(plot.title = element_text(hjust = 0.5))#+
    
    plot5 <- 
      decisionSupport::plot_cashflow(mcSimulation_object = mc_data, 
                                     cashflow_var_name = "Cashflow_AF1_decision", 
                                     x_axis_name = "Timeline of the intervention (years)",
                                     y_axis_name = "Cashflow (€)")+
      ggtitle("Cashflow of the agroforestry decision")+
      theme(plot.title = element_text(hjust = 0.5))#+
    
    plot6 <- 
      decisionSupport::plot_cashflow(mcSimulation_object = mc_data, 
                                     cashflow_var_name = "Cum_Cashflow_AF1_decision", 
                                     x_axis_name = "Timeline of the intervention (years)",
                                     y_axis_name = "Cumulative Cashflow (€)")+
      ggtitle("Cumulative Cashflow of the agroforestry decision")+
      theme(plot.title = element_text(hjust = 0.5))#+
    
    # ---- Send plots to UI immediately ---------------------------------------------
    output$plot1_ui <- renderPlot({ plot1 })
    output$plot2_ui <- renderPlot({ plot2 })
    output$plot3_ui <- renderPlot({ plot3 })
    output$plot4_ui <- renderPlot({ plot4 })
    output$plot5_ui <- renderPlot({ plot5 })
    output$plot6_ui <- renderPlot({ plot6 })
    
    # ---- Ask user whether to run EVPI (takes time!) -------------------------------
    showModal(modalDialog(
      title = "Run EVPI analysis?",
      "This step may take a while. Do you want to run the EVPI analysis now?",
      footer = tagList(
        modalButton("No"),
        actionButton("confirm_evpi", "Yes, run EVPI")
      )
    ))
    
    # ---- Handle user confirmation to run EVPI -------------------------------------
    observeEvent(input$confirm_evpi, {
      removeModal()  # remove popup
      
      # EVPI setup
      evpi_input <- as.data.frame(cbind(
        mc_data$x,
        NPV_decision_AF1 = mc_data$y$NPV_decision_AF1
      ))
      
      evpi_result <- decisionSupport::multi_EVPI(evpi_input, "NPV_decision_AF1")
      
      # Lookup for pretty labels
      var_lookup <- bind_rows(excelData()) %>%
        filter(!is.na(variable), !is.na(name)) %>%
        distinct(variable, name) %>%
        deframe()
      
      # Top 10 EVPI variables (or fewer if < 10 positive EVPI)
      if (!is.null(evpi_result$evpi)) {
        top_evpi <- evpi_result$evpi %>%
          filter(evpi > 0) %>%
          arrange(desc(evpi)) %>%
          slice_head(n = 10)
        
        evpi_result$evpi <- top_evpi  # update to plot only top
      }
      
      # EVPI plot
      plot7 <- plot_evpi(evpi_result, decision_vars = "NPV_decision_AF1") +
        scale_y_discrete(labels = function(x) var_lookup[x]) +
        ggtitle("EVPI for Each Variable") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5))
      
      output$plot7_ui <- renderPlot({ plot7 })
      
    })
    
  })
  
  #-----------------------------------------------------------------------------#
  ## ---- plotting --------------------------------------------------------------
  #-----------------------------------------------------------------------------#
  
}

shinyApp(ui = ui, server = server)
