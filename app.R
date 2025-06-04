# # Install + load libraries ----
# packages <- c("shiny", "readxl", "bslib", "sortable", "shinythemes", "shinyWidgets",
#               "decisionSupport", "tidyverse", "ggridges", "here",
#               "dplyr", "ggplot2", "readr")
# 
# if (Sys.info()[["sysname"]] == "Windows") {
#   install_if_missing <- function(pkg) {
#     if (!requireNamespace(pkg, quietly = TRUE))
#       install.packages(pkg, dependencies = TRUE)
#   }
#   ## install what’s missing
#   invisible(lapply(packages, install_if_missing))
# }
# 
# ## then attach everything
# lapply(packages, library, character.only = TRUE)

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

# source("functions/saveLoad-module.R")
source("functions/Walnut_grain_veg_tub_mcsim-only.R")
# source("functions/Walnut_grain_veg_tub_df.R")
source("functions/dynamic-helper.R")
source("functions/funding_server.R")

file_path_vars <- "data/Walnut_grain_veg_tub.xlsx"
sheet_meta <- readxl::read_excel(file_path_vars, sheet = "sheet_names",
                                 col_types = c("text", "text"))
sheet_names <- sheet_meta$sheet_names
sheet_icons <- setNames(sheet_meta$icon, sheet_meta$sheet_names)





# UI ----
ui <- fluidPage(
  
  theme = bs_theme(version = 5,
                   bootswatch = 'flatly',
                   base_font = font_google("Roboto")), 
  ## Title ----
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
  
  ## Sidebar ----
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
                   
                   ### Save/Load functionality ----
                   # saveLoadUI("savemod"),
                   accordion_panel(
                     title = "Save / Load project", icon = icon("folder-open"),
                     tagList(
                       textInput("state_name", "Project name"),
                       actionButton("save_btn",  label = tagList(icon("floppy-disk"), "Save"),
                                    class = "btn btn-dark"),
                       
                       br(), br(),
                       selectInput("state_picker", "Saved versions", choices = NULL),
                       
                       fluidRow(
                         column(6, actionButton("load_btn",   tagList(icon("rotate"),  "Load"),
                                                class = "btn btn-secondary")),
                         column(6, actionButton("delete_btn", tagList(icon("trash"),   "Delete"),
                                                class = "btn btn-secondary"))
                       ),
                       hr(),
                       downloadButton("download_csv", label = tagList(icon("download"), "Download current inputs (.csv)"))
                     )
                   ),
                   
                   
                   
                   ### Expertise filter ----
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
                   
                   # ### Crop selector and rotation ----
                   # accordion_panel(
                   #   title = "Crops",
                   #   icon = icon("clipboard-question"),
                   #   accordion_panel(
                   #     title = "Crop selector",
                   #     icon = icon("seedling"),
                   #     uiOutput("crop_rot_filter_ui")
                   #   ),
                   #   uiOutput("rotation_builder_ui"),   # rendered only when crops picked
                   #   verbatimTextOutput("rotation_vec") # convenient preview
                   # ),
                   
                   ### funding scheme ----
                   accordion_panel(
                     title = "Funding schemes", icon = icon("euro-sign"),
                     create_funding_ui("funding")
                   ),
                   
                   uiOutput("dynamic_element_ui")
                   
                 )
                 
    ),
    
    ## Main Panel ----
    mainPanel(width = 8,
              # tags$p(
              #   tags$strong(
              #     tagList(
              #       "This app simulates the present value of converting a treeless arable field into alley cropping with fruit and/or high-value timber trees (hedgerows included).",
              #       # tags$br(),
              #       "Open the tabs on the left to adjust the value ranges of every model variable so they reflect local conditions.",
              #       # tags$br(),
              #       "If you do not feel confident about all inputs, use the ",
              #       tags$em("'Expertise categories'"),
              #       " tab to limit edits to those you know.",
              #       # tags$br(),
              #       "Press ", tags$em("Run the model"), " to launch multiple Monte-Carlo runs; each uses a random combination of input values drawn from your ranges.",
              #       # tags$br(),
              #       "After computation the results appear below.",
              #       # tags$br(),
              #       "Under ", tags$em("Funding schemes"), " you can choose the scheme for your region. Missing schemes? Let us know at mjimene1@uni-bonn.de."
              #     )
              #   )
              # ),
              tags$h5(
                tagList(
              "This app serves to simulate the present value of converting a treeless arable field into alley cropping with fruit and/or high-value timber trees. It also allows to account for hedgerows in the hedges of the alley cropping field.",
              "You can modify the value ranges of all the variables of the calculator by opening the tabs in left-hand side of the screen.",
              "By doing so, you can account for the locally-specific environment and socio-economic context.",
              "In the tab 'Expertise categories' you can select the categories for which you want to modify the default values. This is useful if you do not feel confident enough to provide estimates for all the variables of the model.",
              "When clicking 'Run the model', the app will perform multiple runs of the model, each with a unique combination of values of the input variables, always within the range defined by the bars of the left-hand side tabs.",
              "After the model is computed, the results will be displayed here below",
              "In the tab 'Funding schemes' you can select the funding scheme of your region of interest. If you do not find the funding schemes of your regions of interest, please contact us to include it (mjimene1@uni-bonn.de)",
              )
              ),
              #column(width = 4,
              tags$a("Click here for latest info on Sustainable Farming Incentive",
                     href = "https://www.gov.uk/government/publications/sustainable-farming-incentive-scheme-expanded-offer-for-2024",
                     target="_blank",
                     class = "my-btn"
                     #)
              ),
              br(),
              h5('Figure 1:'),
              h5('Probabilistic outcome distributions from Monte Carlo simulation for decision to establish the alley cropping system (green) and the decision to continue farming without planting trees (blue).'),
              plotOutput("plot1_ui"),
              p(
                'Figure 1 shows the Net Present Value (NPV) distributions of the decision to establish the alley cropping system (green) 
                and the decision to continue farming without planting trees (blue) for the timescope of interest.
                The x-axis displays NPV values (i.e.: the sum of discounted annual cash flows).
                The y-axis displays the probability of each NPV amount to occur (i.e.: higer y-values indicate higher probability'
              ),
              h5('Figure 2:'),
              h5('Probabilistic outcome distributions from Monte Carlo simulation for the decision to establish the alley cropping system as compared to the decision to continue farming without planting trees'),
              plotOutput("plot2_ui"),
              p(
                'Figure 2 shows the Net Present Value (NPV) distributions of the decision to establish the alley cropping system (green) 
                as compared to the decision to continue farming without planting trees for the timescope of interest (i.e.: NPV agroforestry - NPV treeless under identical real-world scenarios).
                The x-axis displays NPV values (i.e.: the sum of discounted annual cash flows).
                The y-axis displays the probability of each NPV amount to occur (i.e.: higer y-values indicate higher probability'
              ),
              h5('Figure 3:'),
              h5('Probabilistic outcome distributions from Monte Carlo simulation for the different cost categories associated with the decision to develop the alley-cropping system'),
              plotOutput("plot3_ui"),
              p(
                'Figure 3 shows the costs (expressed in €) associated with each cost category of the decision to develop an alley-cropping system.
                 The middle line of each box shows the median of its probability distribution.
                 The extremes of each boxes show the first and third quartile of the probability distribution. 
                 The extremes of the lines show the 5th and 95th percentile of the probability distribution. Dots are outliers beyond these percentiles.
                 Please note that the "Bureaucratic work" and the "Maintenance" boxes show the sum of the annual costs of every year over the timescope period, whereas the "Planning and design" and "Planting" boxes occur only in one year.'
              ),
              h5('Figure 4:'),
              h5('Probabilistic outcome distributions from Monte Carlo simulation of the annual balance of the alley-cropping intervention'),
              plotOutput("plot4_ui"),
              p(
                'Figure 4 shows the annual balance (expressed in €) of alley-cropping in the intervened field.'
              ),
              h5('Figure 5:'),
              h5('Probabilistic outcome distributions from Monte Carlo simulation of the cumulative annual balance of the alley-cropping intervention'),
              plotOutput("plot5_ui"),
              p(
                'Figure 5 shows the cumulative annual balance (expressed in €) of alley-cropping in the intervened field.'
              ),
              h5('Figure 6:'),
              h5('Probabilistic outcome distributions from Monte Carlo simulation of the difference between the annual balance of the alley-cropping intervention and the option of continue farming without planting trees'),
              plotOutput("plot6_ui"),
              p(
                'Figure 6 shows the difference (expressed in €) between the annual balance of alley-cropping and continue farming without planting trees under identical real-world scenarios.'
              ),
              h5('Figure 7:'),
              h5('Probabilistic outcome distributions from Monte Carlo simulation of the difference between the cumulative annual balance of the alley-cropping intervention and the option of continue farming without planting trees'),
              plotOutput("plot7_ui"),
              p(
                'Figure 7 shows the cumulative difference (expressed in €) between the annual balance of alley-cropping and continue farming without planting trees under identical real-world scenarios.'
              ),
              plotOutput("plot8_ui"),
              plotOutput("plot9_ui")
    )
  )
  
)


# Server ----
server <- function(input, output, session) {
  
  ## Dynamic funding module ----
  funding <- funding_server("funding")   # returns a list of reactives
  
  ## Helper for safe extraction from named vector
  safe_get <- function(vec, name) {
    if (is.null(vec) || length(vec) == 0 || is.na(vec[name])) return(0)
    if (! name %in% names(vec)) return(0)
    as.numeric(vec[name])
  }
  
  ## Wrapper -> *exact* variables the walnut model expects
  funding_variables <- reactive({
    sel <- funding$category_totals()        # named vector per category (gov)
    
    ## private inputs
    onetime_private <- funding$onetime_private_input()
    annual_private  <- funding$annual_private_input()
    
    # area / trees already entered elsewhere in UI
    AF1_area  <- max(0, as.numeric(input$arable_area_c) - as.numeric(input$AF1_tree_row_area_c))
    AF2_area  <- max(0, as.numeric(input$arable_area_c) - as.numeric(input$AF2_tree_row_area_c))
    AF1_trees <- as.numeric(input$AF1_num_trees_c)
    AF2_trees <- sum(as.numeric(input$num_oak_trees_c), as.numeric(input$num_birch_trees_c),
                     as.numeric(input$num_rowan_trees_c), as.numeric(input$num_hazel_trees_c),
                     as.numeric(input$num_damson_trees_c), as.numeric(input$num_bcherry_trees_c))
    
    # funding amounts per category
    per_ha      <- safe_get(sel, "funding_onetime_per_ha_schemes_c")
    per_tree    <- safe_get(sel, "funding_onetime_per_tree_schemes_c")
    annual_ha   <- safe_get(sel, "annual_funding_schemes_c")
    perc_incost <- safe_get(sel, "funding_onetime_percentage_incost_schemes_c")
    perc_cons   <- safe_get(sel, "funding_onetime_percentage_consult_schemes_c")
    
    # Compute totals
    AF1_one_time   <- per_ha   * AF1_area  + per_tree * AF1_trees + onetime_private
    AF2_one_time   <- per_ha   * AF2_area  + per_tree * AF2_trees + onetime_private
    AF1_annual     <- annual_ha * AF1_area + annual_private * AF1_area
    AF2_annual     <- annual_ha * AF2_area + annual_private * AF2_area
    
    AF1_perc <- perc_incost   # tie whichever category decided for AF1
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
  
  
  ## Dynamic expertise-filter module ----
  # helper that sanitises category names into safe IDs
  sanitize <- function(x) gsub("[^A-Za-z0-9]", "_", x)
  
  # all categories across every sheet
  categories <- reactive({
    cats <- unique(unlist(lapply(excelData(), function(df) df$Expertise)))
    cats <- cats[!is.na(cats) & cats != ""]
    trimws(unique(unlist(strsplit(cats, ";"))))
  })
  
  # filter bar UI
  output$category_filter_ui <- renderUI({
    if (length(categories()) == 0) return(NULL)
    tagList(
      lapply(categories(), function(cat){
        checkboxInput(
          paste0("cat_", sanitize_id(cat)), cat, value = FALSE)
      })
    )
  })
  
  
  # ## Dynamic Crop-selector module ----
  # #  all available crops for crop_rotations across every sheet
  # crop_rot <- reactive({
  #   crops <- unique(unlist(lapply(excelData(), \(df) df$Crop_rotation)))
  #   crops <- crops[!is.na(crops) & crops != "" &
  #                    tolower(crops) != "na"]
  #   trimws(unique(unlist(strsplit(crops, ";"))))
  # })
  # 
  # 
  # ### Crop rotation module ----
  # 
  # #### check-boxes ----
  # output$crop_rot_filter_ui <- renderUI({
  #   if (length(crop_rot()) == 0) return(NULL)
  #   tagList(
  #     lapply(crop_rot(), function(crop){
  #       checkboxInput(
  #         paste0("crop_", sanitize_id(crop)), crop, value = FALSE)
  #     })
  #   )
  # })
  # 
  # # output$crop_filter_ui <- renderUI({
  # #   tagList(lapply(crop_rot(), function(crop) {
  # #     checkboxInput(paste0("crop_", crop), crop)
  # #   }))
  # # })
  # 
  # # helper: which crops are ticked?
  # selected_crops <- reactive({
  #   keep <- vapply(
  #     crop_rot(),
  #     \(crop) isTRUE(input[[paste0("crop_", crop)]]),
  #     logical(1)
  #   )
  #   crop_rot()[keep]
  # })
  # 
  # # store the two buckets
  # rv <- reactiveValues(
  #   avail = character(0),   # left bucket (clonable copies)
  #   rot   = character(0)    # right bucket (user’s rotation)
  # )
  # 
  # # keep 'avail' in sync with current ticks
  # observe({
  #   rv$avail <- selected_crops()
  # })
  # 
  # #### Drag-and-drop builder ----
  # output$rotation_builder_ui <- renderUI({
  #   req(length(rv$avail) > 0)          # show only after at least one crop chosen
  #   
  #   sortable::bucket_list(
  #     header      = "Drag crops to the right, duplicate as needed, reorder freely",
  #     orientation = "horizontal",
  #     
  #     sortable::add_rank_list(
  #       input_id = "avail_bucket",
  #       text     = "Available crops",
  #       labels   = rv$avail,
  #       options  = sortable::sortable_options(
  #         group = list(name = "crops", pull = "clone", put = FALSE)
  #       )
  #     ),
  #     
  #     sortable::add_rank_list(
  #       input_id = "rot_bucket",
  #       text     = "Your rotation",
  #       labels   = rv$rot,
  #       options  = sortable::sortable_options(
  #         group = list(name = "crops", pull = FALSE, put = TRUE),
  #         multiDrag = TRUE,
  #         swap      = TRUE
  #       )
  #     )
  #   )
  # })
  # 
  # # keep rv$rot in sync with what user dragged
  # observeEvent(input$rot_bucket, {
  #   rv$rot <- input$rot_bucket
  # })
  # 
  # # expose the final vector
  # output$rotation_vec <- renderPrint(rv$rot)


  ## Dynamic UI inputs ----
  
  # read in input xlsx file
  excelData <- reactive({
    sheet_number <- seq_along(sheet_names)+1
    all_sheets <- lapply(sheet_number, function(sht) {
      readxl::read_excel(file_path_vars, sheet = sht,
                         col_types = c("text", "numeric", "numeric", "text", "text", "text", "text", "guess", "guess", "text", "text")
      )
    })
    names(all_sheets) <- sheet_names
    all_sheets
  })
  
  
  # util: turns a category vector into a JS condition 
  ### render but hide unchecked expertise categories - default show-all ----
  panel_condition <- function(cat_vec) {
    cat_vec <- trimws(cat_vec)
    cat_vec <- cat_vec[cat_vec != "" & !is.na(cat_vec)]
    if (length(cat_vec) == 0) return("true")
    
    cat_ids <- sprintf("input['cat_%s']", sanitize_id(cat_vec))
    
    cat_show_all <- paste0(
      "Object.keys(input).filter(k => k.startsWith('cat_')).",
      "every(k => input[k] === false)"
    )
    
    sprintf("(%s) || (%s)",            # show when *no* cat box ticked
            cat_show_all,              #      …or any matching cat ticked
            paste(cat_ids, collapse = ' || '))
  }
  
  
  output$dynamic_element_ui <- renderUI({
    
    data_list   <- excelData()
    sheet_names <- names(data_list)
    
    # build one accordion panel per sheet
    # the elements are generated via the external function create_ui_element()
    panels <- lapply(seq_along(data_list), function(j) {
      
      sheet <- data_list[[j]]
      
      cats  <- unique(trimws(unlist(strsplit(sheet$Expertise %||% "", ";|,"))))
      cats  <- cats[cats != ""]
      
      ui_elems <- lapply(seq_len(nrow(sheet)), function(i) {
        create_ui_element(sheet[i, ])
      })
      
      conditionalPanel(
        condition = panel_condition(cats),   # hide panel is empty
        accordion_panel(
          title = sheet_names[j],
          icon  = icon(sheet_icons[[ sheet_names[j] ]] %||% "circle-dot"),
          tagList(ui_elems)
        )
      )
    })
    
    tagList(panels)   # render the list
  })
  
  
  ## Save, Load and Delete module
  all_inputs <- reactive({
      names(input)[grepl("(_c$|_p$|_t$|_n$|_cond$)", names(input))]
  })
  
  current_input_table <- reactive({
    variables <- all_inputs()
    # # source("functions/Walnut_grain_veg_tub_mcsim-only.R", local = T)
    # 
    # message("Accessing user input estimates from the interface...")
    # 
    # # 1. Gather current widget values
    # exclude_inputs <- c("collapseSidebar", "save", "load", "delete",
    #                     "confirm_delete", "admin_selected_user",
    #                     "project_name", "version_select", "delete_version_select")
    # 
    # # variables <- setdiff(
    #   names(input)[grepl("(_c$|_p$|_t$|_n$|_cond$)", names(input))],
    #   exclude_inputs
    # )
    
    lower_values <- sapply(variables, function(v) {
      val <- input[[v]]
      if (length(val) == 1) as.numeric(val) else as.numeric(val[1])
    })
    upper_values <- sapply(variables, function(v) {
      val <- input[[v]]
      if (length(val) == 1) as.numeric(val) else as.numeric(val[2])
    })
    
    # 2. Re-read Excel (keeps original bounds & distributions)
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
    
    # 3. Append funding scalars
    fund_df <- funding_variables()
    fund_df$lower[is.na(fund_df$lower)] <- 0
    fund_df$upper[is.na(fund_df$upper)] <- 0
    
    input_file <- bind_rows(
      input_file %>% filter(!variable %in% fund_df$variable),
      fund_df
    )
    
    # # 4. Save UI snapshot (optional)
    # saveRDS(list(sheet_names, input_file), "data/Walnut_grain_veg_tub_ui_updated.RDS")
    
    # 5. clean-up: keep only numeric rows
    input_file <- input_file %>%
      filter(
        !is.na(lower), !is.na(upper),
        is.finite(lower), is.finite(upper)
      )
    
    # write.csv(input_file,"data/input_table.csv",row.names = F)
    
    input_file
  })
  
  ## Save/Load functionality ----
  # saveLoadServer("savemod", current_input_table)
  get_base_dir <- function() {
    if (Sys.info()[["sysname"]] == "Windows")
      "user-states/Belgium"
    else
      "/srv/shiny-app-data/user-states/Belgium"
  }
  
  get_user_dir <- function() {
    uid <- session$user
    safe_uid <- if (is.null(uid) || uid == "") "anon"
    else gsub("[^A-Za-z0-9_.-]", "_", uid)
    dir <- file.path(get_base_dir(), safe_uid)
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    dir
  }
  
  timestamp_name <- function(raw) {
    paste0(format(Sys.time(), "%Y%m%d-%H%M%S"),"_",
                  gsub("[^A-Za-z0-9_.-]", "_", raw), ".rds")
  }
  
  observeEvent(input$save_btn, {
    dir  <- get_user_dir()
    files <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
    
    if (length(files) >= 5) {
      showModal(modalDialog("You already have five versions. Delete one first.",
                            easyClose = TRUE))
      return()
    }
    
    req(nzchar(input$state_name))
    saveRDS(
      list(input_table = current_input_table(),
           raw_inputs  = reactiveValuesToList(input)),
      file.path(dir, timestamp_name(input$state_name))
    )
  })
  
  saved_files <- reactiveFileReader(
    2000, session, get_user_dir(),
    function(dir) sort(list.files(dir, pattern = "\\.rds$", full.names = TRUE),decreasing = T)
  )
  
  observe({
    updateSelectInput(session, "state_picker",
                      choices = basename(saved_files()))
  })
  
  observeEvent(input$load_btn, {
    req(input$state_picker)
    obj <- readRDS(file.path(get_user_dir(), input$state_picker))
    vals <- obj$raw_inputs
    
    restore_one <- function(id, val) {
      if (is.null(val)) return()
      switch(class(val)[1],
             numeric   = updateNumericInput(session, id, value = val),
             integer   = updateNumericInput(session, id, value = val),
             character = updateTextInput   (session, id, value = val),
             logical   = updateCheckboxInput(session, id, value = val),
             factor    = updateSelectInput (session, id, selected = as.character(val)),
             # length-2 numeric == slider
             { if (is.numeric(val) && length(val) == 2)
               updateSliderInput(session, id, value = val) }
      )
    }
    
    # ordinary widgets
    lapply(names(vals), \(id) try(restore_one(id, vals[[id]]), silent = TRUE))
    
    # funding module widgets  (country + state first, the rest after rebuild)
    ## doesn't work cleanly yet - load button needs to be pressed twice
    ns <- NS("funding")   # helper to prepend "funding-"
    
    # (a) push country and state immediately 
    try(updateSelectInput(session, ns("country"),
                          selected = vals[[ns("country")]]), silent = TRUE)
    try(updateSelectInput(session, ns("state"),
                          selected = vals[[ns("state")]]),   silent = TRUE)
    
    # (b) *once* the state really is set, restore the rest
    observeEvent(input[[ns("state")]], {
      if (!identical(input[[ns("state")]], vals[[ns("state")]])) return()
      
      try(updateSelectInput(session, ns("one_schemes"),
                            selected = vals[[ns("one_schemes")]]), silent = TRUE)
      try(updateSelectInput(session, ns("annual_schemes"),
                            selected = vals[[ns("annual_schemes")]]), silent = TRUE)
      try(updateNumericInput(session, ns("onetime_private"),
                             value = vals[[ns("onetime_private")]]),  silent = TRUE)
      try(updateNumericInput(session, ns("annual_private"),
                             value = vals[[ns("annual_private")]]),   silent = TRUE)
    }, once = TRUE, ignoreInit = FALSE)
  })
  
  observeEvent(input$delete_btn, {
    req(input$state_picker)
    unlink(file.path(get_user_dir(), input$state_picker))
  })
  
  output$download_csv <- downloadHandler(
    filename = function() paste0("current_input_", Sys.Date(), ".csv"),
    content  = function(file) write_csv(current_input_table(), file)
  )
  
  
  
  ## Monte Carlo Simulation ----
  mcSimulation_results <- eventReactive(input$run_simulation, {
    # sadly this still needs to be assigned globally at the moment - otherwise the Model function wont see the vector.
    # functionSyntax = plainNames assigns the model to another variable with its own environment (e) - this environment only holds the input table and the model (does not see rot_vec)
    # assigning to globalEnv will assign it to an environment shared by up to 20 people. people could overwrite their crop rotation vector.
    # rot_vec <- rv$rot
    # assign("crop_rotation", rot_vec, envir = .GlobalEnv)
    
    # model_function <- function(x) {
    #   list2env(as.list(x), envir = environment())
    #   crop_rotation <- rot_vec
    #   walnut_local <- Walnut_grain_veg_tub
    #   environment(walnut_local) <- environment()
    #   walnut_local()
    # }
    
    # walnut_local <- Walnut_grain_veg_tub
    # environment(walnut_local) <- list2env(list(crop_rotation = rot_vec),
    #                                       parent = environment(Walnut_grain_veg_tub))
    
    input_file <- current_input_table()
    
    # 6. Run Monte-Carlo
    decisionSupport::mcSimulation(
      estimate          = decisionSupport::as.estimate(input_file),
      model_function    = Walnut_grain_veg_tub,
      numberOfModelRuns = input$num_simulations_c,
      functionSyntax    = "plainNames"
      # ,model_function    = model_function
      # ,model_function    = walnut_local
      # ,functionSyntax    = "data.frameNames"
      # ,crop_rotation     = rot_vec
    )
    
  })
  
  
  
  ## Generating plots ----
  observeEvent(mcSimulation_results(), {
    mc_data <- mcSimulation_results()
    
    plot1 <- 
      decisionSupport::plot_distributions(mcSimulation_object = mc_data, 
                                          vars = c("NPV_Agroforestry_System1", "NPV_Treeless_System"),
                                          method = 'smooth_simple_overlay',
                                          old_names = c("NPV_Agroforestry_System1", "NPV_Treeless_System"),
                                          new_names = c("Agroforestry intervention", "Cultivation without integrating trees"),
                                          x_axis_name = "NPV (€)",
                                          y_axis_name = "Probability")+
      ggtitle("Net Present Value of a farming decision")+
      theme(plot.title = element_text(hjust = 0.5),
            legend.position="bottom")
    
    
    plot2 <- 
      decisionSupport::plot_distributions(mcSimulation_object = mc_data, 
                                          vars = c("NPV_decision_AF1"),
                                          method = 'smooth_simple_overlay',
                                          old_names = c("NPV_decision_AF1"),
                                          new_names = c("Agroforestry intervention"),
                                          x_axis_name = "NPV (€)",
                                          y_axis_name = "Probability")+
      ggtitle("Net Present Value of the decision to undertake the agroforestry intervention")+
      theme(plot.title = element_text(hjust = 0.5),
            legend.position="none"
      )
    
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
      theme(plot.title = element_text(hjust = 0.5))
    
    plot5 <- 
      decisionSupport::plot_cashflow(mcSimulation_object = mc_data, 
                                     cashflow_var_name = "CumCashflow_AF1", 
                                     x_axis_name = "Timeline of the intervention (years)",
                                     y_axis_name = "Cumulative Cashflow (€)")+
      ggtitle("Cumulative Cashflow of the agroforestry intervention")+
      theme(plot.title = element_text(hjust = 0.5))
    
    plot6 <- 
      decisionSupport::plot_cashflow(mcSimulation_object = mc_data, 
                                     cashflow_var_name = "Cashflow_AF1_decision", 
                                     x_axis_name = "Timeline of the intervention (years)",
                                     y_axis_name = "Cashflow (€)")+
      ggtitle("Cashflow of the agroforestry decision")+
      theme(plot.title = element_text(hjust = 0.5))
    
    plot7 <- 
      decisionSupport::plot_cashflow(mcSimulation_object = mc_data, 
                                     cashflow_var_name = "Cum_Cashflow_AF1_decision", 
                                     x_axis_name = "Timeline of the intervention (years)",
                                     y_axis_name = "Cumulative Cashflow (€)")+
      ggtitle("Cumulative Cashflow of the agroforestry decision")+
      theme(plot.title = element_text(hjust = 0.5))
    
    # Send plots to UI
    output$plot1_ui <- renderPlot({ plot1 })
    output$plot2_ui <- renderPlot({ plot2 })
    output$plot3_ui <- renderPlot({ plot3 })
    output$plot4_ui <- renderPlot({ plot4 })
    output$plot5_ui <- renderPlot({ plot5 })
    output$plot6_ui <- renderPlot({ plot6 })
    output$plot7_ui <- renderPlot({ plot7 })
    
    # Ask user whether to run EVPI (takes time!)
    showModal(modalDialog(
      title = "Run EVPI analysis?",
      "Do you want to assess the Expected Value of Perfect Information (EVPI)?
      This step may take a while, but you can explore the other graphs while the EVPI is processed.
      The EVPI graph will appear at the bottom of the page, below the last of the other graphs.",
      footer = tagList(
        modalButton("No"),
        actionButton("confirm_evpi", "Yes, run EVPI")
      )
    ))
    
    # Handle user confirmation to run EVPI
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
      
      # Try running EVPI only if it can return meaningful values
      tryCatch({
        evpi_result <- decisionSupport::multi_EVPI(mc_df, "NPV_decision_AF1")
        
        if (!is.null(evpi_result$evpi) && nrow(evpi_result$evpi) > 0) {
          top_evpi <- evpi_result$evpi %>%
            filter(evpi > 0) %>%
            arrange(desc(evpi)) %>%
            slice_head(n = 10)
          
          plot8 <- plot_evpi(evpi_result, decision_vars = "NPV_decision_AF1") +
            scale_y_discrete(labels = var_lookup) +
            ggtitle("EVPI for Each Variable") +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5))
          
          output$plot8_ui <- renderPlot({ plot7 })
        } else {
          output$plot8_ui <- renderPlot({
            plot.new()
            text(0.5, 0.5, "There are no variables with a positive EVPI. You probably do not need a plot for that.", cex = 1.2)
          })
        }
      }, error = function(e) {
        warning("EVPI plot skipped due to error: ", e$message)
        output$plot8_ui <- renderPlot({
          plot.new()
          text(0.5, 0.5, "There are no variables with a positive EVPI.\nGetting better information will \nnot reduce the level of uncertainty of the decision.", cex = 1.2)
        })
      })
    })
    
  })
  
}

shinyApp(ui = ui, server = server)
