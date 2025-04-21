#----------------------------------------------------------------------#
# UI input based on ui_type (it can be modified) #######################
# Helper function to create a UI input based on ui_type ################
# Modify this function to handle additional ui_type variants as needed #
#----------------------------------------------------------------------#

create_ui_element <- function(row) {
  input_id    <- row[["variable"]]
  label       <- row[["description"]]
  min_val     <- row[["lower"]]
  max_val     <- row[["upper"]]
  distr       <- row[["distribution"]]
  ui_type     <- row[["ui_type"]]
  ui_step     <- row[["ui_steps"]]
  ui_cond     <- row[["ui_conditional"]]
  ui_cond_nam <- row[["ui_conditional_name"]]
  ui_opt_nam  <- row[["ui_option_name"]]
  ui_opt_var  <- row[["ui_option_variable"]]
  
  ### debug setion
  # input_id    <- all_sheets[[1]][1, ][["variable"]]
  # label       <- all_sheets[[1]][1, ][["description"]]
  # min_val     <- all_sheets[[1]][1, ][["lower"]]
  # max_val     <- all_sheets[[1]][1, ][["upper"]]
  # ui_type     <- all_sheets[[1]][1, ][["ui_type"]]
  # ui_step     <- all_sheets[[1]][1, ][["ui_steps"]]
  # ui_cond     <- all_sheets[[1]][1, ][["ui_conditional"]]
  # ui_cond_nam <- all_sheets[[1]][1, ][["ui_conditional_name"]]
  # ui_opt_nam  <- all_sheets[[1]][1, ][["ui_option_name"]]
  # ui_opt_var  <- all_sheets[[1]][1, ][["ui_option_variable"]]
  
  ui_step <- as.numeric(ui_step)
  
  min_val <- as.numeric(min_val)
  max_val <- as.numeric(max_val)
  
  default <- min_val
  default_2side <- c(min_val, max_val)
  
  if (distr %in% c("posnorm","tnorm_0_1")) {
    min_val <- 0
  } else min_val <- min_val - abs(min_val)*0.5
  if (distr == "tnorm_0_1") {
    max_val <- 1
  } else max_val <- max_val + abs(max_val)*0.5

    
  # default <- as.numeric(min_val)
  # default_2side <- c(as.numeric(min_val), as.numeric(max_val))
  # 
  # min_val <- as.numeric(min_val) - abs(min_val)/2
  # max_val <- as.numeric(max_val) + abs(max_val)/2
  # ui_step <- as.numeric(ui_step)
  
  ui_cond <- if (!is.null(ui_cond)) {
    as.logical(ui_cond)
  }else ui_cond <- F
  
  # default <- (min_val+max_val)/2
  # default_2side <- c((min_val+default)/2, (max_val+default)/2)
  
  if (startsWith(ui_type, "select")) {
    ui_opt_nam  <- unlist(strsplit(ui_opt_nam, " next_option "))
    ui_opt_var  <- unlist(strsplit(ui_opt_var, " next_option "))
    names(ui_opt_var) <- ui_opt_nam
  }
  
  
  real_ui <- NULL
  if (ui_type == "header") {
    # 
    # if (ui_cond) {
    #   toggle_id <- paste0(input_id, "_toggle")
    #   checkboxInput(
    #     inputId = toggle_id,
    #     label   = tags$span(style = "font-size: 120%; font-weight: bold;", header),
    #     value   = FALSE
    #   )
    # }else 
    real_ui <- tagList(
      tags$span(style = "font-size: 120%; font-weight: bold;", label),
      br(),
      br(),
    )
    
  }else if (ui_type == "slider1") {
    real_ui <- sliderInput(
      inputId  = input_id,
      label    = label,
      min      = min_val,
      max      = max_val,
      value    = default,
      step     = ui_step
    )
  } else if (ui_type == "slider2") {
    real_ui <- sliderInput(
      inputId  = input_id,
      label    = label,
      min      = min_val,
      max      = max_val,
      value    = default_2side,
      step     = ui_step
    )
  } else if (ui_type == "numeric") {
    real_ui <- numericInput(
      inputId = input_id,
      label   = label,
      value   = default,
      step    = ui_step
    )
  } else if (ui_type == "select") {
    real_ui <- selectInput(
      inputId  = input_id,
      label    = label, 
      choices  = ui_opt_var,
      selected = NULL
    )
  } else if (ui_type == "select2") {
    real_ui <- selectInput(
      inputId  = input_id,
      label    = label, 
      choices  = ui_opt_var,
      selected = NULL,
      multiple = T
    )
  } else {
    # Fallback / unknown ui_type
    real_ui <- textInput(
      inputId = input_id,
      label   = paste(label, "(unrecognized ui_type)"),
      value   = ""
    )
  }
  
  #####################         conditional will only hide single elements         #####################
  if (ui_cond) {
    toggle_id <- paste0(input_id, "_toggle")
    
    return(
      tagList(
        checkboxInput(
          inputId = toggle_id,
          label   = ui_cond_nam,
          value   = FALSE
        ),
        conditionalPanel(
          condition = sprintf("input['%s']", toggle_id),
          real_ui
        )
      )
    )
  } else {
    return(real_ui)
  }
}

#--------------------------------------------------------------------------------- ###
# Accordion panels (helper function to create and accordion panel for each sheet) ####
#--------------------------------------------------------------------------------- ###
# create_accordion_panel <- function(sheet_name, sheet_data) {
#   toggle_id <- NULL
#   display_panel <- TRUE
#   if (grepl("_q$", sheet_name)) {
#     toggle_id <- sheet_name
#     display_panel <- FALSE
#   }
#   
#   ui_elements <- lapply(seq_len(nrow(sheet_data)), function(i) {
#     create_ui_element(sheet_data[i, ])
#   })
#   
#   panel_ui <- accordion_panel(
#     title = sub("_q$", "", sheet_name),
#     do.call(tagList, ui_elements)
#   )
#   
#   if (!is.null(toggle_id)) {
#     return(tagList(
#       checkboxInput(toggle_id, paste("Enable", sub("_q$", "", toggle_id), "?"), FALSE),
#       conditionalPanel(
#         condition = sprintf("input['%s']", toggle_id),
#         panel_ui,
#         br()
#       )
#     ))
#   } else {
#     return(tagList(panel_ui,br()))
#   }
# }

##### server side whole panel


### generate whole accordion based on sheets - less flexible
# # Create UI dynamically based on the sheets
# output$dynamic_sheets_ui <- renderUI({
#   data_list <- excelData()  # triggers read from file
#   sheet_names <- names(data_list)
#   
#   # Build one panel per sheet
#   panel_list <- lapply(sheet_names, function(sht) {
#     create_accordion_panel(sht, data_list[[sht]])
#   })
#   
#   # Return as a list of UI elements
#   tagList(panel_list)
# })
