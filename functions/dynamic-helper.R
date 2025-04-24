# ============================================================================ #
#  dynamic-helper.R – build UI elements from Walnut_grain_veg_tub.xlsx rows    #
#  v2: supports header4 / horizontal line / break rows                         #
# ============================================================================ #

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
sanitize_id <- function(x) gsub("[^A-Za-z0-9]", "_", x)

create_ui_element <- function(row) {
  
  # ---------- quick check for layout-only rows -------------------------------
  special <- tolower(trimws(row[["ui_type"]]))
  if (special %in% c("header4", "horizontal line", "break")) {
    
    ui_obj <- switch(
      special,
      "header4"        = h4(row[["description"]] %||% ""),
      "horizontal line"= tags$hr(),
      "break"          = tags$br()
    )
    
    # still wrap in category filter if column exists --------------------------
    category <- trimws(row[["Expertise"]] %||% "")
    if (category == "" || is.na(category)) return(ui_obj)
    
    cats_vec <- trimws(unlist(strsplit(category, ";|,")))
    if (length(cats_vec) == 0) return(ui_obj)
    
    cat_inputs <- sprintf("input['cat_%s']", sanitize_id(cats_vec))
    show_all   <- paste0(
      "Object.keys(input).filter(k=>k.startsWith('cat_')).",
      "every(k=>input[k]===false)"
    )
    js_cond <- sprintf("(%s) || (%s)", show_all,
                       paste(cat_inputs, collapse = " || "))
    return(conditionalPanel(js_cond, ui_obj))
  }
  
  # ---------- MODIFIED logic for real input widgets --------------------------
  input_id    <- row[["variable"]]
  input_alias <- row[["name"]]
  label       <- row[["description"]]
  min_val     <- as.numeric(row[["lower"]])
  max_val     <- as.numeric(row[["upper"]])
  distr       <- row[["distribution"]]
  ui_type     <- row[["ui_type"]]
  ui_step     <- as.numeric(row[["ui_steps"]])
  ui_cond     <- row[["ui_conditional"]]
  ui_cond_nam <- row[["ui_conditional_name"]]
  ui_opt_nam  <- row[["ui_option_name"]]
  ui_opt_var  <- row[["ui_option_variable"]]
  category    <- row[["Expertise"]] %||% ""
  
  default        <- min_val
  default_2side  <- c(min_val, max_val)
  
  if (distr == "posnorm") {
    min_val <- 0
  } else if  (distr == "tnorm_0_1"){
    min_val <- 0.05
  } else min_val <- min_val - abs(min_val)*0.5
  
  if (distr == "tnorm_0_1") {
    max_val <- 0.95
  } else max_val <- max_val + abs(max_val)*0.5
  
  ui_cond <- if (!is.null(ui_cond)) as.logical(ui_cond) else FALSE
  
  if (startsWith(ui_type, "select")) {
    ui_opt_nam  <- unlist(strsplit(ui_opt_nam,  " next_option "))
    ui_opt_var  <- unlist(strsplit(ui_opt_var,  " next_option "))
    names(ui_opt_var) <- ui_opt_nam
  }
  
  label_with_tooltip <- function(id, label_text) {
    tags$label(`for` = id, title = label_text, label_text)
  }
  
  real_ui <- switch(
    ui_type,
    "header"  = tagList(
      tags$span(style="font-size:120%;font-weight:bold;", label),
      br(), br()),
    "slider1" = tagList(
      label_with_tooltip(input_id, label),
      sliderInput(input_id, NULL, min_val, max_val, default, step = ui_step)
    ),
    "slider2" = tagList(
      label_with_tooltip(input_id, label),
      sliderInput(input_id, NULL, min_val, max_val, default_2side, step = ui_step)
    ),
    "numeric" = tagList(
      label_with_tooltip(input_id, label),
      numericInput(input_id, NULL, default, step = ui_step)
    ),
    "select" = tagList(
      label_with_tooltip(input_id, label),
      selectInput(input_id, NULL, choices = ui_opt_var)
    ),
    "select2" = tagList(
      label_with_tooltip(input_id, label),
      selectInput(input_id, NULL, choices = ui_opt_var, multiple = TRUE)
    ),
    textInput(input_id, paste(label, "(unrecognised ui_type)"), "")
  )
  
  if (ui_cond) {
    toggle_id <- paste0(input_id, "_toggle")
    real_ui <- tagList(
      checkboxInput(toggle_id, ui_cond_nam, FALSE),
      conditionalPanel(sprintf("input['%s']", toggle_id), real_ui)
    )
  }
  
  # ---------- category filter wrapper for normal elements --------------------
  category <- trimws(category)
  if (category == "" || is.na(category)) return(real_ui)
  
  cats_vec <- trimws(unlist(strsplit(category, ";|,")))
  if (length(cats_vec) == 0) return(real_ui)
  
  cat_inputs <- sprintf("input['cat_%s']", sanitize_id(cats_vec))
  show_all   <- paste0(
    "Object.keys(input).filter(k=>k.startsWith('cat_')).",
    "every(k=>input[k]===false)"
  )
  js_cond <- sprintf("(%s) || (%s)", show_all,
                     paste(cat_inputs, collapse = " || "))
  conditionalPanel(js_cond, real_ui)
}
