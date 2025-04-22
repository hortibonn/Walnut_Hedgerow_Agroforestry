# ============================================================================ #
#  dynamic-helper.R – build UI elements from Walnut_grain_veg_tub.xlsx rows    #
#  Added: category-based global filtering                                      #
# ============================================================================ #

# tiny helper used throughout -------------------------------------------------
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
sanitize_id <- function(x) gsub("[^A-Za-z0-9]", "_", x)

# --------------------------------------------------------------------------- #
#  create_ui_element()                                                        #
#  row = one line from the Excel sheet                                        #
# --------------------------------------------------------------------------- #
create_ui_element <- function(row) {
  
  # -------- basic fields from spreadsheet ------------------------------------
  input_id    <- row[["variable"]]
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
  category    <- row[["Expertise"]] %||% ""        # NEW
  
  # -------- value ranges ------------------------------------------------------
  default        <- min_val
  default_2side  <- c(min_val, max_val)
  
  if (distr %in% c("posnorm", "tnorm_0_1")) min_val <- 0
  else min_val <- min_val - abs(min_val) * 0.5
  
  if (distr == "tnorm_0_1") max_val <- 1
  else max_val <- max_val + abs(max_val) * 0.5
  
  ui_cond <- if (!is.null(ui_cond)) as.logical(ui_cond) else FALSE
  
  # -------- select inputs need their choices split ---------------------------
  if (startsWith(ui_type, "select")) {
    ui_opt_nam  <- unlist(strsplit(ui_opt_nam,  " next_option "))
    ui_opt_var  <- unlist(strsplit(ui_opt_var,  " next_option "))
    names(ui_opt_var) <- ui_opt_nam
  }
  
  # -------- create the actual shiny control ----------------------------------
  real_ui <- switch(
    ui_type,
    "header"  = tagList(
      tags$span(style = "font-size: 120%; font-weight: bold;", label),
      br(), br()),
    "slider1" = sliderInput(input_id, label, min_val, max_val,
                            default, step = ui_step),
    "slider2" = sliderInput(input_id, label, min_val, max_val,
                            default_2side, step = ui_step),
    "numeric" = numericInput(input_id, label, default, step = ui_step),
    "select"  = selectInput(input_id, label, choices = ui_opt_var),
    "select2" = selectInput(input_id, label, choices = ui_opt_var,
                            multiple = TRUE),
    # fallback
    textInput(input_id, paste(label, "(unrecognised ui_type)"), "")
  )
  
  # -------- optional element-level toggle ------------------------------------
  if (ui_cond) {
    toggle_id <- paste0(input_id, "_toggle")
    real_ui <- tagList(
      checkboxInput(toggle_id, ui_cond_nam, value = FALSE),
      conditionalPanel(sprintf("input['%s']", toggle_id), real_ui)
    )
  }
  
  # --------------------------------------------------------------------------- #
  #  GLOBAL CATEGORY FILTER                                                    #
  # --------------------------------------------------------------------------- #
  category <- trimws(category)
  if (category == "" || is.na(category)) return(real_ui)
  
  cats_vec <- trimws(unlist(strsplit(category, ";")))
  if (length(cats_vec) == 0) return(real_ui)
  
  # IDs of the cat_* checkboxes defined at top of sidebar (see app.R)
  cat_inputs <- sprintf("input['cat_%s']", sanitize_id(cats_vec))
  or_part    <- paste(cat_inputs, collapse = " || ")
  
  # show when *no* boxes are ticked OR one of its own cats is ticked
  show_all <- paste0(
    "Object.keys(input)",
    ".filter(function(k){return k.startsWith('cat_');})",
    ".every(function(k){ return input[k] === false; })"
  )
  js_condition <- sprintf("(%s) || (%s)", show_all, or_part)
  
  conditionalPanel(js_condition, real_ui)
}
