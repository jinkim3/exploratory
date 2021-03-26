#' Launch the exploratory analysis tool
#'
#' Launches the exploratory analysis tool in a browser on the local machine
#'
#' @param data a data object (a data frame or a data.table)
#' @param sigfig number of significant digits to round to
#' @param select_list_max maximum number of variable names to display
#' for dropdown menus
#' @param saved_analyses_file_name name of the .csv file in which
#' saved analyses will be recorded
#' (default = "exploratory_analyses_saved.csv")
#' @param run_analysis_file_name name of the .csv file in which
#' all conducted analyses will be recorded
#' (default = "exploratory_analyses_run.csv")
#' @return There will be no output from this function. Rather, the
#' exploratory analysis tool (a Shiny App) will open in a browser
#' on the local machine.
#' @examples
#' if (interactive()) {exploratory(data = mtcars)}
#' @export
#' @import data.table ggplot2 shiny shinydashboard
exploratory <- function(
  data = datasets::mtcars,
  sigfig = 3,
  select_list_max = 100000,
  saved_analyses_file_name = "exploratory_analyses_saved.csv",
  run_analysis_file_name = "exploratory_analyses_run.csv"
) {
  # create sidebar menu labels
  action_type_label <-
    c("Upload Data",
      "Exploratory Analyses",
      "Saved Analyses",
      "Descriptive Stats",
      "Frequency Table",
      "Histogram",
      "Scatter plot",
      "Compare Groups",
      "Multiple Regression",
      "View Data")
  # set analysis type name that will be used throughout
  # the current function
  action_type <-
    c("upload_data",
      "exploratory_analysis",
      "saved_analysis",
      "desc_stats",
      "freq_table",
      "histogram",
      "scatterplot",
      "compare_groups",
      "regression",
      "view_data")
  # icons for analyses
  action_icon <-
    c("file-upload", # upload data
      "search", # exploratory analyses
      "bookmark", # saved analysis
      "list", # desc stats
      "sort-amount-down", # freq table
      "chart-bar", # histogram
      "braille", # scatterplot chart-scatter
      "not-equal", # compare groups
      "registered", # regression
      "file-alt" # view data
    )
  # dt for sidebar menu
  sidebar_menu_dt <- data.table(
    action_type, action_type_label, action_icon)

  # set defaults
  number_of_dynamic_ui <- 10
  number_of_static_ui <- 3
  number_of_dynamic_buttons <- 4
  number_of_dynamic_output_sections <- 10
  number_of_max_filter_vars <- 10
  plot_1_height <- 600
  debounce_ms_for_resetting_loaded_inputs <- 500

  # shiny server title
  shiny_update_time <- format(Sys.time(), "%b %d %Y, %H:%M:%S")

  # create the saved analysis csv if it doesn't exist
  if (!file.exists(saved_analyses_file_name)) {
    # initialize the csv for saving analysis
    saved_analysis <-
      data.table(
        id = numeric(),
        time = character(),
        input_type = character(),
        input_value = character())
    fwrite(
      x = saved_analysis,
      file = saved_analyses_file_name)
    message(paste0("The following file was created: ",
                   saved_analyses_file_name))
  }

  # create the run analysis csv if it doesn't exist
  if (!file.exists(run_analysis_file_name)) {
    # initialize the csv for saving analysis
    shiny_run_analysis <-
      data.table(
        id = numeric(),
        time = character(),
        ip = character(),
        input_type = character(),
        input_value = character())
    fwrite(
      x = shiny_run_analysis,
      file = run_analysis_file_name)
    message(paste0("The following file was created: ",
                   run_analysis_file_name))
  }

  # import saved analysis csv for the first time
  saved_analysis <-
    fread(saved_analyses_file_name)

  # import run analysis csv for the first time
  shiny_run_analysis <-
    fread(run_analysis_file_name)

  # var names
  var_names <- c("", names(data))

  # functions to be put inside shiny
  file_upload_input <- function() {
    fileInput(
      inputId = "uploaded_file",
      label = "Choose a data CSV File",
      multiple = FALSE,
      accept = c(
        "text/csv",
        "text/comma-separated-values,text/plain",
        ".csv"))
  }
  one_var_input <- function() {
    selectizeInput(
      inputId = "var",
      label = "Select a variable:",
      choices = var_names,
      selected = NULL,
      multiple = FALSE,
      options = list(maxOptions = select_list_max),
      width = "100%")
  }
  iv_input <- function(multiple = TRUE) {
    selectizeInput(
      inputId = "iv",
      label = "Select IV(s):",
      choices = var_names,
      selected = NULL,
      multiple = multiple,
      options = list(maxOptions = select_list_max),
      width = "100%")
  }
  interaction_2_way_1_input <- function(multiple = TRUE) {
    selectizeInput(
      inputId = "interact_vars_2_way_1",
      label = "Select two variables for intreaction (pair 1):",
      choices = var_names,
      selected = NULL,
      multiple = multiple,
      options = list(maxOptions = select_list_max),
      width = "100%")
  }
  interaction_2_way_2_input <- function(multiple = TRUE) {
    selectizeInput(
      inputId = "interact_vars_2_way_2",
      label = "Select two variables for intreaction (pair 2):",
      choices = var_names,
      selected = NULL,
      multiple = multiple,
      options = list(maxOptions = select_list_max),
      width = "100%")
  }
  interaction_3_way_input <- function(multiple = TRUE) {
    selectizeInput(
      inputId = "interact_vars_3_way",
      label = "Select three variables for intreaction:",
      choices = var_names,
      selected = NULL,
      multiple = multiple,
      options = list(maxOptions = select_list_max),
      width = "100%")
  }
  dv_input <- function(multiple = TRUE) {
    selectizeInput(
      inputId = "dv",
      label = "Select DV(s):",
      choices = var_names,
      selected = NULL,
      multiple = multiple,
      options = list(maxOptions = select_list_max),
      width = "100%")
  }
  mod_input <- function(multiple = TRUE) {
    selectizeInput(
      inputId = "mod",
      label = "Select Moderator(s):",
      choices = var_names,
      selected = NULL,
      multiple = multiple,
      options = list(maxOptions = select_list_max),
      width = "100%")
  }
  medi_input <- function(multiple = TRUE) {
    selectizeInput(
      inputId = "medi",
      label = "Select Mediator(s):",
      choices = var_names,
      selected = NULL,
      multiple = multiple,
      options = list(maxOptions = select_list_max),
      width = "100%")
  }
  iv_order_input <- function(iv_order, backup_iv_order) {
    selectizeInput(
      inputId = "iv_order",
      label = "Order for values in IV (Top to Bottom):",
      choices = "",
      selected = NULL,
      multiple = TRUE,
      options = list(maxOptions = select_list_max),
      width = "100%")
  }
  include_totals_input <- function() {
    selectizeInput(
      inputId = "include_totals",
      label = "Include or exclude Totals?",
      choices = c("Include Totals", "Exclude Totals"),
      selected = NULL,
      multiple = FALSE,
      options = list(maxOptions = select_list_max),
      width = "100%")
  }
  function_name_input <- function() {
    selectizeInput(
      inputId = "function_name",
      label = "Select a function for the cells:",
      choices = c(
        "count_after_removing_na",
        "mean_after_removing_na",
        "median_after_removing_na",
        "sd_after_removing_na",
        "min_after_removing_na",
        "max_after_removing_na",
        "geometric_mean_add_1_to_all_values"),
      selected = NULL,
      multiple = FALSE,
      options = list(maxOptions = select_list_max),
      width = "100%")
  }
  row_var_input <- function(multiple = TRUE) {
    selectizeInput(
      inputId = "row_vars",
      label = "Select row variable(s):",
      choices = var_names,
      selected = NULL,
      multiple = multiple,
      options = list(maxOptions = select_list_max),
      width = "100%")
  }
  col_var_input <- function(multiple = TRUE) {
    selectizeInput(
      inputId = "col_vars",
      label = "Select column variable(s):",
      choices = var_names,
      selected = NULL,
      multiple = multiple,
      options = list(maxOptions = select_list_max),
      width = "100%")
  }
  cell_var_input <- function() {
    selectizeInput(
      inputId = "cell_var",
      label = "Select a cell variable:",
      choices = var_names,
      selected = NULL,
      multiple = FALSE,
      options = list(maxOptions = select_list_max),
      width = "100%")
  }
  saved_analysis_input <- function(saved_analysis_reactive_dt) {
    if (is.null(saved_analysis_reactive_dt)) {
      saved_analysis_to_choose_from <- ""
    } else if (missing(saved_analysis_reactive_dt)) {
      saved_analysis_to_choose_from <- ""
    } else {
      dt01 <- saved_analysis_reactive_dt
      saved_analysis_ids <- sort(unique(dt01$id))
      saved_analysis_to_choose_from <- rep(
        NA, length(saved_analysis_ids))
      for (i in seq_along(saved_analysis_ids)) {
        specific_saved_analysis_dt <-
          dt01[get("id") == saved_analysis_ids[i]]
        saved_analysis_temp_1 <-
          specific_saved_analysis_dt[
            get("input_type") == "sidebar_menu"][["input_value"]]
        saved_analysis_temp_2_var_type <-
          unique(specific_saved_analysis_dt[
            get("input_type") != "sidebar_menu"][["input_type"]])
        saved_analysis_temp_3_string <-
          paste0(
            vapply(saved_analysis_temp_2_var_type, function(x) {
              paste0(
                x, ": ", paste0(
                  specific_saved_analysis_dt[
                    get("input_type") == x][["input_value"]],
                  collapse = ", "))}, character(1L)),
            collapse = " / ")
        saved_analysis_to_choose_from[i] <-
          paste0(saved_analysis_ids[i], " - ",
                 saved_analysis_temp_1, " / ",
                 saved_analysis_temp_3_string)
      }
    }
    selectizeInput(
      inputId = "saved_analysis_choices",
      label = "Choose the saved analysis to view: ",
      choices = saved_analysis_to_choose_from,
      multiple = TRUE,
      options = list(maxOptions = select_list_max),
      width = "100%")
  }
  filter_var_ids <-
    paste0("filter_var_", seq_len(number_of_max_filter_vars))
  names_of_all_inputs_for_analysis <- c(
    "sidebar_menu", "var", "iv", "dv",
    "mod", "medi", "iv_order",
    "include_totals", "function_name", "row_vars", "col_vars",
    "cell_var", "sigfig", "saved_analysis_choices",
    "vars_for_outliers", "sigfig", "names_of_filter_vars",
    filter_var_ids
  )
  outlier_input <- function() {
    selectizeInput(
      inputId = "vars_for_outliers",
      label = "Remove outliers in the following variable(s):",
      choices = var_names,
      multiple = TRUE,
      options = list(maxOptions = select_list_max),
      width = "100%")
  }
  sigfig_input <- function() {
    numericInput(
      inputId = "sigfig",
      label = "Number of significant figures to display:",
      value = sigfig,
      min = 0, max = 20, step = 1)
  }
  filter_vars_input_1 <- function() {
    selectizeInput(
      inputId = "names_of_filter_vars",
      label = "Choose filtering variables:",
      choices = var_names,
      multiple = TRUE,
      options = list(maxOptions = select_list_max),
      width = "100%")
  }
  static_input_uis <-
    list(renderUI({sigfig_input()}),
         renderUI({outlier_input()}),
         renderUI({filter_vars_input_1()}))
  analysis_input_1 <- function(
    active_tab, saved_analysis_reactive_dt) {
    if (active_tab == "upload_data") {
      uis <- list(
        renderUI({file_upload_input()}))
    }
    if (active_tab == "exploratory_analysis") {
      uis <- list(
        renderUI({iv_input(multiple = TRUE)}),
        renderUI({dv_input(multiple = TRUE)}),
        renderUI({mod_input(multiple = TRUE)}),
        renderUI({medi_input(multiple = TRUE)}))
    }
    if (active_tab == "saved_analysis") {
      uis <- list(renderUI({
        saved_analysis_input(saved_analysis_reactive_dt)}))
    }
    if (active_tab %in% c("desc_stats", "freq_table", "histogram")) {
      uis <- list(renderUI({one_var_input()}))
    }
    if (active_tab == "scatterplot") {
      uis <- list(renderUI({iv_input(multiple = FALSE)}),
                  renderUI({dv_input(multiple = FALSE)}))
    }
    if (active_tab == "compare_groups") {
      uis <- list(renderUI({iv_input(multiple = FALSE)}),
                  renderUI({dv_input(multiple = FALSE)}),
                  renderUI({iv_order_input()}))
    }
    if (active_tab == "regression") {
      uis <- list(
        renderUI({iv_input(multiple = TRUE)}),
        renderUI({dv_input(multiple = FALSE)}),
        renderUI({interaction_2_way_1_input(multiple = TRUE)}),
        renderUI({interaction_2_way_2_input(multiple = TRUE)}),
        renderUI({interaction_3_way_input(multiple = TRUE)}))
    }
    if (active_tab == "view_data") {
      uis <- list(NULL)
    }
    return(uis)
  }
  # buttons
  run_btn_input <- function() {
    actionButton(inputId = "run_btn", label = "Run", width = "80%")
  }
  save_btn_input <- function() {
    actionButton(inputId = "save_btn", label = "Save", width = "80%")
  }
  load_saved_btn_input <- function() {
    actionButton(inputId = "load_saved_btn",
                 label = "Load Saved Analysis", width = "80%")
  }
  delete_saved_btn_input <- function() {
    actionButton(inputId = "delete_saved_btn",
                 label = "Delete Saved Analysis", width = "80%")
  }
  button_input_1 <- function(active_tab) {
    if (active_tab == "saved_analysis") {
      buttons <- list(renderUI({load_saved_btn_input()}),
                      renderUI({delete_saved_btn_input()}))
    } else if (active_tab == "view_data") {
      buttons <- list(renderUI({run_btn_input()}))
    } else {
      buttons <- list(renderUI({run_btn_input()}),
                      renderUI({save_btn_input()}))
    }
    return(buttons)
  }
  output_area <- function(active_tab) {
    sections <- list(renderUI({uiOutput("message_to_user_01")}),
                     renderUI({uiOutput("message_to_user_02")}),
                     renderUI({textOutput("outlier_report_1")}),
                     renderUI({DT::DTOutput("outlier_report_table")}))
    if (active_tab %in% c(
      "upload_data", "exploratory_analysis",
      "desc_stats", "freq_table", "regression",
      "iv_dv_table", "pivot_table", "view_data")) {
      sections <- c(sections, list(
        renderUI({DT::DTOutput("table_1")})))
    }
    if (active_tab %in% c("histogram", "scatterplot")) {
      sections <- c(sections, list(renderUI({plotOutput("plot_1")})))
    }
    if (active_tab == "compare_groups") {
      sections <- c(sections, list(
        renderUI({plotOutput("plot_1", height = plot_1_height)}),
        renderUI({DT::DTOutput("table_2")}),
        renderUI({DT::DTOutput("table_3")})))
    }
    return(sections)
  }
  names_of_output_sections <- c(
    "message_to_user_01", "message_to_user_02",
    "outlier_report_1", "outlier_report_table",
    "plot_1", "table_1", "table_2"
  )
  outputs_to_clear_on_sidebar_menu_click <- c(
    paste0("dynamic_input_", seq_len(number_of_dynamic_ui)),
    paste0("dynamic_btn_", seq_len(number_of_dynamic_buttons)),
    paste0("dynamic_output_section_",
           seq_len(number_of_dynamic_output_sections)),
    names_of_output_sections)
  inputs_to_save <- function(analysis) {
    if (analysis == "exploratory_analysis") {
      inputs_to_save <- c("iv", "dv", "mod", "medi")
    }
    if (analysis %in% c("desc_stats", "freq_table", "histogram")) {
      inputs_to_save <- "var"
    }
    if (analysis %in% c("scatterplot", "regression")) {
      inputs_to_save <- c(
        "iv", "dv", "interact_vars_2_way_1", "interact_vars_2_way_2",
        "interact_vars_3_way")
    }
    if (analysis == "compare_groups") {
      inputs_to_save <- c("iv", "dv", "iv_order")
    }
    if (analysis == "iv_dv_table") {
      inputs_to_save <- c("iv", "dv", "include_totals", "function_name")
    }
    if (analysis == "pivot_table") {
      inputs_to_save <- c(
        "row_vars", "col_vars", "cell_var", "function_name")
    }
    if (analysis == "view_data") {
      inputs_to_save <- NULL
    }
    inputs_to_save <- c(
      inputs_to_save, "sidebar_menu", "sigfig",
      "vars_for_outliers", "names_of_filter_vars")
    return(inputs_to_save)
  }
  # header, sidebar, and body
  current_pkg_version <- tryCatch(
    utils::packageVersion("exploratory"), error = function(e) "0.1.1(?)")
  header <- shinydashboard::dashboardHeader(title = paste0(
    "Exploratory v", current_pkg_version))
  sidebar <- shinydashboard::dashboardSidebar(uiOutput("sidebar_01"))
  body <- shinydashboard::dashboardBody(
    uiOutput("body"),
    fluidRow(
      column(
        width = 4,
        lapply(seq_len(number_of_dynamic_ui), function(i) {
          uiOutput(paste0("dynamic_input_", i))}),
        lapply(seq_len(number_of_dynamic_buttons), function(i) {
          column(6, uiOutput(paste0("dynamic_btn_", i)))}),
        lapply(seq_len(number_of_static_ui), function(i) {
          uiOutput(paste0("static_input_", i))}),
        lapply(seq_len(number_of_max_filter_vars), function(i) {
          uiOutput(paste0("filter_var_", i, "_checkboxes"))})),
      # lapply(1:50, function(i) textOutput(paste0("testing", i)))),
      column(
        width = 8,
        lapply(seq_len(number_of_dynamic_output_sections), function(i) {
          uiOutput(paste0("dynamic_output_section_", i))})),
      tags$style(
        type='text/css',
        ".selectize-dropdown-content {max-height: 500px; }
        .content-wrapper, .right-side {background-color: #FFFFFF;}
        .shiny-notification{position: fixed; top: 50%;
        left: 33%; right: 33%;}")))
  # ui
  ui <- shinydashboard::dashboardPage(
    header, sidebar, body, skin = "blue")
  # server
  server <- function(input, output, session) {
    # sidebar ui
    output$sidebar_01 <- renderUI({
      shinydashboard::sidebarMenu(
        id = "sidebar_menu",
        lapply(seq_len(nrow(sidebar_menu_dt)), function(i) {
          shinydashboard::menuItem(
            sidebar_menu_dt$action_type_label[i],
            tabName = sidebar_menu_dt$action_type[i],
            icon = icon(sidebar_menu_dt$action_icon[i]),
            selected = TRUE)}))
    })
    # reactive values
    reactive_values <- reactiveValues()
    reactive_dt <- reactiveValues()
    # default reactive dt
    reactive_dt$saved_analysis <- fread(
      saved_analyses_file_name)
    reactive_dt$run_analysis <- fread(
      run_analysis_file_name)
    reactive_dt$loaded_inputs <- NULL
    # make data reactive for file upload
    reactive_dt$data <- data
    # static input uis
    lapply(seq_along(static_input_uis), function(i) {
      output[[paste0("static_input_", i)]] <- static_input_uis[[i]]
    })
    # analysis input uis
    observe({
      input$sidebar_menu
      input$delete_saved_btn
      req(input$sidebar_menu)
      # clear dynamic ui
      lapply(outputs_to_clear_on_sidebar_menu_click, function(x) {
        output[[x]] <- {}
      })
      # add input ui for given analysis
      analysis_input_uis <- analysis_input_1(
        input$sidebar_menu, reactive_dt$saved_analysis)
      lapply(seq_along(analysis_input_uis), function(i) {
        output[[paste0("dynamic_input_", i)]] <- analysis_input_uis[[i]]
      })
      # add buttons for given analysis
      button_uis <- button_input_1(input$sidebar_menu)
      lapply(seq_along(button_uis), function(i) {
        output[[paste0("dynamic_btn_", i)]] <- button_uis[[i]]
      })
      # add output sections for given analysis
      output_sections <- output_area(input$sidebar_menu)
      lapply(seq_along(output_sections), function(i) {
        output[[paste0("dynamic_output_section_", i)]] <-
          output_sections[[i]]
      })
      # inputs to update
      inputs_to_update_after_upload <- c(
        "var", "iv", "interaction_2_way_1",
        "interaction_2_way_2", "interaction_3_way",
        "dv", "mod", "medi", "row_vars",
        "col_vars", "cell_var", "vars_for_outliers",
        "names_of_filter_vars"
      )
      # update var names in input uis
      req(reactive_values$var_names_for_inputs)
      lapply(inputs_to_update_after_upload, function(x) {
        updateSelectizeInput(
          session, inputId = x,
          choices = reactive_values$var_names_for_inputs)
      })
    })
    # observer for data file upload
    observe({
      req(input$uploaded_file)
      reactive_dt$data <- fread(input$uploaded_file$datapath)
      reactive_values$var_names_for_inputs <- c(
        "", names(reactive_dt$data))
    })
    # observer for iv order
    observe({
      input$iv
      # fill in values
      req(input$sidebar_menu, length(input$iv) == 1)
      if (input$sidebar_menu == "compare_groups") {
        choices <- if (input$iv == "") "" else
          sort(unique(data[[input$iv]]))
        iv_order_to_update_to <- if (length(choices) > 20) {
          NULL} else {choices}
        updateSelectizeInput(
          session, inputId = "iv_order",
          choices = choices,
          selected = iv_order_to_update_to)
      }
    })
    # observer for filter
    observe({
      filter_vars <- input$names_of_filter_vars
      lapply(seq_len(number_of_max_filter_vars), function(i) {
        if (is.null(filter_vars)) {
          output[[paste0("filter_var_", i, "_checkboxes")]] <- {}
        } else {
          output[[paste0("filter_var_", i, "_checkboxes")]] <-
            if (is.na(filter_vars[i])) {} else {
              choices <- sort(unique(
                data[[filter_vars[i]]]), na.last = FALSE)
              renderUI({checkboxGroupInput(
                inputId = paste0("filter_var_", i),
                label = paste0(filter_vars[i], ":"),
                choices = choices, inline = TRUE)})}}})})
    # observer for newly loaded inputs
    observe({
      input$sidebar_menu
      input$iv
      input$names_of_filter_vars
      dt01 <- reactive_dt$loaded_inputs
      unique_input_types <- setdiff(
        unique(dt01[["input_type"]]), "sidebar_menu")
      lapply(setdiff(unique_input_types, filter_var_ids), function(x) {
        updateSelectizeInput(
          session, inputId = x,
          selected = dt01[get("input_type") == x][["input_value"]])
      })
      filter_var_ids_to_load <- intersect(
        unique(dt01[["input_type"]]), filter_var_ids)
      lapply(filter_var_ids_to_load, function(x) {
        updateCheckboxGroupInput(
          session, inputId = x,
          selected = dt01[get("input_type") == x][["input_value"]])
      })
      # isolate({reactive_dt$loaded_inputs <- NULL})
    })
    # load saved analysis
    observeEvent(input$load_saved_btn, {
      req(input$saved_analysis_choices)
      # get id of analysis to load on "load saved" button
      id_of_analysis_to_load <-
        gsub("(^\\d+) - .*", "\\1",
             input$saved_analysis_choices[1]) # use only the first
      # load the saved analysis
      dt01 <- reactive_dt$saved_analysis[
        get("id") == id_of_analysis_to_load]
      # switch to the tab
      sidebar_menu_item_to_switch_to <-
        dt01[get("input_type") == "sidebar_menu"][["input_value"]]
      shinydashboard::updateTabItems(
        session,
        inputId = "sidebar_menu",
        selected = sidebar_menu_item_to_switch_to)
      # reactive dt for loaded inputs
      reactive_dt$loaded_inputs <- dt01
    })
    # reset loaded input values
    load_saved_input_values <- eventReactive(
      c(input$sidebar_menu, input$iv, input$names_of_filter_vars,
        reactive_dt$loaded_inputs), {
          reactive_values$ready_to_load_input_values <- F
          dt01 <- reactive_dt$loaded_inputs
          unique_input_types <- setdiff(
            unique(dt01[["input_type"]]), "sidebar_menu")
          lapply(setdiff(unique_input_types, filter_var_ids), function(x) {
            updateSelectizeInput(
              session, inputId = x,
              selected = dt01[get("input_type") == x][["input_value"]])
          })
          filter_var_ids_to_load <- intersect(
            unique(dt01[["input_type"]]), filter_var_ids)
          lapply(filter_var_ids_to_load, function(x) {
            updateCheckboxGroupInput(
              session, inputId = x,
              selected = dt01[get("input_type") == x][["input_value"]])
          })
          reactive_values$ready_to_load_input_values <- TRUE
        }, ignoreInit = TRUE)
    # debounce for resetting
    load_saved_input_values_debounced <-
      debounce(load_saved_input_values,
               debounce_ms_for_resetting_loaded_inputs)
    observeEvent(load_saved_input_values_debounced(),{
      req(reactive_values$ready_to_load_input_values == TRUE)
      reactive_dt$loaded_inputs <- NULL
    })
    # update input on load saved button click
    observeEvent(input$delete_saved_btn, {
      req(input$sidebar_menu == "saved_analysis")
      # load the saved analysis csv
      saved_analysis <- reactive_dt$saved_analysis
      # get id of the saved analysis
      id_of_saved_analysis <-
        gsub("(^\\d+) - .*", "\\1", input$saved_analysis_choices)
      # get the dt for saved analysis
      saved_analysis <-
        saved_analysis[!get("id") %in% id_of_saved_analysis]
      if (nrow(saved_analysis) == 0) {
        saved_analysis <-
          data.table(
            id = numeric(),
            time = character(),
            ip = character(),
            input_type = character(),
            input_value = character())
      } else {
        # update id numbers
        saved_analysis[
          , get("id") := match(get("id"), unique(get("id")))]
      }
      # save to csv
      fwrite(
        x = saved_analysis,
        file = saved_analyses_file_name)
      # update the reactive dt
      reactive_dt$saved_analysis <- saved_analysis
    })
    # run analysis
    observeEvent(input$run_btn, {
      # get active tab
      active_tab <- input$sidebar_menu
      # get data
      dt01 <- reactive_dt$data
      # remove outliers
      if (!is.null(input$vars_for_outliers)) {
        for (i in seq_along(input$vars_for_outliers)) {
          dt01 <- dt01[
            dt01[[input$vars_for_outliers[i]]] %in%
              dt01[[input$vars_for_outliers[i]]][
                !dt01[[input$vars_for_outliers[i]]] %in%
                  grDevices::boxplot.stats(
                    dt01[[input$vars_for_outliers[i]]])$out], ]
        }
      }
      # filter data
      if (length(input$names_of_filter_vars) > 0) {
        for (i in seq_along(input$names_of_filter_vars)) {
          dt01 <- dt01[dt01[[input$names_of_filter_vars[i]]] %in% gsub(
            "^$", NA, input[[paste0("filter_var_", i)]]), ]}}
      # if iv_order was used as a filter
      # if (!is.null(input$iv_order)) {
      #   dt01 <- dt01[dt01[[input$iv]] %in% input$iv_order, ]
      # }
      # exploratory analysis
      if (active_tab == "exploratory_analysis") {
        # values for testing the code
        # dt01 <- mtcars
        # input <- list(3)
        # names(input) <- "sigfig"
        # get vars by type
        e_iv <- input$iv
        e_dv <- input$dv
        e_mod <- input$mod
        e_medi <- input$medi
        # number of vars by type
        num_e_iv <- length(e_iv)
        num_e_dv <- length(e_dv)
        num_e_mod <- length(e_mod)
        num_e_medi <- length(e_medi)
        # exploratory analyses 1: correlation
        if (num_e_iv > 0 & num_e_dv > 0) {
          # vars for correlation
          e_corr_dt <- data.table(
            iv = rep(e_iv, each = num_e_dv),
            dv = rep(e_dv, times = num_e_iv)
          )
          # count number of distinct vars in the model
          e_corr_dt[["num_of_distinct_vars_in_model"]] <- apply(
            e_corr_dt, 1, function(x) length(unique(x)))
          # maximum number of distinct vars in all models
          corr_model_distin_vars_max <- max(e_corr_dt[[
            "num_of_distinct_vars_in_model"]], na.rm = TRUE)
          # valid models only (models with 2 distinct vars)
          e_corr_dt <- e_corr_dt[
            e_corr_dt[["num_of_distinct_vars_in_model"]] ==
              corr_model_distin_vars_max]
          # delete the column indicating number of distinct vars in the model
          e_corr_dt[["num_of_distinct_vars_in_model"]] <- NULL
          # report progress
          withProgress(
            message = "Conducting correlation analyses: ", value = 0, {
              # correlation r and p
              e_corr_r_p <- lapply(seq_len(nrow(e_corr_dt)), function(i) {
                # increment the progress bar, and update the detail text.
                incProgress(
                  1/nrow(e_corr_dt),
                  detail = paste0(i, " out of ", nrow(e_corr_dt)))
                # values for iv and dv
                e_corr_iv_name <- e_corr_dt[["iv"]][i]
                e_corr_dv_name <- e_corr_dt[["dv"]][i]
                # formula for correlation
                e_corr_formula <- stats::as.formula(paste0(
                  "~ ", e_corr_dv_name, " + ", e_corr_iv_name))
                # correlation function
                cor_test_result <- tryCatch(
                  stats::cor.test(formula = e_corr_formula, data = dt01),
                  error = function(e) "error",
                  warning = function(w) "warning")
                # handle errors or warnings
                if (length(cor_test_result) == 1) {
                  if (cor_test_result == "error") {
                    e_corr_r <- NA_real_
                    e_corr_p <- NA_real_
                    e_corr_note <- "error in cor.test function"
                  } else if (cor_test_result == "warning") {
                    e_corr_r <- NA_real_
                    e_corr_p <- NA_real_
                    e_corr_note <- "warning in cor.test function"
                  }
                } else {
                  e_corr_r <- cor_test_result[["estimate"]]
                  e_corr_p <- cor_test_result[["p.value"]]
                  e_corr_note <- NA_character_
                }
                output <- data.table(
                  r = e_corr_r,
                  corr_p = e_corr_p,
                  note = e_corr_note)
                return(output)
              })
            })
          # rbind data tables
          e_corr_r_p_dt <- rbindlist(e_corr_r_p)
          # drop the note column if no errors or warnings were produced
          if (all(is.na(e_corr_r_p_dt[["note"]]))) {
            e_corr_r_p_dt[, "note" := NULL]
          }
          # add var names
          e_corr_dt <- data.table(e_corr_dt, e_corr_r_p_dt)
          # add the analysis type column at the beginning
          e_corr_dt[["analysis"]] <- "correlation"
          setcolorder(e_corr_dt, "analysis")
          # round
          for (j in c("r", "corr_p")) {
            set(e_corr_dt, j = j,
                value = signif(e_corr_dt[[j]], input$sigfig))
          }
        } else {
          e_corr_dt <- NULL
        }

        # exploratory analyses 2: moderation
        if (num_e_iv > 0 & num_e_dv > 0 & num_e_mod > 0) {
          # vars for moderation
          e_mod_dt <- data.table(
            iv = rep(e_iv, each = num_e_dv * num_e_mod),
            mod = rep(rep(e_mod, each = num_e_dv), times = num_e_iv),
            dv = rep(e_dv, times = num_e_iv * num_e_mod))
          # count number of distinct vars in the model
          e_mod_dt[["num_of_distinct_vars_in_model"]] <- apply(
            e_mod_dt, 1, function(x) length(unique(x)))
          # maximum number of distinct vars in all models
          mod_model_distin_vars_max <- max(e_mod_dt[[
            "num_of_distinct_vars_in_model"]], na.rm = TRUE)
          # valid models only (models with 3 distinct vars)
          e_mod_dt <- e_mod_dt[
            e_mod_dt[["num_of_distinct_vars_in_model"]] ==
              mod_model_distin_vars_max]
          # delete the column indicating number of distinct vars in the model
          e_mod_dt[["num_of_distinct_vars_in_model"]] <- NULL
          # report progress
          withProgress(
            message = "Conducting moderation analyses: ", value = 0, {
              # interaction p
              e_mod_int_p_list <- lapply(
                seq_len(nrow(e_mod_dt)), function(i) {
                  # increment the progress bar, and update the detail text.
                  incProgress(
                    1/nrow(e_mod_dt),
                    detail = paste0(i, " out of ", nrow(e_mod_dt)))
                  # values for iv and dv
                  e_mod_iv_name <- e_mod_dt[["iv"]][i]
                  e_mod_mod_name <- e_mod_dt[["mod"]][i]
                  e_mod_dv_name <- e_mod_dt[["dv"]][i]
                  # formula for regression
                  formula_2 <- stats::as.formula(paste0(
                    e_mod_dv_name, " ~ ", e_mod_iv_name, " * ",
                    e_mod_mod_name
                  ))
                  # correlation function
                  e_mod_int_p <- tryCatch(
                    summary(stats::lm(
                      formula = formula_2, data = dt01))[[
                      "coefficients"]][paste0(
                        e_mod_iv_name, ":", e_mod_mod_name), "Pr(>|t|)"],
                    error = function(e) "error",
                    warning = function(w) "warning")
                  # handle errors or warnings
                  if (e_mod_int_p == "error") {
                    e_mod_int_p <- NA_real_
                    e_mod_note <- "error in the moderation analysis"
                  } else if (e_mod_int_p == "warning") {
                    e_mod_int_p <- NA_real_
                    e_mod_note <- "warning in the moderation analysis"
                  } else {
                    e_mod_note <- NA_character_
                  }
                  output <- data.table(
                    interaction_p_value = signif(e_mod_int_p, input$sigfig),
                    note = e_mod_note)
                  return(output)
                })
            })
          # rbind data tables
          e_mod_int_p_dt <- rbindlist(e_mod_int_p_list)
          # drop the note column if no errors or warnings were produced
          if (all(is.na(e_mod_int_p_dt[["note"]]))) {
            e_mod_int_p_dt[, "note" := NULL]
          }
          # add var names
          e_mod_dt <- data.table(e_mod_dt, e_mod_int_p_dt)
          # add the analysis type column at the beginning
          e_mod_dt[["analysis"]] <- "moderation"
          setcolorder(e_mod_dt, "analysis")[]
          # round
          e_mod_dt[["interaction_p_value"]] <- signif(
            e_mod_dt[["interaction_p_value"]], input$sigfig)
        } else {
          e_mod_dt <- NULL
        }

        # exploratory analyses 3: mediation
        if (num_e_iv > 0 & num_e_dv > 0 & num_e_medi > 0) {
          # vars for mediation
          e_medi_dt <- data.table(
            iv = rep(e_iv, each = num_e_dv * num_e_medi),
            medi = rep(rep(e_medi, each = num_e_dv), times = num_e_iv),
            dv = rep(e_dv, times = num_e_iv * num_e_medi))
          # count number of distinct vars in the model
          e_medi_dt[["num_of_distinct_vars_in_model"]] <- apply(
            e_medi_dt, 1, function(x) length(unique(x)))
          # maximum number of distinct vars in all models
          medi_model_distin_vars_max <- max(e_medi_dt[[
            "num_of_distinct_vars_in_model"]], na.rm = TRUE)
          # valid models only (models with 3 distinct vars)
          e_medi_dt <- e_medi_dt[
            e_medi_dt[["num_of_distinct_vars_in_model"]] ==
              medi_model_distin_vars_max]
          # delete the column indicating number of distinct vars in the model
          e_medi_dt[["num_of_distinct_vars_in_model"]] <- NULL
          # report progress
          withProgress(
            message = "Conducting mediation analyses: ", value = 0, {
              # mediation p
              e_medi_indir_eff_p_list <- lapply(
                seq_len(nrow(e_medi_dt)), function(i) {
                  # increment the progress bar, and update the detail text.
                  incProgress(
                    1/nrow(e_medi_dt),
                    detail = paste0(i, " out of ", nrow(e_medi_dt)))
                  # values for iv and dv
                  e_medi_iv_name <- e_medi_dt[["iv"]][i]
                  e_medi_medi_name <- e_medi_dt[["medi"]][i]
                  e_medi_dv_name <- e_medi_dt[["dv"]][i]
                  # mediation analysis
                  e_medi_indir_eff_p <- tryCatch(
                    mediation_analysis(
                      data = dt01,
                      iv_name = e_medi_iv_name,
                      mediator_name = e_medi_medi_name,
                      dv_name = e_medi_dv_name,
                      output_type = "indirect_effect_p",
                      sigfigs = 10,
                      silent = TRUE),
                    error = function(e) "error",
                    warning = function(w) "warning")
                  # handle errors or warnings
                  if (e_medi_indir_eff_p == "error") {
                    e_medi_indir_eff_p <- NA_real_
                    e_medi_note <- "error in the mediation analysis"
                  } else if (e_medi_indir_eff_p == "warning") {
                    e_medi_indir_eff_p <- NA_real_
                    e_medi_note <- "warning in the mediation analysis"
                  } else {
                    e_medi_note <- NA_character_
                  }
                  output <- data.table(
                    indirect_effect_p_value = e_medi_indir_eff_p,
                    note = e_medi_note)
                  return(output)
                })
            })
          # rbind data tables
          e_medi_indir_eff_p_dt <- rbindlist(e_medi_indir_eff_p_list)
          # drop the note column if no errors or warnings were produced
          if (all(is.na(e_medi_indir_eff_p_dt[[" note"]]))) {
            e_medi_indir_eff_p_dt[, "note" := NULL]
          }
          # add var names
          e_medi_dt <- data.table(e_medi_dt, e_medi_indir_eff_p_dt)
          # add the analysis type column at the beginning
          e_medi_dt[["analysis"]] <- "mediation"
          setcolorder(e_medi_dt, "analysis")[]
          # round
          e_medi_dt[["indirect_effect_p_value"]] <- signif(
            e_medi_dt[["indirect_effect_p_value"]], input$sigfig)
        } else {
          e_medi_dt <- NULL
        }

        # merge data table of exploratory analyses results
        e_result_dt_list <- list(e_corr_dt, e_mod_dt, e_medi_dt)
        # remove null dt
        e_result_dt_list <- Filter(Negate(is.null), e_result_dt_list)
        if (length(e_result_dt_list) > 0) {
          # add id to the exploratory analysis results data sets
          e_result_dt_list <- id_across_datasets(
            dt_list = e_result_dt_list,
            silent = TRUE)
          # merge the data tables above
          e_result_merged <- merge_data_table_list(
            dt_list = e_result_dt_list, id = "id")
          # a column that combines p values
          e_result_merged[["p_value_of_interest"]] <- fcase(
            e_result_merged[["analysis"]] == "correlation",
            e_result_merged[["corr_p"]],
            e_result_merged[["analysis"]] == "moderation",
            e_result_merged[["interaction_p_value"]],
            e_result_merged[["analysis"]] == "mediation",
            e_result_merged[["indirect_effect_p_value"]]
          )
          # var types for correlation
          if ("correlation" %in% e_result_merged[["analysis"]]) {
            e_corr_vars <- c("iv", "dv")
            e_corr_other_cols <- c("r", "corr_p")
          } else {
            e_corr_vars <- e_corr_other_cols <- NULL
          }
          # var types for moderation
          if ("moderation" %in% e_result_merged[["analysis"]]) {
            e_mod_vars <- c("iv", "mod", "dv")
            e_mod_other_cols <- "interaction_p_value"
          } else {
            e_mod_vars <- e_mod_other_cols <- NULL
          }
          # var types for mediation
          if ("mediation" %in% e_result_merged[["analysis"]]) {
            e_medi_vars <- c("iv", "medi", "dv")
            e_medi_other_cols <- "indirect_effect_p_value"
          } else {
            e_medi_vars <- e_medi_other_cols <- NULL
          }
          # order of var types
          e_var_types <- list(e_corr_vars, e_mod_vars, e_medi_vars)
          e_var_types_ordered <- Reduce(
            f = union, x = e_var_types)
          # order of other cols
          e_other_cols <- list(
            e_corr_other_cols, e_mod_other_cols, e_medi_other_cols)
          e_other_cols_ordered <- Reduce(f = union, x = e_other_cols)
          # insert the key p value column
          e_other_cols_ordered <- c(
            "p_value_of_interest", e_other_cols_ordered)
          # new column order
          e_result_merged_new_col_order <- Reduce(f = c, x = list(
            "id", "analysis", e_var_types_ordered, e_other_cols_ordered))
          # reorder columns
          setcolorder(
            e_result_merged,
            neworder = e_result_merged_new_col_order)
          # rename columns
          setnames(e_result_merged, old = c(
            "id", "analysis", "iv", "dv", "mod", "medi",
            "p_value_of_interest", "r", "corr_p",
            "interaction_p_value", "indirect_effect_p_value", "note"
          ), new = c(
            "ID", "Analysis", "IV", "DV", "Moderator", "Mediator",
            "p of Interest",
            "r", "Correlation p", "Interaction p", "Indirect Effect p",
            "Note"), skip_absent = TRUE)
          # output
          output$table_1 <- DT::renderDataTable(
            e_result_merged,
            filter = "top",
            options = list(pageLength = 1000))
        } else {
          showNotification(paste0(
            "No analyses can be conducted. Please check ",
            "the input variables (e.g., whether IV or DV is missing)."),
            duration = 4)
        }
      }
      # descriptive stats
      if (active_tab == "desc_stats") {
        req(input$var)
        desc_stats_dt <- desc_stats(
          dt01[[input$var]], notify_na_count = FALSE,
          sigfigs = input$sigfig,
          print_dt = FALSE)
        output$table_1 <- DT::renderDataTable(
          data.table(
            statistic = names(desc_stats_dt),
            value = desc_stats_dt),
          filter = "top",
          options = list(pageLength = 1000))}
      # frequency table
      if (active_tab == "freq_table") {
        output$table_1 <- DT::renderDataTable(
          tabulate_vector(
            dt01[[input$var]], sigfigs = input$sigfig),
          filter = "top",
          options = list(pageLength = 1000))}
      # histogram
      if (active_tab == "histogram") {
        output$plot_1 <- renderPlot({
          histogram(vector = dt01[[input$var]])
        }, width = 800, height = plot_1_height)}
      # scatterplot
      if (active_tab == "scatterplot") {
        output$plot_1 <- renderPlot({
          withProgress(
            message = "Generating the plot...",
            scatterplot(
              data = dt01,
              x_var_name = input$iv,
              y_var_name = input$dv,
              annotate_stats = TRUE))
        }, width = 800, height = plot_1_height)}
      # compare groups
      if (active_tab == "compare_groups") {
        if (length(unique(dt01[[input$iv]])) > 20) {
          output$message_to_user_01 <- renderText({
            paste0("The IV has more than 20 levels. ",
                   "The server cannot handle this operation.")
          })
        } else {
          # message
          output$message_to_user_01 <- renderText({
            "Generating and uploading the plot..."})
          # histogram by group
          output$plot_1 <- renderPlot({withProgress(
            message = "Generating the plot...",
            histogram_by_group(
              data = dt01,
              iv_name = input$iv,
              dv_name = input$dv,
              order_of_groups_top_to_bot = input$iv_order)
          )}, width = 800, height = plot_1_height)
          # desc stats by group
          output$table_2 <- DT::renderDataTable(
            desc_stats_by_group(
              data = dt01,
              var_for_stats = input$dv,
              grouping_vars = input$iv,
              sigfigs = input$sigfig),
            filter = "top",
            options = list(pageLength = 1000))
          # pairwise comparisons
          output$table_3 <- DT::renderDataTable(
            t_test_pairwise(
              data = dt01,
              iv_name = input$iv,
              dv_name = input$dv,
              sigfigs = input$sigfig,
              mann_whitney = TRUE,
              t_test_stats = TRUE),
            filter = "top",
            options = list(pageLength = 1000))
        }
      }
      # regression
      if (active_tab == "regression") {
        output$table_1 <- DT::renderDataTable({
          # formula components for interacting vars
          if (length(input$interact_vars_2_way_1) == 2) {
            interact_vars_2_way_1 <- paste0(
              input$interact_vars_2_way_1, collapse = ":")
          } else {
            interact_vars_2_way_1 <- NULL
          }
          if (length(input$interact_vars_2_way_2) == 2) {
            interact_vars_2_way_2 <- paste0(
              input$interact_vars_2_way_2, collapse = ":")
          } else {
            interact_vars_2_way_2 <- NULL
          }
          if (length(input$interact_vars_3_way) == 3) {
            interact_vars_3_way <- paste0(
              input$interact_vars_3_way, collapse = ":")
          } else {
            interact_vars_3_way <- NULL
          }
          # merge data table of exploratory analyses results
          rhs_term_list <- list(
            input$iv,
            interact_vars_2_way_1,
            interact_vars_2_way_2,
            interact_vars_3_way)
          # remove null dt
          rhs_term_list <- Filter(Negate(is.null), rhs_term_list)
          # build the formula
          reg_formula_rhs <- paste0(
            unlist(rhs_term_list), collapse = " + ")
          reg_formula <- stats::as.formula(
            paste0(input$dv, " ~ ", reg_formula_rhs))
          # regression
          reg_table <- multiple_regression(
            data = dt01,
            formula = reg_formula,
            sigfigs = input$sigfig)
          # nicer column names
          names(reg_table) <- c(
            "Variable", "B", "SE B", "Std. Beta", "t-stat", "p")
          reg_table
        },
        filter = "top",
        options = list(pageLength = 1000))
      }
      # view data, if active tab is "upload data" or "view data"
      if (active_tab %in% c("upload_data", "view_data")) {
        output$table_1 <- DT::renderDataTable(
          dt01,
          filter = "top",
          options = list(pageLength = 1000))}

      # record action
      dt11 <- reactive_dt$run_analysis
      # save inputs
      input_names <- inputs_to_save(input$sidebar_menu)
      if (length(input$names_of_filter_vars)) {
        input_names <- c(
          input_names,
          paste0("filter_var_", seq_along(input$names_of_filter_vars)))
      }
      input_values <- lapply(seq_along(input_names), function(x) {
        input[[input_names[x]]]
      })
      # new entry
      new_input_type <-
        unlist(rep(input_names, lengths(input_values)))
      new_input_value <-
        unlist(input_values)
      # id, time, etc
      id <- c(dt11$id,
              rep(max(c(dt11$id, 0)) + 1,
                  length(new_input_value)))
      time <- c(dt11$time,
                rep(format(Sys.time(), "%b %d %Y %H%M%S"),
                    length(new_input_value)))
      new_input_type_all <-
        c(dt11$input_type, new_input_type)
      new_input_value_all <-
        c(dt11$input_value, new_input_value)
      # put it together and export to csv
      run_analysis_dt <- data.table(
        id, time,
        input_type = new_input_type_all,
        input_value = new_input_value_all)
      # save to csv
      fwrite(
        x = run_analysis_dt,
        file = run_analysis_file_name)
      # save again
      reactive_dt$run_analysis <- run_analysis_dt
    })
    # save analysis
    observeEvent(input$save_btn, {
      dt01 <- reactive_dt$saved_analysis
      # save inputs
      input_names <- inputs_to_save(input$sidebar_menu)
      if (length(input$names_of_filter_vars)) {
        input_names <- c(
          input_names,
          paste0("filter_var_", seq_along(input$names_of_filter_vars)))
      }
      input_values <- lapply(seq_along(input_names), function(x) {
        input[[input_names[x]]]
      })
      # new entry
      new_input_type <-
        unlist(rep(input_names, lengths(input_values)))
      new_input_value <-
        unlist(input_values)
      # message to user
      output$message_to_user_01 <- renderText({
        "The following analysis was saved:\n"
      })
      output$message_to_user_02 <- renderUI({
        DT::dataTableOutput("saved_analysis_summary_dt")
      })
      output$saved_analysis_summary_dt <- DT::renderDataTable({
        data.table(input_type = new_input_type,
                   input_value = new_input_value)
      },
      filter = "top",
      options = list(pageLength = 100))
      # id, time, ip, etc
      id <- c(dt01$id,
              rep(max(c(dt01$id, 0)) + 1,
                  length(new_input_value)))
      time <- c(dt01$time,
                rep(format(Sys.time(), "%b %d %Y %H%M%S"),
                    length(new_input_value)))
      new_input_type_all <-
        c(dt01$input_type, new_input_type)
      new_input_value_all <-
        c(dt01$input_value, new_input_value)
      # put it together and export to csv
      saved_analysis_dt <- data.table(
        id, time,
        input_type = new_input_type_all,
        input_value = new_input_value_all)
      # save to csv
      fwrite(
        x = saved_analysis_dt,
        file = saved_analyses_file_name)
      # save again
      reactive_dt$saved_analysis <- saved_analysis_dt
    })
    # default tab
    isolate({updateTabItems(
      session,
      inputId = "sidebar_menu",
      selected = "exploratory_analysis")})
  }
  # By default, the file size limit is 5MB. It can be changed by
  # setting this option. Here we'll raise limit to 50MB.
  options(shiny.maxRequestSize = 50 * 1024 ^ 2)
  # create the app to run
  shiny_app <- list(ui = ui, server = server)
  runApp(shiny_app, launch.browser = TRUE)
}
