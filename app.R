# app.R
#-------------------------------------------------------------------------------
# Load Libraries
#-------------------------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(reactable)
library(shinyjs)

#-------------------------------------------------------------------------------
# Creating Dashboards
#-------------------------------------------------------------------------------
dataset_dashboard <- fluidPage(
  titlePanel("Target Datasets"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition='input.dataset=="iris"',
        checkboxGroupInput(inputId="show_vars_iris",
                           label="Columns to show",
                           choices=names(iris),
                           selected=names(iris)
        )
      ),
      conditionalPanel(
        condition='input.dataset=="mtcars"',
        checkboxGroupInput(inputId="show_vars_mtcars",
                           label="Columns to show",
                           choices=names(mtcars),
                           selected=names(mtcars)
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        id='dataset',
        tabPanel("Iris", value="iris", reactableOutput("iris_tbl")),
        tabPanel("Mtcars", value="mtcars", reactableOutput("mtcars_tbl"))
      )
    )
  )
)

#dataset_dashboard <- fluidRow(
#          box(
#            title = "Datasets",
#            width = 12, status = "primary", solidHeader = TRUE,
#           tabsetPanel(
#             tabPanel("Iris", reactableOutput("iris_tbl")),
#             tabPanel("Mtcars", reactableOutput("mtcars_tbl"))
#           )
#         )
#       )

vars_selected_dashboard <- fluidPage(
  fluidRow(
    valueBoxOutput("selected_count", width = 12)
  ),
  conditionalPanel(
    condition = "output.n_selected_iris > 0",
    fluidRow(
      box(
        title = "Selected rows (iris)",
        width = 12, status = "success", solidHeader = TRUE,
        reactableOutput("iris_selected"),
        footer=actionButton("clear_iris", "Clear iris selection", icon=icon("eraser"))
      )
    )
  ),
  conditionalPanel(
    condition = "output.n_selected_mtcars > 0",
    fluidRow(
      box(
        title = "Selected rows (mtcars)",
        width = 12, status = "info", solidHeader = TRUE,
        reactableOutput("mtcars_selected"),
        footer=actionButton("clear_mtcars", "Clear mtcars selection", icon=icon("eraser"))
      )
    )
  ),
  fluidRow(
    box(
      title="Download all selected rows",
      width=12, status="warning", solidHeader=TRUE,
      downloadButton("download_all", "Download CSV (all selected)")
    )
  )
)

comment_submission_page <- fluidPage(
  textInput(inputId = "text1", 
            label="First text",
            placeholder='E-mail address'),
  textAreaInput(inputId = "text2", 
                label="Second text",
                placeholder='Comment'),
  uiOutput("validation_msg"),
  actionButton("append_btn", "Append to file", icon=icon("plus"))
)

#-------------------------------------------------------------------------------
# Create Header, Sidebar and Body components for dashboard
#-------------------------------------------------------------------------------
# Header
custom_header <- dashboardHeader(
  title="Dunedin Data Dictionary",
  titleWidth = 300
)



# Sidebar
custom_sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("tab_1", tabName = "tab_1", icon = icon("table")),
    menuItem(
      "tab_2",
      tabName = "tab_2",
      icon = icon("list"),
      badgeLabel = textOutput("badge_count"),
      badgeColor = "green"
    ),
    menuItem("tab_3", tabName="tab_3", icon=icon("list"))
  )
)

# Body
custom_body <- dashboardBody(
  useShinyjs(),
  tabItems(
    tabItem(
      tabName = "tab_1",
      dataset_dashboard
    ),
    tabItem(
      tabName = "tab_2",
      vars_selected_dashboard
    ),
    tabItem(
      tabName="tab_3",
      comment_submission_page
    )
  )
)

#-------------------------------------------------------------------------------
# UI
#-------------------------------------------------------------------------------
ui <- dashboardPage(
  header=custom_header,
  sidebar=custom_sidebar,
  body=custom_body
)


#-------------------------------------------------------------------------------
# Server
#-------------------------------------------------------------------------------

server <- function(input, output, session) {
  # Main tables with multi-row selection
  output$iris_tbl <- renderReactable({
    cols <- iris_cols()
    validate(need(length(cols) > 0, "Select at least one variable in the sidebar."))
    reactable(
      iris[, cols, drop=FALSE],
      selection = "multiple",
      onClick = "select",
      searchable = TRUE,
      pagination = TRUE,
      highlight = TRUE,
      bordered = TRUE,
      striped = TRUE
    )
  })
  
  output$mtcars_tbl <- renderReactable({
    cols <- mtcars_cols()
    validate(need(length(cols) > 0, "Select at least one variable in the sidebar."))
    reactable(
      mtcars[, cols, drop=FALSE],
      selection = "multiple",
      onClick = "select",
      searchable = TRUE,
      pagination = TRUE,
      highlight = TRUE,
      bordered = TRUE,
      striped = TRUE
    )
  })
  
  # Selection indices for rows
  iris_selected_idx <- reactive({
    sel <- getReactableState("iris_tbl", "selected")
    if (is.null(sel)) integer(0) else sel
  })
  mtcars_selected_idx <- reactive({
    sel <- getReactableState("mtcars_tbl", "selected")
    if (is.null(sel)) integer(0) else sel
  })
  
  # Selection indices for columns
  iris_cols <- reactive({
    cols <- input$show_vars_iris
    if (is.null(cols)) names(iris) else cols
  })
  mtcars_cols <- reactive({
    cols <- input$show_vars_mtcars
    if (is.null(cols)) names(mtcars) else cols
  })
  
  # Get selected rows for download
  iris_selected_rows <- reactive({
    sel <- iris_selected_idx()
    cols <- c("Sepal.Length", "Sepal.Width")
    temp <- iris[sel, cols, drop = FALSE]
    colnames(temp) <- c("var", "varname")
    temp
  })
  
  mtcars_selected_rows <- reactive({
    sel <- mtcars_selected_idx()
    cols <- c("mpg", "cyl")
    temp <- mtcars[sel, cols, drop = FALSE]
    colnames(temp) <- c("var", "varname")
    temp
  })
  
  
  # Expose selection counts (used by conditionalPanels and badge)
  output$n_selected_iris <- renderText(length(iris_selected_idx()))
  outputOptions(output, "n_selected_iris", suspendWhenHidden = FALSE)
  
  output$n_selected_mtcars <- renderText(length(mtcars_selected_idx()))
  outputOptions(output, "n_selected_mtcars", suspendWhenHidden = FALSE)
  
  output$badge_count <- renderText({
    as.character(length(iris_selected_idx()) + length(mtcars_selected_idx()))
  })
  outputOptions(output, "badge_count", suspendWhenHidden = FALSE)
  
  # Value box showing total selected rows
  output$selected_count <- renderValueBox({
    n_iris <- length(iris_selected_idx())
    n_mtcars <- length(mtcars_selected_idx())
    valueBox(
      value = n_iris + n_mtcars,
      subtitle = sprintf("Selected rows (iris: %d, mtcars: %d)", n_iris, n_mtcars),
      icon = icon("check"),
      color = if ((n_iris + n_mtcars) > 0) "green" else "yellow"
    )
  })
  
  # Selected rows previews
  output$iris_selected <- renderReactable({
    idx <- iris_selected_idx()
    req(length(idx) > 0)
    reactable(
      iris[idx, c("Sepal.Length", "Sepal.Width"), drop = FALSE],
      pagination = TRUE,
      bordered = TRUE,
      striped = TRUE,
      highlight = TRUE
    )
  })
  
  output$mtcars_selected <- renderReactable({
    idx <- mtcars_selected_idx()
    req(length(idx) > 0)
    # Include model name as a column for clarity
    data <- cbind(model = rownames(mtcars)[idx], mtcars[idx, c("mpg", "cyl"), drop = FALSE])
    reactable(
      data,
      pagination = TRUE,
      bordered = TRUE,
      striped = TRUE,
      highlight = TRUE
    )
  })
  
  # Buttons to clear selected rows
  observeEvent(input$clear_iris, {
    updateReactable("iris_tbl", selected=integer(0))
  })
  
  observeEvent(input$clear_mtcars, {
    updateReactable("mtcars_tbl", selected=integer(0))
  })
  
  # Bind all rows to one dataframe
  bind_rows_union <- function(dfs) {
    all_names <- unique(unlist(lapply(dfs, names)))
    dfs_aligned <- lapply(dfs, function(df) {
      missing <- setdiff(all_names, names(df))
      if (length(missing)) {
        for (nm in missing) df[[nm]] <- NA
      }
      df[all_names]
    })
    do.call(rbind, dfs_aligned)
  }
  
  # Download combined dataframe
  output$download_all <- downloadHandler(
    filename = function() paste0("all_selected_", Sys.Date(), ".csv"),
    content = function(file) {
      iris_df <- iris_selected_rows()
      iris_df <- data.frame(dataset = "iris", iris_df, check.names = FALSE)
      
      mtcars_df <- mtcars_selected_rows()
      mtcars_df <- data.frame(dataset = "mtcars", mtcars_df, check.names = FALSE)
      
      combined <- bind_rows_union(list(iris_df, mtcars_df))
      write.csv(combined, file, row.names = FALSE)
    }
  )
  
  # Tab 3: enable/disable Append button based on trimmed inputs + validation message
  valid_inputs <- reactive({
    nzchar(trimws(input$text1 %||% "")) && nzchar(trimws(input$text2 %||% ""))
  })
  
  observeEvent(list(input$text1, input$text2), {
    shinyjs::toggleState("append_btn", condition = isTRUE(valid_inputs()))
  }, ignoreInit = FALSE)
  
  output$validation_msg <- renderUI({
    if (!valid_inputs()) {
      tags$div(
        style = "color:#a94442; margin-top:5px;",
        "Please enter text in both fields to enable Append."
      )
    }
  })
  
  # Stream-append without reading whole file; clear inputs after success
  file_path <- "user_comments.csv"
  observeEvent(input$append_btn, {
    req(valid_inputs())
    new_row <- data.frame(
      text1 = trimws(input$text1 %||% ""),
      text2 = trimws(input$text2 %||% ""),
      stringsAsFactors = FALSE
    )
    need_header <- !(file.exists(file_path) && isTRUE(file.info(file_path)$size > 0))
    write.table(
      new_row,
      file = file_path,
      sep = ",",
      row.names = FALSE,
      col.names = need_header,
      append = !need_header,
      quote = TRUE
    )
    showNotification("Row appended to combined_text.csv", type = "message", duration = 2)
    updateTextInput(session, "text1", value = "")
    updateTextInput(session, "text2", value = "")
  })
}

shinyApp(ui=ui, server=server)
