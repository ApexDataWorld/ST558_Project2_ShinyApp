

library(shiny)


ui <- fluidPage(
  titlePanel("Seoul Bike Sharing Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Subset data (applies on button click)"),
      
      # Categorical filters
      selectizeInput("cat1", "Categorical filter 1",
                     choices = cat_choices,
                     selected = if (length(cat_choices)) cat_choices[1] else NULL),
      uiOutput("cat1_levels"),
      
      selectizeInput("cat2", "Categorical filter 2",
                     choices = cat_choices,
                     selected = if (length(cat_choices) > 1) cat_choices[2] else NULL),
      uiOutput("cat2_levels"),
      
      # Numeric filters (dynamic sliders appear)
      selectInput("num1", "Numeric filter 1",
                  choices = c("None", num_choices),
                  selected = if (length(num_choices)) num_choices[1] else "None"),
      uiOutput("num1_range"),
      
      selectInput("num2", "Numeric filter 2",
                  choices = c("None", num_choices),
                  selected = if (length(num_choices) > 1) num_choices[2] else "None"),
      uiOutput("num2_range"),
      
      actionButton("subset_button", "Subset Data", class = "btn-primary")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "About",
          h3("Purpose"),
          p("Explore the Seoul Bike Sharing dataset. Use the sidebar to choose filters and click ",
            strong("Subset Data"), " to apply them. Then download or explore summaries/plots of the subset."),
          h4("Data Source"),
          p("UCI Machine Learning Repository – Seoul Bike Sharing Demand ",
            a(href = "https://archive.ics.uci.edu/dataset/560/seoul+bike+sharing+demand",
              "Dataset link", target = "_blank")),
          h4("Tabs"),
          tags$ul(
            tags$li(strong("Data Download:"), " table view of the current subset and CSV download."),
            tags$li(strong("Data Exploration:"), " one or two-way tables, numeric summaries by group, and multiple plots.")
          ),
          br(),
          { if (file.exists("www/seoul_bike.jpg")) tags$img(src = "seoul_bike.jpg",
                                                            style = "max-width:420px;border-radius:12px;") }
        ),
        
        tabPanel(
          "Data Download",
          tableOutput("tbl"),
          downloadButton("dl", "Download current subset")
        ),
        
        tabPanel(
          "Data Exploration",
          
          h4("Categorical summaries"),
          radioButtons("which_cats", NULL, inline = TRUE,
                       choices = c("One way" = "one", "Two way" = "two"),
                       selected = "one"),
          uiOutput("cat_summary_ui"),
          tableOutput("cat_table"),
          tags$hr(),
          
          h4("Numeric summaries by group"),
          fluidRow(
            column(6, selectInput("num_summary", "Numeric variable",
                                  choices = num_choices,
                                  selected = if (length(num_choices)) num_choices[1] else NULL)),
            column(6, selectInput("grp_summary", "Group by (categorical)",
                                  choices = cat_choices,
                                  selected = if (length(cat_choices)) cat_choices[1] else NULL))
          ),
          tableOutput("num_table"),
          tags$hr(),
          
          h4("Graphics"),
          fluidRow(
            column(4, selectInput("xvar", "X",
                                  choices = c(cat_choices, num_choices),
                                  selected = if (length(num_choices)) num_choices[1] else cat_choices[1])),
            column(4, selectInput("yvar", "Y",
                                  choices = num_choices,
                                  selected = if (length(num_choices)) num_choices[1] else NULL)),
            column(4, selectInput("color_by", "Color by",
                                  choices = c("None", cat_choices),
                                  selected = if ("Seasons" %in% cat_choices) "Seasons" else "None"))
          ),
          fluidRow(
            column(6, checkboxInput("facet_on", "Facet by Seasons", value = TRUE)),
            column(6, selectInput("plot_type", "Plot type",
                                  choices = c("Scatter+smooth", "Boxplot",
                                              "Histogram", "Bar (count)",
                                              "Heatmap (tile)"),
                                  selected = "Scatter+smooth"))
          ),
          plotOutput("explore_plot", height = 420)
        )
      )
    )
  )
)
