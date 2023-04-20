shinyUI(
  fluidPage(
    titlePanel("SDG 6.3.2 water quality score card"),
    sidebarLayout(
      sidebarPanel(
        fileInput("file", 
                  "Choose CSV File",
                  accept = c(".csv")),
        HTML("<h6>Please make sure that:
           <br>- First row is the header,
           <br>- There are no merged cells, and
           <br>- Each row is a single monitoring event for one parameter.</h6>"),
        br(),
        uiOutput("relevant_columns"),
        br(),
        uiOutput("relevant_wb_types"),
        br(),
        uiOutput("relevant_parameters")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Your data",
            dataTableOutput("data_table"),
            br(),
            downloadButton("download_table", "Download table data")
          ),
          tabPanel(
            "Score card",
            plotlyOutput("plot_rbd"),
            br(),
            column(width = 6,
                   plotlyOutput("plot_natl")),
            column(width = 6,
                   plotOutput("pie_natl")),
            br(),
            downloadButton("download_agg_data", "Download aggregate data")
          ),
          tabPanel(
            "Score card by  water body type",
            plotlyOutput("plot_wb_type"),
            br(),
            plotOutput("pie_wb_type")
            # column(width = 4,
            #        plotOutput("pie_wb_type_g")),
            # column(width = 4,
            #        plotOutput("pie_wb_type_l")),
            # column(width = 4,
            #        plotOutput("pie_wb_type_r"))
          )
        )
      )
    )
  )
)