options(shiny.maxRequestSize = 3000 * 1024 ^ 2) # to increase max file upload size

shinyServer(function(input, output, session) {
  
  # load data from csv file
  data <- reactive({
    req(input$file)
    read_csv(input$file$datapath, col_names = TRUE)
  })
  
  # populate drop-down menus with column names
  output$relevant_columns <- renderUI({
    req(input$file)
    names <- names(data())
    tagList(
      h4("Select relevant columns"),
      selectInput("wb_type", "Water body type", choices = c("", names), selected = "WB type"
                  ),
      selectInput("rbd", "Reporting basin district", choices = c("", names), selected = "Region"
                  ),
      selectInput("wb", "Water body", choices = c("", names), selected = "Waterbody"
                  ),
      selectInput("location", "Monitoring location", choices = c("", names), selected = "LawaSiteID"
                  ),
      selectInput("date", "Monitoring event date", choices = c("", names), selected = "Date sampled"
                  ),
      selectInput("parameter", "Parameter", choices = c("", names), selected = "Indicator"
                  ),
      selectInput("unit", "Unit of measure", choices = c("", names), selected = "IndicatorUnitOfMeasure"
                  ),
      selectInput("value", "Monitoring value", choices = c("", names), selected = "Value"
                  ),
      selectInput("wq_score", "WQ score (good=1, not good=0)", choices = c("", names), selected = "WQ_score"
      )
    )
  })
  
  # list column inputs for checking
  inputs_col <- reactive({
    c(input$wb_type,
      input$rbd, 
      input$wb,
      input$location,
      input$date,
      input$parameter,
      input$unit,
      input$value,
      input$wq_score)
  })
  
  # narrow down data
  data_narrow <- reactive({
    req(length(inputs_col()) == length(unique(inputs_col())))
    data() %>% 
      select("Water body type" = input$wb_type,
             "RBD" = input$rbd,
             "Water body" = input$wb, 
             "Location" = input$location,
             "Date" = input$date,
             "Parameter" = input$parameter,
             "Unit" = input$unit,
             "Value" = input$value,
             "WQ score" = input$wq_score)
  })
  
  # populate drop-down menus for water body type
  output$relevant_wb_types <- renderUI({
    req(length(inputs_col()) == length(unique(inputs_col())))
    wb_types <- unique(data_narrow()$`Water body type`)
    
    tagList(
      h4("Select relevant water body types"),
      selectInput("wb_type_r", "River", choices = c("", wb_types), multiple = TRUE, selected = "River"
                  ),
      selectInput("wb_type_l", "Lake", choices = c("", wb_types), multiple = TRUE, selected = "Lake"
                  ),
      selectInput("wb_type_g", "Groundwater", choices = c("", wb_types), multiple = TRUE, selected = "Groundwater"
                  )
    )
  })
  
  # populate drop-down menus for parameter names
  output$relevant_parameters <- renderUI({
    req(length(inputs_col()) == length(unique(inputs_col())))
    parameters <- unique(data_narrow()$Parameter)

    tagList(
      h4("Assign relevant parameters groups"),
      selectInput("param_oxy", "Oxygenation", choices = c("", parameters), multiple = TRUE),
      selectInput("param_sal", "Salinity", choices = c("", parameters), multiple = TRUE),
      selectInput("param_n", "Nitrogen", choices = c("", parameters), multiple = TRUE),
      selectInput("param_p", "Phosphorus", choices = c("", parameters), multiple = TRUE),
      selectInput("param_ph", "Acidification", choices = c("", parameters), multiple = TRUE)
    )
  })
  
  # list wb type inputs for checking
  inputs_wb_type <- reactive({
    c(input$wb_type_r, 
      input$wb_type_l,
      input$wb_type_g)
  })
  
  # list parameter inputs for checking
  inputs_param <- reactive({
    c(input$param_oxy, 
      input$param_sal,
      input$param_n,
      input$param_p,
      input$param_ph)
  })
  
  # shorten data
  data_short <- reactive({
    data_narrow() %>% 
      filter(`Water body type` %in% inputs_wb_type(),
             Parameter %in% inputs_param()) %>% 
      # rename for consistency
      mutate(`Water body type` = case_when(`Water body type` %in% input$wb_type_r ~ "River",
                                           `Water body type` %in% input$wb_type_l ~ "Lake",
                                           `Water body type` %in% input$wb_type_g ~ "Groundwater"),
             `Parameter group` = case_when(Parameter %in% input$param_oxy ~ "Oxygenation",
                                   Parameter %in% input$param_sal ~ "Salinity",
                                   Parameter %in% input$param_n ~ "Nitrogen",
                                   Parameter %in% input$param_p ~ "Phosphorus",
                                   Parameter %in% input$param_ph ~ "Acidification"))
  })
  
  # show users resulting dataset
  output$data_table <- renderDataTable({
    data_short() %>% 
      select(-Unit, -Value, -Parameter)
  })
  
  # prepare data for plotting
  data_score <- reactive({
    req(length(inputs_col()) == length(unique(inputs_col())))
    data_short() %>%
      filter(!is.na(Value)) %>%  # exclude monitoring events with no data
      group_by(`Water body type`, RBD, `Water body`, `Parameter group`) %>%
      summarise(n_good = sum(`WQ score`),
                n = n(),
                prop_good = n_good / n) %>%
      mutate(good = ifelse(prop_good >= 0.8, 1, 0)) %>%  # 80% threshold for good wq
      ungroup()
  })
  
  # generate rbd plot
  output$plot_rbd <- renderPlotly({
    req(nrow(data_score()) > 0)
    data_score() %>% 
      group_by(RBD, `Parameter group`) %>% 
      summarise(n_good = sum(good),
                n = n(),
                prop_good = n_good / n) %>% 
      ungroup() %>% 
      mutate(Good = prop_good * 100,
             `Not good` = 100 - Good) %>% 
      select(RBD, `Parameter group`, Good, `Not good`) %>% 
      gather(key = "Status", value = "Percentage", c(Good, `Not good`)) %>% 
      ggplot(aes(x = `Parameter group`, y = Percentage, fill = Status)) +
      geom_col(position = "stack") +
      labs(title = "Water quality status by RBD\n", fill = "") +
      scale_fill_brewer(palette = "Blues") +
      facet_wrap(~ RBD %>% str_to_title()) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 15))
  })
  
  # generate national plot
  output$plot_natl <- renderPlotly({
    req(nrow(data_score()) > 0)
    data_score() %>% 
      group_by(`Parameter group`) %>% 
      summarise(n_good = sum(good),
                n = n(),
                prop_good = n_good / n) %>% 
      ungroup() %>% 
      mutate(Good = prop_good * 100,
             `Not good` = 100 - Good) %>% 
      select(`Parameter group`, Good, `Not good`) %>% 
      gather(key = "Status", value = "Percentage", c(Good, `Not good`)) %>% 
      ggplot(aes(x = `Parameter group`, y = Percentage, fill = Status)) +
      geom_col(position = "stack") +
      labs(title = "National water quality status", fill = "") +
      scale_fill_brewer(palette = "Blues") +
      theme_classic() +
      theme(legend.position = "none")
    
  })
  
  # generate national pie
  output$pie_natl <- renderPlot({
    req(nrow(data_score()) > 0)
    data_score() %>% 
      group_by(`Parameter group`) %>% 
      summarise(n_good = sum(good),
                n = n(),
                prop_good = n_good / n) %>% 
      ungroup() %>% 
      mutate(Status = cut(prop_good,
                          breaks = c(-Inf, 0.50, 0.65, 0.80, 0.95, Inf),
                          labels = c("Poor", "Marginal", "Fair", "Good", "Excellent"))) %>% 
      arrange(desc(Status)) %>%
      mutate(width = 1,
             position = cumsum(width) - 0.5) %>% 
      pie_param()
  })
  
  # generate water body type plot
  output$plot_wb_type <- renderPlotly({
    req(nrow(data_score()) > 0)
    data_score() %>% 
      group_by(`Water body type`, `Parameter group`) %>% 
      summarise(n_good = sum(good),
                n = n(),
                prop_good = n_good / n) %>% 
      ungroup() %>% 
      mutate(Good = prop_good * 100,
             `Not good` = 100 - Good) %>% 
      select(`Water body type`, `Parameter group`, Good, `Not good`) %>% 
      gather(key = "Status", value = "Percentage", c(Good, `Not good`)) %>% 
      ggplot(aes(x = `Parameter group`, y = Percentage, fill = Status)) +
      geom_col(position = "stack") +
      labs(title = "Water quality status by water body type\n", fill = "") +
      scale_fill_brewer(palette = "Blues") +
      facet_wrap(~ `Water body type` %>% str_to_title()) +
      theme_classic()
  })
  
  # generate water body type pie
  output$pie_wb_type <- renderPlot({
    req(nrow(data_score()) > 0)
    
    bind_rows(data_for_pie(data_score(), "Groundwater"),
              data_for_pie(data_score(), "Lake"),
              data_for_pie(data_score(), "River")) %>% 
      pie_param() +
      facet_wrap(~`Water body type`, scales = "free")
  })
  
  # output$pie_wb_type_l <- renderPlot({
  #   req(nrow(data_score()) > 0)
  #   data_score() %>% 
  #     filter(`Water body type` == "Lake") %>% 
  #     pie_param() +
  #     theme(legend.position = "none")
  # })
  # 
  # output$pie_wb_type_r <- renderPlot({
  #   req(nrow(data_score()) > 0)
  #   data_score() %>% 
  #     filter(`Water body type` == "River") %>% 
  #     pie_param()
  # })
  
  # download buttons
  output$download_table <- downloadHandler(
    filename = function(){"table_data.csv"},
    content = function(file){write.csv(data_short(), file, row.names = FALSE)}
  )
  
  output$download_agg_data <- downloadHandler(
    filename = function(){"agg_data.csv"},
    content = function(file){write.csv(data_score(), file, row.names = FALSE)}
  )
  
})



