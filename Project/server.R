server <- function(input, output){
  output$job_stab <- renderPlotly({ 
    get_job_stab_plot(input$Industry, as.numeric(input$Year[1]), as.numeric(input$Quarter[1]),
                      as.numeric(input$Year[2]), as.numeric(input$Quarter[2])) 
  })
  output$growth <- renderPlotly({
    get_ind_growth(input$Industry, as.numeric(input$Year[1]), as.numeric(input$Quarter[1]),
                   as.numeric(input$Year[2]), as.numeric(input$Quarter[2]))
  })
  output$rankings_stability <- renderPrint({
    get_rank(input$Industry, as.numeric(input$Year[1]), as.numeric(input$Quarter[1]),
             as.numeric(input$Year[2]), as.numeric(input$Quarter[2]), "job stability")
  })
  output$rankings_growth <- renderPrint({
    get_rank(input$Industry, as.numeric(input$Year[1]), as.numeric(input$Quarter[1]),
             as.numeric(input$Year[2]), as.numeric(input$Quarter[2]), "growth")
  })
  output$time_period_quarter <- renderGirafe({
    get_comparison_time(input$Industry4, input$Quarter1, input$Quarter2, "Quarter")
  })
  output$time_period_year <- renderGirafe({
    get_comparison_time(input$Industry4, input$Year3, input$Year4, "Year")
  })
  output$comp_industry <- renderGirafe({
    get_comparison_industry(input$Industry2, input$Industry3, input$Year2)
  })
  output$time_period_quarter_both <- renderGirafe({
    get_comparison_time(input$Industry7, input$Quarter3, input$Quarter4, "Quarter")
  })
  output$time_period_year_both <- renderGirafe({
    get_comparison_time(input$Industry7, input$Year6 ,input$Year7, "Year")
  })
  output$comp_industry_both <- renderGirafe({
    get_comparison_industry(input$Industry5, input$Industry6, input$Year5)
  })
  output$wage_industry <- renderPlotly({
    get_wage_industry(input$IndustryWage,input$WageYear, input$WageType)
  })
  #########2019 and 2020 wages are separated because there are different levels for the same industry in 2019 and 2020.##########
  ########################                 2019 Wage                        ###########################
  output$wage_AFS2019Level <- renderPlotly({
    get_wage_level(input$IndustryWage, input$AFS2019Level, input$WageYear, input$WageType)
  })
  output$wage_AER2019Level <- renderPlotly({
    get_wage_level(input$IndustryWage, input$AER2019Level, input$WageYear, input$WageType)
  })
  output$wage_CO2019Level<- renderPlotly({
    get_wage_level(input$IndustryWage, input$CO2019Level, input$WageYear, input$WageType)
  })
  output$wage_EHS2019Level<- renderPlotly({
    get_wage_level(input$IndustryWage, input$EHS2019Level, input$WageYear, input$WageType)
  })
  output$wage_FI2019Level <- renderPlotly({
    get_wage_level(input$IndustryWage, input$FI2019Level, input$WageYear, input$WageType)
  })
  output$wage_IT2019Level <- renderPlotly({
    get_wage_level(input$IndustryWage, input$IT2019Level, input$WageYear, input$WageType)
  })
  output$wage_MAN2019Level <- renderPlotly({
    get_wage_level(input$IndustryWage, input$MAN2019Level, input$WageYear, input$WageType)
  })
  output$wage_TS2019Level <- renderPlotly({
    get_wage_level(input$IndustryWage, input$TS2019Level, input$WageYear, input$WageType)
  })
  output$wage_WRT2019Level <- renderPlotly({
    get_wage_level(input$IndustryWage, input$WRT2019Level, input$WageYear, input$WageType)
  })
  
  ################                      2020 Wage                       ###########################
  output$wage_AFS2020Level <- renderPlotly({
    get_wage_level(input$IndustryWage, input$AFS2020Level, input$WageYear, input$WageType)
  })
  output$wage_AER2020Level <- renderPlotly({
    get_wage_level(input$IndustryWage, input$AER2020Level, input$WageYear, input$WageType)
  })
  output$wage_CO2020Level<- renderPlotly({
    get_wage_level(input$IndustryWage, input$CO2020Level, input$WageYear, input$WageType)
  })
  output$wage_EHS2020Level<- renderPlotly({
    get_wage_level(input$IndustryWage, input$EHS2020Level, input$WageYear, input$WageType)
  })
  output$wage_FI2020Level <- renderPlotly({
    get_wage_level(input$IndustryWage, input$FI2020Level, input$WageYear, input$WageType)
  })
  output$wage_IT2020Level <- renderPlotly({
    get_wage_level(input$IndustryWage, input$IT2020Level, input$WageYear, input$WageType)
  })
  output$wage_MAN2020Level <- renderPlotly({
    get_wage_level(input$IndustryWage, input$MAN2020Level, input$WageYear, input$WageType)
  })
  output$wage_TS2020Level <- renderPlotly({
    get_wage_level(input$IndustryWage, input$TS2020Level, input$WageYear, input$WageType)
  })
  output$wage_WRT2020Level <- renderPlotly({
    get_wage_level(input$IndustryWage, input$WRT2020Level, input$WageYear, input$WageType)
  })
  
  #4th TAB - JOB SEARCH
  observe({
    updateSelectizeInput(session = getDefaultReactiveDomain(), inputId = "select_job", label = "Enter or Choose a Job", 
                         choices = jobdata[jobdata$industry==input$search_industry,]$occupation)
  })
  
  value1 <- reactive(input$search_industry)
  value2 <- reactive(input$select_job)
  
  find_job_L <- eventReactive(input$select_job, {overview(value1(), value2(), "Location")})
  output$job_search_location <- renderLeaflet(find_job_L())
  
  find_job_CL <- eventReactive(input$select_job, {overview(value1(), value2(), "Career Level")})
  output$job_search_career_level <- renderPlot(find_job_CL())
  
  find_job_Q <- eventReactive(input$select_job, {overview(value1(), value2(), "Qualifications")})
  output$job_search_qualifications <- renderPlotly(find_job_Q())
  
  find_job_S <- eventReactive(input$select_job, {overview(value1(), value2(), "Salary")})
  output$job_search_salary <- renderPlotly(find_job_S())
  
  find_job_E <- eventReactive(input$select_job, {overview(value1(), value2(), "Experience")})
  output$job_search_experience <- renderPlotly(find_job_E())
  
  # 5th TAB - COMPARE JOBS
  value3 <- reactive(input$postal)
  value4 <- reactive(input$compare)
  
  by_all <- eventReactive(input$criteria, {display(value1(), value2(), input$criteria)})
  output$show_all <- renderDataTable(by_all(), options = list(pageLength = 2, autoWidth = T))
  
  by_salary <- eventReactive(input$criteria, {display(value1(), value2(), input$criteria)})
  output$show_salary <- renderDataTable(by_salary(), options = list(pageLength = 2, autoWidth = T))
  
  by_location <- eventReactive(value3(), {display(value1(), value2(), input$criteria, value3())})
  output$show_location <- renderDataTable(by_location(), options = list(pageLength = 2, autoWidth = T))
  
  
  
  compare_S <- eventReactive(value4(), {comparison(input$compare_role1, input$compare_role2, "Salary")})
  output$compare_salary <- renderPlotly(compare_S())
  
  compare_CL <- eventReactive(value4(), {comparison(input$compare_role1, input$compare_role2, "Career Level")})
  output$compare_career_level <- renderPlot(compare_CL())
  
  compare_Q <- eventReactive(value4(), {comparison(input$compare_role1, input$compare_role2, "Qualifications")})
  output$compare_qualifications <- renderPlot(compare_Q())
  
  compare_E <- eventReactive(value4(), {comparison(input$compare_role1, input$compare_role2, "Experience")})
  output$compare_experience <- renderPlotly(compare_E())
  
  compare_L <- eventReactive(value4(), {comparison(input$compare_role1, input$compare_role2, "Location")})
  output$compare_location <- renderLeaflet(compare_L())
}



