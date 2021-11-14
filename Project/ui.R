pacman::p_load(dplyr, tidyr, hash, tidyverse, ggplot2, stringr, plotly, Rcpp, ggiraph, shiny, leaflet, leaflet.extras, shinydashboard,
               shinythemes,shinycssloaders, GGally, rvest, geosphere, ggthemes, ggpubr, fmsb, ggeasy, shinydashboard, fontawesome, treemap, ggmap, RColorBrewer) 

#Data as global var
data <- read.csv('trend.data.csv') %>% subset(select = -X)

############################################### JOB STABILITY ############################################################################
get_job_stab_plot <- function(industry, start_year, start_quarter, end_year, end_quarter){
  
  filtered <- data[data$year >=start_year & data$quarter >= start_quarter & data$year <= end_year & data$quarter <=end_quarter &
                     trimws(data$industry2) == industry,]
  
  filtered <- filtered %>% 
    dplyr::select(Year, industry2, emp_ch_rate, retrench_rate) %>% 
    group_by(Year) %>% 
    summarise('Industry Average Employment Rate'= mean(emp_ch_rate, na.rm = T),
              'Industry Average Retrenchment Rate' = mean(retrench_rate, na.rm = T))
  
  filtered$`Industry Average Employment Rate` <- as.numeric(format(round(filtered$`Industry Average Employment Rate`,3), nsmall=3))
  filtered$`Industry Average Retrenchment Rate` <- as.numeric(format(round(filtered$`Industry Average Retrenchment Rate`,3), nsmall=3))
  
  #Average for all industries
  avgs <- data[data$year >=start_year & data$quarter >=start_quarter & data$year <= end_year & data$quarter <= end_quarter, ]%>% 
    group_by(Year) %>% 
    summarise("Average Employment Rate" = mean(emp_ch_rate, na.rm = T),
              "Average Retrenchment Rate" = mean(retrench_rate, na.rm = T))
  
  
  avgs$`Average Employment Rate` <- as.numeric(format(round(avgs$`Average Employment Rate`,3), nsmall=3))
  avgs$`Average Retrenchment Rate` <- as.numeric(format(round(avgs$`Average Retrenchment Rate`, 3), nsmall=3))
  
  filtered  = filtered %>% gather(Metric, rate, `Industry Average Employment Rate`: `Industry Average Retrenchment Rate`)
  avgs = avgs %>% gather(Metric, rate, `Average Employment Rate`:`Average Retrenchment Rate`)
  combined = rbind(filtered, avgs)
  
  #for x label later
  q <- paste0("Q", as.character(rep(1:4)))
  #Adjust based on user start quarter
  xlab <- paste(as.character(seq(2010, 2021)), paste0("Q",start_quarter))
  industry <- str_to_title(industry)
  
  #####################################JOB STABILITY PLOT#################################################
  js_plot <- ggplot(data = combined, aes(x=Year, y=rate, group = 1)) + geom_line(aes(color = Metric), size=1) +geom_point(aes(color = Metric))+
    scale_color_manual(name='Legend', values = c("cornflowerblue", "springgreen3", "midnightblue", "forestgreen"))+
    scale_x_discrete(breaks=xlab, guide=guide_axis(check.overlap = TRUE)) +
    scale_y_continuous(guide=guide_axis(check.overlap = TRUE)) + 
    labs(title = paste('Job Stability for', industry),
         x = "Year, Quarter",
         y = "Rate (%)")+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(panel.background =element_rect(fill = "white", color = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(colour = "cornflowerblue", fill=NA, size=2))+
    theme(plot.background =element_rect(fill = "aliceblue", color = "cornflowerblue"))+
    theme(legend.background = element_rect(fill="aliceblue",
                                           size=0.5, linetype="solid", 
                                           colour ="darkblue"))
  
  return (ggplotly(js_plot))
}


########################################### INDUSTRY GROWTH ########################################################################
##Filters not added to UI yet.
get_ind_growth <- function(industry, start_year, start_quarter, end_year, end_quarter){
  
  filtered <- data %>% 
    filter(industry2==industry & year>= start_year & year<=end_year & quarter>=start_quarter & quarter <=end_quarter) %>%
    group_by(Year) %>% summarise("Industry Average Employment Rate"=mean(emp_ch_rate, na.rm = T),
                                 "Industry Average Job Vacancy"=mean(job_vacancy_rate, na.rm = T),
                                 "Industry Average Recruitment Rate"=mean(recruitment_rate, na.rm = T))
  
  filtered$`Industry Average Employment Rate` <- as.numeric(format(round(filtered$`Industry Average Employment Rate`,3), nsmall=3))
  filtered$`Industry Average Job Vacancy` <- as.numeric(format(round(filtered$`Industry Average Job Vacancy`,3), nsmall=3))
  filtered$`Industry Average Recruitment Rate` <- as.numeric(format(round(filtered$`Industry Average Recruitment Rate`,3), nsmall=3))
  
  # Averages
  avgs <- data %>% 
    filter(year>= start_year & year<=end_year & quarter>=start_quarter & quarter <=end_quarter) %>%
    group_by(Year) %>% 
    summarise("Average Employment Rate"=mean(emp_ch_rate), 
              "Average Job Vacancy"=mean(job_vacancy_rate),
              "Average Recruitment Rate"=mean(recruitment_rate))
  avgs$`Average Employment Rate` <- as.numeric(format(round(avgs$`Average Employment Rate`,3), nsmall=3))
  avgs$`Average Job Vacancy` <- as.numeric(format(round(avgs$`Average Job Vacancy`,3), nsmall=3))
  avgs$`Average Recruitment Rate` <- as.numeric(format(round(avgs$`Average Recruitment Rate`,3), nsmall=3))
  
  filtered  = filtered %>% gather(Metric, rate, `Industry Average Employment Rate`: `Industry Average Recruitment Rate`)
  avgs = avgs %>% gather(Metric, rate, `Average Employment Rate`:`Average Recruitment Rate`)
  combined = rbind(filtered, avgs)
  
  #Adjust based on user start quarter
  xlab <- paste(as.character(seq(2010, 2021)), paste0("Q",start_quarter))
  
  industry <- str_to_title(industry)
  ###################################################INDUSTRY GROWTH PLOT##############################################
  is_plot <-ggplot(data = combined, aes(x=Year, y=rate, group = 1)) + geom_line(aes(color = Metric), size=1) +geom_point(aes(color = Metric))+
    scale_color_manual(name='Legend', values = c("cornflowerblue", "orange3", "springgreen3", "midnightblue", "orangered3", "forestgreen"))+
    scale_x_discrete(breaks=xlab, guide=guide_axis(check.overlap = TRUE)) +
    scale_y_continuous(guide=guide_axis(check.overlap = TRUE)) + 
    labs(title=paste("Industry Growth for", industry, "industry", sep=" "), x="Year, Quarter", y="Rate (%)") + 
    theme(plot.title = element_text(hjust = 0.5)) +
    theme_classic() +
    theme(panel.background =element_rect(fill = "white", color = "slategray4"),
          panel.grid.minor = element_line(colour = "black"),
          panel.border = element_rect(colour = "slategray4", fill=NA, size=2)) +
    theme(plot.background =element_rect(fill = "#ffffd9", color = "slategray4")) +
    theme(legend.background = element_rect(fill="#ffffd9",
                                           size=0.5, linetype="solid", 
                                           colour ="slategray4"))
  
  return(ggplotly(is_plot))
}

########################################################### STABILITY INDEX  #############################################################
get_points <- function(){
  q <- quantile(data$emp_ch_rate, probs=c(0, 0.25, 0.5, 0.75))
  data$emp_ch_points <- as.numeric(findInterval(data$emp_ch_rate, q))
  
  q <- quantile(data$job_vacancy_rate, probs=c(0, 0.25, 0.5, 0.75))
  data$vacancy_points <- as.numeric(findInterval(data$job_vacancy_rate, q))
  
  q <- quantile(data$recruitment_rate, probs=c(0, 0.25, 0.5, 0.75))
  data$rec_points <- as.numeric(findInterval(data$recruitment_rate, q))
  
  # Resignation (opposite)
  q <- quantile(data$resignation_rate, probs=c(0, 0.25, 0.5, 0.75, 1))
  data$res_points <- cut(data$resignation_rate, breaks=q, labels=c(4, 3, 2, 1))
  
  # Retrenchment (opposite)
  q <- quantile(data$retrench_rate, probs=c(0, 0.25, 0.5, 0.75, 1))
  data$ret_points <- cut(data$retrench_rate, breaks=q, labels=c(4, 3, 2, 1))
  data$ret_points[is.na(data$ret_points)] <- 4
  
  data$res_points <- as.numeric(as.character(data$res_points))
  data$ret_points <- as.numeric(as.character(data$ret_points))
  return(data)
}
#Run this function
data <- get_points()

# User enters 2 periods
get_rankings <- function(industry, start.year, start.quarter, end.year, end.quarter, type){
  period.data <- data %>% 
    filter(year==start.year:end.year) %>% 
    filter(!(year==start.year & quarter<start.quarter), !(year==end.year & quarter>end.quarter))
  if (type == 'job stability'){
    # JOB STABILITY
    # Compute average across industries over the period
    js.avgs <- period.data %>% group_by(industry2) %>%
      summarise(js.score=(emp_ch_points+ret_points)) %>% summarise(js.score=sum(js.score))
    js.avgs <- quantile(js.avgs$js.score, probs=c(0, 0.25, 0.5, 0.75))
    
    # Compute actual for industry over the period
    js.actual <- period.data %>% 
      filter(industry2==industry) %>% 
      summarise(js.score=emp_ch_points+ret_points) %>% summarise(js.score=sum(js.score))
    
    js.rank <- findInterval(js.actual, js.avgs)
    return(js.rank)
  }else{
    # INDUSTRY GROWTH
    ig.avgs <- period.data %>% group_by(industry2) %>%
      summarise(ig.score=(emp_ch_points+vacancy_points+rec_points)) %>% summarise(ig.score=sum(ig.score))
    ig.avgs <- quantile(ig.avgs$ig.score, probs=c(0, 0.25, 0.5, 0.75))
    
    # Compute actual for industry over the period
    ig.actual <- period.data %>% 
      filter(industry2==industry) %>% 
      summarise(ig.score=emp_ch_points+ret_points) %>% summarise(ig.score=sum(ig.score))
    
    ig.rank <- findInterval(ig.actual, ig.avgs)
    return(ig.rank)
  }
}

get_rank <- function(industry, start.year, start.quarter, end.year, end.quarter, criteria) {
  rank = get_rankings(industry, start.year, start.quarter, end.year, end.quarter, criteria)
  
  if (rank==4) {
    percentile <- "above 75th"
  } else if(rank==3) {
    percentile <- "within the 50th to 75th"
  } else if(rank==2) {
    percentile <- "within the 25th to 50th"
  } else {
    percentile <- "below the 25th"
  }
  if(criteria=="job stability") {
    return(print(paste("The stability of the", industry, "industry is ranked", percentile, "percentile.")))
  }
  else {
    return(print(paste("The growth of the", industry, "industry is ranked", percentile, "percentile.")))
  }
}
#EXAMPLE
#get_rank("financial and insurance services", 2013, 1, 2014, 4,"job stability")


########################################COMPARE STATS ACROSS TWO TIME PERIODS FOR A FIXED INDUSTRY #####################################
get_comparison_time <- function(industry, period1, period2, flag){
  ui1 = period1
  ui2 = period2
  col = c('Year', 'year','industry2','emp_ch_rate', 'retrench_rate', 'job_vacancy_rate', 'recruitment_rate', 'resignation_rate')
  #if by quarter
  if (flag !='Year'){
    tp1 <-data[data$Year == ui1 & trimws(data$industry2) == industry, col]
    tp2 <-data[data$Year == ui2 & trimws(data$industry2) == industry, col]
    
    avg1 <- tp1 %>% group_by(Year) %>% summarise(`Average Employment Rate` = mean(emp_ch_rate),
                                                 `Average Retrenchment Rate` = mean(retrench_rate),
                                                 `Average Job Vacancy Rate` = mean(job_vacancy_rate),
                                                 `Average Recruitment Rate` = mean(recruitment_rate),
                                                 `Average Resignation Rate` = mean(resignation_rate))
    avg2 <- tp2 %>% group_by(Year) %>% summarise(`Average Employment Rate` = mean(emp_ch_rate),
                                                 `Average Retrenchment Rate` = mean(retrench_rate),
                                                 `Average Job Vacancy Rate` = mean(job_vacancy_rate),
                                                 `Average Recruitment Rate` = mean(recruitment_rate),
                                                 `Average Resignation Rate` = mean(resignation_rate))
  }else{
    tp1 <-data[data$year == as.integer(ui1) & trimws(data$industry2) == industry, col]
    tp2 <-data[data$year == as.integer(ui2) & trimws(data$industry2) == industry, col]
    
    avg1 <- tp1 %>% group_by(year) %>% summarise(`Average Employment Rate` = mean(emp_ch_rate),
                                                 `Average Retrenchment Rate` = mean(retrench_rate),
                                                 `Average Job Vacancy Rate` = mean(job_vacancy_rate),
                                                 `Average Recruitment Rate` = mean(recruitment_rate),
                                                 `Average Resignation Rate` = mean(resignation_rate))
    avg2 <- tp2 %>% group_by(year) %>% summarise(`Average Employment Rate` = mean(emp_ch_rate),
                                                 `Average Retrenchment Rate` = mean(retrench_rate),
                                                 `Average Job Vacancy Rate` = mean(job_vacancy_rate),
                                                 `Average Recruitment Rate` = mean(recruitment_rate),
                                                 `Average Resignation Rate` = mean(resignation_rate))
  }
  
  avg1<- avg1 %>% gather('stats', 'rate', `Average Employment Rate`:`Average Resignation Rate`)
  avg2<- avg2 %>% gather('stats', 'rate', `Average Employment Rate`:`Average Resignation Rate`)
  
  avg1$rate <- as.numeric(format(round(avg1$rate, 3), nsmall = 3))
  avg2$rate <- as.numeric(format(round(avg2$rate, 3), nsmall = 3))
  avg1 <- avg1 %>% arrange(stats)
  avg2 <- avg2 %>% arrange(stats)
  
  vec = c(ui1, ui2)
  avg1 <- avg1 %>% mutate(Renamed = paste0(stats,": ", rate, "%"))
  avg2 <- avg2 %>% mutate(Renamed = paste0(stats,": ", rate, "%"))
  
  #######################################################      PLOT          ##########################################################
  p = ggplot(height = 500)
  for (i in seq(1:5)){
    if (abs(avg1$rate[i]) > abs(avg2$rate[i])){
      p = p+annotate("segment", x = i, xend = i, y = 0, yend = avg1$rate[i], size= 1,
                     colour = "grey74")
      p = p+annotate("segment", x = i, xend = i, y = 0, yend = avg2$rate[i],size=1,
                     colour = "dodgerblue2")
    }else{
      p = p+annotate("segment", x = i, xend = i, y = 0, yend = avg2$rate[i], size =1,
                     colour = "dodgerblue2")
      p = p+annotate("segment", x = i, xend = i, y = 0, yend = avg1$rate[i],size=1,
                     colour = "grey74")
    }
  }
  p <- p + scale_x_discrete(labels = c('Employment Change', 'Job Vacancy', "Recruitment", "Retrenchment", "Resignation"))+theme_bw()+
    geom_point_interactive(data = avg1, aes(x = stats, y = rate,color = "grey74",tooltip = Renamed, data_id = stats), size =4)+
    geom_point_interactive(data = avg2, aes(x = stats, y = rate, color = "dodgerblue2",tooltip = Renamed, data_id = stats),size =4)+
    labs(color = "Legend") +
    scale_color_manual(values = c("grey74"="grey74", "dodgerblue2" = "dodgerblue2"), labels = vec)+
    coord_flip() + 
    labs(title = paste('Employment Metrics Comparison\n(By Time Period) for', str_to_title(industry)),
         x = "Metric",
         y = "Rate %")+
    geom_hline(yintercept=0, linetype = 'dashed', color = 'red')+
    theme(text = element_text(size=10),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(color = 'black', size = "9"),
          axis.text.y = element_text(color = '#081d58', size = "9"),
          panel.background =element_rect(fill = "white", color = "thistle1"),
          plot.background =element_rect(fill = "#ffffd9", color = "blue"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.background = element_rect(fill="lightblue",
                                           size=0.5, linetype="solid", 
                                           colour ="darkblue"))
  
  return (girafe(ggobj = p,width_svg = 8, height_svg = 3))
}

#EXAMPLE
#get_comparison_industry("manufacturing", "2019 Q2", "2012 Q1", "Quarter")
#get_comparison_industry("manufacturing", "2019", "2012", "Year")

## Helper function to organise Legend later
compress_input <- function(str){
  return(gsub("and ", "and \n", gsub(", ", ",\n", str)))
}
###############################  COMPARE INDUSTRIES WITH FIXED YEAR ##############################################################
get_comparison_industry <- function(industry1, industry2, year){
  ind1 = industry1
  ind2 = industry2
  col = c('year','quarter', 'industry2','emp_ch_rate', 'retrench_rate', 
          'job_vacancy_rate', 'recruitment_rate', 'resignation_rate')
  t1 <- data[trimws(data$industry2) == ind1 & data$year == as.integer(year),col]
  t2 <- data[trimws(data$industry2) == ind2 & data$year == as.integer(year),col]
  
  ind1 = compress_input(ind1)
  ind2 = compress_input(ind2)
  
  avg_t1 <- t1 %>% group_by(year) %>% summarise(`Average Employment Rate` = mean(emp_ch_rate),
                                                `Average Retrenchment Rate` = mean(retrench_rate),
                                                `Average Job Vacancy Rate` = mean(job_vacancy_rate),
                                                `Average Recruitment Rate` = mean(recruitment_rate),
                                                `Average Resignation Rate` = mean(resignation_rate))
  avg_t2 <- t2 %>% group_by(year) %>% summarise(`Average Employment Rate` = mean(emp_ch_rate),
                                                `Average Retrenchment Rate` = mean(retrench_rate),
                                                `Average Job Vacancy Rate` = mean(job_vacancy_rate),
                                                `Average Recruitment Rate` = mean(recruitment_rate),
                                                `Average Resignation Rate` = mean(resignation_rate))
  
  avg_t1<- avg_t1 %>% gather('stats', 'rate', `Average Employment Rate`:`Average Resignation Rate`)
  avg_t2<- avg_t2 %>% gather('stats', 'rate', `Average Employment Rate`:`Average Resignation Rate`)
  
  avg_t1$rate <- as.numeric(format(round(avg_t1$rate, 3), nsmall = 3))
  avg_t2$rate <- as.numeric(format(round(avg_t2$rate, 3), nsmall = 3))
  
  avg_t1 <- avg_t1 %>% arrange(stats)
  avg_t2 <- avg_t2 %>% arrange(stats)
  
  ####################################################### PLOT ##############################################################
  p = ggplot()
  for (i in seq(1:5)){
    if (abs(avg_t1$rate[i]) > abs(avg_t2$rate[i])){
      p = p+annotate("segment", x = i, xend = i, y = 0, yend = avg_t1$rate[i], size= 1,
                     colour = "tomato3")
      p = p+annotate("segment", x = i, xend = i, y = 0, yend = avg_t2$rate[i],size=1,
                     colour = "royalblue3")
    }else{
      p = p+annotate("segment", x = i, xend = i, y = 0, yend = avg_t2$rate[i], size =1,
                     colour = "royalblue3")
      p = p+annotate("segment", x = i, xend = i, y = 0, yend = avg_t1$rate[i],size=1,
                     colour = "tomato3")
    }
  }
  vec = c(ind1, ind2)
  vec
  avg_t1 <- avg_t1 %>% mutate(Renamed = paste0(stats,": ", rate, "%"))
  avg_t2 <- avg_t2 %>% mutate(Renamed = paste0(stats,": ", rate, "%"))
  
  p <- p + scale_x_discrete(labels = c('Employment Change', 'Job Vacancy', "Recruitment", "Retrenchment", "Resignation"))+theme_bw()+
    geom_point_interactive(data = avg_t1, aes(x = stats, y = rate,color = "tomato3", tooltip = Renamed, data_id = stats), size =4)+
    geom_point_interactive(data = avg_t2, aes(x = stats, y = rate, color = "royalblue3", tooltip = Renamed, data_id = stats),size =4)+
    labs(color = "Legend") +
    scale_color_manual(values = c("tomato3"="tomato3", "royalblue3" = "royalblue3"), labels = vec)+
    geom_hline(yintercept=0, linetype = 'dashed', color = 'red')+
    coord_flip()+
    labs(title = paste('Employment Metrics Comparison\n(By Industry) for Year', year),
         x = "Metric",
         y = "Rate %")+
    theme(text = element_text(size=10),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(color = 'black', size = "9"),
          axis.text.y = element_text(color = '#081d58', size = "9"),
          panel.background =element_rect(fill = "white", color = "thistle1"),
          plot.background =element_rect(fill = "#ffffd9", color = "blue"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.background = element_rect(fill="lightblue",
                                           size=0.5, linetype="solid", 
                                           colour ="darkblue"))
  
  return(girafe(ggobj = p, width_svg = 8, height_svg = 3))
}
#EXAMPLE
#get_comparison_industry("manufacturing", "construction", "2020")

wages <- read.csv('wage.data.csv')


#########################################     HELPER FUNCTION FOR WAGE TAB    ##################################################
store_levels <- function(data, year){
  hashmap = hash()
  data = data[data$Year == year,]
  for (industry in unique(data$Industry)){
    temp = unique(data[data$Industry==industry, "Level"])
    names(temp) = gsub("And", "and", str_to_title(temp))
    hashmap[[industry]] = temp
  }
  return(hashmap)
}

color <- c('lightslategray', '#c7e9b4', '#41b6c4', '#225ea8', '#253494', 'lavenderblush', 'lemonchiffon', 'lightgoldenrod', 'lightblue4')
hashmap = hash()
un_ind <- unique(wages$Industry)
count = 1
for (ind in un_ind){
  hashmap[ind] = color[count]
  count = count+1
}


######################################### *LEVEL* COMPARISON WAGES IN SELECTED INDUSTRY ##########################################
get_wage_industry <- function(industry, year, type){
  
  filtered <- wages[trimws(wages$Industry) == industry & wages$Year==as.integer(year), ]
  #if basic
  if (type=='basic'){
    levels <- filtered %>% group_by(Level) %>% summarise(`Average 25th Perc.` = round(mean(Basic.Wage....25th.Percentile....)),
                                                         `Average Median` = round(mean(Basic.Wage....Median.....)),
                                                         `Average 75th Perc.` = round(mean(Basic.Wage....75th.Percentile....)))
  }else{
    levels <- filtered %>% group_by(Level) %>% summarise(`Average 25th Perc.` = round(mean(Gross.Wage....25th.Percentile....)),
                                                         `Average Median` = round(mean(Gross.Wage....Median.....)),
                                                         `Average 75th Perc.` = round(mean(Gross.Wage....75th.Percentile....)))
  }
  
  levels$Level = factor(gsub("And ", "and\n", str_to_title(gsub(", ", ",\n", levels$Level))))
  levels$Level
  
  width = 4/nrow(levels)
  
  p <-ggplot(data = levels, aes(x = Level, y = `Average Median`)) +
    geom_crossbar(aes(ymin = `Average 25th Perc.`, ymax = `Average 75th Perc.`), width =width, fill = hashmap[[industry]]) +
    scale_x_discrete(limits = rev(levels(levels$Level)))+
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10),guide = guide_axis(check.overlap = TRUE))+
    theme_bw()+
    coord_flip() +
    theme(text = element_text(size=12))+
    theme(axis.text.x = element_text(color = '#081d58', size = "10.5"))+
    theme(axis.text.y = element_text(color = '#081d58', size = "10"))+
    labs(title = paste(year, str_to_title(industry), 'Wage Comparison'),
         y = paste(str_to_title(type),"Salary $"))+
    theme(panel.background =element_rect(fill = "white", color = "black"),
          axis.title.y=element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.background =element_rect(fill = "#ffffd9", color = "blue"))
  return(ggplotly(p)) 
}
#EXAMPLE
#get_wage_industry('art, entertainment, recreation and other services', 2020, 'basic')
#get_wage_industry('wholesale and retail trade', 2019, 'gross')


######################################### *OCCUPATION* COMPARISON WAGES IN SELECTED INDUSTRY+LEVEL ##########################################
get_wage_level<- function(industry, level, year, type){
  filtered <- wages[trimws(wages$Industry) == industry & wages$Year==as.integer(year) & trimws(wages$Level) == level, ]
  
  if (type == 'basic'){
    occupation <- filtered %>% group_by(Occupation) %>% summarise(`Average 25th Perc.` = mean(Basic.Wage....25th.Percentile....),
                                                                  `Average Median` = mean(Basic.Wage....Median.....),
                                                                  `Average 75th Perc.` = mean(Basic.Wage....75th.Percentile....))
  }else{
    occupation <- filtered %>% group_by(Occupation) %>% summarise(`Average 25th Perc.` = mean(Gross.Wage....25th.Percentile....),
                                                                  `Average Median` = mean(Gross.Wage....Median.....),
                                                                  `Average 75th Perc.` = mean(Gross.Wage....75th.Percentile....))
  }
  occupation$Occupation = gsub("It", "IT",str_to_title(occupation$Occupation))
  
  #print(occupation)
  if (nrow(occupation)<=4){
    width = 0.5
  }else{
    width = 5.5/nrow(occupation)
  }
  occupation$`Job Title` = as.factor(occupation$Occupation)
  p <-ggplot(data = occupation, aes(x = `Job Title`, y = `Average Median`)) +
    geom_crossbar(aes(ymin = `Average 25th Perc.`, ymax = `Average 75th Perc.`), width =width, fill = hashmap[[industry]]) +
    scale_x_discrete(limits = rev(levels(occupation$`Job Title`)))+
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10),guide = guide_axis(check.overlap = TRUE))+
    theme_bw()+
    coord_flip() +
    theme(text = element_text(size=12))+
    theme(axis.text.x = element_text(color = 'midnightblue', size = "11"))+
    theme(axis.text.y = element_text(color = 'midnightblue', size = "9.5"))+
    labs(title = 'Wage Comparison by Category',
         y = "Salary $")+
    theme(panel.background =element_rect(fill = "white", color = "black"),
          axis.title.y=element_blank(),
          plot.title = element_text(hjust = 0.5))+
    theme(plot.background =element_rect(fill = "lightcyan", color = "blue"))
  return(ggplotly(p))
}
#EXAMPLE
#get_wage_level('manufacturing', 'managers', 2019, 'basic')


industry_list <- list("Accomodation And Food Services" = "accommodation and food services", 
                      "Administrative And Support Services" = "administrative and support services",
                      "Art, Entertainment, Recreation And Other Services" = "art, entertainment, recreation and other services",
                      "Construction" = "construction",
                      "Education, Health And Social Services" = "education, health and social services",
                      "Finance And Insurance Services" = "financial and insurance services",
                      "Information and Communications" = "information and communications",
                      "Manufacturing" = "manufacturing",
                      "Professional Services" = "professional services",
                      "Transportation And Storage" = "transportation and storage",
                      "Wholesale And Retail Trade" = "wholesale and retail trade"
)
year_lst <- list("2021" = 2021,
                 "2020" = 2020,
                 "2019" = 2019,
                 "2018" = 2018,
                 "2017" = 2017,
                 "2016" = 2016,
                 "2015" = 2015,
                 "2014" = 2014,
                 "2013" = 2013,
                 "2012" = 2012,
                 "2011" = 2011,
                 "2010" = 2010)


yr_qtr_lst <- list("2021 Q2" = "2021 Q2", 
                   "2021 Q1" = "2021 Q1",
                   "2020 Q4" = "2020 Q4",
                   "2020 Q3" = "2020 Q3",
                   "2020 Q2" = "2020 Q2",
                   "2020 Q1" = "2020 Q1",
                   "2019 Q4" = "2019 Q4",
                   "2019 Q3" = "2019 Q3",
                   "2019 Q2" = "2019 Q2",
                   "2019 Q1" = "2019 Q1",
                   "2018 Q4" = "2018 Q4",
                   "2018 Q3" = "2018 Q3",
                   "2018 Q2" = "2018 Q2",
                   "2018 Q1" = "2018 Q1",
                   "2017 Q4" = "2017 Q4",
                   "2017 Q3" = "2017 Q3",
                   "2017 Q2" = "2017 Q2",
                   "2017 Q1" = "2017 Q1",
                   "2016 Q4" = "2016 Q4",
                   "2016 Q3" = "2016 Q3",
                   "2016 Q2" = "2016 Q2",
                   "2016 Q1" = "2016 Q1",
                   "2015 Q4" = "2015 Q4",
                   "2015 Q3" = "2015 Q3",
                   "2015 Q2" = "2015 Q2",
                   "2015 Q1" = "2015 Q1",
                   "2014 Q4" = "2014 Q4",
                   "2014 Q3" = "2014 Q3",
                   "2014 Q2" = "2014 Q2",
                   "2014 Q1" = "2014 Q1",
                   "2013 Q4" = "2013 Q4",
                   "2013 Q3" = "2013 Q3",
                   "2013 Q2" = "2013 Q2",
                   "2013 Q1" = "2013 Q1",
                   "2012 Q4" = "2012 Q4",
                   "2012 Q3" = "2012 Q3",
                   "2012 Q2" = "2012 Q2",
                   "2012 Q1" = "2012 Q1",
                   "2011 Q4" = "2011 Q4",
                   "2011 Q3" = "2011 Q3",
                   "2011 Q2" = "2011 Q2",
                   "2011 Q1" = "2011 Q1",
                   "2010 Q4" = "2010 Q4",
                   "2010 Q3" = "2010 Q3",
                   "2010 Q2" = "2010 Q2",
                   "2010 Q1" = "2010 Q1"
)
#hashmaps for wage tab
hm2019 = store_levels(wages, 2019)
hm2020 = store_levels(wages, 2020)


wage_industry_list <- list("Accomodation And Food Services"= "accommodation and food services",
                           "Art, Entertainment, Recreation And Other Services" = "art, entertainment, recreation and other services",
                           "Construction" = "construction",
                           "Education, Health And Social Services" = "education, health and social services",
                           "Finance And Insurance Services" = "financial and insurance services",
                           "Information And Communications" = "information and communications",
                           "Manufacturing" = "manufacturing",
                           "Transportation And Storage" = "transportation and storage",
                           "Wholesale And Retail Trade" = "wholesale and retail trade")



######################################### JOB SEARCH TAB #################################################################

jobdata <- read.csv("jobdata.csv")


overview <- function(selected.industry, job, option)
{
  filtered <- jobdata %>% filter(industry == selected.industry, occupation %in% job)
  
  if (option=="Salary")
  {
    salary_data <- filtered
    salary_data$salary <- gsub(",", "", salary_data$salary)
    salary_data$min <- as.numeric(str_extract(salary_data$salary, "\\d+\\,*\\d*"))
    salary_data$max <- as.numeric(str_extract(salary_data$salary, "(\\d+)(?!.*\\d)"))
    salary_data <- salary_data %>% 
      group_by(occupation) %>% 
      summarise(min.avg=round(mean(min)), 
                max.avg=round(mean(max))) %>% 
      as.data.frame()
    
    salary.plot <- ggplotly(ggplot(data = salary_data) + 
                              geom_segment(aes(x = occupation, xend = occupation, 
                                               y = min.avg, yend = max.avg),
                                           size = 5, alpha = 0.6, color="#081d58") + 
                              coord_flip() + 
                              labs(title="Average Range of Salaries of Jobs Selected", x="", y = "Salary ($)") + 
                              theme_classic())
    return(salary.plot)
  }

  if (option=="Location")
  {
   locations <- data.frame(paste(filtered$company, " Singapore"))
   locations <- geocode(locations$paste.filtered.company....Singapore..)
   location.plot <- leaflet() %>% setView(lat=1.290270 , lng=103.851959 , zoom=11) %>% addTiles() %>% addMarkers(data = locations, lng = ~lon, lat = ~lat, label =~filtered$company)
   return(location.plot)
  }
  
  
  if (option=="Career Level")
  {
    careerlevels <- data.frame(filtered$careerlevel)
    names(careerlevels) <- c("careerlevel")
    careerlevels <- careerlevels %>% 
      group_by(careerlevel) %>% 
      summarise(count=n())
    careerlevel.plot <- treemap(careerlevels, index = c("careerlevel"), vSize = "count", palette="YlGnBu", title="Career Levels of Jobs Selected")
    
    
    return(careerlevel.plot)
  }
  
  
  if (option=="Experience")
  {
    
    filtered <- filtered[,c("jobtype", "experience")]
    filtered <- filtered[grep("years", filtered$experience),]
    filtered$years <- as.numeric(gsub("([0-9]+).*$", "\\1", filtered$experience))
    filtered  <- filtered %>% 
      group_by(jobtype, experience, years) %>% 
      summarise(Count=as.integer(n())) %>% 
      rename(Years=years)
    experience.plot <- filtered %>% 
      group_by(jobtype) %>% 
      plot_ly(x = ~Years, y=~Count, type="bar", orientation="v", name=~jobtype, 
              color= ~jobtype,
              colors = 'YlGnBu',
              marker = list(line = list(color = "#081d58",
                          width = 2))) %>%
      layout(barmode = 'stack', bargap = 0.6, xaxis=list(tickformat=',d'), showlegend=T, title = "Years of Experience Required \n Based on Job Type")
    return(experience.plot)
    
  }
  
  if (option=="Qualifications")
  {
    
    qualifications <- data.frame(filtered$qualifications)
    names(qualifications) <- c("qualifications")
    
    qualifications$`Primary/Secondary School/O Level` <- 0
    qualifications$`Higher Secondary/Pre-U/'A' Level` <- 0
    qualifications$Diploma <- 0
    qualifications$`Advanced/Higher/Graduate Diploma` <- 0
    qualifications$`Professional Certificate/NiTEC` <- 0
    qualifications$`Post Graduate Diploma` <- 0
    qualifications$`Bachelor's Degree` <- 0
    qualifications$`Professional Degree` <- 0
    qualifications$`Master's Degree` <- 0
    qualifications$`Doctorate (PhD)` <- 0
    qualifications$`Not Specified` <- 0
    
    qualifications$`Primary/Secondary School/O Level`[grepl("Primary/Secondary School/O Level", qualifications$qualifications)] = 1
    qualifications$`Higher Secondary/Pre-U/'A' Level`[grepl("Higher Secondary/Pre-U/'A' Level", qualifications$qualifications)] = 1
    qualifications$Diploma[qualifications$qualifications=="Diploma"] = 1
    qualifications$`Advanced/Higher/Graduate Diploma`[grepl("Advanced/Higher/Graduate Diploma", qualifications$qualifications)] = 1
    qualifications$`Professional Certificate/NiTEC`[grepl("Professional Certificate/NiTEC", qualifications$qualifications)] = 1
    qualifications$`Post Graduate Diploma`[grepl("Post Graduate Diploma", qualifications$qualifications)] = 1
    qualifications$`Bachelor's Degree`[grepl("Bachelor's Degree", qualifications$qualifications)] = 1
    qualifications$`Professional Degree`[grepl("Professional Degree", qualifications$qualifications)] = 1
    qualifications$`Master's Degree`[grepl("Master's Degree", qualifications$qualifications)] = 1
    qualifications$`Doctorate (PhD)`[grepl("Doctorate (PhD)", qualifications$qualifications)] = 1
    qualifications$`Not Specified`[grepl("Not Specified", qualifications$qualifications)] = 1
    
    qualifications$qualifications <- NULL
    qualifications$Job <- c(paste("Job", 1:nrow(qualifications)))
    qualifications <- qualifications %>% gather(Qualifications, Value, -Job)
    qualifications <- qualifications %>% group_by(Qualifications) %>% summarise(Count=sum(Value)) %>% filter(Count!=0)
    qualifications.plot <- plot_ly(data=qualifications, x=~Count, y=~Qualifications, type="bar", orientation="h", marker = list(color = ~Count, colorscale = list(c(0, 1), c("#ffffd9", "#081d58")), 
                                                                                                                                line = list(color = "#081d58",
                                                                                                                                width = 2))) %>% layout(title = 'Qualifications Required/Accepted',
                                                                                                                                                              xaxis = list(title = "Count"),
                                                                                                                                                              yaxis = list(title = ""))
    return(qualifications.plot)
  }
}


display <- function(selected.industry, job, option, postal)
{
  filtered <- jobdata %>% 
    filter(industry == selected.industry, occupation %in% job) %>% 
    select(jobtitle, company, occupation, salary, joblink)
  names(filtered) <- c("Job Title", "Company", "Occupation", "Salary", "Job Link")
  
  if (option=="In Any Order")
  {
    filtered <- filtered %>% select(-Salary)
    return(filtered)
  }
  
  if (option=="Highest Salary")
  {
    filtered$Salary <- gsub(",", "", filtered$Salary)
    filtered$min <- as.numeric(str_extract(filtered$Salary, "\\d+\\,*\\d*"))
    filtered$max <- as.numeric(str_extract(filtered$Salary, "(\\d+)(?!.*\\d)"))
    filtered <- filtered %>% mutate(Avg.Salary=(min+max)/2) %>% 
      arrange(-Avg.Salary) %>% 
      select(-c(Salary, min, max, Avg.Salary))
    return(filtered)
  }
  
  if (option=="Nearest Location")
  { current.location <- data.frame(lon = 0, lat = 0)
  current.location <- geocode(as.character(postal))
  filtered$userlat <- current.location$lat
  filtered$userlon <- current.location$lon
  company.location <- geocode(paste(filtered$Company, "Singapore"))
  filtered$companylat <- company.location$lat
  filtered$companylon <- company.location$lon
  filtered$dist <- with(filtered, distHaversine(cbind(userlon, userlat), cbind(companylon, companylat)))
  filtered <- filtered %>% 
    arrange(dist) %>% 
    select("Job Title", "Company", "Occupation", "Job Link")  %>% as.data.frame()
  return(filtered)
  }
}


######################################### JOB COMPARISON TAB #################################################################

comparison <- function(jobselect1, jobselect2, option, postal) {
  job1 <- jobdata %>% filter(joblink==jobselect1)
  job2 <- jobdata %>% filter(joblink==jobselect2)
  
  compare <- as.data.frame(rbind(job1, job2))
  
  if (option=="Salary")
  {
    compare$salary <- gsub(",", "", compare$salary)
    compare$min <- as.numeric(str_extract(compare$salary, "\\d+\\,*\\d*"))
    compare$max <- as.numeric(str_extract(compare$salary, "(\\d+)(?!.*\\d)"))
    compare$naming <- paste(compare$jobtitle, compare$company, sep="\n")
    salary.plot <- ggplot(data = compare) + 
      geom_segment(aes(x = naming, 
                       xend = naming, 
                       y = min, 
                       yend = max), 
                   size = 5, 
                   colour = "#1d91c0", 
                   alpha = 0.6) + 
      coord_flip() + 
      labs(x = "", y = "Salary ($)", title="Comparison of Salaries") + theme_classic()
    
    return(salary.plot)
  }
  
  if (option=="Career Level")
  {
    careerlevels <- data.frame(compare$careerlevel)
    names(careerlevels) <- c("careerlevel")
    careerlevels <- careerlevels %>% group_by(careerlevel) %>% summarise(count=n())
    careerlevel.plot <- treemap(careerlevels, index = c("careerlevel"), vSize = "count", palette="YlGnBu", title="Comparison of Career Levels")
    return(careerlevel.plot)
  }
  
  if (option=="Qualifications") {
    qualifications <- data.frame(compare$qualifications)
    names(qualifications) <- c("Qualifications")
    qualifications$`Primary/Secondary School/O Level` <- "No"
    qualifications$`Higher Secondary/Pre-U/'A' Level` <- "No"
    qualifications$Diploma <- "No"
    qualifications$`Advanced/Higher/Graduate Diploma` <- "No"
    qualifications$`Professional Certificate/NiTEC` <- "No"
    qualifications$`Post Graduate Diploma` <- "No"
    qualifications$`Bachelor's Degree` <- "No"
    qualifications$`Professional Degree` <- "No"
    qualifications$`Master's Degree` <- "No"
    qualifications$`Doctorate (PhD)` <- "No"
    qualifications$`Not Specified` <- "No"
    
    qualifications$`Primary/Secondary School/O Level`[grepl("Primary/Secondary School/O Level", qualifications$Qualifications)] = "Yes"
    qualifications$`Higher Secondary/Pre-U/'A' Level`[grepl("Higher Secondary/Pre-U/'A' Level", qualifications$Qualifications)] = "Yes"
    qualifications$Diploma[qualifications$qualifications=="Diploma"] = "Yes"
    qualifications$`Advanced/Higher/Graduate Diploma`[grepl("Advanced/Higher/Graduate Diploma", qualifications$Qualifications)] = "Yes"
    qualifications$`Professional Certificate/NiTEC`[grepl("Professional Certificate/NiTEC", qualifications$Qualifications)] = "Yes"
    qualifications$`Post Graduate Diploma`[grepl("Post Graduate Diploma", qualifications$Qualifications)] = "Yes"
    qualifications$`Bachelor's Degree`[grepl("Bachelor's Degree", qualifications$Qualifications)] = "Yes"
    qualifications$`Professional Degree`[grepl("Professional Degree", qualifications$Qualifications)] = "Yes"
    qualifications$`Master's Degree`[grepl("Master's Degree", qualifications$Qualifications)] = "Yes"
    qualifications$`Master's Degree`[grepl("Master's Degree", qualifications$Qualifications)] = "Yes"
    qualifications$`Doctorate (PhD)`[grepl("Doctorate (PhD)", qualifications$Qualifications)] = "Yes"
    qualifications$`Not Specified`[grepl("Not Specified", qualifications$Qualifications)] = "Yes"
    
    qualifications$Qualifications <- NULL
    qualifications$jobtitle <- paste(compare$jobtitle, compare$company, sep="\n")
    qualifications <- qualifications %>% gather(Qualifications, Value, -jobtitle) %>% rename(`Job Title`=jobtitle)
    qualifications.plot <- ggplot(qualifications,aes(x=Qualifications, y=`Job Title`)) + 
      geom_tile(aes(fill=Value)) + coord_flip() + 
      labs(title="Comparison of Qualifications Required/Accepted", x="", y="") + 
      scale_fill_manual(values=c("#081d58", "#c7e9b4"))
    return(qualifications.plot)
  }
  
  if (option=="Experience") {
    compare$years <- as.numeric(gsub("([0-9]+).*$", "\\1", compare$experience))
    compare$Job <- paste(compare$jobtitle, compare$company, sep="\n")
    compare <- compare %>% rename(Years=years) %>% group_by(jobtype)
    experience.plot <- ggplotly(ggplot(data=compare, aes(x=Job, y=Years)) + theme_bw() + geom_bar(fill = "#7fcdbb", color="#081d58", stat="identity", width = 0.5) + 
                                  labs(x="", y = "Years", title="Comparison of Years of Experience Required"))
    return(experience.plot)
  }
  
  if (option=="Location")
  {
    compare$naming <- paste(compare$jobtitle, compare$company, sep="\n")
    compare$complocation <- paste(compare$company, "Singapore")
    companylocation <- geocode(compare$complocation)
    compare$lat <- companylocation$lat
    compare$lon <- companylocation$lon
    location.plot <- leaflet() %>% setView(lat=1.290270 , lng=103.851959 , zoom=11) %>% addTiles() %>% addMarkers(data = compare, lng = ~lon, lat = ~lat, popup = ~naming) %>% addMarkers(data = mylocation, lng = ~lon, lat = ~lat)
    return(location.plot)
  }
}


# SHINY INTERFACE
########################################################################################################

ui <- dashboardPage(skin="purple",
  dashboardHeader(),
  dashboardSidebar(
    
    
    
    sidebarMenu(
      menuItem("Job Industry Overview", tabName = "job_industry_overview", icon = icon("calendar")),
      menuItem("Compare Industry Conditions", tabName = "compare_industry_conditions", icon = icon("book")),
      menuItem("Compare Wages", tabName = "compare_wages", icon = icon("money-bill-wave")),
      menuItem("Search Jobs", tabName = "search_jobs", icon = icon("search")),
      menuItem("Compare Jobs", tabName = "compare_jobs", icon = icon("building"))
  )
),

  



  dashboardBody(
    tabItems(
      
      # First tab - Industry overview
      tabItem(tabName = "job_industry_overview",
              h1("Overview of Job Industries"),
              
              
              fluidRow(
                
              sidebarPanel(
                selectInput("Industry", label = h3("Select Industry"), 
                            choices = industry_list, 
                            selected = 1),
                br(),
                sliderInput("Year", label = h3("Select Year"), min = 2010, 
                            max = 2021, value = c(2015, 2021), sep= ""),
                br(),
                sliderInput("Quarter", label = h3("Select Quarter"), min = 1, 
                            max = 4, value = c(1,2), sep= "")
               
              ),
              
              box(title = "Job Stability Graph", plotlyOutput("job_stab"), width=8, footer= textOutput("rankings_stability"))
              ),
              fluidRow(
                column(12,offset=4,
                       box(title = "Industry Growth Graph", plotlyOutput("growth"), width=8,footer= textOutput("rankings_growth"))
                )
              )
            ),
      
      
      # Second tab - Compare industry conditions for an industry over 2 different periods or 2 separate industries over the same period
      tabItem(tabName = "compare_industry_conditions",
              h1("Compare Industry Conditions"),
              
              
              fluidRow(
                
                sidebarPanel(
                  radioButtons("type", label = h3("Compare"),
                               choices = c("Industry (Fixed Period)" =1, "Period (Fixed Industry)"=2, "Both"=3), 
                               selected = 1),
                  conditionalPanel(
                    condition ="input.type== 1 ",
                    selectInput("Industry2", label = h4("Select First Industry"), 
                                choices = industry_list,
                                width = "80%",
                                selected = 1),
                    selectInput("Industry3", label = h4("Select Second Industry"), 
                                choices = industry_list, 
                                width = "80%",
                                selected = 2),
                    selectInput("Year2", label = h4("Select Year"),
                                choices = year_lst,
                                width = "40%",
                                selected=1)
                  ),
                  
                  conditionalPanel(
                    condition ="input.type== 2 ",
                    selectInput("Industry4", label = h4("Select Industry"), 
                                choices = industry_list, 
                                width = "80%",
                                selected = 1),
                    radioButtons("secondtype", label=h4("Select Period Type"),
                                 choices = c("Year"=1, "Year, Quarter"=2),
                                 selected=1),
                    conditionalPanel(
                      condition = "input.secondtype==1",
                      selectInput("Year3", label=h4("Select First Year \nto Compare"),
                                  choices = year_lst,
                                  width = '40%',
                                  selected=1),
                      selectInput("Year4", label=h4("Select Second Year \nto Compare"),
                                  choices = year_lst,
                                  width = "40%",
                                  selected=1),
                    ),
                    conditionalPanel(
                      condition = "input.secondtype==2",
                      selectInput("Quarter1", label=h4("Select First Year & Quarter to Compare"),
                                  choices = yr_qtr_lst,
                                  width = '40%',
                                  selected=1),
                      selectInput("Quarter2", label=h4("Select Second Year & Quarter to Compare"),
                                  choices = yr_qtr_lst,
                                  width = "40%",
                                  selected=1)
                    )
                    
                  ),
                  
                  conditionalPanel(
                    condition = "input.type ==3",
                    
                    h3("Compare between Industries"),
                    selectInput("Industry5", label = h4("Select First Industry"), 
                                choices = industry_list,
                                width = "80%",
                                selected = 1),
                    selectInput("Industry6", label = h4("Select Second Industry"), 
                                choices = industry_list, 
                                width = "80%",
                                selected = 2),
                    selectInput("Year5", label = h4("Select Year"),
                                choices = year_lst,
                                width = "40%",
                                selected=1),
                    
                    h3("Compare between Time Periods"),
                    selectInput("Industry7", label = h4("Select Industry"), 
                                choices = industry_list, 
                                width = "80%",
                                selected = 1),
                    radioButtons("secondtypeboth", label=h4("Select Period Type"),
                                 choices = c("Year"=1, "Year, Quarter"=2),
                                 selected=1),
                    conditionalPanel(
                      condition = "input.secondtypeboth==1",
                      selectInput("Year6", label=h4("Select First Year \nto Compare"),
                                  choices = year_lst,
                                  width = '40%',
                                  selected=1),
                      selectInput("Year7", label=h4("Select Second Year \nto Compare"),
                                  choices = year_lst,
                                  width = "40%",
                                  selected=1)
                    ),
                    conditionalPanel(
                      condition = "input.secondtypeboth==2",
                      selectInput("Quarter3", label=h4("Select First Year & Quarter \nto Compare"),
                                  choices = yr_qtr_lst,
                                  width = '40%',
                                  selected=1),
                      selectInput("Quarter4", label=h4("Select Second Year & Quarter \nto Compare"),
                                  choices = yr_qtr_lst,
                                  width = "40%",
                                  selected=1)
                    )
                    
                  )
                ),
                box(
                  conditionalPanel(
                    condition ="input.type== 1",
                    girafeOutput("comp_industry"),
                    br(),
                  ),
                  conditionalPanel(
                    condition = "input.type==2 && input.secondtype==1",
                    girafeOutput("time_period_year"),
                    br()
                  ),
                  conditionalPanel(
                    condition = "input.type==2 && input.secondtype==2",
                    girafeOutput("time_period_quarter"),
                    br()
                  ),
                  conditionalPanel(
                    condition = "input.type==3",
                    girafeOutput("comp_industry_both"),
                    br()
                  ),
                  conditionalPanel(
                    condition = "input.type==3 && input.secondtypeboth==1",
                    girafeOutput("time_period_year_both")
                  ),
                  conditionalPanel(
                    condition = "input.type==3 && input.secondtypeboth==2",
                    girafeOutput("time_period_quarter_both")
                  ),
                width=7)
              )
            ),
      
      
      # Third tab - Comparing wages across industries and levels of occupation
      tabItem(tabName = "compare_wages",
              h1("Compare Wages of Jobs Across Different Industries"),
              
              
              fluidRow(
                
                sidebarPanel(
                  selectInput("IndustryWage", label = h4("Select Industry"), 
                              choices = wage_industry_list, 
                              width = "80%",
                              selected = 1),
                  selectInput("WageType", label = h4("Select Basic Wage or Gross Wage"), 
                              choices = c('Basic' = 'basic', 'Gross' = 'gross'), 
                              width = "80%",
                              selected = 1),
                  radioButtons("WageYear", label = h4("Select Year"),
                               choices = c("2019"=2019, "2020" =2020),
                               width = "40%",
                               selected=2019),
                  
                  ################################# 2019 #####################################
                  conditionalPanel(
                    condition = "input.IndustryWage == 'accommodation and food services' && input.WageYear==2019",
                    selectInput("AFS2019Level", label = h4("Select Category"),
                                choices = hm2019[['accommodation and food services']],
                                selected=1)
                  ),
                  conditionalPanel(
                    condition = "input.IndustryWage == 'art, entertainment, recreation and other services' && input.WageYear==2019",
                    selectInput("AER2019Level", label = h4("Select Category"),
                                choices = hm2019[['art, entertainment, recreation and other services']],
                                selected=1)
                  ),
                  conditionalPanel(
                    condition = "input.IndustryWage == 'construction' && input.WageYear==2019",
                    selectInput("CO2019Level", label = h4("Select Category"),
                                choices = hm2019[['construction']],
                                selected=1)
                  ),
                  conditionalPanel(
                    condition = "input.IndustryWage == 'education, health and social services' && input.WageYear==2019",
                    selectInput("EHS2019Level", label = h4("Select category"),
                                choices = hm2019[['education, health and social services']],
                                selected=1)
                  ),
                  conditionalPanel(
                    condition = "input.IndustryWage == 'financial and insurance services' && input.WageYear==2019",
                    selectInput("FI2019Level", label = h4("Select Category"),
                                choices = hm2019[['financial and insurance services']],
                                selected=1)
                  ),
                  conditionalPanel(
                    condition = "input.IndustryWage == 'information and communications' && input.WageYear==2019",
                    selectInput("IT2019Level", label = h4("Select Category"),
                                choices = hm2019[['information and communications']],
                                selected=1)
                  ),
                  conditionalPanel(
                    condition = "input.IndustryWage == 'manufacturing' && input.WageYear==2019",
                    selectInput("MAN2019Level", label = h4("Select Category"),
                                choices = hm2019[['manufacturing']],
                                selected=1)
                  ),
                  conditionalPanel(
                    condition = "input.IndustryWage == 'transportation and storage' && input.WageYear==2019",
                    selectInput("TS2019Level", label = h4("Select Category"),
                                choices = hm2019[['transportation and storage']],
                                selected=1)
                  ),
                  conditionalPanel(
                    condition = "input.IndustryWage == 'wholesale and retail trade' && input.WageYear==2019",
                    selectInput("WRT2019Level", label = h4("Select Category"),
                                choices = hm2019[['wholesale and retail trade']],
                                selected=1)
                  ),
                  
                  ######################################  2020 #############################################
                  
                  conditionalPanel(
                    condition = "input.IndustryWage == 'accommodation and food services' && input.WageYear==2020",
                    selectInput("AFS2020Level", label = h4("Select Category"),
                                choices = hm2020[['accommodation and food services']],
                                selected=1)
                  ),
                  conditionalPanel(
                    condition = "input.IndustryWage == 'art, entertainment, recreation and other services' && input.WageYear==2020",
                    selectInput("AER2020Level", label = h4("Select Category"),
                                choices = hm2020[['art, entertainment, recreation and other services']],
                                selected=1)
                  ),
                  conditionalPanel(
                    condition = "input.IndustryWage == 'construction' && input.WageYear==2020",
                    selectInput("CO2020Level", label = h4("Select Category"),
                                choices = hm2020[['construction']],
                                selected=1)
                  ),
                  conditionalPanel(
                    condition = "input.IndustryWage == 'education, health and social services' && input.WageYear==2020",
                    selectInput("EHS2020Level", label = h4("Select Category"),
                                choices = hm2020[['education, health and social services']],
                                selected=1)
                  ),
                  conditionalPanel(
                    condition = "input.IndustryWage == 'financial and insurance services' && input.WageYear==2020",
                    selectInput("FI2020Level", label = h4("Select Category"),
                                choices = hm2020[['financial and insurance services']],
                                selected=1)
                  ),
                  conditionalPanel(
                    condition = "input.IndustryWage == 'information and communications' && input.WageYear==2020",
                    selectInput("IT2020Level", label = h4("Select Category"),
                                choices = hm2020[['information and communications']],
                                selected=1)
                  ),
                  conditionalPanel(
                    condition = "input.IndustryWage == 'manufacturing' && input.WageYear==2020",
                    selectInput("MAN2020Level", label = h4("Select Category"),
                                choices = hm2020[['manufacturing']],
                                selected=1)
                  ),
                  conditionalPanel(
                    condition = "input.IndustryWage == 'transportation and storage' && input.WageYear==2020",
                    selectInput("TS2020Level", label = h4("Select Category"),
                                choices = hm2020[['transportation and storage']],
                                selected=1)
                  ),
                  conditionalPanel(
                    condition = "input.IndustryWage == 'wholesale and retail trade' && input.WageYear==2020",
                    selectInput("WRT2020Level", label = h4("Select Category"),
                                choices = hm2020[['wholesale and retail trade']],
                                selected=1)
                  )
                ),
                
                box(title = "Breakdown of Industry with Level Selected",
                    plotlyOutput("wage_industry"),
                    width = 8)
                ),
              fluidRow(
                column(12,offset=4,
                box(title ="Breakdown of Industry with Category Selected",
                  
                  #################################2019 #####################################
                  conditionalPanel(
                    condition = "input.IndustryWage == 'accommodation and food services' && input.WageYear==2019",
                    plotlyOutput("wage_AFS2019Level")
                  ),
                  conditionalPanel(
                    condition = "input.IndustryWage == 'art, entertainment, recreation and other services' && input.WageYear==2019",
                    plotlyOutput("wage_AER2019Level")
                  ),
                  conditionalPanel(
                    condition = "input.IndustryWage == 'construction' && input.WageYear==2019",
                    plotlyOutput("wage_CO2019Level")
                  ),
                  conditionalPanel(
                    condition = "input.IndustryWage == 'education, health and social services' && input.WageYear==2019",
                    plotlyOutput("wage_EHS2019Level")
                  ),
                  conditionalPanel(
                    condition = "input.IndustryWage == 'financial and insurance services' && input.WageYear==2019",
                    plotlyOutput("wage_FI2019Level")
                  ),
                  conditionalPanel(
                    condition = "input.IndustryWage == 'information and communications' && input.WageYear==2019",
                    plotlyOutput("wage_IT2019Level")
                  ),
                  conditionalPanel(
                    condition = "input.IndustryWage == 'manufacturing' && input.WageYear==2019",
                    plotlyOutput("wage_MAN2019Level")
                  ),
                  conditionalPanel(
                    condition = "input.IndustryWage == 'transportation and storage' && input.WageYear==2019",
                    plotlyOutput("wage_TS2019Level")
                  ),
                  conditionalPanel(
                    condition = "input.IndustryWage == 'wholesale and retail trade' && input.WageYear==2019",
                    plotlyOutput("wage_WRT2019Level")
                  ),
                  
                  ######################################  2020 #############################################
                  conditionalPanel(
                    condition = "input.IndustryWage == 'accommodation and food services' && input.WageYear==2020",
                    plotlyOutput("wage_AFS2020Level")
                  ),
                  conditionalPanel(
                    condition = "input.IndustryWage == 'art, entertainment, recreation and other services' && input.WageYear==2020",
                    plotlyOutput("wage_AER2020Level")
                  ),
                  conditionalPanel(
                    condition = "input.IndustryWage == 'construction' && input.WageYear==2020",
                    plotlyOutput("wage_CO2020Level")
                  ),
                  conditionalPanel(
                    condition = "input.IndustryWage == 'education, health and social services' && input.WageYear==2020",
                    plotlyOutput("wage_EHS2020Level")
                  ),
                  conditionalPanel(
                    condition = "input.IndustryWage == 'financial and insurance services' && input.WageYear==2020",
                    plotlyOutput("wage_FI2020Level")
                  ),
                  conditionalPanel(
                    condition = "input.IndustryWage == 'information and communications' && input.WageYear==2020",
                    plotlyOutput("wage_IT2020Level")
                  ),
                  conditionalPanel(
                    condition = "input.IndustryWage == 'manufacturing' && input.WageYear==2020",
                    plotlyOutput("wage_MAN2020Level")
                  ),
                  conditionalPanel(
                    condition = "input.IndustryWage == 'transportation and storage' && input.WageYear==2020",
                    plotlyOutput("wage_TS2020Level")
                  ),
                  conditionalPanel(
                    condition = "input.IndustryWage == 'wholesale and retail trade' && input.WageYear==2020",
                    plotlyOutput("wage_WRT2020Level")
                  ),
                  width=8)
                )
              )
      ),
      # 4th TAB - JOB SEARCH
      tabItem(tabName = "search_jobs",
              h1("Job Search and Analysis"),
              
              
              fluidRow(
                
                box(
                  selectInput("search_industry", label = h3("Select Your Preferred Industry"), 
                              choices = unique(jobdata$industry),
                              width = "80%",
                              selected = 1),
                  br(),
                  
                  selectizeInput("select_job", label="Enter or Choose a Job", 
                                 choices = NULL, multiple = T
                                 
                  ), width=5),
                
                
                box(title = "General Locations of Jobs Selected", leafletOutput("job_search_location"), width=7)),
              
              
              fluidRow(
                tabBox(
                  tabPanel(title = "Career Levels", plotOutput("job_search_career_level")),
                  tabPanel(title = "Qualifications", plotlyOutput("job_search_qualifications")),
                  tabPanel(title = "Experience Required Based on Job Type", plotlyOutput("job_search_experience"))
                  
                ),
                box(title = "Range of Salaries", plotlyOutput("job_search_salary"))
              )
      ),
      
      
      
      
      
      
      # 5th TAB - COMPARE JOBS
      tabItem(tabName = "compare_jobs",
              h1("Compare Job Specifications"),
              
              
              fluidRow(
                
                box(
                  radioButtons("criteria", label = h3("Sort by:"),
                               choices = list("In Any Order" = "In Any Order", "Highest Salary" = "Highest Salary", "Nearest Location" = "Nearest Location"), 
                               selected = 1),
                  
                  conditionalPanel(
                    condition ="input.criteria == 'Nearest Location'",
                    textInput("postal", label="Enter Your Postal Code", value ="")),
                  
                  textInput("compare_role1", label = "Enter the First Job Link", value = ""),
                  textInput("compare_role2", label = "Enter the Second Job Link", value = ""),
                  
                  br(),
                  actionButton("compare", label = "Compare"), width=4),
                
                box(title="Search Results",
                    
                    conditionalPanel(
                      condition ="input.criteria == 'In Any Order'",
                      dataTableOutput("show_all"), style="overflow-y: scroll;overflow-x: scroll;"),
                    
                    conditionalPanel(
                      condition ="input.criteria == 'Highest Salary'",
                      dataTableOutput("show_salary"), style="overflow-y: scroll;overflow-x: scroll;"),
                    
                    conditionalPanel(
                      condition ="input.criteria == 'Nearest Location'",
                      dataTableOutput("show_location"), style="overflow-y: scroll;overflow-x: scroll;"),
                    
                    width=8)),
              
              
              fluidRow(
                tabBox(
                  # Career Level, Experience/Job Type
                  tabPanel(title = "Career Level", plotOutput("compare_career_level")),
                  tabPanel(title = "Qualifications", plotOutput("compare_qualifications")),
                  tabPanel(title = "Experience Required", plotlyOutput("compare_experience"))
                  
                ),
                tabBox(
                  # Salary and Locations
                  tabPanel(title = "Salary Range of Jobs Selected", plotlyOutput("compare_salary")),
                  tabPanel(title = "Locations of Jobs Selected", leafletOutput("compare_location"))
                  
                )
              )
      )
      
    )
  )
)









