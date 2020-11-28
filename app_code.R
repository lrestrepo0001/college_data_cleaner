#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(ggplot2)
library(dplyr)
library(shinyWidgets)
library(RColorBrewer)
library(grid)
library(DT)
library(Hmisc)

D1 <- read.csv("Interpretter.csv")
colnames(D1)[which(colnames(D1)=="rr_weight")] = "lrr_weight"
colnames(D1)[which(colnames(D1)=="earnings_weight")] = "nep_weight"

callback <- '$("td.dataTables_empty").css("text-align", "left");'

datatable(D1, callback = JS(callback))

D1$Fail = NA
reject_list = c("ROTC",
                "Basic skills",
                "Citizenship Act",
                "Interpersonal And Social Skills",
                "Leisure Act",
                "Self-Improvement",
                "HS Diplomas",
                "Residency")

'%notin%' = Negate('%in%')

x_var_name = c("Loan Repayment Rate")
x_Var_descrip = c("The rate at which a former student of a program pays off their debt")
x_var_tab = cbind(x_var_name, x_Var_descrip)
colnames(x_var_tab) = c("Var_name", "Var_descrip")
y_var_name = c("Net Earnings Premium: Main",
               "Net Earnings Premium: Main, NOOP",
               "Net Earnings Premium: Median High School",
               "Net Earnings Premium: Median High School, NOOP",
               "Net Earnings Premium: 75th Percentile",
               "Net Earnings Premium: 75th Percentile, NOOP")
y_var_descrip = c("The net earnings of a former student",
                  "The net earnings of a former student before accounting for out pocket costs",
                  "The net earnings of a former student relative to the median earnings of a high school graduate",
                  "The net earnings of a former student relative to the median earnings of a high school graduate before accounting for out of pocket expenses",
                  "The net earnings of a former student relative to the earnings of a high school graduate at the 75th percentile",
                  "The net earnings of a former student relative to the earnings of a high school graduate at the 75th percentile before accounting for out of pocket expenses")
y_var_tab = cbind(y_var_name, y_var_descrip)
colnames(y_var_tab) = c("Var_name", "Var_descrip")
var_table = data.frame(rbind(x_var_tab, y_var_tab))

D1 = D1 %>%
  dplyr::filter(Prog %notin% reject_list)
D1$Prog = factor(D1$Prog)
colnames(D1)[which(colnames(D1)=="Repay_rate")] = "Loan Repayment Rate"
colnames(D1)[which(colnames(D1)=="nep_main")] = "Net Earnings Premium: Main"
colnames(D1)[which(colnames(D1)=="nep_main_noop")] = "Net Earnings Premium: Main, NOOP"
colnames(D1)[which(colnames(D1)=="nep_hsp75")] = "Net Earnings Premium: 75th Percentile"
colnames(D1)[which(colnames(D1)=="nep_hsp75_noop")] = "Net Earnings Premium: 75th Percentile, NOOP"
colnames(D1)[which(colnames(D1)=="nep_hs")] =  "Net Earnings Premium: Median High School"
colnames(D1)[which(colnames(D1)=="nep_hs_noop")] = "Net Earnings Premium: Median High School, NOOP"

colnames(D1)[which(colnames(D1)=="School_type")] = "School Type"
colnames(D1)[which(colnames(D1)=="Prog")] = "Program"

D1$student_deg = as.character(D1$student_deg)

old_deg = c("Cert", "AA", "BA", "FP", "GradCert", "MA", "PhD")
new_deg = c("Undergraduate Certificate", "Associate Degree", "Bachelor's Degree",
            "First Professional Degree", "Graduate Certificate", "Master's Degree", "Doctoral Degree")
for(i in 1:length(old_deg)){
  D1$student_deg[which(D1$student_deg==old_deg[i])] = new_deg[i]
}


D1$student_deg = factor(D1$student_deg, levels = c("Undergraduate Certificate", "Associate Degree", "Bachelor's Degree",
                                                   "First Professional Degree", "Graduate Certificate", "Master's Degree",
                                                   "Doctoral Degree"))

qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
col_vector = col_vector[1:length(levels(D1$Program))]
names(col_vector) = levels(D1$Program)
colScale = scale_colour_manual(name = "Program",values = col_vector)

# Determines whether something is an outlier over the 95th quantile
outlier_lims <- function(arg,weight){
  r = if(all(is.na(weight))){
    Hmisc::wtd.quantile(arg, probs = c(.05, .95), na.rm = TRUE)
  }else{
    Hmisc::wtd.quantile(arg, weights = weight, probs = c(.05, .95), na.rm = TRUE)
  }
  l = r[1]
  u = r[2]
  z = NULL
  for (i in 1:length(arg)){
    z[i] <- ifelse(arg[i]>u|arg[i]<l, 1, 0)
  }
  return(z)
}

f2 <- function(x,y1) {
  r = if(all(is.na(weight))){
    Hmisc::wtd.quantile(arg, probs = c(.05, .95), na.rm = TRUE)
  }else{
    Hmisc::wtd.quantile(arg, weights = weight, probs = c(.05, .95), na.rm = TRUE)
  }
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  return(r)
}

formatter1 <- function(x){
  sprintf("%.2f", x)
}

formatter2 <- function(x){
  sprintf("%1.0f", x)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Accounting for Performance in Higher Education"),
  p("This interactive visualization is designed to help stakeholders understand how performance differences across post-secondary institutions, sectors, and programs affect student outcomes.  This beta version depicts variation in program-level performance metrics based on “Towards a Framework for Accountability for Federal Financial Assistance Programs in Postsecondary Education
 (Matsudaira and Turner 2020 (hyperlinked))”, and allows users to explore performance across credential types (CIP codes), sectors,  and credential levels."),
  p("Use the visualization to explore:"),
  p("1) Scatter plots of net earnings premia and repayment rates for different types of programs; or"),
  p("2) Box plots of the distribution of either metric for different types of programs."),
  tabsetPanel(
    tabPanel("Scatter Plot",
             fluidRow(
               column(width = 2,
                      p("User Controls"),
                      selectInput(inputId = "var1",
                                  label = "Metric 1 (X-Axis)",
                                  choices = "Loan Repayment Rate",
                                  selected = "Loan Repayment Rate"),
                      selectInput(inputId = "var2",
                                  label = "Metric 2 (Y-Axis)",
                                  choices = c("Net Earnings Premium: Main",
                                              "Net Earnings Premium: Main, NOOP",
                                              "Net Earnings Premium: 75th Percentile",
                                              "Net Earnings Premium: 75th Percentile, NOOP",
                                              "Net Earnings Premium: Median High School",
                                              "Net Earnings Premium: Median High School, NOOP"),
                                  selected = "Net Earnings"),
                      shinyWidgets::pickerInput(inputId = "filt1",
                                                label = "Program of Study (Max of 10 options)",
                                                multiple = TRUE,
                                                options = list(`action-box` = TRUE,
                                                               `max-options` = 10),
                                                choices = as.character(levels(D1$Program)),
                                                selected = c("CIS", "Business", "Social Sci",
                                                             "Education", "Services")),
                      selectInput(inputId = "filt3",
                                  label = "Credential Level (Select 1)",
                                  choices = as.character(levels(D1$student_deg)),
                                  selected = c(levels(D1$student_deg)[1])),
                      checkboxGroupInput(inputId = "filt2",
                                         label = "School Type",
                                         choices = as.character(levels(D1$`School Type`)),
                                         selected = as.character(levels(D1$`School Type`))),
                      checkboxInput(inputId = "filt7",
                                    label = "Include extreme values",
                                    value = TRUE)),
               column(width = 10,
                      plotOutput("plot1"),
                      fluidRow(
                        column(width = 5,
                               p(" "),
                               tableOutput("plot4")),
                        column(width = 1,
                               tableOutput("plot2"))
                      ))
             )),
    tabPanel("Box Plot",
             fluidRow(
               column(width = 2,
                      selectInput(inputId = "var3",
                                  label = "Y-Axis",
                                  choices = c("Net Earnings Premium: Main",
                                              "Net Earnings Premium: Main, NOOP",
                                              "Net Earnings Premium: 75th Percentile",
                                              "Net Earnings Premium: 75th Percentile, NOOP",
                                              "Net Earnings Premium: Median High School",
                                              "Net Earnings Premium: Median High School, NOOP",
                                              "Loan Repayment Rate"),
                                  selected = "Loan Repayment Rate"),
                      selectInput(inputId = "filt6",
                                  label = "Credential Level",
                                  choices = as.character(levels(D1$student_deg)),
                                  selected = c(levels(D1$student_deg)[1])),
                      shinyWidgets::pickerInput(inputId = "filt4",
                                                label = "Select Max of 10 options",
                                                multiple = TRUE,
                                                options = list(`action-box` = TRUE,
                                                               `max-options` = 10),
                                                choices = as.character(levels(D1$Program)),
                                                selected = c("CIS", "Business", "Social Sci",
                                                             "Education", "Services")),
                      checkboxGroupInput(inputId = "filt5",
                                         label = "School Type",
                                         choices = levels(D1$`School Type`),
                                         selected = c(levels(D1$`School Type`)))),
               column(width = 10,
                      plotOutput("plot3"),
                      fluidRow(
                        column(width = 10, div(style = "height:600px"),
                               "*Outliers have been filtered out of this boxplot. Whiskers extend to the 5th and 95th percentiles respecitvely.")
                      ))
             ))
  )
)

server <- function(input, output) {
  output$plot1 = renderPlot({
    # generate bins based on input$bins from ui.R
    D2 = D1 %>% dplyr::filter(Program %in% input$filt1)
    D2 = D2 %>% dplyr::filter(`School Type` %in% input$filt2)
    D2 = D2 %>% dplyr::filter(student_deg %in% input$filt3)
    D2 = D2 %>%
      group_by(Program, `School Type`, student_deg)%>%
      mutate(x_outlier = outlier_lims(arg = unlist(.data[[input$var1]]), weight = unlist(.data[["lrr_weight"]])),
             y_outlier = outlier_lims(arg = unlist(.data[[input$var2]]), weight = unlist(.data[["nep_weight"]])))
    D2_N = D2[which(D2$x_outlier == 0 & D2$y_outlier == 0),]         # No Outliers
    D2_A = D2[which(D2$x_outlier == 1 & (D2$y_outlier == 1 | D2$y_outlier == 0)),]         # Outlier for only repay rate, with either 1's or 0's
    D2_B = D2[which((D2$x_outlier == 1 | D2$x_outlier == 0) & D2$y_outlier == 1),]         # Outlier for net pay, with either either 1's or 0's
    if(input$filt7==TRUE){
      D2 = rbind(D2_N,D2_A, D2_B)
    }else{
      D2 = D2_N
    }
    D2$Program = as.character(factor(D2$Program))
    options(repr.plot.width = 1, repr.plot.height = 2)
    D2 = as.data.frame(D2)
    plot1 = ggplot(D2)+
      geom_hline(aes(yintercept = 0), linetype = "dashed")+
      geom_vline(aes(xintercept = 0), linetype = "dashed")+
      geom_point(aes(x = D2[,which(colnames(D2)==input$var1)],
                     y = D2[,which(colnames(D2)==input$var2)],
                     color= Program,
                     shape=`School Type`,
      ))+
      colScale+
      xlab(input$var1)+
      ylab(input$var2)+
      scale_y_continuous(labels = scales::comma)+
      scale_shape(solid = FALSE)+
      theme(axis.text = element_text(size = 14),
            legend.text=element_text(size=14),
            axis.title = element_text(size=14),
            legend.position = "right")+
      guides(colour = guide_legend(nrow = 13))
    plot1
  })
  output$plot2 = renderTable(
    height = 100,
    width = 300,
    {
      D2 = D1 %>% dplyr::filter(Program %in% input$filt1)
      D2 = D2 %>% dplyr::filter(`School Type` %in% input$filt2)
      D2 = D2 %>% dplyr::filter(student_deg %in% input$filt3)
      D2$Fail[which(D2[[input$var2]]<0 & D2[[input$var1]]<0)] = 1
      Disp = D2 %>%
        group_by(Program)%>%
        dplyr::summarise("Percent of Enrollment in Programs that are Failing" =
                    round(sum(.data[["both_weight"]][Fail==1], na.rm =TRUE)/sum(.data[["both_weight"]],na.rm = TRUE)*100,
          digits = 2))
      colnames(Disp)[1] = "Program of Study"
      Disp
    })
  
  
  output$plot3 = renderPlot(
    height =1000,
    {if(input$var3=="Loan Repayment Rate"){
      D2 = D1 %>% dplyr::filter(Program %in% input$filt4) %>%
        dplyr::filter(`School Type` %in% input$filt5) %>%
        dplyr::filter(student_deg %in% input$filt6) %>%
        group_by(Program, `School Type`, student_deg) %>%
        mutate(outlier = outlier_lims(arg = .data[[input$var3]], weight = .data[["lrr_weight"]])) %>%
        dplyr::filter(outlier == 0)
      D2 = D2[which(is.na(D2[[input$var3]])==FALSE),]
      D2 = D2[order(D2$Program, D2$student_deg,D2$`School Type`),]
      D3 = D2 %>%
        group_by(student_deg,`School Type`,Program)%>%
        summarise(ymin = wtd.quantile(x = .data[[input$var3]], weights = .data[["lrr_weight"]],
                                      probs = c(0)),
                  lower = wtd.quantile(x = .data[[input$var3]], weights = .data[["lrr_weight"]],
                                       probs = c(0.25)),
                  middle = wtd.quantile(x = .data[[input$var3]], weights = .data[["lrr_weight"]],
                                        probs = c(0.50)),
                  upper = wtd.quantile(x = .data[[input$var3]], weights = .data[["lrr_weight"]],
                                       probs = c(0.75)),
                  ymax = wtd.quantile(x = .data[[input$var3]], weights = .data[["lrr_weight"]],
                                      probs = c(1.00)))
      plot3 = ggplot(data = D3)+
        facet_wrap(~`School Type`,
                   nrow = length(levels(D3$`School Type`)),
                   ncol=1)+
        geom_boxplot(
          stat = "identity",
          aes(x = Program,
              color = Program,
              ymin = ymin,
              ymax = ymax,
              lower = lower,
              upper = upper,
              middle = middle))+
        colScale+
        ylab(input$var3)+
        xlab("Program of Study")+
        scale_y_continuous(labels = formatter1)+
        theme(axis.text = element_text(size = 14),
              legend.text=element_text(size=14),
              axis.title = element_text(size=14),
              strip.text.x = element_text(size = 14),
              legend.position = c(1.5,.5))
    }else{
      D2 = D1 %>% dplyr::filter(Program %in% input$filt4) %>%
        dplyr::filter(`School Type` %in% input$filt5) %>%
        dplyr::filter(student_deg %in% input$filt6) %>%
        group_by(Program, `School Type`, student_deg) %>%
        mutate(outlier = outlier_lims(arg = .data[[input$var3]], weight = .data[["nep_weight"]])) %>%
        dplyr::filter(outlier == 0)
      D2 = D2[which(is.na(D2[[input$var3]])==FALSE),]
      D2 = D2[order(D2$Program, D2$student_deg,D2$`School Type`),]
      D3 = D2 %>%
        group_by(student_deg,`School Type`,Program)%>%
        summarise(ymin = wtd.quantile(x = .data[[input$var3]], weights = .data[["nep_weight"]],
                                      probs = c(0)),
                  lower = wtd.quantile(x = .data[[input$var3]], weights = .data[["nep_weight"]],
                                       probs = c(0.25)),
                  middle = wtd.quantile(x = .data[[input$var3]], weights = .data[["nep_weight"]],
                                        probs = c(0.50)),
                  upper = wtd.quantile(x = .data[[input$var3]], weights = .data[["nep_weight"]],
                                       probs = c(0.75)),
                  ymax = wtd.quantile(x = .data[[input$var3]], weights = .data[["nep_weight"]],
                                      probs = c(1.00)))
      plot3 = ggplot(data = D3)+
        facet_wrap(~`School Type`,
                   nrow = length(levels(D3$`School Type`)),
                   ncol=1)+
        geom_boxplot(
          stat = "identity",
          aes(x = Program,
              ymin = ymin,
              color = Program,
              ymax = ymax,
              lower = lower,
              upper = upper,
              middle = middle))+
        colScale+
        ylab(input$var3)+
        xlab("Program of Study")+
        scale_y_continuous(labels = formatter1)+
        theme(axis.text = element_text(size = 14),
              legend.text=element_text(size=14),
              axis.title = element_text(size=14),
              strip.text.x = element_text(size = 14),
              legend.position = c(1.5,.5))
      
    }
      plot3
    })
  output$plot4 = renderTable({
    selections = c(input$var1, input$var2)
    vars = var_table %>% dplyr::filter(Var_name %in% selections)
    colnames(vars) = c("Variable", "Description")
    vars
  },
  options = list(columnDefs = list(list(width = '10px', targets = c(1,2))
  ))
  )}

shinyApp(ui = ui, server = server)
