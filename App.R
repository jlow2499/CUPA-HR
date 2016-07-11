library(shiny)
library(shinydashboard)library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(ggvis)
library(ggplot2)
library(scales)
library(ggthemes)
library(plotly)
library(DT)

sidebar <- dashboardSidebar(library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(ggvis)
library(ggplot2)
library(scales)
library(ggthemes)
library(plotly)
library(DT)

fields <- c('Question1', 'Question2', "Question3", "Question4", "Question5")

saveData <- function(data) {
  data <- as.data.frame(t(data))
  if (exists("responses")) {
    responses <<- rbind(responses, data)
  } else {
    responses <<- data
  }
}

loadData <- function() {
  if (exists("responses")) {
    responses
  }
}

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Information",tabName="info",icon=icon("dashboard")),
    menuItem("Plot 1",tabName="plot1",
      menuSubItem("Description",tabName="Description"),
      menuSubItem("Plot", tabName = "plotone", icon = icon("line-chart")),
      menuSubItem("Data Table",tabName = "table1",icon=icon("database"))),
    menuItem("Plot 2",tabName = "plot2", 
             menuSubItem("Description",tabName="description2"),
             menuSubItem("Plot",tabName = "plottwo", icon = icon("bar-chart-o")),
             menuSubItem("Data Table",tabName = "table2",icon=icon("database"))),
    menuItem("Plot 3",tabName = "plot3", 
             menuSubItem("Description",tabName="description3"),
             menuSubItem("Plot",tabName = "plotthree",icon = icon("bar-chart-o")),
             menuSubItem("Data Table",tabName = "table3",icon=icon("database"))),
    menuItem("Quick Survey",tabName="survey",icon=icon("tasks"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName="info",
            h1("Powered By:"),tags$img(src="cupa.png",height=600, width =1000),
            h1("      "),
            fluidRow(
              box("This application was made for CUPA-HR by Jeremiah Lowhorn. 
                  It is intended as an interactive platform to explore salary data for academic 
                  institutions across the United States. 
                  The application is coded in R and uses the plotting packages ggvis, ggplot2, and plot.ly. 
                  Click on a menu within the sidebar to the left to expand their submenus."))),
    tabItem(tabName="Description",
            fluidRow(box(6,"Plot one is intended to explore Tenure of the academic staff versus the median salary. 
                                  The plot has three embedded slicers which are: Affiliation, Region, and Gender.
                                  Below the plot there is also a selectize menu to add position types which are: 
                                  Executive Level Officials, 
                                  Mid-Level Officials, 
                                  and Professors."),
                box(6,"The third tab of Plot one is used for an interactive data table. 
                             The data table can be exported as a CSV, 
                             copied to your clipboard, or printed if needed.")),
            fluidRow(
                box(12,"Plot one has three features that make the plot more user friendly than most plotting packages. 
                          The first is the ability to expand the plot to fit the user's screen. 
                          On the bottom right of the plot you will notice a arrow; 
                          click, hold, and drag this arrow to resize the plot. 
                          You may also save the plot once you have edited the slicers to your choosing. 
                          Simply click on the gear in the top right of the plot, 
                          choose SVG or Canvas, and click the download button to save to your computer.
                         The final feature that makes the plot interactive is the ability to hover over the points on the line graph. 
                        Once you hover over these points the data values of the points will pop up in a dialogue box. 
                        This feature will not be incorporated into any downloads.")),
            fluidRow(
            box(title="How to Extend the Plot",tags$img(src="extend.png",height=513, width =651)),
            box(title="How to Save the Plot",tags$img(src="gear.png",height=513, width =651)))),
    tabItem(tabName = "plotone",
            fluidRow(
                            box(width=3,
                                selectInput("filter","Affiliation",
                                            choices=c("Private for-Profit",
                                                          "Private Independent",
                                                          "Private Religious",
                                                          "Public"),
                                            selected=c("Private for-Profit",
                                                          "Private Independent",
                                                          "Private Religious",
                                                          "Public"),
                                            multiple=T,
                                            selectize=T
                                               )
                           ),
                           box(width=3,
                               selectInput("filter2","Region",
                                           choices=levels(adminsalaries$US.Census.Region),
                                           selected=levels(adminsalaries$US.Census.Region),
                                           selectize=T,
                                           multiple=T
                               )    
                               ),
                           box(width=3,
                               selectInput("filter3","Gender",
                                           choices=levels(adminsalaries$gender),
                                           selected=levels(adminsalaries$gender),
                                           selectize=T,
                                           multiple=T))
            
                     ),
          
            ggvisOutput("theplot"),
            uiOutput("plotcontrol")
                 
            
    ),
    tabItem(tabName="table1",
            dataTableOutput("df1")),
    tabItem(tabName="description2",
            fluidRow(box(6,"Plot two is intended to explore NCAA Division by Median Salary faceted by job category. 
                         This plot shows a clear trend with regards to Executive pay in Division 1 salaries for Executives.
                         The median salary is only slightly higher for Mid-Level Officials and Professors."),
            box(6,"The third tab of Plot one is used for an interactive data table. 
                The data table can be exported as a CSV, copied to your clipboard, or printed if needed.")),
            fluidRow(box(6,"Plot two uses ggplot two wrapped in plot.ly which makes it interactive.
                         Additionally there are three slicers at the top of the plot to drill down the data by three different factor variables:
                         Affiliation, Region, and Gender.")),
            fluidRow(box(width=10,title="How to use Plot.ly controls",tags$img(src="plot2.png",height=332.25, width =1236.75)))),
    tabItem(tabName="plottwo",
            fluidRow(
              box(width=3,
                  selectInput("filter4","Affiliation",
                              choices=levels(adminsalaries$Affiliation),
                              selected=levels(adminsalaries$Affiliation),
                              multiple=T,
                              selectize=T
                  )
              ),
              box(width=3,
                  selectInput("filter5","Region",
                              choices=levels(adminsalaries$US.Census.Region),
                              selected=levels(adminsalaries$US.Census.Region),
                              selectize=T,
                              multiple=T
                  )    
              ),
              box(width=3,
                  selectInput("filter6","Gender",
                              choices=levels(adminsalaries$gender),
                              selected=levels(adminsalaries$gender),
                              selectize=T,
                              multiple=T))
              
              
              
              ),
            
            
            fluidRow(
            box(width=12,
            plotlyOutput("trendPlot")
            )
            )
            ),
    tabItem(tabName="table2",
            dataTableOutput("df2")),
    tabItem(tabName="description3",
            fluidRow(box(6,"Plot three was created to allow the user to fully control what plot they are building 
                         from the data set. 
                         The user is able to aggregate the data by choosing four different group variables, 
                         summarizing them by five different summary variables, 
                         and computing the aggregate by either the mean or median. 
                         Plot three could be expanded to include any number of variables or statistical operators 
                         including standard deviation or even a linear model."),
                     box(6,"Simply use the radio buttons at the top of the plot to control what varialbes the plot outputs. 
                         The plot is coded again with ggplot2 embedded with plot.ly.")),
            fluidRow(box(width=10,title="Plot 3 Controls",tags$img(src="plot3.png",height=506.25, width =1254.75)))),
    tabItem(tabName="plotthree",
      fluidRow(
        box(width=3,
            radioButtons("filter7","Group Variable",
                        choices=c("Gender"="gender",
                                  "Ethnicity" = "ethnicity",
                                  "Affiliation" = "Affiliation",
                                  "Region"="US.Census.Region")
            )
        ),
        box(width=3,
            radioButtons("filter8","Summary Variable",
                        choices=c("Salary"="salary",
                                  "Tenure"="years.in.position",
                                  "Operating Expenses"="Operating.expenses",
                                  "Total Students"="Total.Student.FTE",
                                  "Total Faculty"="Total.Faculty.FTE"))
                        ),
        box(width=3,
            radioButtons("filter9","Statistical Operator",
                         choices=c("Mean"="mean",
                                   "Median"="median")))
      ),
      box(width=12,
          plotlyOutput("trendPlot2")
        
        
        
      )
      
      ),
    tabItem(tabName="table3",
            dataTableOutput("df3")),
    tabItem(tabName="survey",
          
           box(width=10,
                   sliderInput(label="The application was informative, and helped the user understand 
                               not only the purpose of the plots, but how to use them.",
                               inputId="Question1",min=1,max=10,value=10,step=1)),
           box(width=10,
               sliderInput(label="The application was enjoyable and a pleasure to use.",
                           inputId="Question2",min=1,max=10,value=10,step=1)),
           box(width=10,
               sliderInput(label="The slicers helped me answer further questions about the data and understand the relationships between the variables.",
                           inputId="Question3",min=1,max=10,value=10,step=1)),
           box(width=10,
               sliderInput(label="I did not encounter any bugs while using this application.",
                           inputId="Question4",min=1,max=10,value=10,step=1)),
           box(width=10,
               sliderInput(label="The plots create were meaningful to the analysis of the data in question.",
                           inputId="Question5",min=1,max=10,value=10,step=1)),
           fluidRow(column(4),
                    column(4,actionButton("submit","Submit")),
                    column(4)),
           fluidRow(
           column(3),           
           infoBoxOutput("approval"))
           
                  

    )
  ))



ui <- dashboardPage(
  dashboardHeader(title="CUPA-HR",
                  dropdownMenuOutput("messageMenu"))
                 ,
  sidebar,
  body,
  shinyjs::useShinyjs()
)

server <- function(input, output,session) {
  
  observe({
    if (input$submit > 0) {
      shinyjs::info("Your submission was recieved, thank you!")
    }
  }) 
  formData <- reactive({
    data <- sapply(fields, function(x) input[[x]])
    data
  })
  observeEvent(input$submit, {
    saveData(formData())
  })
  output$responses <- reactive({ 
    input$submit
    loadData()
  })
  
  results <- reactive({
    if(exists("responses")){
    input$submit
    loadData()
    
    responses <- responses %>%
      mutate(Score = (Question1 + Question2 + Question3 + Question4 + Question5)/50)
    
    text <- mean(responses$Score)}else{
      text <- 1}
    
    
 
      paste(percent(text),"approval rating")
   
  })
  
  


output$approval <- renderInfoBox({
  infoBox(title="Results from the survey",value=results())
})

  string1 <- reactive({
    c(input$filter[1],
      input$filter[2],
      input$filter[3],
      input$filter[4])
  })
  
  string2 <- reactive({
    c(input$filter2[1],
      input$filter2[2],
      input$filter2[3],
      input$filter2[4])
  })
  
  string3 <- reactive({
    c(input$filter3[1],
      input$filter3[2],
      input$filter3[3],
      input$filter3[4])
  })
  
  string4 <- reactive({
    c(input$filter4[1],
      input$filter4[2],
      input$filter4[3],
      input$filter4[4])
  })
  
  string5 <- reactive({
    c(input$filter5[1],
      input$filter5[2],
      input$filter5[3],
      input$filter5[4])
  })
  
  string6 <- reactive({
    c(input$filter6[1],
      input$filter6[2],
      input$filter6[3],
      input$filter6[4])
  })
  
 
  plot1 <- reactive({
    a <- adminsalaries[adminsalaries$Affiliation %in% string1(),]
    b <- a[a$US.Census.Region %in% string2(),]
    c <- b[b$gender %in% string3(),]
    
    tenure <- c %>%
      group_by(years.in.position,VETS.100.Category) %>%
      summarize(Mean_Salary = median(salary))
    tenure$VETS.100.Category <- as.factor(tenure$VETS.100.Category)
    
    tenure <- plyr::rename(tenure,c("years.in.position"="Tenure"))
    
    tenure <- as.data.frame(tenure)
    tenure <- if(nrow(tenure)==0){
      years}else{
        tenure
      }
    
 
    tenure
    
  })
    
 
  observe({
 if(is.null(input$filter)||is.null(input$filter2)||is.null(input$filter3)){
   
   shinyjs::info("The dataframe is empty, please do not delete all inputs!!!")
   shinyjs::reset("filter")
   shinyjs::reset("filter2")
   shinyjs::reset("filter3")
      
    }else{
  plot1 %>%
    ggvis(~Tenure,~Mean_Salary,stroke=~VETS.100.Category) %>%
    filter(VETS.100.Category %in% eval(input_select(c("Exec/Sr Level Officials",
                                                      "First/Mid Level Officials",
                                                      "Prof"),
                                                    selected="Exec/Sr Level Officials",
                                                    multiple=T,
                                                    selectize=T))) %>%
    layer_points() %>%
    add_tooltip(function(data){
      paste0("Median Salary: ", scales::dollar(data$Mean_Salary), 
      "<br>", "Tenure: ",paste(data$Tenure,"Years"))
      }, "hover")%>%
    layer_lines() %>%
    add_axis('y', title='Median Salary',
             properties=axis_props(labels=list(fontSize=12), 
                                   title=list(fontSize=16,dy=-35))) %>%
    add_axis("x",title="Tenure",
             properties=axis_props(labels=list(fontSize=12), 
                                   title=list(fontSize=16))) %>%
    bind_shiny("theplot",controls_id="plotcontrol")}

 
 })
 
 
 plot2 <- reactive({
   a <- adminsalaries[adminsalaries$Affiliation %in% string4(),]
   b <- a[a$US.Census.Region %in% string5(),]
   c <- b[b$gender %in% string6(),]
   
   plot<- c %>%
     group_by(NCAA.Division,VETS.100.Category) %>%
     summarize(Median_Salary = median(salary))
   plot
   
 })
 
 output$trendPlot <- renderPlotly({
   p <- ggplot(plot2(),aes(x=NCAA.Division,
                                 y=Median_Salary,
                                 fill=VETS.100.Category,
                                 group=VETS.100.Category)) 
   p <- p + geom_bar(stat = "identity", 
                     width = .75, 
                     position = "dodge",
                     color="black") 
  # p <- p + geom_text(aes(label=scales::dollar(Median_Salary)), 
   #                   vjust=-.3, 
    #                  color="black", 
     #                 size=3.5)
   p <- p + facet_grid(. ~ VETS.100.Category)
   p <- p + scale_y_continuous(labels=scales::dollar) 
   p <- p + theme(axis.text.x = element_text(angle = 30)) 
   p <- p + ylab("    ") 
   p <- p + xlab("NCAA Division") 
   p <- p + theme(axis.title=element_text(size=8,color="black")) 
   p <- p + theme(axis.text=element_text(size=8,color="black")) 
   p <- p + theme_igray() + scale_colour_tableau()
   p <- p + scale_fill_discrete(name="VETS 100 Category") 
   
   plot <- ggplotly(p)
 plot
 })
 
  plot3 <- reactive({
   data <- adminsalaries %>%
     group_by(if(input$filter7=="gender"){
       gender}
       else{
         if(input$filter7=="ethnicity"){
           ethnicity}
         else{
           if(input$filter7=="Affiliation"){
             Affiliation
           }else{
             US.Census.Region
           }
         }               
         }

       ) %>%
     summarize(Aggregate = ifelse(input$filter8 == "salary" && input$filter9 == "mean",mean(salary),
                           ifelse(input$filter8 == "salary" && input$filter9 == "median",median(salary),
                           ifelse(input$filter8 == "years.in.position" && input$filter9 =="mean",mean(years.in.position),
                           ifelse(input$filter8 == "years.in.position" && input$filter9 =="median",median(years.in.position),
                           ifelse(input$filter8 == "Operating.expenses" && input$filter9 =="mean",mean(Operating.expenses),
                           ifelse(input$filter8 == "Operating.expenses" && input$filter9 =="median",median(Operating.expenses),
                           ifelse(input$filter8 == "Total.Student.FTE" && input$filter9 =="mean",mean(Total.Student.FTE),
                           ifelse(input$filter8 == "Total.Student.FTE" && input$filter9 =="median",median(Total.Student.FTE),
                           ifelse(input$filter8 == "Total.Faculty.FTE" && input$filter9 =="mean",mean(Total.Faculty.FTE),
                                  median(Total.Faculty.FTE)
                           ))))))))))
                         
                                         
                                       
   colnames(data)[1] <- "Group_Variable"
   data
   
 })
 
 output$trendPlot2 <- renderPlotly({
   p <- ggplot(plot3(),aes(x=Group_Variable,
                           y=Aggregate)) 
   p <- p + geom_bar(stat = "identity", 
                     width = .75, 
                     color="black") 
   p <- p + theme_igray() + scale_colour_tableau()
   p <- p + xlab("    ") 
   p <- p + ylab("    ")
   p <- p + scale_y_continuous(labels=scales::comma) 
   
   plot <- ggplotly(p)
   plot
 })
 
 output$df1 <- DT::renderDataTable({
   
    t <- datatable(plot1(),extensions = 'TableTools', rownames=FALSE,class = 'cell-border stripe',filter="top",
                   options = list(
                     searching=TRUE,
                     autoWidth=TRUE,
                     paging=FALSE,
                     "sDom" = 'T<"clear">lfrtip',
                     "oTableTools" = list(
                       "sSwfPath" = "//cdnjs.cloudflare.com/ajax/libs/datatables-tabletools/2.1.5/swf/copy_csv_xls.swf",
                       "aButtons" = list(
                         "copy",
                         "print",
                         list("sExtends" = "collection",
                              "sButtonText" = "Save",
                              "aButtons" = c("csv","xls"))))))
    t
   
 })
 
 output$df2 <- DT::renderDataTable({
   
   t <- datatable(plot2(),extensions = 'TableTools', rownames=FALSE,class = 'cell-border stripe',filter="top",
                  options = list(
                    searching=TRUE,
                    autoWidth=TRUE,
                    paging=FALSE,
                    "sDom" = 'T<"clear">lfrtip',
                    "oTableTools" = list(
                      "sSwfPath" = "//cdnjs.cloudflare.com/ajax/libs/datatables-tabletools/2.1.5/swf/copy_csv_xls.swf",
                      "aButtons" = list(
                        "copy",
                        "print",
                        list("sExtends" = "collection",
                             "sButtonText" = "Save",
                             "aButtons" = c("csv","xls"))))))
   t
   
 })
 
 output$df3 <- DT::renderDataTable({
   
   t <- datatable(plot3(),extensions = 'TableTools', rownames=FALSE,class = 'cell-border stripe',filter="top",
                  options = list(
                    searching=TRUE,
                    autoWidth=TRUE,
                    paging=FALSE,
                    "sDom" = 'T<"clear">lfrtip',
                    "oTableTools" = list(
                      "sSwfPath" = "//cdnjs.cloudflare.com/ajax/libs/datatables-tabletools/2.1.5/swf/copy_csv_xls.swf",
                      "aButtons" = list(
                        "copy",
                        "print",
                        list("sExtends" = "collection",
                             "sButtonText" = "Save",
                             "aButtons" = c("csv","xls"))))))
   t
   
 })
  
}
shinyApp(ui, server)
  sidebarMenu(
    menuItem("Information",tabName="info",icon=icon("dashboard")),
    menuItem("Plot 1",tabName="plot1",
      menuSubItem("Description",tabName="Description"),
      menuSubItem("Plot", tabName = "plotone", icon = icon("line-chart")),
      menuSubItem("Data Table",tabName = "table1",icon=icon("database"))),
    menuItem("Plot 2",tabName = "plot2", 
             menuSubItem("Description",tabName="description2"),
             menuSubItem("Plot",tabName = "plottwo", icon = icon("bar-chart-o")),
             menuSubItem("Data Table",tabName = "table2",icon=icon("database"))),
    menuItem("Plot 3",tabName = "plot3", 
             menuSubItem("Description",tabName="description3"),
             menuSubItem("Plot",tabName = "plotthree",icon = icon("bar-chart-o")),
             menuSubItem("Data Table",tabName = "table3",icon=icon("database")))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName="info",
            h1("Powered By:"),tags$img(src="cupa.png",height=600, width =1000),
            h1("      "),
            fluidRow(
              box("This application was made for CUPA-HR by Jeremiah Lowhorn. 
                  It is intended as an interactive platform to explore salary data for academic 
                  institutions across the United States. 
                  The application is coded in R and uses the plotting packages ggvis, ggplot2, and plot.ly. 
                  Click on a menu within the sidebar to the left to expand their submenus."))),
    tabItem(tabName="Description",
            fluidRow(box(6,"Plot one is intended to explore Tenure of the academic staff versus the median salary. 
                                  The plot has three embedded slicers which are: Affiliation, Region, and Gender.
                                  Below the plot there is also a selectize menu to add position types which are: 
                                  Executive Level Officials, 
                                  Mid-Level Officials, 
                                  and Professors."),
                box(6,"The third tab of Plot one is used for an interactive data table. 
                             The data table can be exported as a CSV, 
                             copied to your clipboard, or printed if needed.")),
            fluidRow(
                box(12,"Plot one has three features that make the plot more user friendly than most plotting packages. 
                          The first is the ability to expand the plot to fit the user's screen. 
                          On the bottom right of the plot you will notice a arrow; 
                          click, hold, and drag this arrow to resize the plot. 
                          You may also save the plot once you have edited the slicers to your choosing. 
                          Simply click on the gear in the top right of the plot, 
                          choose SVG or Canvas, and click the download button to save to your computer.
                         The final feature that makes the plot interactive is the ability to hover over the points on the line graph. 
                        Once you hover over these points the data values of the points will pop up in a dialogue box. 
                        This feature will not be incorporated into any downloads.")),
            fluidRow(
            box(title="How to Extend the Plot",tags$img(src="extend.png",height=513, width =651)),
            box(title="How to Save the Plot",tags$img(src="gear.png",height=513, width =651)))),
    tabItem(tabName = "plotone",
            fluidRow(
                            box(width=3,
                                selectInput("filter","Affiliation",
                                            choices=c("Private for-Profit",
                                                          "Private Independent",
                                                          "Private Religious",
                                                          "Public"),
                                            selected=c("Private for-Profit",
                                                          "Private Independent",
                                                          "Private Religious",
                                                          "Public"),
                                            multiple=T,
                                            selectize=T
                                               )
                           ),
                           box(width=3,
                               selectInput("filter2","Region",
                                           choices=levels(adminsalaries$US.Census.Region),
                                           selected=levels(adminsalaries$US.Census.Region),
                                           selectize=T,
                                           multiple=T
                               )    
                               ),
                           box(width=3,
                               selectInput("filter3","Gender",
                                           choices=levels(adminsalaries$gender),
                                           selected=levels(adminsalaries$gender),
                                           selectize=T,
                                           multiple=T))
            
                     ),
          
            ggvisOutput("theplot"),
            uiOutput("plotcontrol")
                 
            
    ),
    tabItem(tabName="table1",
            dataTableOutput("df1")),
    tabItem(tabName="description2",
            fluidRow(box(6,"Plot two is intended to explore NCAA Division by Median Salary faceted by job category. 
                         This plot shows a clear trend with regards to Executive pay in Division 1 salaries for Executives.
                         The median salary is only slightly higher for Mid-Level Officials and Professors."),
            box(6,"The third tab of Plot one is used for an interactive data table. 
                The data table can be exported as a CSV, copied to your clipboard, or printed if needed.")),
            fluidRow(box(6,"Plot two uses ggplot two wrapped in plot.ly which makes it interactive.
                         Additionally there are three slicers at the top of the plot to drill down the data by three different factor variables:
                         Affiliation, Region, and Gender.")),
            fluidRow(box(width=10,title="How to use Plot.ly controls",tags$img(src="plot2.png",height=332.25, width =1236.75)))),
    tabItem(tabName="plottwo",
            fluidRow(
              box(width=3,
                  selectInput("filter4","Affiliation",
                              choices=levels(adminsalaries$Affiliation),
                              selected=levels(adminsalaries$Affiliation),
                              multiple=T,
                              selectize=T
                  )
              ),
              box(width=3,
                  selectInput("filter5","Region",
                              choices=levels(adminsalaries$US.Census.Region),
                              selected=levels(adminsalaries$US.Census.Region),
                              selectize=T,
                              multiple=T
                  )    
              ),
              box(width=3,
                  selectInput("filter6","Gender",
                              choices=levels(adminsalaries$gender),
                              selected=levels(adminsalaries$gender),
                              selectize=T,
                              multiple=T))
              
              
              
              ),
            
            
            fluidRow(
            box(width=12,
            plotlyOutput("trendPlot")
            )
            )
            ),
    tabItem(tabName="table2",
            dataTableOutput("df2")),
    tabItem(tabName="description3",
            fluidRow(box(6,"Plot three was created to allow the user to fully control what plot they are building 
                         from the data set. 
                         The user is able to aggregate the data by choosing four different group variables, 
                         summarizing them by five different summary variables, 
                         and computing the aggregate by either the mean or median. 
                         Plot three could be expanded to include any number of variables or statistical operators 
                         including standard deviation or even a linear model."),
                     box(6,"Simply use the radio buttons at the top of the plot to control what varialbes the plot outputs. 
                         The plot is coded again with ggplot2 embedded with plot.ly.")),
            fluidRow(box(width=10,title="Plot 3 Controls",tags$img(src="plot3.png",height=506.25, width =1254.75)))),
    tabItem(tabName="plotthree",
      fluidRow(
        box(width=3,
            radioButtons("filter7","Group Variable",
                        choices=c("Gender"="gender",
                                  "Ethnicity" = "ethnicity",
                                  "Affiliation" = "Affiliation",
                                  "Region"="US.Census.Region")
            )
        ),
        box(width=3,
            radioButtons("filter8","Summary Variable",
                        choices=c("Salary"="salary",
                                  "Tenure"="years.in.position",
                                  "Operating Expenses"="Operating.expenses",
                                  "Total Students"="Total.Student.FTE",
                                  "Total Faculty"="Total.Faculty.FTE"))
                        ),
        box(width=3,
            radioButtons("filter9","Statistical Operator",
                         choices=c("Mean"="mean",
                                   "Median"="median")))
      ),
      box(width=12,
          plotlyOutput("trendPlot2")
        
        
        
      )
      
      ),
    tabItem(tabName="table3",
            dataTableOutput("df3"))
  ))



ui <- dashboardPage(
  dashboardHeader(title="CUPA-HR"),
  sidebar,
  body,
  shinyjs::useShinyjs()
)

server <- function(input, output,session) {
  

  string1 <- reactive({
    c(input$filter[1],
      input$filter[2],
      input$filter[3],
      input$filter[4])
  })
  
  string2 <- reactive({
    c(input$filter2[1],
      input$filter2[2],
      input$filter2[3],
      input$filter2[4])
  })
  
  string3 <- reactive({
    c(input$filter3[1],
      input$filter3[2],
      input$filter3[3],
      input$filter3[4])
  })
  
  string4 <- reactive({
    c(input$filter4[1],
      input$filter4[2],
      input$filter4[3],
      input$filter4[4])
  })
  
  string5 <- reactive({
    c(input$filter5[1],
      input$filter5[2],
      input$filter5[3],
      input$filter5[4])
  })
  
  string6 <- reactive({
    c(input$filter6[1],
      input$filter6[2],
      input$filter6[3],
      input$filter6[4])
  })
  
 
  plot1 <- reactive({
    a <- adminsalaries[adminsalaries$Affiliation %in% string1(),]
    b <- a[a$US.Census.Region %in% string2(),]
    c <- b[b$gender %in% string3(),]
    
    tenure <- c %>%
      group_by(years.in.position,VETS.100.Category) %>%
      summarize(Mean_Salary = median(salary))
    tenure$VETS.100.Category <- as.factor(tenure$VETS.100.Category)
    
    tenure <- plyr::rename(tenure,c("years.in.position"="Tenure"))
    
    tenure <- as.data.frame(tenure)
    tenure <- if(nrow(tenure)==0){
      years}else{
        tenure
      }
    
 
    tenure
    
  })
    
 
  observe({
 if(is.null(input$filter)||is.null(input$filter2)||is.null(input$filter3)){
   
   shinyjs::info("The dataframe is empty, please do not delete all inputs!!!")
   shinyjs::reset("filter")
   shinyjs::reset("filter2")
   shinyjs::reset("filter3")
      
    }else{
  plot1 %>%
    ggvis(~Tenure,~Mean_Salary,stroke=~VETS.100.Category) %>%
    filter(VETS.100.Category %in% eval(input_select(c("Exec/Sr Level Officials",
                                                      "First/Mid Level Officials",
                                                      "Prof"),
                                                    selected="Exec/Sr Level Officials",
                                                    multiple=T,
                                                    selectize=T))) %>%
    layer_points() %>%
    add_tooltip(function(data){
      paste0("Median Salary: ", scales::dollar(data$Mean_Salary), 
      "<br>", "Tenure: ",paste(data$Tenure,"Years"))
      }, "hover")%>%
    layer_lines() %>%
    add_axis('y', title='Median Salary',
             properties=axis_props(labels=list(fontSize=12), 
                                   title=list(fontSize=16,dy=-35))) %>%
    add_axis("x",title="Tenure",
             properties=axis_props(labels=list(fontSize=12), 
                                   title=list(fontSize=16))) %>%
    bind_shiny("theplot",controls_id="plotcontrol")}

 
 })
 
 
 plot2 <- reactive({
   a <- adminsalaries[adminsalaries$Affiliation %in% string4(),]
   b <- a[a$US.Census.Region %in% string5(),]
   c <- b[b$gender %in% string6(),]
   
   plot<- c %>%
     group_by(NCAA.Division,VETS.100.Category) %>%
     summarize(Median_Salary = median(salary))
   plot
   
 })
 
 output$trendPlot <- renderPlotly({
   p <- ggplot(plot2(),aes(x=NCAA.Division,
                                 y=Median_Salary,
                                 fill=VETS.100.Category,
                                 group=VETS.100.Category)) 
   p <- p + geom_bar(stat = "identity", 
                     width = .75, 
                     position = "dodge",
                     color="black") 
  # p <- p + geom_text(aes(label=scales::dollar(Median_Salary)), 
   #                   vjust=-.3, 
    #                  color="black", 
     #                 size=3.5)
   p <- p + facet_grid(. ~ VETS.100.Category)
   p <- p + scale_y_continuous(labels=scales::dollar) 
   p <- p + theme(axis.text.x = element_text(angle = 30)) 
   p <- p + ylab("    ") 
   p <- p + xlab("NCAA Division") 
   p <- p + theme(axis.title=element_text(size=8,color="black")) 
   p <- p + theme(axis.text=element_text(size=8,color="black")) 
   p <- p + theme_igray() + scale_colour_tableau()
   p <- p + scale_fill_discrete(name="VETS 100 Category") 
   
   plot <- ggplotly(p)
 plot
 })
 
  plot3 <- reactive({
   data <- adminsalaries %>%
     group_by(if(input$filter7=="gender"){
       gender}
       else{
         if(input$filter7=="ethnicity"){
           ethnicity}
         else{
           if(input$filter7=="Affiliation"){
             Affiliation
           }else{
             US.Census.Region
           }
         }               
         }

       ) %>%
     summarize(Aggregate = ifelse(input$filter8 == "salary" && input$filter9 == "mean",mean(salary),
                           ifelse(input$filter8 == "salary" && input$filter9 == "median",median(salary),
                           ifelse(input$filter8 == "years.in.position" && input$filter9 =="mean",mean(years.in.position),
                           ifelse(input$filter8 == "years.in.position" && input$filter9 =="median",median(years.in.position),
                           ifelse(input$filter8 == "Operating.expenses" && input$filter9 =="mean",mean(Operating.expenses),
                           ifelse(input$filter8 == "Operating.expenses" && input$filter9 =="median",median(Operating.expenses),
                           ifelse(input$filter8 == "Total.Student.FTE" && input$filter9 =="mean",mean(Total.Student.FTE),
                           ifelse(input$filter8 == "Total.Student.FTE" && input$filter9 =="median",median(Total.Student.FTE),
                           ifelse(input$filter8 == "Total.Faculty.FTE" && input$filter9 =="mean",mean(Total.Faculty.FTE),
                                  median(Total.Faculty.FTE)
                           ))))))))))
                         
                                         
                                       
   colnames(data)[1] <- "Group_Variable"
   data
   
 })
 
 output$trendPlot2 <- renderPlotly({
   p <- ggplot(plot3(),aes(x=Group_Variable,
                           y=Aggregate)) 
   p <- p + geom_bar(stat = "identity", 
                     width = .75, 
                     color="black") 
   p <- p + theme_igray() + scale_colour_tableau()
   p <- p + xlab("    ") 
   p <- p + ylab("    ")
   p <- p + scale_y_continuous(labels=scales::comma) 
   
   plot <- ggplotly(p)
   plot
 })
 
 output$df1 <- DT::renderDataTable({
   
    t <- datatable(plot1(),extensions = 'TableTools', rownames=FALSE,class = 'cell-border stripe',filter="top",
                   options = list(
                     searching=TRUE,
                     autoWidth=TRUE,
                     paging=FALSE,
                     "sDom" = 'T<"clear">lfrtip',
                     "oTableTools" = list(
                       "sSwfPath" = "//cdnjs.cloudflare.com/ajax/libs/datatables-tabletools/2.1.5/swf/copy_csv_xls.swf",
                       "aButtons" = list(
                         "copy",
                         "print",
                         list("sExtends" = "collection",
                              "sButtonText" = "Save",
                              "aButtons" = c("csv","xls"))))))
    t
   
 })
 
 output$df2 <- DT::renderDataTable({
   
   t <- datatable(plot2(),extensions = 'TableTools', rownames=FALSE,class = 'cell-border stripe',filter="top",
                  options = list(
                    searching=TRUE,
                    autoWidth=TRUE,
                    paging=FALSE,
                    "sDom" = 'T<"clear">lfrtip',
                    "oTableTools" = list(
                      "sSwfPath" = "//cdnjs.cloudflare.com/ajax/libs/datatables-tabletools/2.1.5/swf/copy_csv_xls.swf",
                      "aButtons" = list(
                        "copy",
                        "print",
                        list("sExtends" = "collection",
                             "sButtonText" = "Save",
                             "aButtons" = c("csv","xls"))))))
   t
   
 })
 
 output$df3 <- DT::renderDataTable({
   
   t <- datatable(plot3(),extensions = 'TableTools', rownames=FALSE,class = 'cell-border stripe',filter="top",
                  options = list(
                    searching=TRUE,
                    autoWidth=TRUE,
                    paging=FALSE,
                    "sDom" = 'T<"clear">lfrtip',
                    "oTableTools" = list(
                      "sSwfPath" = "//cdnjs.cloudflare.com/ajax/libs/datatables-tabletools/2.1.5/swf/copy_csv_xls.swf",
                      "aButtons" = list(
                        "copy",
                        "print",
                        list("sExtends" = "collection",
                             "sButtonText" = "Save",
                             "aButtons" = c("csv","xls"))))))
   t
   
 })
  
}
shinyApp(ui, server)
library(DT)
library(dplyr)
library(ggvis)
library(ggplot2)
library(scales)
library(ggthemes)
library(plotly)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Plot1", tabName = "plot1", icon = icon("bar-chart-o")),
    menuItem("Plot2",tabName = "plot2", icon = icon("bar-chart-o")),
    menuItem("Plot3",tabName = "plot3", icon = icon("bar-chart-o"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "plot1",
            fluidRow(
                            box(width=3,
                                selectInput("filter","Affiliation",
                                            choices=c("Private for-Profit",
                                                          "Private Independent",
                                                          "Private Religious",
                                                          "Public"),
                                            selected=c("Private for-Profit",
                                                          "Private Independent",
                                                          "Private Religious",
                                                          "Public"),
                                            multiple=T,
                                            selectize=T
                                               )
                           ),
                           box(width=3,
                               selectInput("filter2","Region",
                                           choices=levels(adminsalaries$US.Census.Region),
                                           selected=levels(adminsalaries$US.Census.Region),
                                           selectize=T,
                                           multiple=T
                               )    
                               ),
                           box(width=3,
                               selectInput("filter3","Gender",
                                           choices=levels(adminsalaries$gender),
                                           selected=levels(adminsalaries$gender),
                                           selectize=T,
                                           multiple=T))
            
                     ),
          
            ggvisOutput("theplot"),
            uiOutput("plotcontrol")
                 
            
    ),
    tabItem(tabName="plot2",
            fluidRow(
              box(width=3,
                  selectInput("filter4","Affiliation",
                              choices=levels(adminsalaries$Affiliation),
                              selected=levels(adminsalaries$Affiliation),
                              multiple=T,
                              selectize=T
                  )
              ),
              box(width=3,
                  selectInput("filter5","Region",
                              choices=levels(adminsalaries$US.Census.Region),
                              selected=levels(adminsalaries$US.Census.Region),
                              selectize=T,
                              multiple=T
                  )    
              ),
              box(width=3,
                  selectInput("filter6","Gender",
                              choices=levels(adminsalaries$gender),
                              selected=levels(adminsalaries$gender),
                              selectize=T,
                              multiple=T))
              
              
              
              ),
            
            
            fluidRow(
            box(width=12,
            plotlyOutput("trendPlot")
            )
            )
            ),
    tabItem(tabName="plot3",
      fluidRow(
        box(width=3,
            radioButtons("filter7","Group Variable",
                        choices=c("Gender"="gender",
                                  "Ethnicity" = "ethnicity",
                                  "Affiliation" = "Affiliation",
                                  "Region"="US.Census.Region")
            )
        ),
        box(width=3,
            radioButtons("filter8","Summary Variable",
                        choices=c("Salary"="salary",
                                  "Tenure"="years.in.position",
                                  "Operating Expenses"="Operating.expenses",
                                  "Total Students"="Total.Student.FTE",
                                  "Total Faculty"="Total.Faculty.FTE"))
                        ),
        box(width=3,
            radioButtons("filter9","Statistical Operator",
                         choices=c("Mean"="mean",
                                   "Median"="median")))
      ),
      box(width=12,
          plotlyOutput("trendPlot2")
        
        
        
      )
      
      )
  ))



ui <- dashboardPage(
  dashboardHeader(title="CUPA-HR"),
  sidebar,
  body,
  shinyjs::useShinyjs()
)

server <- function(input, output,session) {
  
  string1 <- reactive({
    c(input$filter[1],
      input$filter[2],
      input$filter[3],
      input$filter[4])
  })
  
  string2 <- reactive({
    c(input$filter2[1],
      input$filter2[2],
      input$filter2[3],
      input$filter2[4])
  })
  
  string3 <- reactive({
    c(input$filter3[1],
      input$filter3[2],
      input$filter3[3],
      input$filter3[4])
  })
  
  string4 <- reactive({
    c(input$filter4[1],
      input$filter4[2],
      input$filter4[3],
      input$filter4[4])
  })
  
  string5 <- reactive({
    c(input$filter5[1],
      input$filter5[2],
      input$filter5[3],
      input$filter5[4])
  })
  
  string6 <- reactive({
    c(input$filter6[1],
      input$filter6[2],
      input$filter6[3],
      input$filter6[4])
  })
  
 
  plot1 <- reactive({
    a <- adminsalaries[adminsalaries$Affiliation %in% string1(),]
    b <- a[a$US.Census.Region %in% string2(),]
    c <- b[b$gender %in% string3(),]
    
    tenure <- c %>%
      group_by(years.in.position,VETS.100.Category) %>%
      summarize(Mean_Salary = median(salary))
    tenure$VETS.100.Category <- as.factor(tenure$VETS.100.Category)
    
    tenure <- plyr::rename(tenure,c("years.in.position"="Tenure"))
    
    tenure <- as.data.frame(tenure)
    tenure <- if(nrow(tenure)==0){
      years}else{
        tenure
      }
    
 
    tenure
    
  })
    
 
  observe({
 if(is.null(input$filter)||is.null(input$filter2)||is.null(input$filter3)){
   
   shinyjs::info("The dataframe is empty, please do not delete all inputs!!!")
   shinyjs::reset("filter")
   shinyjs::reset("filter2")
   shinyjs::reset("filter3")
      
    }else{
  plot1 %>%
    ggvis(~Tenure,~Mean_Salary,stroke=~VETS.100.Category) %>%
    filter(VETS.100.Category %in% eval(input_select(c("Exec/Sr Level Officials",
                                                      "First/Mid Level Officials",
                                                      "Prof"),
                                                    selected="Exec/Sr Level Officials",
                                                    multiple=T,
                                                    selectize=T))) %>%
    layer_points() %>%
    add_tooltip(function(data){
      paste0("Median Salary: ", scales::dollar(data$Mean_Salary), 
      "<br>", "Tenure: ",paste(data$Tenure,"Years"))
      }, "hover")%>%
    layer_lines() %>%
    add_axis('y', title='Median Salary',
             properties=axis_props(labels=list(fontSize=12), 
                                   title=list(fontSize=16,dy=-35))) %>%
    add_axis("x",title="Tenure",
             properties=axis_props(labels=list(fontSize=12), 
                                   title=list(fontSize=16))) %>%
    bind_shiny("theplot",controls_id="plotcontrol")}

 
 })
 
 
 plot2 <- reactive({
   a <- adminsalaries[adminsalaries$Affiliation %in% string4(),]
   b <- a[a$US.Census.Region %in% string5(),]
   c <- b[b$gender %in% string6(),]
   
   plot<- c %>%
     group_by(NCAA.Division,VETS.100.Category) %>%
     summarize(Median_Salary = median(salary))
   plot
   
 })
 
 output$trendPlot <- renderPlotly({
   p <- ggplot(plot2(),aes(x=NCAA.Division,
                                 y=Median_Salary,
                                 fill=VETS.100.Category,
                                 group=VETS.100.Category)) 
   p <- p + geom_bar(stat = "identity", 
                     width = .75, 
                     position = "dodge",
                     color="black") 
  # p <- p + geom_text(aes(label=scales::dollar(Median_Salary)), 
   #                   vjust=-.3, 
    #                  color="black", 
     #                 size=3.5)
   p <- p + facet_grid(. ~ VETS.100.Category)
   p <- p + scale_y_continuous(labels=scales::dollar) 
   p <- p + theme(axis.text.x = element_text(angle = 30)) 
   p <- p + ylab("    ") 
   p <- p + xlab("NCAA Division") 
   p <- p + theme(axis.title=element_text(size=8,color="black")) 
   p <- p + theme(axis.text=element_text(size=8,color="black")) 
   p <- p + theme_igray() + scale_colour_tableau()
   p <- p + scale_fill_discrete(name="VETS 100 Category") 
   
   plot <- ggplotly(p)
 plot
 })
 
  plot3 <- reactive({
   data <- adminsalaries %>%
     group_by(if(input$filter7=="gender"){
       gender}
       else{
         if(input$filter7=="ethnicity"){
           ethnicity}
         else{
           if(input$filter7=="Affiliation"){
             Affiliation
           }else{
             US.Census.Region
           }
         }               
         }

       ) %>%
     summarize(Aggregate = ifelse(input$filter8 == "salary" && input$filter9 == "mean",mean(salary),
                           ifelse(input$filter8 == "salary" && input$filter9 == "median",median(salary),
                           ifelse(input$filter8 == "years.in.position" && input$filter9 =="mean",mean(years.in.position),
                           ifelse(input$filter8 == "years.in.position" && input$filter9 =="median",median(years.in.position),
                           ifelse(input$filter8 == "Operating.expenses" && input$filter9 =="mean",mean(Operating.expenses),
                           ifelse(input$filter8 == "Operating.expenses" && input$filter9 =="median",median(Operating.expenses),
                           ifelse(input$filter8 == "Total.Student.FTE" && input$filter9 =="mean",mean(Total.Student.FTE),
                           ifelse(input$filter8 == "Total.Student.FTE" && input$filter9 =="median",median(Total.Student.FTE),
                           ifelse(input$filter8 == "Total.Faculty.FTE" && input$filter9 =="mean",mean(Total.Faculty.FTE),
                                  median(Total.Faculty.FTE)
                           ))))))))))
                         
                                         
                                       
   colnames(data)[1] <- "Group_Variable"
   data
   
 })
 
 output$trendPlot2 <- renderPlotly({
   p <- ggplot(plot3(),aes(x=Group_Variable,
                           y=Aggregate)) 
   p <- p + geom_bar(stat = "identity", 
                     width = .75, 
                     color="black") 
   p <- p + theme_igray() + scale_colour_tableau()
   p <- p + xlab("    ") 
   p <- p + ylab("    ")
   p <- p + scale_y_continuous(labels=scales::comma) 
   
   plot <- ggplotly(p)
   plot
 })
  
}
shinyApp(ui, server)
