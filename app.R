#load library
library(readxl)
library(dplyr)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(shinyWidgets)


#read datafiles
region<-data.frame(read_excel('newsumsheet.xlsx',sheet=1))
sexregion<-data.frame(read_excel('newsumsheet.xlsx',sheet=2))
ageregion<-data.frame(read_excel('newsumsheet.xlsx',sheet=3))
agecases<-data.frame(read_excel('cleansheet.xlsx',sheet=3))
gendercases<-data.frame(read_excel('cleansheet.xlsx',sheet=4))
locationcases<-data.frame(read_excel('cleansheet.xlsx',sheet=5))
areaandregion<-data.frame(read_excel('areaandregionname.xlsx'))

#calculate total number of cases and total number of victims
total_count<-sum(region$Total)
total_female_count<-sexregion%>%
  filter(AFM.Sex=='Females')
total_female_count<-sum(total_female_count$Total)

total_male_count<-sexregion%>%
  filter(AFM.Sex=='Males')
total_male_count<-sum(total_male_count$Total)

#design sidebar

sidebar<-dashboardSidebar(
  #add year filter
  selectInput(inputId = "SelectedYear",
              label = "Select Year here",
              choices =c('2016','2017','2018','2019','2020'),
              multiple = FALSE),
  
  #add region filter
  checkboxGroupInput(inputId = 'SelectedRegion',
              label="Region selections",
              choices=list("1 North West Metro",
                        "2 Eastern",
                        "3 Southern Metro",
                        "4 Western"),
              selected = list("1 North West Metro",
                           "2 Eastern",
                           "3 Southern Metro",
                           "4 Western")),
  #add search location function
  pickerInput('location', 
              'Search local area here', 
              c(areaandregion$Local.Government.Area), 
              options = list(`live-search`=TRUE)),
  
  #add menus
  sidebarMenu(
    menuItem("Dashboard",tabName = 'dashboard'),
    menuItem('Description',tabName='description'),
    menuItem("Raw data",tabName = 'rawdata')
  )
)


#design body of dashboard 
body <- dashboardBody(
  
  #when click on dashboard menu
  tabItems(
    #create boxes to store graph
    tabItem("dashboard",
            fluidRow(
              #format box
              box(title = "Total case at regions",
                  solidHeader = TRUE,
                  status = 'danger',
                  collapsible = TRUE,
                  plotOutput('VisRegion',width='100%',height=250)),
              
              #format box
              box(title='Map',
                  solidHeader = TRUE,
                  status = "warning",
                  collapsible = TRUE,
                  img(src='policeregioncapture.png',
                      height=250,
                      width='100%'))
            ),
            #format box
            fluidRow(
              box(
                title = "Gender of Affected Family Member", 
                solidHeader = TRUE, 
                status = "primary",
                collapsible = TRUE,
                plotOutput('VisGender',width='100%',height=250)
              ),
              #format box
              box(
                title = "Detail of location", 
                status = 'info',
                solidHeader = TRUE,
                collapsible = TRUE,
                height=300,
                htmlOutput('textlocation',
                           width='100%')
              ),
            ),
            fluidRow(
              box(
                title="Age of Affected Family Member", 
                width=12, 
                status='success',
                solidHeader = TRUE,
                collapsible = TRUE,
                plotOutput('VisAge',width='100%',height=200)
              )
            ),
            #add info box
            fluidRow(
              infoBoxOutput("totalcaseBox"),
              infoBoxOutput("totalmaleBox"),
              infoBoxOutput("totalfemaleBox")
            ),
            
            #add value box
            fluidRow(
              valueBoxOutput("casechangeBox"),
              valueBoxOutput("malechangeBox"),
              valueBoxOutput("femalechangeBox")
            )
    
                    ),
    #design dashboard for description page
    tabItem("description",
            valueBox("Overall",'Most incidents happened at Eastern and Western regions.',
                     icon =icon('location-arrow'),width = '100px' ),
            br(),
            valueBox("Sadly",'Majority of victims are female.',color='purple',
                     icon =icon('venus'),width = '100px'),
            br(),
            valueBox('Age group','of 25-34 years and 35-44 years are the most vulnerable groups.',
                     icon=icon('earlybirds'),width='100px',color='orange'),
            br(),
            h4('Want to know more?'),
            actionButton('story','Tell me a story',
                         icon('arrow-alt-circle-down'),
                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
            br(),
            textOutput("narrative")
            ),
    
    #when click on raw data menu
    tabItem("rawdata",
            #add download option
            downloadButton("downloadregion","Download selected region data here",
                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
                           ),
            br(),
            downloadButton("downloadgender","Download selected gender data here",
                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
            br(),
            downloadButton("downloadage","Download selected age data here",
                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
            br(),
            #provide link to the Crime Statistics Agency
            h4("All data and materials are reserved by the right of Crime Statistics Agency."),
            br(),
            tags$a(href="https://www.crimestatistics.vic.gov.au/",'For more information, please find link here.')
  )  
  
))


#design ui
ui <- dashboardPage(
  skin='black',
  dashboardHeader(title = "Family Incident Dashboard",
                  titleWidth =300),
  sidebar,
  body
)

# define server
server <- function(input, output) {
  #add reactivity 
  #subtract new dataframe based on input of year and region
  selected_year_region<-reactive(region%>%
                                   filter(region$Year==input$SelectedYear))
  
  
  selected_year_age<-reactive(ageregion%>%
                                filter(ageregion$Year==input$SelectedYear))
  
  selected_year_gender<-reactive(sexregion%>%
                                   filter(sexregion$Year==input$SelectedYear))
  
  selected_location<-reactive(locationcases%>%
                                filter((locationcases$Year==input$SelectedYear) &
                                      (locationcases$Local.Government.Area==input$location)))
  selected_age<-reactive(agecases%>%
                           filter((agecases$Year==input$SelectedYear) &
                                    (agecases$Local.Government.Area==input$location)))
  
  selected_gender<-reactive(gendercases%>%
                           filter((gendercases$Year==input$SelectedYear) &
                                    (gendercases$Local.Government.Area==input$location)))
  
  male_selected_year<-reactive(sexregion%>%
                                 filter((sexregion$Year==input$SelectedYear)&
                                 (sexregion$AFM.Sex=='Males')))
  female_selected_year<-reactive(sexregion%>%
                                 filter((sexregion$Year==input$SelectedYear)&
                                          (sexregion$AFM.Sex=='Females')))
  
  selected_region_region<-reactive(region%>%
                                      filter(region$Police.Region %in% input$SelectedRegion))
  selected_region_age<-reactive(ageregion%>%
                                     filter(ageregion$Police.Region%in%input$SelectedRegion))
  selected_region_gender<-reactive(sexregion%>%
                                      filter(sexregion$Police.Region%in%input$SelectedRegion))
  
  #join subtracted dataframe based on selected year and region
  final_regioninput<-reactive(inner_join(selected_year_region(), selected_region_region()))
  final_genderinput<-reactive(inner_join(selected_year_gender(), selected_region_gender()))
  final_ageinput<- reactive(inner_join(selected_year_age(), selected_region_age()))
  
 
 #output region bar chart 
  output$VisRegion<-renderPlot({
    final_regioninput()%>%
      ggplot(aes(factor(Police.Region),y=Total,
                 fill=Police.Region))+
      geom_bar(stat='identity',
               position='dodge',
               size=0.5)+
      xlab("Police Region")+
      ylab("Number of incidents")+
      guides(fill=guide_legend(title="Region"))+
      theme(plot.title = element_text(h=0.5))+
      scale_fill_brewer(palette="Set2")+
      geom_text(aes(label=Total),
                color='black',
                vjust=1.5)
    
  })
  
  #output gender multi-set bar chart
  output$VisGender<-renderPlot({
    final_genderinput()%>%
      ggplot(aes(x=Police.Region,y=Total,
                 fill=AFM.Sex,height = '100%'))+
      geom_bar(stat='identity',position=position_dodge())+
      xlab("Police Region")+
      ylab("Number of affected members")+
      guides(fill=guide_legend(title="Gender"))+
      theme(plot.title = element_text(h=0.5))+
      geom_text(aes(label=Total),
                color='black',
                position=position_dodge(0.9),
                vjust=1.5)
  })
  
  #output age group pie chart
  output$VisAge<-renderPlot({
    final_ageinput()%>%
      ggplot(aes(x="",y=Total,fill=Age.Group,color=Age.Group,height='100%'))+
      geom_bar(stat = "identity", color = "white",position = "fill")+
      coord_polar('y')+
      facet_grid(.~Police.Region)+
      scale_fill_brewer(palette="Set3")+
      theme_void()
      
  })
  
  #output info box
  output$totalcaseBox<-renderInfoBox({
    infoBox(
      'Cases this year',
      sum(selected_year_region()$Total),
      icon=icon('sad-tear'),
      color='red',
      fill=TRUE
      )
  })
  #output info box
  output$totalmaleBox<-renderInfoBox({
    infoBox(
      'Affected males this year',
      sum(male_selected_year()$Total),
      icon=icon('male'),
      color='blue',
      fill=TRUE
    )
  })
  #output info box
  output$totalfemaleBox<-renderInfoBox({
    infoBox(
      'Affected females this year',
      sum(female_selected_year()$Total),
      icon=icon('female'),
      color='maroon',
      fill=TRUE
    )
  })
  
  #output value box
  output$casechangeBox<-renderValueBox({
    total_per_count<-round(sum(selected_year_region()$Total)/total_count,4)*100
    
    valueBox(
      'OF TOTAL CASES',
      value = paste0(total_per_count, '%'),
      icon=icon('sad-tear'),
      color='red'
    )
  })
  
  #output value box
  output$malechangeBox<-renderValueBox({
    male_per_count<-round(sum(male_selected_year()$Total)/total_male_count,4)*100
    
    valueBox(
      'OF TOTAL AFFECTED MALES',
      value = paste0(male_per_count, '%'),
      icon=icon('male'),
      color='blue'
    )
  })
  
  #output value box
  output$femalechangeBox<-renderValueBox({
    female_per_count<-round(sum(female_selected_year()$Total)/total_female_count,4)*100
    
    valueBox(
      'OF TOTAL AFFECTED FEMALES',
      value = paste0(female_per_count, '%'),
      icon=icon('female'),
      color='maroon'
    )
  })
  
  #output selected location detail
  output$textlocation<-renderUI({
    str1 <- paste("You have selected", input$location, ".",
                  '<br/>',"It belongs to", 
                  selected_location()$Police.Region, '.')
    str2 <- paste("There were", selected_location()$Family.Incidents,
                   "cases in", input$SelectedYear,'.')
    str3 <- paste(selected_gender()$AFM.Counter[1], 'females and ',
                  selected_gender()$AFM.Counter[2], 'males were affected.' )
    str4<-paste(selected_age()$AFM.Counter[1], 'victims are age between 0-17 years old.','<br/>',
                selected_age()$AFM.Counter[2], 'victims are age between 18-24 years old.','<br/>',
                selected_age()$AFM.Counter[3], 'victims are age between 25-34 years old.','<br/>',
                selected_age()$AFM.Counter[4], 'victims are age between 35-44 years old.','<br/>',
                selected_age()$AFM.Counter[5], 'victims are age between 45-54 years old.','<br/>',
                selected_age()$AFM.Counter[6], 'victims are age over 55 years old.','<br/>', '(Exclude unknown data)')
    
    HTML(paste(str1, str2, str3,str4, sep = '<br/>'))
  })
  
  #provide downloadable file for selected region data
  output$downloadregion<-downloadHandler(
    filename=function(){
      paste('final_regioninput','csv',sep='.')
    },
    
    content=function(file){
      write.csv(final_regioninput(),file)
    }
  )
 
  #provide downloadable file for selected gender data
  output$downloadgender<-downloadHandler(
    filename=function(){
      paste('final_genderinput','csv',sep='.')
    },
    
    content=function(file){
      write.csv(final_genderinput(),file)
    }
  )
  
  #provide downloadable file for selected age data
  output$downloadage<-downloadHandler(
    filename=function(){
      paste('final_ageinput','csv',sep='.')
    },
    
    content=function(file){
      write.csv(final_ageinput(),file)
    }
  )
  
  storylist<-c("Nearly two thirds (66.1%) of children under the age of 10 years old  were recorded as witnesses at a police attended incident at the date of the incident. 
                    35% of children were under the age of 5 years old.",
                  
                  "Over 1 in 50 children in Victoria between 1 July 2018 and 30 June 2019 were recorded by police as having been exposed to family violence.",
                  
                  "Research found that 74% of recorded family violence perpetrators were male, with an average age of 35.5 years, and five per cent were Aboriginal. 
                    Half (51%) of perpetrators had been recorded for at least one other family violence incident in the previous 12 years.",
                  
                  "There was an increase in the number of victim survivors or clients aged 55 or older who accessed family violence services during the second quarter of 2020.",
                  
                  "Family related incidents increased 7.5% in the last 12 months to the highest on record, 90,056 incidents. 
                    The rate of family incidents recorded increased by 5.7% to 1,342.9 incidents per 100,000 population.", 
                  
                  "Ambulance Victoria and Victorian public hospital emergency presentations for family violence related injuries increased compared to the prior financial year. 
                    In 2019/20 Ambulance Victoria recorded a 14.7% increase in the number of family violence related attendances, and emergency room presentations increased by 13.5% compared with 2018-19.")
  
  
  #narrative text input
  narrative_sample<-eventReactive(input$story,{
    
    #ramdonly select one story for each click
    samplestory<-sample(storylist,1)
    
    })
  
  # narrative text output
  output$narrative <- renderText(narrative_sample())
  
}
#call shinyapp
shinyApp(ui,server)
