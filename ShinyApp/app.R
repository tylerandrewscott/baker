#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

require(shiny);require(lubridate)
require(dplyr)
require(ggplot2);require(ggnetwork);require(ggthemes)
dat = read.csv('network_df.csv',row.names=1)
dat$MeetingTopic = as.character(dat$MeetingTopic)
dat$MeetingDate = ymd(dat$MeetingDate)
dat$NodeSize = ifelse(is.na(dat$MeetingDate),4,7)
y1 = min(dat$MeetingYear[!is.na(dat$MeetingYear)])
y2 = max(dat$MeetingYear[!is.na(dat$MeetingYear)])
tot.meetings = length(unique(dat$MeetingDate))

# Define UI for application that draws a histogram

ui = fluidPage(
  title = 'Baker Meeting Attendance',
  plotOutput("distPlot", height = 600,
             click = clickOpts(
               id = "plot_click"
             )),
  hr(),
  fluidRow(
    column(4,
             sliderInput('year',label = h3("Years"),min=y1,max=y2,value=c(y1,y2),
                         sep='',step=1,ticks=FALSE)),
    column(4, checkboxGroupInput("topic",
                                label = h3("Topics"),
                                choices = list("Aquatic" = 'aquatic/fish',
                                               "Cultural" = 'cultural', "Economic" = 'economic',
                                               'Admin'= 'admin','Public/General' = 'public/general',
                                               'Recreation'='recreational','Terrestrial'='terrestrial'
                                ),
                                selected = c('public/general'))),
    column(4,
           sliderInput('attended',label = h3("Meetings Attended"),min=1,max=tot.meetings,1,
                       sep='',step=1,ticks=FALSE))
    ))



server <- function(input, output)  {
  output$distPlot <- renderPlot({
ggplot(dat %>% filter(MeetingTopic %in% c('aa',input$topic)) %>% 
             filter(is.na(MeetingYear)|(MeetingYear %in% input$year)) %>%
         filter(is.na(MeetingsAttended)|MeetingsAttended>=input$attended),
           aes(x, y, xend = xend, yend = yend)) +
      geom_edges(alpha = 0.5,colour='grey50',curvature = 0.1)+
      geom_point(aes(shape=Level,colour=MeetingTopic,size=10))+
     # geom_nodelabel(aes(label=vertex.names)) +
      # geom_nodes(aes(shape=Level,colour=MeetingTopic)) +
      scale_shape_manual(name='',values=c(22,20))+
      scale_color_colorblind(name='Topic',labels = c('',input$topic))+
      guides(colour = guide_legend(nrow=2,override.aes = list(shape = c(NA,rep(22,length(input$topic))),size=5)),
             shape=FALSE,size=FALSE) +theme_tufte(ticks=F) +
      theme(axis.text=element_blank(),axis.title=element_blank(),
            legend.position=c(0.8,.05),legend.title.align = .7,
            legend.key.size=unit(1,'cm'),
            legend.text=element_text(size=18),legend.title=element_text(size=20))})
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)