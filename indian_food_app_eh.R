
indian_food<- read_csv(here("indian_food_data.csv"))%>% 
  clean_names()%>%
  na_if("-1")%>%
  mutate_all(tolower)%>%
  transform(ingredients=str_split(ingredients,","))%>%
  unnest(ingredients)%>%
  mutate(total_cook_time=as.numeric(cook_time)+as.numeric(prep_time))

library(shiny)
library(DT)
library(ggplot2)
ui <- fluidPage(
  title="Indian Food E-Cookbook",
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("show_vars", "Select What You Want To Find", names(indian_food), selected=names(indian_food))
    ), 
    mainPanel(
      id='indian_food', 
      tabPanel("indian food", DT::dataTableOutput("mytable"))
    )
  ))


server <- function(input, output) {
  indian_food=indian_food[sample(nrow(indian_food), 1136),]
  output$mytable = DT::renderDataTable({
    DT::datatable(indian_food[,input$show_vars, drop=FALSE], options=list(orderClasses=TRUE))
  })
}

shinyApp(ui,server)
