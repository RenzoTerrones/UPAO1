library(shiny)
library(shinydashboard)




ui <- dashboardPage(skin = "blue",
  
  dashboardHeader( title = "Plataform Donation" ),
  dashboardSidebar(
    sidebarMenu(id="sbmenu",
                menuItem("Donaciones Blockchain", tabName = "pds", icon = icon("th")),
                menuItem("Cargar datos",tabName = "main1",icon = icon("upload"),
                         menuSubItem('Etapa 1', tabName = 'c2'),
                         menuSubItem('Data 2', tabName = 'c3'),
                         menuSubItem('Data 3', tabName = 'c4')
                ),
                
                menuItem("Plots",tabName = "main2" ,icon = icon("bar-chart-o"),
                         menuSubItem('indicador 1', tabName = 'p1'),
                         menuSubItem('indicador 2', tabName = 'p2'),
                         menuSubItem('indicador 3', tabName = 'p3')
                ),
                
                menuItem("Datos",tabName = "main3" ,icon = icon("table"),
                         menuSubItem('Sub Menu 1', tabName = 'dt1'),
                         menuSubItem('Sub Menu 2', tabName = 'dt2'),
                         menuSubItem('Sub Menu 3', tabName = 'dt3')
                ),
                
                menuItem("Damnificados",tabName = "main4" ,icon = icon("heart"),
                         menuSubItem('Sub Menu 1', tabName = 'dm1'),
                         menuSubItem('Sub Menu 2', tabName = 'dm2'),
                         menuSubItem('Sub Menu 3', tabName = 'dm3')
                ),
                
                menuItem("Donaciones",tabName = "main5" ,icon = icon("usd"),
                         menuSubItem('Sub Menu 1', tabName = 'dn1'),
                         menuSubItem('Sub Menu 2', tabName = 'dn2'),
                         menuSubItem('Sub Menu 3', tabName = 'dn3')
                ),
                
                menuItem("Acerca de",tabName = "main6",icon = icon("info"),
                         menuSubItem('Sub Menu 1', tabName = 'a1'),
                         menuSubItem('Sub Menu 2', tabName = 'a2'),
                         menuSubItem('Sub Menu 3', tabName = 'a3')
                )
    )
  ),
  
  dashboardBody(
    tabItems(
      
      tabItem("c2",h2("Administrador"), 
      fluidRow(box(title = "Ingreso de Datos", status = "primary",width=6,solidHeader = FALSE,
            collapsible = TRUE,selectInput("select", label = h4("Tipo de archivo: "), 
        choices = c('text'='1','csv'='2','xlsx'='3','json'='4','xml'='5')),
        fileInput("file1", "Choose File",
        accept = c("text/csv","text/json","text/comma-separated-values,text/plain",".csv","XLSX file",".json",".xlsx",".xls",".xml")),
        helpText ( " Max. Tamaño de archivo: 30MB " )),
      
          box(title = "Informacion de los Datos", status = "primary",width=6,solidHeader = FALSE,
            collapsible = TRUE,dataTableOutput("tabla1"))),
     
       fluidRow( box(title = "Vista Previa" ,status = "primary",width=12, solidHeader = FALSE,
            collapsible = TRUE,dataTableOutput("tabla2")))),
        
      tabItem("c3",h1("Pagina 2 en construccion"),
              box("Box content here"),
              box("Box content here"),
              box("Box content here")),
      tabItem("c4",h1("Pagina 3 en construccion"),
              box("Box content here"),
              box("Box content here"),
              box("Box content here")),
      
      tabItem("dt1",h1("Pagina 1 en construccion"),
              box("Box content here"),
              box("Box content here"),
              box("Box content here")),
      tabItem("dt2",h1("Pagina 2 en construccion"),
              box("Box content here"),
              box("Box content here"),
              box("Box content here")),
      tabItem("dt3",h1("Pagina 3 en construccion"),
              box("Box content here"),
              box("Box content here"),
              box("Box content here")),
      
      tabItem("dn1",h1("Pagina 1 en construccion"),
              box("Box content here"),
              box("Box content here"),
              box("Box content here")),
      tabItem("dn2",h1("Pagina 2 en construccion"),
              box("Box content here"),
              box("Box content here"),
              box("Box content here")),
      tabItem("dn3",h1("Pagina 3 en construccion"),
              box("Box content here"),
              box("Box content here"),
              box("Box content here")),
      
      tabItem("dm1",h1("Pagina 1 en construccion"),box("Box content here"),
              box("Box content here"),
              box("Box content here")),
      tabItem("dm2",h1("Pagina 2 en construccion"),
              box("Box content here"),
              box("Box content here"),
              box("Box content here")),
      tabItem("dm3",h1("Pagina 3 en construccion"),
              box("Box content here"),
              box("Box content here"),
              box("Box content here")),
      
      tabItem("p1",h1("Pagina 1 en construccion"),
              box("Box content here"),
              box("Box content here"),
              box("Box content here")),
      tabItem("p2",h1("Pagina 2 en construccion"),box("Box content here"),
              box("Box content here"),
              box("Box content here")),
      tabItem("p3",h1("Pagina 3 en construccion"),
              box("Box content here"),
              box("Box content here"),
              box("Box content here")),
      
      tabItem("a1",h1("Pagina 1 en construccion"),
              box("Box content here"),
              box("Box content here"),
              box("Box content here")),
      tabItem("a2",h1("Pagina 2 en construccion"),
              box("Box content here"),
              box("Box content here"),
              box("Box content here")),
      tabItem("a3",h1("Pagina 3 en construccion"),
              box("Box content here"),
              box("Box content here"),
              box("Box content here"))
    )
  )
)

options(shiny.maxRequestSize=30*1024^2)
server <- function(input, output) {
  observe(print(input$sbmenu))
  
  output$tabla1 <- renderDataTable({
    if(is.null(input$file1)){return ()}
    input$file1
    
  },options = list(scrollX = TRUE,searching = FALSE,paging = FALSE))
  
  
  output$tabla2 <- renderDataTable({
    tabla <- input$file1
    if (is.null(tabla))
    {return(NULL)}
    box<- input$select
    if(box == "1")    {
      read.csv(tabla$datapath)
    } else{  if(box == "2") {
      read.csv(tabla$datapath)
    } else { if(box == "3") {
      read_xlsx(tabla$datapath)
    } else { if(box == "4")
    { a<-fromJSON(tabla$datapath)
    data<- as.data.frame(a)
    print(data)
    } else { if(box=="5")
    { xmldataframe <- xmlToDataFrame(tabla$datapath)
    print(xmldataframe)} }  }
    } }
  },options = list(scrollX = TRUE,searching = FALSE,pageLength = 15))
  
}

shinyApp(ui,server)