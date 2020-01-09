
library(shiny)
library(shinyauthr)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(shinyWidgets) # action button customize
library(DT)
library(RPostgreSQL)

user_base <- data.frame(
  user = c("user1", "user2"),     # 로그인 ID
  password = c("pass1", "pass2"), # 로그인 password
  id = c('0000','1234'),
  permissions = c("admin", "standard"),
  name = c("Lee", "Kim"),
  age = c('24','27'),
  stringsAsFactors = FALSE,
  row.names = NULL
)

# PostgreSQL 접속 : docker 의 demo_db1 라는 DB 에 demo_t 라는 table 을 만들어 놓았음

global_con <- dbConnect(dbDriver("PostgreSQL"), dbname = "demo_db1", host = "localhost", port = 5555, user = "postgres", password = "password1")
global_sql_query <- "select * from demo_t;"

# send query
sqlInput <- dbGetQuery(global_con, global_sql_query)

ui <- dashboardPage(
  
  dashboardHeader(
    title = "R shiny with PostgreSQL",
    titleWidth = 300,
    tags$li(class = "dropdown", style = "padding: 8px;", shinyauthr::logoutUI("logout"))
  ),
  
  dashboardSidebar(
    sidebarMenuOutput("sidebar")
  ),
  
  dashboardBody(
    shinyjs::useShinyjs(),
    
    shinyauthr::loginUI("login"),
    
    tabItems(
      
      tabItem("tab1", 
              
              fluidRow(
                column(width = 10,
                       
                       box(
                         title = "Information", width = 12, solidHeader = TRUE, background = "maroon",
                         tableOutput("user_table")
                       ),
                       
                       box(title = "Name", 
                           textInput("text_input_1", "", ""),
                           
                           status = "success", solidHeader = TRUE, width = 3
                       ),
                       box(title = "Class",
                           
                           selectInput("text_input_2", "",
                                       c("Class A","Class B","Class C")),
                           
                           status = "success", solidHeader = TRUE, width = 3
                       ),
                       box(title = "Save Data",
                           
                           useShinyjs(),
                           
                           textOutput("value"),
                           
                           actionBttn(
                             inputId = "submit",
                             label = "Save !",
                             color = "success",
                             style = "bordered",    
                             icon = icon("bell"),
                             block = TRUE
                           ),
                           status = "success", solidHeader = TRUE, width = 3
                       ),
                       box(title = "Show Table",
                           actionButton("showTable", "Show Table", icon = icon("table")),
                           bsModal("modalExample", "j", "showTable", size = "large",
                                   dataTableOutput("tbl")),
                           verbatimTextOutput('aaa'),
                           status = "primary", solidHeader = TRUE, width = 3
                       )
                )
              )
      ),
      
      tabItem("tab2", 
              
              # action button customizing : https://rdrr.io/cran/shinyWidgets/man/actionBttn.html
              
              actionBttn(
                inputId = "submit_list_update",
                label = "List Update!",
                color = "success",
                style = "jelly",   
                icon = icon("bell"),
                block = TRUE
              ),
              
              h2(""), 
              
              DT::dataTableOutput("filteredTable"),
              DT::dataTableOutput("filteredTableSelected"),
              
              useShinyjs(),
              
              actionBttn(
                inputId = "submit_save_data",
                label = "Save !",
                color = "success",
                style = "jelly",    
                size = 'md',
                icon = icon("bell"),
                block = TRUE
              )
      ),
      
      tabItem("tab3", 
              
              actionBttn(
                inputId = "submit_list_update2",
                label = "List Update!",
                color = "success",
                style = "jelly",  
                icon = icon("bell"),
                block = TRUE
              ),
              
              h2(""), # actionbutton 밑에 빈칸을 넣는 방법
              
              DT::dataTableOutput("filteredTable2"),
              DT::dataTableOutput("filteredTableSelected2"),
              
              useShinyjs(),
              
              actionBttn(
                inputId = "submit_save_data2",
                label = "Save !",
                color = "success",
                style = "jelly",    
                size = 'md',
                icon = icon("bell"),
                block = TRUE
              )
      )
    )
  )
)


# SERVER 시작

server <- function(input, output, session) {
  
  logout_init <- callModule(shinyauthr::logout, 
                            id = "logout", 
                            active = reactive(credentials()$user_auth))
  
  credentials <- callModule(shinyauthr::login, 
                            id = "login", 
                            data = user_base,
                            user_col = user,
                            pwd_col = password,
                            log_out = reactive(logout_init()))
  
  
  output$user_table <- renderTable({ 
    user_data <- reactive({credentials()$info})
    
    req(credentials()$user_auth) 
    
    user_data()[3:6] 
  }) 
  
  output$sidebar <- renderMenu({
    req(credentials()$user_auth)
    sidebarMenu(
      id = "tabs",
      menuItem("Menu Item 1", icon = icon("bar-chart-o"), startExpanded = T,
               menuSubItem("Menu Sub Item 1", tabName = "tab1"),
               menuSubItem("Menu Sub Item 2", tabName = "tab2"),
               menuSubItem("Menu Sub Item 3", tabName = "tab3")
      )
    )
  })
  
  output$tbl = DT::renderDataTable({
    
    datatable(
      user_base
    )
  })
  
  # action button 을 누르면 DB 에 한 줄 쌓는 쿼리
  
  saveData <- function(data) {
    
    con <- dbConnect(dbDriver("PostgreSQL"), dbname = "demo_db1", host = "localhost", port = 5432, user = "postgres", password = "password1")
    user_data <- reactive({credentials()$info})
    req(credentials()$user_auth) 
    
    # PostgreSQL 에 insert 를 날려보자.
    # R shiny 에서 input 으로 받은 변수는 %s 로 받은 이후에 'input$변수명' 으로 insert 가 가능하다
    # insert 문 내부에 있는 current_date 는 R 이 아닌, PostgreSQL 내부의 함수이다.
    
    dbSendQuery(con, 
                sprintf("insert into demo_t (c1, c2, c3, c4) values (current_date,'%s','%s', 1)",
                        input$text_input_1, input$text_input_2 )
    )
    dbDisconnect(con)
  }
  
  # action button 을 누르면 DB 에 한 줄 쌓기
  
  observeEvent(input$submit, {
    saveData(formData())
  })
  
  # action butoon 을 누르면 팝업창 뜨게 하기
  
  observeEvent(input$submit, {
    showModal(modalDialog(
      title = "Save complete !!",
      h4("Add data complete!"),
      easyClose = TRUE
    ))
  })
  
  # 여기부터 tab2 시작
  
  loadData <- function() {
    
    input$submit_list_update # 이걸 넣어서 실시간 업데이트가 가능
    
    con  <- dbConnect(dbDriver("PostgreSQL"), dbname = "demo_db1", host = "localhost", port = 5555, user = "postgres", password = "password1")
    query <- "select * from demo_t ;"
    data  <- dbGetQuery(con, query)
    dbDisconnect(con)
    data
  }
  
  filteredTable_data <- reactive({
    loadData()
  })
  
  output$filteredTable <- DT::renderDataTable({
    req(credentials()$user_auth)
    datatable(
      filteredTable_data(),
      filter = 'bottom',
      selection = list(mode = "multiple"),
      #caption = "aaaaaaa",
      #colnames = c('c1', 'c2','c3','c4','c5'),
      #extensions = 'Buttons',
      options = list(scrollX = TRUE
                     #, dom = 'Bfrtip'
                     #, buttons = list('excel')
      )
    )
  })
  
  filteredTable_selected <- reactive({
    ids <- input$filteredTable_rows_selected
    filteredTable_data()[ids,]
  })
  
  output$filteredTableSelected <- DT::renderDataTable({
    req(credentials()$user_auth)
    datatable(
      filteredTable_selected(), # filteredTable_selected()[,2:5] 이렇게 하면 2~5 컬럼만 선택됨
      selection = list(mode = "none"),
      options = list(scrollX = TRUE)
    )
  })
  
  # R 에서 data.frame 을 DB 에 쌓는 방법 : https://stackoverflow.com/questions/33634713/rpostgresql-import-dataframe-into-a-table
  
  saveData_ <- function(data) {
    
    pcon <- dbConnect(dbDriver("PostgreSQL"), dbname = "demo_db1", host = "localhost", port = 5555, user = "postgres", password = "password1")
    
    req(credentials()$user_auth) 
    
    df = filteredTable_selected()
    
    dbWriteTable(pcon, "demo_t", df, row.names=FALSE, append=TRUE)
    
    dbDisconnect(pcon)
  }
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$submit_save_data, {
    saveData_(formData())
  })
  
  # action butoon 을 누르면 팝업창 뜨게 하기
  
  observeEvent(input$submit_save_data, {
    showModal(modalDialog(
      title = "Save complete !!",
      h4("Selected data save complete!"),
      easyClose = TRUE
    ))
    
  })
  
  # 여기부터 tab3 시작
  
  loadData2 <- function() {
    input$submit_list_update2 # 이걸 넣어서 실시간 업데이트가 가능
    con  <- dbConnect(dbDriver("PostgreSQL"), dbname = "demo_db1", host = "localhost", port = 5555, user = "postgres", password = "password1")
    query <- "select * from demo_t ;"
    data  <- dbGetQuery(con, query)
    dbDisconnect(con)
    data
  }
  
  filteredTable_data2 <- reactive({
    loadData2()
  })
  
  output$filteredTable2 <- DT::renderDataTable({
    req(credentials()$user_auth)
    datatable(
      filteredTable_data2(),
      filter = 'bottom',
      selection = list(mode = "multiple"),
      #caption = "aaaaaaa",
      #colnames = c('c1', 'c2','c3','c4','c5'),
      #extensions = 'Buttons',
      options = list(scrollX = TRUE
                     #, dom = 'Bfrtip'
                     #, buttons = list('excel')
      )
    )
  })
  
  filteredTable_selected2 <- reactive({
    ids <- input$filteredTable2_rows_selected
    filteredTable_data2()[ids,]
  })
  
  output$filteredTableSelected2 <- DT::renderDataTable({
    req(credentials()$user_auth)
    datatable(
      filteredTable_selected2(), # filteredTable_selected_revise()[,2:5] 이렇게 하면 2~5 컬럼만 선택됨
      selection = list(mode = "none"),
      #caption = "bbbbb",
      options = list(scrollX = TRUE)
    )
  })
  
  saveData_2 <- function(data) {
    
    pcon <- dbConnect(dbDriver("PostgreSQL"), dbname = "demo_db1", host = "localhost", port = 5555, user = "postgres", password = "password1")
    
    req(credentials()$user_auth) 
    
    # 상태를 변경시킬때 마다 num 을 1씩 늘리자
    num_temp = filteredTable_selected2()[4] + 1
    
    # dataframe 으로 만들어서 cbind 로 나중에 합칠 예쩡
    temp <- data.frame(
      num = num_temp,
      stringsAsFactors = FALSE
    )
    
    df = cbind(filteredTable_selected2()[,1:3],temp)
    
    dbWriteTable(pcon, "demo_t", df, row.names=FALSE, append=TRUE)
    
    dbDisconnect(pcon)
  }
  
  observeEvent(input$submit_save_data2, {
    saveData_2(formData())
  })
  
  # action butoon 을 누르면 팝업창 뜨게 하기
  
  observeEvent(input$submit_save_data2, {
    showModal(modalDialog(
      title = "Save complete !!",
      h4("Selected data save complete!"),
      easyClose = TRUE
    ))
  })
  
  # 아래의 조건들(if 절)을 만족시키지 않으면 submit 버튼 비활성화되게 하기
  # PostgreSQL 에서 c2, c3 의 길이를 10 이하로 제한해서, 11 이상으로 입력을하고 save 를 하면 PostgreSQL 오류가 나므로 R shiny 가 오류가 나서 꺼진다.
  # 이것을 대비해서 길이는 1 이상 10 이하인 경우만 입력 가능하게 action button 을 제한하자.
  
  observe({
    # 로그아웃하면 의뢰버튼 뜨지 않게 하게(logout)
    req(credentials()$user_auth)
    
    shinyjs::hide("submit")
    
    if( (input$text_input_1 != '') & (nchar(input$text_input_1) <= 10)  & (input$text_input_2 != '') & (nchar(input$text_input_2) <= 10) )  
      shinyjs::show("submit")
  })
  
  # 여기부터 actionButton 에 조건 붙이기
  
  output$value <- renderText({
    validate(
      need(input$text_input_1 != '', 'Please Write Name'),
      need(input$text_input_2 != '', 'Please Select Class')
    )
  })
}

# 연결된 모든 db connection 모두 disconnection 하는 방법
drv <- dbDriver("PostgreSQL")

cons <- dbListConnections(drv)

for(con in cons){
  dbDisconnect(con)  
}

shinyApp(ui = ui, server = server)