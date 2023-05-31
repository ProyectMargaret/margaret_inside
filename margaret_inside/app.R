library(shiny)
library(shinydashboard)
library(dplyr)
library(readr)

proyectos_2017 <- read_csv("https://docs.google.com/spreadsheets/d/1aSS-VUlN0N0BwOlyd8cbPVyOqsmmAwE8jLFut21eqo0/export?format=csv&gid=1021110299")
producto_esperado <- read_csv("https://docs.google.com/spreadsheets/d/1aSS-VUlN0N0BwOlyd8cbPVyOqsmmAwE8jLFut21eqo0/export?format=csv&gid=1600566319")
adscripcion <- read_csv("https://docs.google.com/spreadsheets/d/1aSS-VUlN0N0BwOlyd8cbPVyOqsmmAwE8jLFut21eqo0/export?format=csv&gid=932379407")

usuarios <- data.frame(
  usuario = c("usuario1", "usuario2"),
  contraseña = c("contraseña1", "contraseña2")
)

ui <- dashboardPage(
  dashboardHeader(title = "Inicio de Sesión"),
  dashboardSidebar(),
  dashboardBody(
    uiOutput("pagina_redireccionada")
  )
)

server <- function(input, output, session) {
  # Estado del inicio de sesión
  login_status <- reactiveVal(FALSE)
  
  # Función para verificar el inicio de sesión
  verificarInicioSesion <- function(usuario, contraseña) {
    usuario_valido <- usuarios$usuario == usuario
    contraseña_valida <- usuarios$contraseña == contraseña
    if (sum(usuario_valido & contraseña_valida) > 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  
  # Acciones para el botón de inicio de sesión
  observeEvent(input$login, {
    usuario <- input$usuario
    contraseña <- input$contraseña
    if (verificarInicioSesion(usuario, contraseña)) {
      showModal(modalDialog(
        title = "Inicio de sesión",
        "Inicio de sesión exitoso",
        easyClose = TRUE
      ))
      login_status(TRUE)
    } else {
      showModal(modalDialog(
        title = "Inicio de sesión",
        "Usuario o contraseña incorrectos",
        easyClose = TRUE
      ))
    }
  })
  
  # Acciones para el botón de registro
  observeEvent(input$registro, {
    showModal(modalDialog(
      title = "Registro",
      "Acción de registro",
      easyClose = TRUE
    ))
  })
  
  # Página redireccionada
  output$pagina_redireccionada <- renderUI({
    if (login_status()) {
      fluidPage(
        titlePanel("Project Data Explorer"),
        sidebarLayout(
          sidebarPanel(
            selectInput("project_id", "Select Project ID:", choices = unique(proyectos_2017$ID_PROYECTO))
          ),
          mainPanel(
            tabsetPanel(
              tabPanel("Proyectos", tableOutput("proyectos_table")),
              tabPanel("Producto Esperado", tableOutput("producto_table")),
              tabPanel("Adscripcion", tableOutput("adscripcion_table"))
            )
          )
        )
      )
    } else {
      fluidPage(
        fluidRow(
          column(width = 4, offset = 4,
                 box(
                   title = "Iniciar Sesión",
                   solidHeader = TRUE,
                   width = 12,
                   align = "center",
                   div(
                     id = "login-box",
                     tags$head(tags$style(HTML("
              body {
                background-color: #f8f9fa;
              }
              .login-box {
                max-width: 400px;
                margin: 0 auto;
                background-color: #fff;
                border: 1px solid #ddd;
                padding: 20px;
                margin-top: 50px;
                box-shadow: 0px 2px 10px rgba(0,0,0,0.1);
              }
              .login-box .form-control {
                border: 1px solid #ddd;
                border-radius: 4px;
                padding: 10px;
                margin-bottom: 10px;
              }
              .login-box .btn {
                padding: 10px 16px;
                font-size: 16px;
                width: 48%;
              }
              .login-box .btn-primary {
                background-color: #007BFF;
                border-color: #007BFF;
                color: #fff;
              }
              .login-box .btn-primary:hover {
                background-color: #0069D9;
                border-color: #0069D9;
              }
              .login-box .btn-success {
                background-color: #28A745;
                border-color: #28A745;
                color: #fff;
              }
              .login-box .btn-success:hover {
                background-color: #218838;
                border-color: #218838;
              }
              .login-box .icon {
                display: inline-block;
                vertical-align: middle;
                margin-right: 5px;
              }
              .login-box .icon i {
                font-size: 20px;
              }
            "))),
                     tags$div(
                       style = "text-align: center;",
                       tags$div(
                         class = "icon",
                         tags$i(class = "fas fa-user-circle"),
                         textInput("usuario", NULL, placeholder = "Usuario", value = "")
                       ),
                       tags$div(
                         class = "icon",
                         tags$i(class = "fas fa-lock"),
                         passwordInput("contraseña", NULL, placeholder = "Contraseña", value = "")
                       )
                     ),
                     br(),
                     tags$div(
                       style = "display: flex; justify-content: space-between;",
                       actionButton("login", "Iniciar sesión", class = "btn btn-primary", style = "width: 48%;"),
                       actionButton("registro", "Registrarse", class = "btn btn-success", style = "width: 48%;")
                     )
                   )
                 )
          )
        )
      )
    }
  })
  
  # Filter data based on selected project ID
  selected_project <- reactive({
    id <- input$project_id
    list(
      proyectos = proyectos_2017[proyectos_2017$ID_PROYECTO == id, ],
      producto = producto_esperado[producto_esperado$ID_PROYECTO == id, ],
      adscripcion = adscripcion[adscripcion$ID_PROYECTO == id, ]
    )
  })
  
  # Generate tables for display
  output$proyectos_table <- renderTable({ selected_project()$proyectos })
  output$producto_table <- renderTable({ selected_project()$producto })
  output$adscripcion_table <- renderTable({ selected_project()$adscripcion })
}

shinyApp(ui = ui, server = server)
