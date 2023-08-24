# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
# Find out more about building applications with Shiny here: http://shiny.rstudio.com/
#
#LIBRERIAS----------------------------------------------------------------------

library(installr)
library(rsconnect)
library(janitor)
library(shiny)
library(googlesheets4)
library(shinydashboard)
library(DT)
library(shinyjs)
library(sodium)
library(dplyr)
library(readxl)
library(stringr)
library(readxl)
library(tidyverse)



#DATOS-USUARIOS-----------------------------------------------------------------

indx <- function(vec, index) {
  if (index <= length(vec)) {
    colnames(vec)[index]
  } else {
    return(NA)
  }
}

#Traigo la información de jefes y equipo al que pertenece cada persona:

nomina <- data.frame(read_excel(
  "B31-DGMV-RRHH-DEVOLUCIONES.xlsx",
  sheet="LISTADO FINAL DE MAILS IMPORT",
  range = "A1:E300",
  col_names = TRUE,
  col_types = NULL,
  na = "",
  trim_ws = TRUE,
  skip = 0) %>% 
  clean_names())

usuarixs <- data.frame(unique(nomina$mail_jefe)) %>% 
          rename( "mail_por_rol_actual" = "unique.nomina.mail_jefe.") %>% 
          left_join(nomina,by = "mail_por_rol_actual") %>% 
          filter(!is.na(.[[1]]))

usuarixs$passod  = sapply(usuarixs$pass,password_store)

equipos <- read_excel(
  "B31-DGMV-RRHH-DEVOLUCIONES.xlsx",
  sheet="LISTADO FINAL DE MAILS IMPORT",
  range = "G1:I300",
  col_names = TRUE,
  col_types = NULL,
  na = "",
  trim_ws = TRUE,
  skip = 0) %>% 
  clean_names()


#DATOS-DEVOLUCIONES-------------------------------------------------------------

c_observaciones <- read_excel(
  "B31-DGMV-RRHH-DEVOLUCIONES.xlsx", sheet="OBSERVACIONES", skip = 4) 

c_mesa_op <-  read_excel(   "B31-DGMV-RRHH-DEVOLUCIONES.xlsx",  sheet="C_MESA_OP", skip = 4) %>% 
  filter(!is.na(`Orientación de la devolución`))

c_objetivo <-  read_excel(   "B31-DGMV-RRHH-DEVOLUCIONES.xlsx",  sheet="C_OBJETIVO", skip = 4)%>% 
  filter(.[[3]] == "Líder a Colaborador/a")

c_especificas <-  read_excel(   "B31-DGMV-RRHH-DEVOLUCIONES.xlsx",  sheet="C_ESPECIFICAS", skip = 4)%>% 
  filter(.[[3]] == "Líder a Colaborador/a")%>% 
  rename("Competencia Optativa Seleccionada:" = "Seleccionar Competencia Optativa", "Competencia BONUS track definida:" = "Definí la competencia bonus track elegida")

c_liderazgo <-  read_excel(   "B31-DGMV-RRHH-DEVOLUCIONES.xlsx",  sheet="C_LIDERAZGO", skip = 4)%>% 
  filter(.[[3]] != "Interna Mesa Operativa (PMs-Coordis)") %>% 
  filter(!is.na(.[[35]])) %>% 
  mutate("PROMEDIO DESARROLLO DE PERSONAS" = `DESARROLLO DE PERSONAS : ¿Es capaz de generar adhesión y compromiso de su equipo con los resultados esperados de la gestión?`,
          "PROMEDIO PLANIFICACIÓN" = `DESARROLLO DE PERSONAS : ¿Es capaz de generar adhesión y compromiso de su equipo con los resultados esperados de la gestión?`,
          "PROMEDIO TOMA DE DECISIONES" = `TOMA DE DECISIONES: ¿Asume responsabilidad de las decisiones propias o del equipo que lidera y afronta sus consecuencias?`,
          "PROMEDIO EMPODERAMIENTO" = `EMPODERAMIENTO: ¿Construye la confianza suficiente en su equipo de trabajo para delegar responsabilidades en función del rol que cada uno tiene?`,
          across(c(34,36,38,40), ~case_when(. == "Desarrollo destacado" ~ 5,
                                           . == "Desarrollada" ~ 4,
                                           . == "En desarrollo" ~ 3,
                                           . == "Poco desarrollada" ~ 2,
                                           . == "A trabajar" ~ 1,
                                           TRUE ~ as.numeric(.)))
         )
  

#### Desde acá todo Competencias de liderazgo evaluadas de Colaborador a lider-

indx_variables_promedio <- c(34,39,43,47)
indx_variables_lya <- c(35,40,44,48)
indx_variables_a_promediar <- c(36:38,41:42,45:46,49:50)
  
c_liderazgo_c <-  read_excel("B31-DGMV-RRHH-DEVOLUCIONES.xlsx",  sheet="C_LIDERAZGO_C", skip = 4)%>% 
  #saco las evaluaciones de la mesa operativa:
  filter(.[[3]] != "Interna Mesa Operativa (PMs-Coordis)")%>% 
  #saco los que tienen la primera variable de evaluaciones Colaborador a lider vacías:
  filter(!is.na(.[[36]])) %>% 
  #reemplazo las variables categóricas por numéricas y calculo medias en las columnas creadas para eso:
  mutate(across(indx_variables_a_promediar, ~case_when(. == "Desarrollo destacado" ~ 5,
                                                      . == "Desarrollada" ~ 4,
                                                      . == "En desarrollo" ~ 3,
                                                      . == "Poco desarrollada" ~ 2,
                                                      . == "A trabajar" ~ 1,
                                                      TRUE ~ as.numeric(.))),
         "PROMEDIO DESARROLLO DE PERSONAS" = rowMeans(across(36:38), na.rm = TRUE),
         "PROMEDIO PLANIFICACIÓN" = rowMeans(across(41:42), na.rm = TRUE),
         "PROMEDIO TOMA DE DECISIONES" = rowMeans(across(45:46), na.rm = TRUE),
         "PROMEDIO EMPODERAMIENTO" = rowMeans(across(49:50), na.rm = TRUE)
         
  ) %>% 
  #uno con la nómina para tener el mail del jefe de la persona que hizo la evaluacion, porque es a quien está evaluando:
  left_join(nomina, by = c("direccion_de_correo_electronico" = "mail_por_rol_actual"))

#Agrupo observaciones para mostrar las medias de las variables desagregadas y los valores máximos y mínimos de cada una teniendo en cuenta que cada lider puede tener más de un colaborador que lo ha evaluado:
  c_liderazgo_col <- c_liderazgo_c %>% 
  group_by(mail_jefe) %>%
  #renombro las variables con números para simplificar las operaciones porque no conseguí llamarlas por índice dentro de las funciones....
  rename_with(~ as.character(seq_along(.)), everything()) %>% 
  summarise(
    across(c("34", "39", "43", "47"), ~ mean(.), .names = "{.col}"),
    across(c("35", "40","44", "48"), ~ mean(.), .names = "{.col}"),
    across(c("36":"38","41":"42","45":"46","49":"50"), ~paste0(min(.),"-",max(.)), .names = "{.col}")
    ) %>% 
  #redondeo a 2 decimales:
  mutate_if(is.numeric,round,digits = 2) %>% 
  #reordeno las variables numericamente porque sus nombres asignados respondían a su índice:
  select(order(as.numeric(colnames(.))))

  #devuelvo el nombre que tenían las variables en el df original y renombro la última:
  colnames(c_liderazgo_col) <- colnames(c_liderazgo_c[, 34:50])
  colnames(c_liderazgo_col)[18] <- "mail_por_rol_actual"
  
  c_liderazgo_col <- left_join(c_liderazgo_col, nomina, by = "mail_por_rol_actual")
  colnames(c_liderazgo_col)[19] <- "nombre_persona_a_evaluar"

#### Hasta acá todo Competencias de liderazgo evaluadas de Colaborador a lider-


c_generales <-  read_excel(   "B31-DGMV-RRHH-DEVOLUCIONES.xlsx",  sheet="C_GENERALES", skip = 4) %>% 
  filter(.[[3]] != "Interna Mesa Operativa (PMs-Coordis)")


recorte_nomina <- select(nomina,"mail_por_rol_actual","equipo_al_que_pertenece","mail_jefe" )

c_equipo <-  read_excel(   "B31-DGMV-RRHH-DEVOLUCIONES.xlsx",  sheet="C_EQUIPO", skip = 4) %>% 
  filter(.[[3]] == "Colaborador/a (a pares, líder y autoevaluación)") %>% 
  left_join( recorte_nomina, by = c("direccion_de_correo_electronico" = "mail_por_rol_actual")) 
  
                      
#PAGINA-LOGIN-------------------------------------------------------------------

loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   tags$h2("Registrate", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput("userName", placeholder="Mail por rol", label = tagList(icon("user"), "Mail por rol")),
                   passwordInput("passwd", placeholder="Contraseña", label = tagList(icon("unlock-alt"), "Contraseña mail por rol")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("login", "ENTRAR", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("Ups! Contraseña o mail incorrectos!",
                                  style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                  class = "text-center"))),
                     
                   ))
)

#UI-------------------------------------------------------------------------



header <- dashboardHeader( title = "| DEVOLUCIONES",titleWidth = 12, uiOutput("logoutbtn"))
sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
ui<-dashboardPage(header, sidebar, body, skin = "blue")


#SERVER-------------------------------------------------------------------------
server <- function(input, output, session) {
  # ... (your existing code)
  
  # Create a reactive value to store the currently selected tab in the sidebar menu
  selectedTab <- reactiveVal("devoluciones_personales_tab")
  
 
#ANTES: server <- function(input, output, session) {
  
  #REGISTRO-----------------------------------------------------------------------  
  login = FALSE
  USER <- reactiveValues(login = login)
  
  user <- reactive({
    if (USER$login == TRUE) {
      input$userName
    }
  })
  
  
  lista_colaboradores <- reactive({
    subset(nomina, mail_jefe == user(), select = rrhh_apellido_y_nombre)
  })
  
  lista_equipos <- reactive({
    subset(nomina, mail_jefe == user(), select = unique(equipo_al_que_pertenece))
  })
  
  
  observe({ 
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if (length(which(usuarixs$mail_por_rol_actual == Username)) == 1) { 
            pasmatch <- usuarixs$passod[which(usuarixs$mail_por_rol_actual == Username)]
            pasverify <- password_verify(pasmatch, Password)
            if (pasverify) {
              USER$login <- TRUE
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }    
  })
  
#LOGOUT-------------------------------------------------------------------------  
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("fas fa-sign-out-alt"), "Salir", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
  })
  
#SIDEBARPANEL-------------------------------------------------------------------    
  
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE ) {
      sidebarMenu(
        menuItem("Devoluciones Personales", tabName = "devoluciones_personales_tab", icon = icon("fas fa-people-arrows")),
        menuItem("Devoluciones Equipo", tabName = "devoluciones_equipo_tab", icon = icon("fas fa-users"))
      )
    }
  })
  
  observeEvent(input$sidebarpanel, {
    # Update the selectedTab reactive value with the currently selected tab
    selectedTab(input$sidebarpanel)
  })

 #BODY---------------------------------------------------------------------------
  output$body <- renderUI({
    if (USER$login == TRUE) {
      tagList(
        tabItems(
          # Tab 1: Devoluciones Personales
          tabItem(
            tabName = "devoluciones_personales_tab", class = "active",
            fluidRow(
              box(width = 12,
                  selectInput("colaboradorSelect", label = "Seleccionar un/a colaborador/a", choices = lista_colaboradores(), selected = NULL),
                  tabsetPanel(
                    id = "competenciasPersonales",
                    tabPanel("Competencias Generales", DT::dataTableOutput("cgeneralesTable")),
                    tabPanel("Competencias Especificas y Bonus", DT::dataTableOutput("cespecificasTable")),
                    tabPanel("Objetivo SMART", DT::dataTableOutput("cobjetivoTable")),
                    tabPanel("Competencias de liderazgo", DT::dataTableOutput("cliderazgoTable")),
                    tabPanel("Mesa Operativa", DT::dataTableOutput("mesaOpTable")),
                    tabPanel("Observaciones", DT::dataTableOutput("observacionesTable"))#,
                    #h4("Esta información es solo una guía para orientar la conversación en las instancias presenciales de devolución")
                  )
              )
            )
          ),
          # Tab 2: Devoluciones Equipo
          tabItem(
            tabName = "devoluciones_equipo_tab", class = "active",
            fluidRow(
              box(width = 12,
                  selectInput("equipoSelect", label = "Seleccionar un equipo", choices = lista_equipos(), selected = NULL),
                  tabsetPanel(
                    id = "competenciasEquipo",
                    tabPanel("Competencias Equipo", DT::dataTableOutput("cEquipoTable"))#,
                  #h5("Esta información es solo una guía para orientar la conversación en las instancias Equipo de devolución")
              )
            )
          )
      )))
    } else {
      loginpage
    }
  })

  
#TAB COMP GENERALES----------------------------------------------
  
  output$cgeneralesTable <- DT::renderDataTable({
    
    #Defino selectedColaborador segun el input seleccionado en el input:
    selectedColaborador <- input$colaboradorSelect
    
    #Subset de c_generales según persona a evaluar, después transpose para que queden las competencias como filas y después hago que la columna 1 (rownames) sea una columna con el nombre "COMPETENCIAS":
    
    c_generales_auto <- subset(c_generales, nombre_persona_a_evaluar == selectedColaborador & 
                                 persona_a_evaluar == direccion_de_correo_electronico,
                                 select = 34:45) 
    
    c_generales_auto <- data.frame(t(c_generales_auto)) %>% 
      rownames_to_column("COMPETENCIAS GENERALES")
    
    if (ncol(c_generales_auto) >= 2) {
      c_generales_auto <- c_generales_auto %>% 
        rename("AUTOEVALUACIÓN" = 2)
    }

   
    #Eso era para las autoevaluaciones, ahora hago lo mismo para las evaluaciones de lider a colab:
    
    c_generales_lac <- subset(c_generales, nombre_persona_a_evaluar == selectedColaborador & 
                                           persona_a_evaluar != direccion_de_correo_electronico, 
                                           select = 34:45) 
   
     c_generales_lac <- data.frame(t(c_generales_lac)) %>% 
      rownames_to_column("COMPETENCIAS GENERALES")
    
    if (ncol(c_generales_lac) >= 2) {
      c_generales_lac <- c_generales_lac %>% 
        rename("DEVOLUCIÓN LIDER" = 2)
    }
    
     if (ncol(c_generales_lac) >= 2) {
          c_generales_tot <- left_join(c_generales_auto, c_generales_lac, by = "COMPETENCIAS GENERALES")}
     else { c_generales_tot <- filter(c_generales_auto,1 == "Ñ") }
   
    
    DT::datatable(c_generales_tot, options = list(
      width = "80%",
      searching = FALSE,
      rownames = FALSE,
      pageLength = -1,
      ###scrollY = 600,
      bPaginate = FALSE,
      dom = 'btp',
      language = list(zeroRecords = "No existen datos para los requerimientos seleccionados"),
      columnDefs = list(
        list(width = '400px', targets = 1),
        list(width = '2px', targets = 1),
        list(width = '6px', targets = 0),
        list(className = "dt-center", targets = 2:3)
      )
    ))
  })
  
  
  
  observeEvent(input$colaboradorSelect, {
    # Update the selected tab to "Competencias Generales"
    updateTabsetPanel(session, "competenciasPersonales", selected = "Competencias Generales")
  })
  
#TAB COMP ESPECIFICAS Y BONUS------------------------------------
  
  output$cespecificasTable <- DT::renderDataTable({
    
    #Defino selectedColaborador segun el input seleccionado en el input:
    selectedColaborador <- input$colaboradorSelect
    
    #COMO NO HAY AUTEVALUACIONES DE ESPECIFICAS Y BONUS, HAGO SOLO DE LIDER A COLABORADOR:
    #Subset de c_especificas de lider a colaborador, después transpose para que queden las competencias como filas y después hago que la columna 1 (rownames) sea una columna con el nombre "COMPETENCIAS":
    
    c_especificas_tot <- subset(c_especificas, seleccione_el_tipo_de_evaluacion_a_realizar == "Líder a Colaborador/a" & 
                                  nombre_persona_a_evaluar == selectedColaborador, 
                                  select = 34:46) 
    c_especificas_tot <- data.frame(t(c_especificas_tot)) %>% 
      rownames_to_column("COMPETENCIAS ESPECIFICAS Y BONUS")
    
    
    #RENOMBRO LAS COLUMNAS
    if (ncol(c_especificas_tot) >= 2) {
      c_especificas_tot <- c_especificas_tot %>% 
        filter(!is.na(.[[2]])) %>%
        rename("DEVOLUCIÓN" = 2)
    } else {c_especificas_tot <- filter(c_especificas_tot,1 == "Ñ")}
    
    
    
    DT::datatable(c_especificas_tot, options = list(
      width = "75%",
      searching = FALSE,
      rownames = FALSE,
      pageLength = -1,
      #scrollY = 600,
      bPaginate = FALSE,
      dom = 'btp',
      language = list(zeroRecords = "No existen datos para los requerimientos seleccionados"),
      columnDefs = list(
        list(width = '400px', targets = 1)
      )
    ))
  })
  
  observeEvent(input$colaboradorSelect, {
    # Update the selected tab to "Competencias Especificas"
    updateTabsetPanel(session, "competenciasPersonales", selected = "Competencias Especificas")
  })
  
  
#TAB OBJETIVO SMART------------------------------------------------------
  
  output$cobjetivoTable <- DT::renderDataTable({
    
    #Defino selectedColaborador segun el input seleccionado en el input:
    selectedColaborador <- input$colaboradorSelect
    
    #COMO NO HAY AUTEVALUACIONES DE ESPECIFICAS Y BONUS, HAGO SOLO DE LIDER A COLABORADOR:
    #Subset de c_especificas de lider a colaborador, después transpose para que queden las competencias como filas y después hago que la columna 1 (rownames) sea una columna con el nombre "COMPETENCIAS":
    
    c_objetivo_tot <- subset(c_objetivo, nombre_persona_a_evaluar == selectedColaborador, select = 34:36) 
    c_objetivo_tot <- data.frame(t(c_objetivo_tot)) %>% 
      rownames_to_column("OBJETIVO Y MEDICIÓN")
    
    #RENOMBRO LAS COLUMNAS
    if (ncol(c_objetivo_tot) >= 2) {
      c_objetivo_tot <- c_objetivo_tot %>%
        rename("DEFINICIONES" = 2) %>% 
        filter(!is.na("DEFINICIONES"))
    } else {
      c_objetivo_tot <- filter(c_objetivo_tot,"OBJETIVO Y MEDICIÓN" == "Ñ")
    } 
    
    
    DT::datatable(c_objetivo_tot, options = list(
      width = "75%",
      searching = FALSE,
      rownames = FALSE,
      pageLength = 30,
      ##scrollY = 600,
      bPaginate = FALSE,
      dom = 'btp',
      language = list(zeroRecords = "No existen datos para los requerimientos seleccionados"),
      columnDefs = list(
        list(width = '300px', targets = 1)
      )
    ))
  })
  
  observeEvent(input$colaboradorSelect, {
    # Update the selected tab to "Objetivo SMART"
    updateTabsetPanel(session, "competenciasPersonales", selected = "Objetivo SMART")
  })
  
#TAB COMP DE LIDERAZGO-------------------------------------------
  
  output$cliderazgoTable <- DT::renderDataTable({
    
    #Defino selectedColaborador segun el input seleccionado en el input:
    selectedColaborador <- input$colaboradorSelect
    
    #Subset de c_liderazgo del lider a colaborador, después transpose para que queden las competencias como filas y después hago que la columna 1 (rownames) sea una columna con el nombre "COMPETENCIAS":
    c_liderazgo_l <- subset(c_liderazgo, 
                            nombre_persona_a_evaluar == selectedColaborador & 
                            persona_a_evaluar != direccion_de_correo_electronico, 
                            select = c(34:41)) 
    
    c_liderazgo_l <- data.frame(t(c_liderazgo_l)) %>% 
      rownames_to_column("COMPETENCIAS LIDERAZGO")
    
    if (ncol(c_liderazgo_l) >= 2) {
      c_liderazgo_l <- c_liderazgo_l %>%
        rename("LIDER A COLAB" = 2)
    } 
    
    #Ahora hago lo mismo para las autoevaluaciones de competencias de liderazgo que están en la misma hoja:
    c_liderazgo_a <- subset(c_liderazgo, 
                            nombre_persona_a_evaluar == selectedColaborador & 
                            persona_a_evaluar == direccion_de_correo_electronico, 
                            select = c(34:41)) 
    
    c_liderazgo_a <- data.frame(t(c_liderazgo_a)) %>% 
      rownames_to_column("COMPETENCIAS LIDERAZGO")
    
    if (ncol(c_liderazgo_a) >= 2) {
      c_liderazgo_a <- c_liderazgo_a %>%
        rename("AUTOEVALUACIÓN" = 2)
    }
    
    #Ahora hago lo mismo de colaborador a lider que están en otra hoja:
    c_liderazgo_col <- subset(c_liderazgo_col, 
                              nombre_persona_a_evaluar == selectedColaborador, 
                              select = c(1:17))
    
    c_liderazgo_col <- data.frame(t(c_liderazgo_col)) %>% 
      rownames_to_column("COMPETENCIAS LIDERAZGO")
    
    if (ncol(c_liderazgo_col) >= 2) {
      c_liderazgo_col <- c_liderazgo_col %>%
        rename("COLABORADORES A LIDER" = 2)
    }
    
    if (ncol(c_liderazgo_col) >= 1) {
    c_liderazgo_tot <- full_join(c_liderazgo_col, c_liderazgo_a, by = "COMPETENCIAS LIDERAZGO" )
    
    c_liderazgo_tot <- full_join(c_liderazgo_tot,c_liderazgo_l, by = "COMPETENCIAS LIDERAZGO")
    }
    
    else {
      c_liderazgo_tot <- filter(c_liderazgo_col, 1 == "Ñ")
    }
  
    
    #Filtro las filas donde no hay respuestas (porque son las de PM a Coordi o viceversa y no corresponden): 
     #if (ncol(c_liderazgo_tot) < 2) {
      #c_liderazgo_tot <- filter(c_liderazgo_tot,"COMPETENCIAS LIDERAZGO" == "Ñ")
    #}  
    
    DT::datatable(c_liderazgo_tot, options = list(
      width = "75%",
      searching = FALSE,
      rownames = FALSE,
      pageLength = -1,
      ##scrollY = 600,
      bPaginate = FALSE,
      dom = 'btp',
      language = list(zeroRecords = "No existen datos para los requerimientos seleccionados"),
      columnDefs = list(
        list(width = '600px', targets = 1)#,
        #list(className = "dt-center", targets = 2:3)
      )
    ))
  })
  
  
  
  observeEvent(input$colaboradorSelect, {
    # Update the selected tab to "Competencias de liderazgo"
    updateTabsetPanel(session, "competenciasPersonales", selected = "Competencias de liderazgo")
  })
  
#TAB MESA OPERATIVA----------------------------------------------------
  
  output$mesaOpTable <- DT::renderDataTable({
    
    #Defino selectedColaborador segun el input seleccionado en el input:
    selectedColaborador <- input$colaboradorSelect
    
    #Subset de c_mesa_op según persona a evaluar, después transpose para que queden las competencias como filas y después hago que la columna 1 (rownames) sea una columna con el nombre "COMPETENCIAS":
    dev_mesa_op <- subset(c_mesa_op, nombre_persona_a_evaluar == selectedColaborador, select = c(36:42, 44:50))
    
    dev_mesa_op <- data.frame(t(dev_mesa_op)) %>% 
      rownames_to_column("COMPETENCIAS")
    #Filtro las filas donde no hay respuestas (porque son las de PM a Coordi o viceversa y no corresponden): 
    if (ncol(dev_mesa_op) >= 2) {
      dev_mesa_op <- dev_mesa_op %>%
        filter(!is.na(.[[2]]))
    } else { if (ncol(dev_mesa_op) < 2) {
      dev_mesa_op <- filter(dev_mesa_op,COMPETENCIAS == "Ñ")
    }}
    
    
    DT::datatable(dev_mesa_op, options = list(
      width = "75%",
      searching = FALSE,
      rownames = FALSE,
      pageLength = -1,
      ##scrollY = 600,
      bPaginate = FALSE,
      dom = 'btp',
      language = list(zeroRecords = "No existen datos para los requerimientos seleccionados"),
      columnDefs = list(
        list(width = '600px', targets = 1),
        list(width = '6px', targets = 0)
      )
    ))
  })
  
  observeEvent(input$colaboradorSelect, {
    # Update the selected tab to "Mesa Operativa"
    updateTabsetPanel(session, "competenciasPersonales", selected = "Mesa Operativa")
  })
  
#TAB OBSERVACIONES------------------------------------------------------

  output$observacionesTable <- DT::renderDataTable({
    
    #Defino selectedColaborador segun el input seleccionado en el input:
    selectedColaborador <- input$colaboradorSelect
    
    #COMO NO HAY AUTEVALUACIONES DE ESPECIFICAS Y BONUS, HAGO SOLO DE LIDER A COLABORADOR:
    #Subset de c_especificas de lider a colaborador, después transpose para que queden las competencias como filas y después hago que la columna 1 (rownames) sea una columna con el nombre "COMPETENCIAS":
    
    c_observaciones_tot <- subset(c_observaciones, nombre_persona_a_evaluar == selectedColaborador, select = 34)
    c_observaciones_tot <- data.frame(c_observaciones_tot) %>% 
    filter(!is.na(.[[1]]))

    
    DT::datatable(c_observaciones_tot, options = list(
      width = "75%",
      pageLength = 10,
      searching = FALSE,
      ###scrollY = 600,
      bPaginate = FALSE,
      dom = 'btp',
      language = list(zeroRecords = "No existen datos para los requerimientos seleccionados"),
      columnDefs = list(
        list(width = '600px', targets = 1),
        list(width = '6px', targets = 0)))
    )
  })
  
  observeEvent(input$colaboradorSelect, {
    # Update the selected tab to "Observaciones"
    updateTabsetPanel(session, "competenciasPersonales", selected = "Observaciones")
  })
  
  
#TAB DEVOLUCIONES Equipo------------------------------------------------------
  
  output$cEquipoTable <- DT::renderDataTable({
  
  #Defino selectedColaborador segun el input seleccionado en el input:
  selectedEquipo <- input$equipoSelect
  
  #COMO NO HAY AUTEVALUACIONES DE ESPECIFICAS Y BONUS, HAGO SOLO DE LIDER A COLABORADOR:
  #Subset de c_especificas de lider a colaborador, después transpose para que queden las competencias como filas y después hago que la columna 1 (rownames) sea una columna con el nombre "COMPETENCIAS":
  
  c_equipo_tot <- subset(c_equipo, equipo_al_que_pertenece == selectedEquipo, select = 34:42)
  c_equipo_tot <- data.frame(t(c_equipo_tot)) %>% 
    rownames_to_column("COMPETENCIAS")
  
  
  
  DT::datatable(c_equipo_tot, options = list(
  width = "75%",
  rownames = FALSE,
  pageLength = -1,
  searching = FALSE,
  scrollY = 800,
  bPaginate = FALSE,
  dom = 'btp',
  language = list(zeroRecords = "No existen datos para los requerimientos seleccionados"),
  columnDefs = list(
    list(width = '400px', targets = 1),
    list(width = '6px', targets = 0))
  ))
  })
  
  observeEvent(input$colaboradorSelect, {
   #Update the selected tab to "Competencias Equipo"
   updateTabsetPanel(session, "competenciasEquipo", selected = "Competencias Equipo")
 })
  

  #esta ultima llave va solo en lo ultimo que defino antes de runApp, no en cada tab.
}


# Run the application 
shinyApp(ui = ui, server = server)
 

