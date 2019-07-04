#####################################################################
### @Author: Sergio Fernández Rubio @ Eléctronica Martínez        ###
### @Version: 0.1.6                                               ###
### @Comments: WebApp for giving Internet access to a new client  ###
#####################################################################

# Para activar el servidor, copiar la siguiente línea (sin comentar) Por defecto escucha en el puerto 8080
# shiny::runApp(appDir = "/home/tecnico/WebApp", launch.browser = FALSE, port = 8080, host = "0.0.0.0")

# Para hacer un update de los paquetes utilizados, copiar la siguiente línea (sin comentar)
# update.packages()

# Para instalar los paquetes (para nueva instalación), copiar la siguiente línea (sin comentar)
# install.packages(c("shiny", "stringr")) 
library(shiny)
library(shinyjs)


options(shiny.trace=TRUE) # Activamos los logs

shinyUI(fluidPage(useShinyjs(),
  #tags$title("Shiny WebApp"),
  navbarPage(title="Router Express", id="navbar", position="static-top", inverse=F, theme = "bootstrap.css",
             #collapsible=T, #icon="ico_basesdatos.ico",
             
    ############################################# XGEST - RADIUS MANAGER - MAC ####################################################
    ###############################################################################################################################
    
    tabPanel("Sincronizador XGEST-RADIUS-ACS", value=1,
      
       fluidRow(
         sidebarPanel(width = 5,
                         
                  tags$legend("Sincronizador Xgest-Radius-ACS-Tickets"), 
                              
                  textInput("client1", "Código de cliente: "), 
                  selectInput("selectExt", "Número de conexión: ", c("Sin extensión" = 1, 2:30)), 
                  uiOutput("direccion1"),
                  uiOutput("direccion2"),
                  uiOutput("direccion3"),
                  uiOutput("service"),
                  
                  dateInput('dateExp',
                            label = 'Fecha de expiración del servicio: (En blanco: Fecha de expiración en 2020)',
                            value = "", language = "es"),
                  
                  radioButtons(
                    inputId="select",
                    label="¿Quiere crear el archivo de configuración MAC?",
                    choices=list(
                      "Sí",
                      "No"
                    ),
                    selected="No",
                    inline=TRUE),
                  
                  radioButtons(
                    inputId="selectTICKET",
                    label="¿Quiere crear e incorporar ticket? (~15 segundos de espera adicionales)",
                    choices=list(
                      "Sí",
                      "No"
                    ),
                    selected="No",
                    inline=T),
                  
                  
                  actionButton("sincronizar", "--  Sincronizar todo  --", icon("paper-plane")), br(), br(),
                    
                  textOutput("errores"),
                  textOutput("erroresHS")

         ), br(),
         mainPanel(width = 5,
                   uiOutput("macSelect"),
                   uiOutput("particularEmpresas"),
                   splitLayout(
                     uiOutput("telefono2"),
                     uiOutput("FaxoTlfn")
                   ),
                   uiOutput("pass_SIP"),
                   splitLayout(
                     uiOutput("telefono2_ext2"),
                     uiOutput("FaxoTlfn_ext2")
                   ),
                   uiOutput("pass_SIP_ext2"),
                   uiOutput("mac1"),
                   uiOutput("selectLanIP"),
                   uiOutput("mac2"),
                   uiOutput("mac3"), br(),
                   uiOutput("macOK"), br(),
                   tags$textarea(id="CampoNotas", rows=5, cols=60, 
                                 placeholder="Incluya aquí notas adicionales, se incluirán en el Radius Manager PPPoE",
                                 value = ""), br(),
                   uiOutput("ticketLocalizacion"),
                   uiOutput("ticketUbicacion"),
                   uiOutput("TICKETtipo"),
                   uiOutput("CampoNotasTicket"),  br(),
                   a("FAQ: Frequently Asked Questions", target="_blank", href="http://OWN.IP.LOCAL.HOST:8787/files/WebApp/FAQ/FAQ.html"), br(), br(),
                   uiOutput("numeroTICKET"),
                   uiOutput("TextoTelegram"),
                   uiOutput("TextoPPPoE"),
                   uiOutput("errorNoExistenteXGEST")
         )
       )
      
    ),
    
    ############################################### ARCHIVOS DHCP66 TENDA ###############################################################
    #####################################################################################################################################
    
    tabPanel("Archivos Tenda W308R", value=2,
      
      sidebarLayout(
        sidebarPanel(width = 5,
                            
                tags$legend("Configuración Routers Tenda W308R"),
                
                textInput("client", "Código de cliente (Para la extensión de contraseña WiFi): "),
                textInput("initials", "Iniciales del cliente (Para el SSID WiFi): "),
                textInput("user", "Usuario PPPoE: "),
                textInput("pass", "Contraseña PPPoE: "),
                textInput("mac", "MAC del Router a instalar (Importante que sea correcto): ", value="C83A35")
                            
           ),
               
       mainPanel(width = 5, 
           h3("Resultado: "),
           textOutput("client"),
           textOutput("initials"),
           textOutput("user"),
           textOutput("pass"),
           textOutput("mac"), br(),
           actionButton("upload", "Subir al Servidor TFTP"), br(), br(),
           textOutput("ok"), 
           h4("Descargar archivo para configuración manual: "),
           downloadButton("downloadData", "Descarga")
           )
       )
            
    ),
    
    ############################################### ARCHIVOS DHCP66 GS ###############################################################
    ##################################################################################################################################
    
    tabPanel("Archivos Grandstream HT502", value=3,
             
             fluidRow(
               sidebarPanel(width = 5,
             
                            tags$legend("Configuración Routers de voz GS HT502"),
                            
                            textInput("nombreGS", "Nombre del Cliente: "), 
                            textInput("userGS", "Usuario PPPoE: "), 
                            textInput("passGS", "Contraseña PPPoE: "), 
                            textInput("telefonoGS", "Número de teléfono asociado: "),
                            textInput("passSIPGS", "Contraseña SIP: "),
                            radioButtons(
                              inputId="selectParticularEmpresaGS",
                              label="¿Es un particular o una empresa?",
                              choices=list(
                                "Particular",
                                "Empresa"
                              ),
                              selected="Particular",
                              inline=TRUE),
                            textInput("macGS", "MAC: ", value="000b82")
                            
                            
               ),
               
               mainPanel(width = 5, 
                         h3("Resultado: "),
                         textOutput("nombreGS"),
                         textOutput("userGS"),
                         textOutput("passGS"),
                         textOutput("telefonoGS"),
                         textOutput("passSIPGS"),
                         textOutput("selectParticularEmpresaGS"),
                         textOutput("macGS"), br(),
                         actionButton("uploadGS", "Subir al Servidor HTTP"), br(), br(),
                         textOutput("okGS"), 
                         h4("Descargar archivo para configuración manual: "),
                         downloadButton("downloadDataGS", "Descarga")
               )
             )
    ),
    tabPanel("Prueba de equipos", value=4,
             sidebarPanel(width = 5,
                radioButtons("pruebaSelect", "Elija Router para crear archivo:", c("Tenda W308R", "Grandstream HT502"), selected = "Tenda W308R", inline = T),
                uiOutput("pruebaMAC"),
                actionButton("probarEquipo", "Probar Equipo", icon("plug", lib = "font-awesome")),
                hr(),
                radioButtons("borraMACelige", "Elija Router para borrar archivo MAC:", c("Tenda W308R", "Grandstream HT502"), selected = "Tenda W308R", inline = T),
                uiOutput("borraMAC"),
                actionButton("borrarMAC", "Borrar archivo", icon("trash", lib = "font-awesome"))
                
            ),
            
            mainPanel(
                uiOutput("okPRUEBA")
                
            )
    )
  )

))