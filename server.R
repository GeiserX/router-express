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
# install.packages(c("shiny", "RMySQL", "digest", "stringr")) 

library(shiny)
library(RMySQL)
library(digest)
library(stringr)

options(shiny.trace=TRUE) # Activamos los logs en la consola de abajo
setwd("/home/tecnico/WebApp")

limpiaMAC <- function(x) return( str_replace_all(str_replace_all(str_replace_all(x, ":", ""), "-", ""), " ", "") )
limpiaESPACIOS <- function(x) return(str_replace_all(x, " ", ""))

###### SERVICIOS CSV ######
radiusS <- dbConnect(MySQL(), user="root", password="PASSWORD", db="radius", host="RA.DI.US.IP")
rm_services <- dbGetQuery(radiusS, "select srvid, srvname from rm_services")
dbDisconnect(radiusS)
rm_services <- rm_services[!row.names(rm_services) %in% grep("^srvid", rm_services$srvname),]
write.csv(rm_services, file = "services.csv")

###### LOCALIZACIONES CSV ######

# Las anteriores líneas se ejecutan sólo 1 vez, cuando se arranca el servidor

shinyServer(function(input, output, session) {
  
  # Para la lista desplegable de selección de MBs contratados

  output$service <- renderUI({
    rm_services <- read.csv("services.csv")
    choices <- setNames(rm_services$srvid, rm_services$srvname)
    selectInput("service", "Servicio contratado: ", choices)
  })

  output$direccion1 <- renderUI({
    if(as.numeric(input$selectExt) >= 2){
      textInput("direccion11", "Dirección de la instalación: ")
    }
  })
  
  output$direccion2 <- renderUI({
    if(as.numeric(input$selectExt) >= 2){
      textInput("direccion22", "Ciudad: ")
    }
  })
  
  output$direccion3 <- renderUI({
    if(as.numeric(input$selectExt) >= 2){
      textInput("direccion33", "Región: ")
    }
  })

  
 
  # Para la aparición de los cuadros de MAC cuando se ha seleccionado "Sí":
  output$macSelect <- renderUI({
    if (input$select == "No")
      return()
    
    else radioButtons(
      inputId="selectRouter",
      label="Elige el tipo de Router",
      choices=list(
        "Tenda W308R",
        "Grandstream HT502",
        "Tenda W300D"
      ),
      selected="Tenda W308R",
      inline=TRUE)
    })
  
  output$particularEmpresas <- renderUI({
    if (input$select == "Sí" && !is.null(input$selectRouter)){
      if (input$selectRouter == "Grandstream HT502"){
        radioButtons(
          inputId="selectParticularEmpresa",
          label="¿Es un particular o una empresa?",
          choices=list(
            "Particular",
            "Empresa"
          ),
          selected="Particular",
          inline=TRUE)
      }
      else return()
    }
    else return()
  })
  
  output$telefono2 <- renderUI({
    if(as.numeric(input$service) == 23  && !is.null(input$service)){
      textInput("telefono", "Introduzca el Nº de teléfono: ")
    }
    else if (input$select == "Sí"  && !is.null(input$selectRouter)){
      
      if (input$selectRouter == "Grandstream HT502"){
          textInput("telefono", "Introduzca el Nº de teléfono: ")

      }
      else return()
      
    }
    else return()
  })
  
  output$FaxoTlfn <- renderUI({

    if (input$select == "Sí"  && !is.null(input$selectRouter)){
      
      if (input$selectRouter == "Grandstream HT502"){
        radioButtons("FaxoTlfn1", label = NULL, choices = c("Teléfono", "Fax/Alarma"), inline = T)
        
      }
      else return()
      
    }
    else return()
  })
  
  output$pass_SIP <- renderUI({
    if (input$select == "Sí"  && !is.null(input$selectRouter)){
      
      if (input$selectRouter == "Grandstream HT502")
          textInput("passSIP", "Introduzca el secret de Asterisk: ")
        
        else return()
      
    }
  })
  
  output$telefono2_ext2 <- renderUI({
    if (input$select == "Sí"  && !is.null(input$selectRouter)){
      
      if (input$selectRouter == "Grandstream HT502"){
        textInput("telefono_ext2", "Introduzca el Nº de teléfono (2ª): ")
        
      }
      else return()
      
    }
    else return()
  })
  
  output$FaxoTlfn_ext2 <- renderUI({
    if (input$select == "Sí"  && !is.null(input$selectRouter)){
      
      if (input$selectRouter == "Grandstream HT502"){
        radioButtons("FaxoTlfn2", label = NULL, choices = c("Teléfono", "Fax/Alarma"), inline = T)
        
      }
      else return()
      
    }
    else return()
  })
  
  output$pass_SIP_ext2 <- renderUI({
    if (input$select == "Sí"  && !is.null(input$selectRouter)){
      
      if (input$selectRouter == "Grandstream HT502")
        textInput("passSIP_ext2", "Introduzca el secret de Asterisk (2ª): ")
      
      else return()
      
    }
  })
  
  
  output$mac1 <- renderUI({
    if (input$select == "No")
      return()
    
    else if (input$selectRouter == "Tenda W308R"  && !is.null(input$selectRouter)) {
      textInput("MAC_1", "Introduzca la MAC del Router: ", value="C83A35")
    }
    else if (input$selectRouter == "Grandstream HT502" && !is.null(input$selectRouter)) {
      rm_services <- read.csv("services.csv")
      choices <- setNames(rm_services$srvid, rm_services$srvname)
      updateSelectInput(session, "service", "Servicio contratado: ", choices, selected = "Telefonia")
      textInput("MAC_1", "Introduzca la MAC del Router: ", value="000B82")
    }
    else if (input$selectRouter == "Tenda W300D" && !is.null(input$selectRouter)) {
      textInput("MAC_1", "Introduzca la MAC del Router: ", value="C83A35")
    }
  })

  output$selectLanIP <- renderUI({
    if (input$select == "No")
      return()
    else if (input$selectRouter == "Tenda W308R"  && !is.null(input$selectRouter)) {
      radioButtons(
        inputId="LanIP",
        label="Escoja LAN IP",
        choices=list(
          "192.168.0.1",
          "192.168.1.1"
        ),
        selected="192.168.0.1",
        inline=TRUE)
    }
  })
  
  
  output$mac2 <- renderUI({
    if (input$select == "No" )
      return()
    
    else if(!is.null(input$selectRouter))
      if (input$selectRouter == "Tenda W300D")
        return()
    
    else h4("Descargar archivo para configuración manual: ")
  })

  output$mac3 <- renderUI({
    if (input$select == "No" )
      return()
    
    else if(!is.null(input$selectRouter))
      if (input$selectRouter == "Tenda W300D")
        return()
    
    else downloadButton("downloadData2", "Descarga")
  })
  
  output$macOK <- renderUI({
    textOutput("erroresMAC")
  })
  
  output$downloadData2 <- downloadHandler(
    filename = function() {
      if(input$selectRouter == "Grandstream HT502") MAC_GS else MACapp
    },
    content = function(file) {
      if(input$selectRouter == "Grandstream HT502") saveXML(archivo, file, prefix = '<?xml version="1.0" encoding="UTF-8"?>')
      else write(archivo, file)
    }
  )
  
  output$CampoNotasTicket <- renderUI({
    if(input$selectTICKET == "No"){
      return()
    }
    else{
      tags$textarea(id="CampoNotasTICKETS", rows=5, cols=60,
                    placeholder="Incluya aquí notas adicionales de tickets, se incluirán en la BBDD Web Help Desk",
                    value = "") 
    }
  })
  
  output$TICKETtipo  <- renderUI({
    if(input$selectTICKET == "No"){
      return()
    }
    else{
      radioButtons(
        inputId="tipoTICKETui",
        label="Elija tipo de ticket: ",
        choices=list(
          "Nueva instalación",
          "Infraestructura"
        ),
        selected="Nueva instalación",
        inline=T)
    }
  })
  
  
  
  output$ticketLocalizacion <- renderUI({
    if(input$selectTICKET == "No"){
      return()
    }
    else{
      library("RPostgreSQL")
      drv <- dbDriver("PostgreSQL")
      con <- dbConnect(drv, user="whd", password="whd", dbname="whd", host="helpdesk.app.example", port=20293)
      query <- dbGetQuery(con, "SELECT location_name, location_id FROM location ORDER BY location_name")
      dbDisconnect(con)
      #query <- query[[1]][!grepl("^0", query[[1]])]
      query <- query[!grepl("^0", query[[1]]),]
      choices <- setNames(query[,2], query[,1])
      selectInput("ticketLocalizacion1", "Localización: ", choices)
    }
  })
  
  output$ticketUbicacion <- renderUI({
    if(input$selectTICKET == "No"){
      return()
    }
    else{
       library("RPostgreSQL")
       drv <- dbDriver("PostgreSQL")
       con <- dbConnect(drv, user="whd", password="whd", dbname="whd", host="helpdesk.app.example", port=20293)
       queryUBI <- dbGetQuery(con, "SELECT location_id, room_name FROM room ORDER BY room_name")
       dbDisconnect(con)
       queryUBI <- queryUBI[queryUBI[,1] == input$ticketLocalizacion1, 2]
       queryUBI <- c("Sin especificar", queryUBI) # Deja poner character() pero luego no se muestra
       selectInput("ticketUbicacion1", "Ubicación: ", queryUBI)
    }
  })

    
    observeEvent(
      input$sincronizar,{
        
        if(input$client1 != ""){
          
          disable("sincronizar")
          
          withProgress(message = 'Ejecutando R-Express', value = 0, {
        
        
        ###############################################################################
        ############################ PARTE LLAMADA A XGEST ############################
        ###############################################################################
            
            incProgress(1/6, detail = "Llamando XGEST")
            
            print(Sys.time())
            print("Botón pulsado, llamando a XGEST...")
            
            xgest <- dbConnect(MySQL(), user="tecnico", password="PASSWORD", db="xgestevo", host="XG.ES.T.IP", port=3307)
            usuario_elegido <- dbGetQuery(xgest, sprintf("select * from fccli001 WHERE CCODCL = '%s'", input$client1))
            
            if(dim(usuario_elegido)[1] == 0){
              output$errorNoExistenteXGEST <- renderUI({ h3("No existe en XGEST", style = "color: red")})
            } else {
              Encoding(usuario_elegido$CDOM) <- "latin1"
              Encoding(usuario_elegido$CNOM) <- "latin1"
              
              dbDisconnect(xgest)
              
            
            ##################################################################################
            ########################## PARTE RADIUS MANAGER PPPoE ############################
            ##################################################################################
            
              #incProgress(2/6, detail = "Llamando a RADIUS MANAGER")
              
              print("Llamando a Radius Manager PPPoE")
              
              radius <- dbConnect(MySQL(), user="root", password="PASSWORD", db="radius", host="RA.DI.US.IP") # RADIUS SERVER
              radcheck <- dbGetQuery(radius, "select username, id from radcheck ORDER BY id")
              rm_users <- dbGetQuery(radius, "select username from rm_users")
              
      
              numeroCliente <- input$client1
              # Concatenamos cliente con su extensión/conexión
              if(as.numeric(input$selectExt) >= 2){
                numeroCliente <- paste(as.numeric(input$client1), as.numeric(input$selectExt), sep = "e")
              }
              if(as.numeric(input$service) == 23){
                numeroCliente <- paste0(numeroCliente, "t")
              }
              
              numeroClienteSinExtension <- input$client1
              
              # Si no existe el usuario, lo creamos en radcheck, si existe, extraemos su contraseña y la sobreescribimos de passPlain
              if (!(numeroCliente %in% radcheck$username)){
                #set.seed(numeroClienteSinExtension)
                passPlain <- paste(sample(c(0:9, letters[-12], LETTERS[-9]), size=8, replace=TRUE), collapse="") # Exceptuamos las letras "l" e "I", dan lugar a confusión
                dbSendQuery(radius, 
                            sprintf("insert into radcheck (id, username, attribute, op, value) values (%i, '%s', 'Cleartext-Password', ':=', '%s')",
                                    tail(radcheck$id, n=1)+1, numeroCliente, passPlain))
                dbSendQuery(radius, 
                            sprintf("insert into radcheck (id, username, attribute, op, value) values (%i, '%s', 'Simultaneous-Use', ':=', '1')",
                                    tail(radcheck$id, n=1)+2, numeroCliente))
              }
              else{
                passPlainExistent <- dbGetQuery(radius, sprintf("select attribute, value from radcheck WHERE username = '%s'", numeroCliente))
                passPlainExistent <- passPlainExistent[passPlainExistent$attribute == "Cleartext-Password",]
                passPlain <- passPlainExistent$value
              }
              
              # Calculamos MD5
              passCipher <- digest(passPlain, algo="md5", ascii = F, serialize = F)
              
              # Calculamos fecha de expiración
              if(as.character(input$dateExp) == Sys.Date()) dateExp <- "2020-01-01"
              else dateExp <- as.character(input$dateExp)
              
              username <- numeroCliente
              password <- passCipher
              enableuser <- 1
              firstname <- usuario_elegido$CNOM
              lastname <- if(as.numeric(input$service) == 23) input$telefono else ""
              phone <- usuario_elegido$CTEL1
              mobile <- usuario_elegido$CTEL2
              address <- gsub(x = gsub(x = usuario_elegido$CDOM, pattern = ",", replacement = ""), pattern = "'", replacement = "")
              city <- usuario_elegido$CPOB
              zip <- usuario_elegido$CCODPO
              country <- "Spain"
              state <- usuario_elegido$CPAIS
              comment <- sprintf("Contrasena: %s\n%s", passPlain, input$CampoNotas)
              expiration <- sprintf("%s 00:00:00", dateExp)
              srvid <- input$service 
              createdon <- Sys.Date()
              createdby <- "sincronizador"
              owner <- "sincronizador"
              
              if(as.numeric(input$selectExt) >= 2){
                address <- gsub(x = gsub(x = input$direccion11, pattern = ",", replacement = ""), pattern = "'", replacement = "")
                city <- input$direccion22
                state <- input$direccion33
              }
              
              # Si existe, no reemplazar
              if(!(numeroCliente %in% rm_users$username)){
                dbSendQuery(radius, 
                            sprintf("INSERT INTO rm_users (username, password, enableuser,
                                    firstname, lastname, phone, mobile, address, city, zip, country, state, comment, 
                                    expiration, srvid, createdon, createdby, owner) 
                                    SELECT '%s', '%s', '%s', '%s', '%s', '%s', '%s',
                                    '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s' 
                                    FROM dual 
                                    WHERE NOT EXISTS (SELECT 1 FROM rm_users WHERE username='%s')", username,
                                    password, enableuser, firstname, lastname, phone, mobile, address, city, zip,
                                    country, state, comment, expiration, srvid, createdon, createdby, owner, username))
                output$errores <- renderText("Correcto. El usuario no existía en Radius Manager PPPoE")
                
              }
              else {
                output$errores <- renderText("Error. El usuario ya existe en Radius Manager PPPoE")
              }
              
              dbDisconnect(radius)
              
              
            
              ####################################################################################
              ########################## PARTE RADIUS MANAGER HOTSPOT ############################
              ####################################################################################
        
              if(!(as.numeric(input$service) == 23)){
                
                #incProgress(3/6, detail = "Llamando a RADIUS HOTSPOT")
                
                print("Llamando a Radius Manager Hotspot")
                
                radiusHS <- dbConnect(MySQL(), user="root", password="PASSWORD", db="radius", host="RA.DI.US.IP") # RADIUS HOTSPOT SERVER
                rm_usersHS <- dbGetQuery(radiusHS, "select username from rm_users")
                radcheckHS <- dbGetQuery(radiusHS, "select username, id from radcheck ORDER BY id")
                
                if(as.numeric(input$selectExt) >= 2){
                  print("Updating el número de conexiones simultáneas hotspot")
                  numeroClienteSinExtension <- input$client1
                  UsoSimultaneo <- as.numeric(input$selectExt)
                  dbSendQuery(radiusHS, sprintf("UPDATE radcheck SET value = '%s' WHERE username = '%s' AND 
                                              attribute = 'Simultaneous-Use'", UsoSimultaneo, numeroClienteSinExtension))
                  pass <- dbGetQuery(radiusHS, sprintf("SELECT value FROM radcheck WHERE username = '%s' AND
                                                       attribute = 'Cleartext-Password'", numeroClienteSinExtension))
                  pass <- pass[[1]]
                }
                else {
                  
                  # Se busca como contraseña el número móvil, y si no tiene, la contraseña es su móvil + su número de cliente
                  passPosible <- usuario_elegido$CTEL2
                  if (passPosible == "") passPosible <- usuario_elegido$CTEL1
                  if (passPosible == "") pass <- sprintf("emartinez%s", input$client)
                  else pass <- limpiaESPACIOS(passPosible)
                  
                  if (!(numeroClienteSinExtension %in% radcheckHS$username)){
                    dbSendQuery(radiusHS, 
                                sprintf("insert into radcheck (id, username, attribute, op, value) values (%i, '%s', 'Cleartext-Password', ':=', '%s')",
                                        tail(radcheckHS$id, n=1)+1, numeroCliente, pass))
                    dbSendQuery(radiusHS, 
                                sprintf("insert into radcheck (id, username, attribute, op, value) values (%i, '%s', 'Simultaneous-Use', ':=', '1')",
                                        tail(radcheckHS$id, n=1)+2, numeroCliente))
                  }
                }
                
                
                username <- numeroClienteSinExtension
                password <- digest(pass, algo="md5", ascii = F, serialize = F)
                groupid <- 9
                
                phone <- usuario_elegido$CTEL1
                if(usuario_elegido$CTEL2=="") {
                  mobile <- if(usuario_elegido$CTEL1 %in% c("0","6","7")) usuario_elegido$CTEL1 else ""
                }
          
                comment <- sprintf("Contrasena: %s", pass)
                srvid <- 70
                
                # El resto de valores ya han sido introducidos anteriormente al introducir el usuario en Radius Manager PPPoE
                
                # Si existe, no reemplazar, si proviene de una extensión, no se crea ya que en radcheck sumamos el Simultaneous-Use
                if(!(numeroClienteSinExtension %in% rm_usersHS$username)){
  
                  dbSendQuery(radiusHS, 
                              sprintf("INSERT INTO rm_users (username, password, groupid, enableuser,
                                      firstname, phone, mobile, address, city, zip, country, state, comment, 
                                      expiration, srvid, createdon, createdby, owner) 
                                      SELECT '%s', '%s', '%s', '%s', '%s', '%s', '%s',
                                      '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s' 
                                      FROM dual 
                                      WHERE NOT EXISTS (SELECT 1 FROM rm_users WHERE username='%s')", username,
                                      password, groupid, enableuser, firstname, phone, mobile, address, city, zip,
                                      country, state, comment, expiration, srvid, createdon, createdby, owner, username))
                  
                  output$erroresHS <- renderText("Correcto. El usuario no existía en Radius Manager HotSpot")
                }
                else {
                  if(as.numeric(input$selectExt) >= 2){
                    output$erroresHS <- renderText(sprintf("Correcto. Pero el usuario ya existe en Radius Manager HotSpot así que no se ha añadido 
                                                   (Ahora el usuario puede usar %s dispositivos simultáneamente)", input$selectExt))
                  }
                  else{
                    output$erroresHS <- renderText("Error. El usuario ya existe en Radius Manager HotSpot")
                  }
                }
         
                  dbDisconnect(radiusHS)
                }
              
                observe({
                  output$TextoTelegram <- renderUI({
                    verbatimTextOutput("TextoTelegrama")
                  })
              
        
                  output$TextoTelegrama <- renderText({
                    
                    iniciales <- 0
                    for (k in 1:length(str_split(firstname, " ")[[1]])){
                      iniciales[k] <- str_split(str_split(firstname, " ")[[1]], "")[[k]][1]
                    }
                    iniciales <- paste0(iniciales, collapse = '')
                    
                    if (as.numeric(input$service) != 23){
                      sprintf("- WiFi: \nSSID: EMARTINEZ_%s \nPass: PASS%s
                            \n- HotSpot: \nUser: %s \nPass: %s
                            \n- Teléfonos: \nContacto: 968165000 \nGuardia: 676966000
                            \n- PPPoE: \nUser: %s \nPass: %s ", 
                              iniciales, numeroClienteSinExtension, numeroClienteSinExtension, pass, numeroCliente, passPlain)
                    } else {
                      sprintf("- WiFi: \nSSID: EMARTINEZ_%s \nPass: PASS%s
                            \n- PPPoE: \nuser: %s \npass: %s", 
                              iniciales, numeroClienteSinExtension, numeroCliente, passPlain)
                    }
                  })
                
              })
                
            
                ####################################################################################
                ############################## PARTE BBDD TICKETS ##################################
                ####################################################################################
                
                if(input$selectTICKET == "Sí"){ 
                  
                  incProgress(2/6, detail = "BBDD Tickets")
                  
                  print("Llamando a BBDD Tickets")
                  
                  # Creación del archivo de ticket
                  ticket <- read.csv("ticket.csv", check.names = F, na.strings = "", encoding = "ASCII")
                  ticket[2] <- format(Sys.time(), "%d/%m/%y %R")
                  ticket[7] <- input$client1
    
                  library("RPostgreSQL")
                  drv <- dbDriver("PostgreSQL")
                  con <- dbConnect(drv, user="whd", password="PASSWORD", dbname="whd", host="HE.LPD.ESK.IP", port=20293)
                  query <- dbGetQuery(con, "SELECT location_name, location_id FROM location ORDER BY location_name")
                  dbDisconnect(con)
                  ticket[8] <- query[query[,2] == input$ticketLocalizacion1, 1]
                  
                  if(input$tipoTICKETui == "Nueva instalación") ticket[9] <- "Instalaciones" else ticket[9] <- "Infraestructura"
                  
                  iniciales <- 0
                  for (k in 1:length(str_split(firstname, " ")[[1]])){
                    iniciales[k] <- str_split(str_split(firstname, " ")[[1]], "")[[k]][1]
                  }
                  iniciales <- paste0(iniciales, collapse = '')
                  
                  ticketNotaAdicional <- if (as.numeric(input$service) != 23){
                    sprintf("- WiFi: \nSSID: EMARTINEZ_%s \nPass: PASSWORD%s
                            \n- HotSpot: \nUser: %s \nPass: %s
                            \n- PPPoE: \nUser: %s \nPass: %s ", 
                            iniciales, numeroClienteSinExtension, numeroClienteSinExtension, pass, numeroCliente, passPlain)
                  } else {
                    sprintf("- WiFi: \nSSID: EMARTINEZ_%s \nPass: PASSWORD%s
                            \n- PPPoE: \nuser: %s \npass: %s", 
                            iniciales, numeroClienteSinExtension, numeroCliente, passPlain)
                  }
                  
                  ticket[12] <- paste(input$CampoNotasTICKETS, ticketNotaAdicional, sep = "\n")
                  ticket[13] <- "avisos"
                  ticket[16] <- if(input$ticketUbicacion1 == "Sin especificar") "" else input$ticketUbicacion1
                  write.csv(ticket, file="ticketFIN.csv", na = "", row.names = F)
                  
                  #Creación de usuario en la BBDD
                  Nombre <- str_split(firstname, " ")[[1]][1]
                  if (is.na(str_split(firstname, " ")[[1]][2]) || str_split(firstname, " ")[[1]][2] == "") { 
                    Apellido <- "Sin-Apellido" 
                  } else if (is.na(str_split(firstname, " ")[[1]][3])) { 
                    Apellido <- str_split(firstname, " ")[[1]][2] 
                  } else {
                    Apellido <- paste(str_split(firstname, " ")[[1]][2], str_split(firstname, " ")[[1]][3])
                  }
                  Apellido <- paste(Apellido, input$client1)
                  
                  if (is.na(usuario_elegido$CMAIL1) || usuario_elegido$CMAIL1 == "") { 
                    Email <- paste0("Sin-Email-", numeroClienteSinExtension) 
                  } else { 
                    Email <- usuario_elegido$CMAIL1
                  }
                  
                  print(sprintf("python tickets.py '%s' '%s' '%s' '%s' '%s'",  Nombre, str_replace_all(string = Apellido, pattern = "Ñ", replacement = "N"), Email, input$client1, phone))
                  system(sprintf("python tickets.py '%s' '%s' '%s' '%s' '%s'",  Nombre, str_replace_all(string = Apellido, pattern = "Ñ", replacement = "N"), Email, input$client1, phone))
                  
                  # Para saber número de ticket:
                  library("RPostgreSQL")
                  drv <- dbDriver("PostgreSQL")
                  con <- dbConnect(drv, user="whd", password="whd", dbname="whd", host="HEL.PDE.SK.IP", port=20293)
                  query <- dbGetQuery(con, "SELECT client_id, job_ticket_id FROM job_ticket ORDER BY report_date DESC")
                  dbDisconnect(con)
                  
                  query <- query[query[,1] == input$client1,]
                  query <- query[complete.cases(query),]
                  numeroDeTicket <- query[1,2]
                  
                  #output$numeroTICKET <- renderText(paste0("Número de ticket: ", numeroDeTicket))
                }
              
            
            ##############################################################################
            ############################ PARTE MAC TENDA W308R ###########################
            ##############################################################################
            
            print("Creando el archivo de configuración (Si se ha pulsado el botón correspondiente)")
            
            if (input$select == "Sí"){
              
              incProgress(5/6, detail = "Creando archivo")
              
              if(input$selectRouter == "Tenda W308R"){
              
                
                tryCatch({
                  
                  archivo <<- scan("data/archivo-base-tendaW308R.cfg", what = character(), skip = 1, sep = "\n")
                  ids <- vector(length = 10)
                  ids[[1]] <- pmatch('tftp_reboot_flag=', archivo) # Parámetro de reconexión
                  ids[[2]] <- pmatch('wan0_pppoe_passwd=', archivo) # PPPoE Password
                  ids[[3]] <- pmatch('wan0_pppoe_username=', archivo) # PPPoE User
                  ids[[4]] <- pmatch('wl0_ssid=', archivo) # SSID
                  ids[[5]] <- pmatch('wl_ssid=', archivo) # SSID
                  ids[[6]] <- pmatch('wl_wpa_psk=', archivo) # WPA PASSWORD
                  ids[[7]] <- pmatch('wl0_wpa_psk=', archivo) # WPA PASSWORD
                  ids[[8]] <- pmatch('wl0_akm=', archivo) # WPA/WPA2 TYPE
                  ids[[9]] <- pmatch('wl_akm=', archivo) # WPA/WPA2 TYPE
                  ids[[10]] <- pmatch('rm_web_ip=', archivo) # Remote Web Management IP
                  
                  if(input$LanIP == "192.168.1.1"){
                    ids[[11]] <- pmatch('lan_gateway=', archivo)
                    ids[[12]] <- pmatch('dhcp_start=', archivo)
                    ids[[13]] <- pmatch('dhcp_end=', archivo)
                    ids[[14]] <- pmatch('lan_ipaddr=', archivo)
                    
                    archivo[ids[11]] <- 'lan_gateway=192.168.1.1'
                    archivo[ids[12]] <- 'dhcp_start=192.168.1.100'
                    archivo[ids[13]] <- 'dhcp_end=192.168.1.200'
                    archivo[ids[14]] <- 'lan_ipaddr=192.168.1.1'
                  }
                
                  MACapp <<- paste0(toupper(limpiaMAC(input$MAC_1)), '.cfg')
                  #Iniciales wifi
                  iniciales <- 0
                  for (k in 1:length(str_split(firstname, " ")[[1]])){
                    iniciales[k] <- str_split(str_split(firstname, " ")[[1]], "")[[k]][1]
                  }
                  iniciales <- paste0(iniciales, collapse = '')
        
                  # tftp_reboot_flag necesita estar aquí ya que de otro modo, le asigna siempre la misma tftp_reboot_flag
                  archivo[ids[1]] <- sprintf("tftp_reboot_flag=%s", runif(1,0,1)) # Random number para tiempo de reconexión a tftp
                  archivo[ids[2]] <- sprintf("wan0_pppoe_passwd=%s", passPlain)
                  archivo[ids[3]] <- sprintf("wan0_pppoe_username=%s", numeroCliente)
                  archivo[ids[4]] <- sprintf("wl0_ssid=EMARTINEZ_%s", iniciales)
                  archivo[ids[5]] <- sprintf("wl_ssid=EMARTINEZ_%s", iniciales)
                  archivo[ids[6]] <- sprintf("wl_wpa_psk=emartinez%s", numeroClienteSinExtension)
                  archivo[ids[7]] <- sprintf("wl0_wpa_psk=emartinez%s", numeroClienteSinExtension)
                  archivo[ids[8]] <- sprintf("wl0_akm=%s", "psk2")
                  archivo[ids[9]] <- sprintf("wl_akm=%s", "psk2")
                  archivo[ids[10]] <- "rm_web_ip=185.44.24.23"
                  
                  write(archivo, paste0("./archivos-samba/", MACapp))
                  system(sprintf("smbclient -U dhcp66 //X.X.X.X/tftpSRV PASSWORD -c \"put %s %s\"", 
                                  sprintf("./archivos-samba/%s", MACapp), MACapp))
                  output$erroresMAC <- renderText("Correcto. No han habido errores subiendo el archivo de configuración")
        
                }, warning = function(w){
                  output$erroresMAC <- renderText("Error. Han habido errores subiendo el archivo de configuración")         
                })
              }
              
              ####################################################################################
              ############################ PARTE MAC GrandStream HT502 ###########################
              ####################################################################################
              
              else if (input$selectRouter == "Grandstream HT502"){
                  
                  tryCatch({
                    
                    
                    library(XML)
                 
                    archivo <<- xmlParse("data/archivo-base-grandstreamHT502.xml")
                    posicion <- xpathSApply(archivo, "//P8")
                    xmlValue(posicion[[1]]) <- "2"
                    posicion <- xpathSApply(archivo, "//P30")
                    xmlValue(posicion[[1]]) <- "Y.Y.Y.Y"
                    posicion <- xpathSApply(archivo, "//P38")
                    xmlValue(posicion[[1]]) <- "48"
                    posicion <- xpathSApply(archivo, "//P57")
                    xmlValue(posicion[[1]]) <- "8"
                    posicion <- xpathSApply(archivo, "//P64")
                    xmlValue(posicion[[1]]) <- "CET-1CEST-2,M3.5.0/02:00:00,M10.5.0/03:00:00"
                    posicion <- xpathSApply(archivo, "//P133")
                    xmlValue(posicion[[1]]) <- "0"
                    posicion <- xpathSApply(archivo, "//P189")
                    xmlValue(posicion[[1]]) <- "1"
                    posicion <- xpathSApply(archivo, "//P190")
                    xmlValue(posicion[[1]]) <- "1"
                    posicion <- xpathSApply(archivo, "//P231")
                    xmlValue(posicion[[1]]) <- "1"
                    posicion <- xpathSApply(archivo, "//P243")
                    xmlValue(posicion[[1]]) <- "1"
                    posicion <- xpathSApply(archivo, "//P246")
                    xmlValue(posicion[[1]]) <- "GMT+0BST,M3.5.0,M10.5.0"
                    posicion <- xpathSApply(archivo, "//P258")
                    xmlValue(posicion[[1]]) <- "1"
                    posicion <- xpathSApply(archivo, "//P277")
                    xmlValue(posicion[[1]]) <- "1"
                    posicion <- xpathSApply(archivo, "//P854")
                    xmlValue(posicion[[1]]) <- "10"
                    posicion <- xpathSApply(archivo, "//P901")
                    xmlValue(posicion[[1]]) <- "8080"
                    
                    posicion <- xpathSApply(archivo, "//P2363")
                    xmlValue(posicion[[1]]) <- "0"
                    posicion <- xpathSApply(archivo, "//P4200")
                    xmlValue(posicion[[1]]) <- "{[6789]xxxxxxxx| x+ | *x+ | *xx*x+ }"
                    posicion <- xpathSApply(archivo, "//P4567")
                    xmlValue(posicion[[1]]) <- "1"
                    posicion <- xpathSApply(archivo, "//P5001")
                    xmlValue(posicion[[1]]) <- "0"
                    posicion <- xpathSApply(archivo, "//P26003")
                    xmlValue(posicion[[1]]) <- "0"
                    posicion <- xpathSApply(archivo, "//mac")
                    xmlValue(posicion[[1]]) <- toupper(limpiaMAC(input$MAC_1))
                    posicion <- xpathSApply(archivo, "//P58")
                    xmlValue(posicion[[1]]) <- "8"
                    posicion <- xpathSApply(archivo, "//P59")
                    xmlValue(posicion[[1]]) <- "8"
                    posicion <- xpathSApply(archivo, "//P60")
                    xmlValue(posicion[[1]]) <- "8"
                    posicion <- xpathSApply(archivo, "//P61")
                    xmlValue(posicion[[1]]) <- "8"
                    posicion <- xpathSApply(archivo, "//P62")
                    xmlValue(posicion[[1]]) <- "8"
                    posicion <- xpathSApply(archivo, "//P46")
                    xmlValue(posicion[[1]]) <- "8"
                    posicion <- xpathSApply(archivo, "//P98")
                    xmlValue(posicion[[1]]) <- "8"
                    
                    posicion <- xpathSApply(archivo, "//P92")
                    xmlValue(posicion[[1]]) <- "185"
                    posicion <- xpathSApply(archivo, "//P93")
                    xmlValue(posicion[[1]]) <- "44"
                    posicion <- xpathSApply(archivo, "//P94")
                    xmlValue(posicion[[1]]) <- "24"
                    posicion <- xpathSApply(archivo, "//P95")
                    xmlValue(posicion[[1]]) <- "1"
                    MAC_GS <<- sprintf("cfg%s.xml", tolower(limpiaMAC(input$MAC_1)))
                    # Elegimos la IP según si es servidor de empresas o de clientes
                    ipSIP <- if(input$selectParticularEmpresa == "Particular") "IP.P.P.P" else "IP.X.X.X"
                    # Password LARGA del SIP
                    passwordLargaSIP <- input$passSIP
                    
                    # Password del PPPoE telefonía
                    passwordPPPoETLFN <- passPlain
                    #Añadimos siblings
                    posicion <- xpathSApply(archivo, "//P854") ##última posición, para que sea fácil buscar estos datos
                    addSibling(posicion[[1]], newXMLNode("P196", "emartinez"))
                    addSibling(posicion[[1]], newXMLNode("P83", passwordPPPoETLFN))
                    addSibling(posicion[[1]], newXMLNode("P2", "PASSWORD"))
                    addSibling(posicion[[1]], newXMLNode("P35", input$telefono))
                    addSibling(posicion[[1]], newXMLNode("P47", ipSIP))
                    addSibling(posicion[[1]], newXMLNode("P269", "telefonia"))
                    addSibling(posicion[[1]], newXMLNode("P34", passwordLargaSIP))
                    addSibling(posicion[[1]], newXMLNode("P3", firstname))
                    # Añadir "-t" a la extensión si no existe
                    extension <<- tail(unlist(strsplit(numeroCliente, "-")), n=1)
                    if (!(extension == "t")) numeroCliente <<- paste0(numeroCliente, "t")
                    addSibling(posicion[[1]], newXMLNode("P82", numeroCliente))
                    
                    
                    ### FXS PORT 2 ###
                    addSibling(posicion[[1]], newXMLNode("P735", input$telefono_ext2))
                    addSibling(posicion[[1]], newXMLNode("P703", firstname))
                    addSibling(posicion[[1]], newXMLNode("P734", input$passSIP_ext2))
                    posicion <- xpathSApply(archivo, "//P747")
                    xmlValue(posicion[[1]]) <- ipSIP
                    if(input$telefono_ext2 != ""){
                      posicion <- xpathSApply(archivo, "//P401")
                      xmlValue(posicion[[1]]) <- "1"
                    }
                    
                    ### FAX/ALARMA PORT 1 ###
                    if(input$FaxoTlfn1 == "Fax/Alarma"){
                      posicion <- xpathSApply(archivo, "//P191")
                      xmlValue(posicion[[1]]) <- "0"
                      posicion <- xpathSApply(archivo, "//P850")
                      xmlValue(posicion[[1]]) <- "100"
                      posicion <- xpathSApply(archivo, "//P851")
                      xmlValue(posicion[[1]]) <- "100"
                      posicion <- xpathSApply(archivo, "//P852")
                      xmlValue(posicion[[1]]) <- "100"
                      posicion <- xpathSApply(archivo, "//P57")
                      xmlValue(posicion[[1]]) <- "8"
                      posicion <- xpathSApply(archivo, "//P58")
                      xmlValue(posicion[[1]]) <- "0"
                      posicion <- xpathSApply(archivo, "//P59")
                      xmlValue(posicion[[1]]) <- "18"
                      posicion <- xpathSApply(archivo, "//P824")
                      xmlValue(posicion[[1]]) <- "1"
                    }
                    
                    ### FAX/ALARMA PORT 2 ###
                    if(input$FaxoTlfn2 == "Fax/Alarma"){
                      posicion <- xpathSApply(archivo, "//P751")
                      xmlValue(posicion[[1]]) <- "0"
                      posicion <- xpathSApply(archivo, "//P860")
                      xmlValue(posicion[[1]]) <- "100"
                      posicion <- xpathSApply(archivo, "//P861")
                      xmlValue(posicion[[1]]) <- "100"
                      posicion <- xpathSApply(archivo, "//P862")
                      xmlValue(posicion[[1]]) <- "100"
                      posicion <- xpathSApply(archivo, "//P757")
                      xmlValue(posicion[[1]]) <- "8"
                      posicion <- xpathSApply(archivo, "//P758")
                      xmlValue(posicion[[1]]) <- "0"
                      posicion <- xpathSApply(archivo, "//P759")
                      xmlValue(posicion[[1]]) <- "18"
                      posicion <- xpathSApply(archivo, "//P825")
                      xmlValue(posicion[[1]]) <- "1"
                    }
                  
                    saveXML(archivo, file=paste0("./archivos-samba-http/", MAC_GS), prefix = '<?xml version="1.0" encoding="UTF-8"?>')
                    system(sprintf('cp /home/tecnico/WebApp/archivos-samba-http/%s /var/www/html/gs/', MAC_GS))
                    saveXML(archivo, file=paste0("/var/www/html/gs/", MAC_GS), prefix = '<?xml version="1.0" encoding="UTF-8"?>')
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
  #                   archivo <<- scan("data/archivo-base-grandstreamHT502.xml", what = character(), sep = "\n")
  #                   
  #                   ids <<- vector(length=24)
  #                   ids[1] <<- pmatch('\t\t<P8>', archivo)
  #                   ids[2] <<- pmatch('\t\t<P30>', archivo) # NTP
  #                   ids[4] <<- pmatch('\t\t<P38>', archivo)
  #                   ids[5] <<- pmatch('\t\t<P57>', archivo) # Vocoder 1
  #                   ids[6] <<- pmatch('\t\t<P64>', archivo) # Time Zone
  #                   ids[8] <<- pmatch('\t\t<P109>', archivo)
  #                   ids[9] <<- pmatch('\t\t<P133>', archivo)
  #                   ids[10] <<- pmatch('\t\t<P189>', archivo)
  #                   ids[11] <<- pmatch('\t\t<P190>', archivo)
  #                   ids[12] <<- pmatch('\t\t<P231>', archivo)
  #                   ids[13] <<- pmatch('\t\t<P243>', archivo)
  #                   ids[14] <<- pmatch('\t\t<P246>', archivo) # Time Zone
  #                   ids[15] <<- pmatch('\t\t<P258>', archivo)
  #                   ids[16] <<- pmatch('\t\t<P277>', archivo)
  #                   ids[17] <<- pmatch('\t\t<P401>', archivo)
  #                   ids[18] <<- pmatch('\t\t<P854>', archivo)
  #                   ids[19] <<- pmatch('\t\t<P901>', archivo) # Puerto 8080
  #                   ids[20] <<- pmatch('\t\t<P2363>', archivo) # ANTES NO ESTABA ABAJO
  #                   ids[21] <<- pmatch('\t\t<P4200>', archivo)
  #                   ids[22] <<- pmatch('\t\t<P4567>', archivo)
  #                   ids[23] <<- pmatch('\t\t<P5001>', archivo)
  #                   ids[24] <<- pmatch('\t\t<P26003>', archivo) # ANTES NO ESTABA ABAJO
  #                   ids[25] <<- pmatch('<mac>', archivo) #MAC del router en mayúsculas, SIN TABULADOR
  #                   ids[26] <<- pmatch('\t\t<P58>', archivo) # Vocoder 2
  #                   ids[27] <<- pmatch('\t\t<P59>', archivo) # Vocoder 3
  #                   ids[28] <<- pmatch('\t\t<P60>', archivo) # Vocoder 4
  #                   ids[29] <<- pmatch('\t\t<P61>', archivo) # Vocoder 5
  #                   ids[30] <<- pmatch('\t\t<P62>', archivo) # Vocoder 6
  #                   ids[31] <<- pmatch('\t\t<P46>', archivo) # Vocoder 7
  #                   ids[32] <<- pmatch('\t\t<P98>', archivo) # Vocoder 8
  #                   ids[33] <<- pmatch('\t\t<P92>', archivo) # DNS
  #                   ids[34] <<- pmatch('\t\t<P93>', archivo) # DNS
  #                   ids[35] <<- pmatch('\t\t<P94>', archivo) # DNS
  #                   ids[36] <<- pmatch('\t\t<P95>', archivo) # DNS
  #                   #### FX2:
  #                   #ids[37] <<- pmatch('\t\t<P401>', archivo) #Activo
  #                   
  #                   
  #                   
  #                   MAC_GS <<- sprintf("cfg%s.xml", tolower(limpiaMAC(input$MAC_1)))
  #                   
  #                   # Elegimos la IP según si es servidor de empresas o de clientes
  #                   ipSIP <<- if(input$selectParticularEmpresa == "Particular") "X.X.X.X" else "Z.Z.Z.Z" 
  #                   
  #                   # Password LARGA del SIP
  #                   passwordLargaSIP <<- input$passSIP
  #                   
  #                   # Password del PPPoE telefonía
  #                   passwordPPPoETLFN <<- passPlain
  #                   
  #                   # Hacemos hueco al final y añadimos los P-valores que no existían en el archivo de configuración
  #                   l <<- length(archivo)
  #                   
  #                   archivo[l+9] <<- archivo[l]
  #                   archivo[l+8] <<- archivo[l-1]
  #                   # Añadimos las líneas inexistentes en la parte final del archivo
  #                   archivo[l+7] <<- "\t\t<P196>emartinez</P196>"
  #                   archivo[l+6] <<- sprintf("\t\t<P83>%s</P83>", passwordPPPoETLFN)
  #                   archivo[l+5] <<- "\t\t<P2>PASSWORD</P2>"
  #                   archivo[l+4] <<- sprintf("\t\t<P35>%s</P35>", input$telefono)
  #                   archivo[l+3] <<- sprintf("\t\t<P47>%s</P47>", ipSIP) # Puede ser de clientes o empresas
  #                   
  #                   # Añadir "-t" a la extensión si no existe
  #                   extension <<- tail(unlist(strsplit(numeroCliente, "-")), n=1)
  #                   if (!(extension == "t")) numeroCliente <<- paste0(numeroCliente, "-t")
  #                   archivo[l+2] <<- sprintf("\t\t<P82>%s</P82>", numeroCliente)
  #                   
  #                   archivo[l+1] <<- "\t\t<P269>telefonia</P269>"
  #                   archivo[l] <<- sprintf("\t\t<P34>%s</P34>", passwordLargaSIP)
  #                   archivo[l-1] <<- sprintf("\t\t<P3>%s</P3>", firstname)
  #                   
  #                   # Añadimos el resto
  #                   archivo[ids[1]] <<- "\t\t<P8>2</P8>"
  #                   archivo[ids[2]] <<- "\t\t<P30>X.X.X.X</P30>"
  #                   archivo[ids[4]] <<- "\t\t<P38>48</P38>"
  #                   archivo[ids[5]] <<- "\t\t<P57>8</P57>"
  #                   archivo[ids[6]] <<- "\t\t<P64>CET-1CEST-2,M3.5.0/02:00:00,M10.5.0/03:00:00</P64>" 
  #                   archivo[ids[8]] <<- "\t\t<P109>0</P109>"
  #                   archivo[ids[9]] <<- "\t\t<P133>0</P133>"
  #                   archivo[ids[10]] <<- "\t\t<P189>1</P189>"
  #                   archivo[ids[11]] <<- "\t\t<P190>1</P190>"
  #                   archivo[ids[12]] <<- "\t\t<P231>1</P231>"
  #                   archivo[ids[13]] <<- "\t\t<P243>1</P243>"
  #                   archivo[ids[14]] <<- "\t\t<P246>GMT+0BST,M3.5.0,M10.5.0</P246>"
  #                   archivo[ids[15]] <<- "\t\t<P258>1</P258>"
  #                   archivo[ids[16]] <<- "\t\t<P277>1</P277>"
  #                   archivo[ids[17]] <<- "\t\t<P401>0</P401>"
  #                   archivo[ids[18]] <<- "\t\t<P854>10</P854>"
  #                   archivo[ids[19]] <<- "\t\t<P901>8080</P901>"
  #                   archivo[ids[20]] <<- "\t\t<P2363>0</P2363>"
  #                   archivo[ids[21]] <<- "\t\t<P4200>{[6789]xxxxxxxx| x+ | *x+ | *xx*x+ }</P4200>" 
  #                   archivo[ids[22]] <<- "\t\t<P4567>1</P4567>"
  #                   archivo[ids[23]] <<- "\t\t<P5001>0</P5001>"
  #                   archivo[ids[24]] <<- "\t\t<P26003>0</P26003>"
  #                   archivo[ids[25]] <<- sprintf("<mac>%s</mac>", toupper(limpiaMAC(input$MAC_1)))
  #                   archivo[ids[26]] <<- "\t\t<P58>8</P58>"
  #                   archivo[ids[27]] <<- "\t\t<P59>8</P59>"
  #                   archivo[ids[28]] <<- "\t\t<P60>8</P60>"
  #                   archivo[ids[29]] <<- "\t\t<P61>8</P61>"
  #                   archivo[ids[30]] <<- "\t\t<P62>8</P62>"
  #                   archivo[ids[31]] <<- "\t\t<P46>8</P46>"
  #                   archivo[ids[32]] <<- "\t\t<P98>8</P98>"
  #                   archivo[ids[33]] <<- "\t\t<P92>185</P92>"
  #                   archivo[ids[34]] <<- "\t\t<P93>44</P93>"
  #                   archivo[ids[35]] <<- "\t\t<P94>24</P94>"
  #                   archivo[ids[36]] <<- "\t\t<P95>1</P95>"
  #                   
  #                  
  #                   write(archivo, paste0("./archivos-samba-http/", MAC_GS))
                    system(sprintf("smbclient -U dhcp66 //Y.Y.Y.Y/httpSRV PASSWORD -c \"put %s %s\"", sprintf("./archivos-samba-http/%s", MAC_GS), MAC_GS))
                    output$erroresMAC <- renderText("Correcto. No han habido errores subiendo el archivo de configuración")
                    
                  }, warning = function(w){
                    output$erroresMAC <- renderText("Error. Han habido errores subiendo el archivo de configuración")         
                  })
                  
    
              }
              
              ##############################################################################
              ############################ PARTE MAC TENDA W300D ###########################
              ##############################################################################
              
              else if (input$selectRouter == "Tenda W300D"){
                
                tryCatch({
                  
                  deviceID <- tolower(limpiaMAC(input$MAC_1))
                  system(sprintf("curl -i 'http://GENIEACS.IP:7557/presets/%s' -X DELETE", deviceID))
                  iniciales <- 0
                  for (k in 1:length(str_split(firstname, " ")[[1]])){
                    iniciales[k] <- str_split(str_split(firstname, " ")[[1]], "")[[k]][1]
                  }
                  iniciales <- paste0(iniciales, collapse = '')
                  
                  informInterval <- "15"
                  
                  datos1 <- sprintf("{ \"type\": \"value\", \"name\": \"InternetGatewayDevice.LANDevice.1.WLANConfiguration.1.SSID\", \"value\": \"EMARTINEZ_%s\" }", iniciales)
                  datos2 <- sprintf("{ \"type\": \"value\", \"name\": \"InternetGatewayDevice.LANDevice.1.WLANConfiguration.1.X_BROADCOM_COM_WlanAdapter.WlVirtIntfCfg.1.WlSsid\", \"value\": \"EMARTINEZ_%s\" }", iniciales)
                  datos3 <- sprintf("{ \"type\": \"value\", \"name\": \"InternetGatewayDevice.LANDevice.1.WLANConfiguration.1.X_BROADCOM_COM_WlanAdapter.WlVirtIntfCfg.1.WlWpaPsk\", \"value\": \"PASSWORD%s\" }", numeroClienteSinExtension)
                  datos4 <- sprintf("{ \"type\": \"value\", \"name\": \"InternetGatewayDevice.LANDevice.1.WLANConfiguration.1.X_BROADCOM_COM_WlanAdapter.WlVirtIntfCfg.2.WlWpaPsk\", \"value\": \"PASSWORD%s\" }", numeroClienteSinExtension)
                  datos5 <- sprintf("{ \"type\": \"value\", \"name\": \"InternetGatewayDevice.LANDevice.1.WLANConfiguration.1.X_BROADCOM_COM_WlanAdapter.WlVirtIntfCfg.3.WlWpaPsk\", \"value\": \"PASSWORD%s\" }", numeroClienteSinExtension)
                  datos6 <- sprintf("{ \"type\": \"value\", \"name\": \"InternetGatewayDevice.LANDevice.1.WLANConfiguration.1.X_BROADCOM_COM_WlanAdapter.WlVirtIntfCfg.4.WlWpaPsk\", \"value\": \"PASSWORD%s\" }", numeroClienteSinExtension)
                  datos7 <- sprintf("{ \"type\": \"value\", \"name\": \"InternetGatewayDevice.LANDevice.1.WLANConfiguration.1.PreSharedKey.1.KeyPassphrase\", \"value\": \"PASSWORD%s\" }", numeroClienteSinExtension)
                  datos8 <- "{ \"type\": \"value\", \"name\": \"InternetGatewayDevice.LANDevice.1.WLANConfiguration.1.BeaconType\", \"value\": \"basic\" }"
                  datos9 <- sprintf("{ \"type\": \"value\", \"name\": \"InternetGatewayDevice.ManagementServer.PeriodicInformInterval\", \"value\": \"%s\" }", informInterval)
                  datos10 <- sprintf("{ \"type\": \"value\", \"name\": \"InternetGatewayDevice.X_Tenda_QuicksetupCfgObject.EasyInstUser\", \"value\": \"%s\" }", numeroCliente)
                  datos11 <- sprintf("{ \"type\": \"value\", \"name\": \"InternetGatewayDevice.X_Tenda_QuicksetupCfgObject.EasyInstPwd\", \"value\": \"%s\" }", passPlain)
                  datos12 <- sprintf("{ \"type\": \"value\", \"name\": \"InternetGatewayDevice.WANDevice.3.WANConnectionDevice.1.WANPPPConnection.6.Username\", \"value\": \"%s\" }", numeroCliente)
                  datos13 <- sprintf("{ \"type\": \"value\", \"name\": \"InternetGatewayDevice.WANDevice.3.WANConnectionDevice.1.WANPPPConnection.6.Password\", \"value\": \"%s\" }", passPlain)
                  datos14 <- sprintf("{ \"type\": \"value\", \"name\": \"InternetGatewayDevice.WANDevice.1.WANConnectionDevice.3.WANPPPConnection.1.Username\", \"value\": \"%s\" }", numeroCliente)
                  datos15 <- sprintf("{ \"type\": \"value\", \"name\": \"InternetGatewayDevice.WANDevice.1.WANConnectionDevice.3.WANPPPConnection.1.Password\", \"value\": \"%s\" }", passPlain)
                  datos16 <- sprintf("{ \"type\": \"value\", \"name\": \"InternetGatewayDevice.WANDevice.1.WANConnectionDevice.1.WANPPPConnection.1.Username\", \"value\": \"%s\" }", numeroCliente)
                  datos17 <- sprintf("{ \"type\": \"value\", \"name\": \"InternetGatewayDevice.WANDevice.1.WANConnectionDevice.1.WANPPPConnection.1.Password\", \"value\": \"%s\" }", passPlain)
                  datos18 <- sprintf("{ \"type\": \"value\", \"name\": \"InternetGatewayDevice.WANDevice.1.WANConnectionDevice.4.WANPPPConnection.1.Username\", \"value\": \"%s\" }", numeroCliente)
                  datos19 <- sprintf("{ \"type\": \"value\", \"name\": \"InternetGatewayDevice.WANDevice.1.WANConnectionDevice.4.WANPPPConnection.1.Password\", \"value\": \"%s\" }", passPlain)
                  datos20 <- sprintf("{ \"type\": \"value\", \"name\": \"InternetGatewayDevice.WANDevice.3.WANConnectionDevice.1.WANPPPConnection.1.Username\", \"value\": \"%s\" }", numeroCliente)
                  datos21 <- sprintf("{ \"type\": \"value\", \"name\": \"InternetGatewayDevice.WANDevice.3.WANConnectionDevice.1.WANPPPConnection.1.Password\", \"value\": \"%s\" }", passPlain)
                  datos22 <- sprintf("{ \"type\": \"add_tag\", \"tag\": \"%s\" }", deviceID)
                  datos23 <- "{ \"type\": \"value\", \"name\": \"InternetGatewayDevice.Time.Enable\", \"value\": \"true\" }"
                  
                  datosFin <- sprintf("'{ \"weight\": 30, \"precondition\": \"{\\\"summary.serialNumber\\\":\\\"%s\\\"}\", \"configurations\": [ %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s ] }'",
                                      deviceID, datos1, datos2, datos3, datos4, datos5, datos6, datos7, datos8, datos9, datos10, datos11, datos12, datos13, datos14, datos15, datos16, datos17, datos18, datos19, datos20, datos21, datos22, datos23)
                  system(sprintf("curl -i 'http://185.44.24.18:7557/presets/%s' -X PUT --data %s", deviceID, datosFin))
                  
                })
              
              }
              
            }
            }
            }) # End barra de progreso
          enable("sincronizar")
          
          } # End IF(campo client1 != vacío)
        })
    
    #####################################################################################################################################
    ########################################################## ARCHIVOS DHCP66 TENDA ####################################################
    #####################################################################################################################################
    
    # Siguientes líneas de código sirven para mostrar un reactive output del input introducido
    output$client <- renderText({ 
      paste("Código de cliente: ", input$client)
    })
    
    output$initials <- renderText({ 
      paste("Iniciales del cliente: ", input$initials)
    })
    
    output$user <- renderText({ 
      paste("Usuario PPPoE: ", input$user)
    })
    
    output$pass <- renderText({ 
      paste("Contraseña PPPoE: ", input$pass)
    })
    
    output$mac<- renderText({ 
      paste("MAC del Router: ", toupper(limpiaMAC(input$mac)))
    })
    
    # Botón de descarga, lo que se hace tras la pulsación es esto:
    output$downloadData <- downloadHandler(
      filename = function() {
        MAC
      },
      content = function(file) {
        write(archivo, file)
      }
    )
    
    observeEvent(
      input$upload,{
        archivo <<- scan("data/archivo-base-tendaW308R.cfg", what = character(), skip = 1, sep = "\n")
        ids <<- vector(length = 7)
        ids[1] <<- pmatch('tftp_reboot_flag=', archivo) # Parámetro de reconexión
        ids[2] <<- pmatch('wan0_pppoe_passwd=', archivo) # PPPoE Password
        ids[3] <<- pmatch('wan0_pppoe_username=', archivo) # PPPoE User
        ids[4] <<- pmatch('wl0_ssid=', archivo) # SSID
        ids[5] <<- pmatch('wl_ssid=', archivo) # SSID
        ids[6] <<- pmatch('wl_wpa_psk=', archivo) # WPA PASSWORD
        ids[7] <<- pmatch('wl0_wpa_psk=', archivo) # WPA PASSWORD
        ids[8] <<- pmatch('wl0_akm=', archivo) # WPA/WPA2 TYPE
        ids[9] <<- pmatch('wl_akm=', archivo) # WPA/WPA2 TYPE
        ids[[10]] <- pmatch('rm_web_ip=', archivo) # Remote Web Management IP
        
        MAC <<- paste0(toupper(limpiaMAC(input$mac)), '.cfg')
        archivo[ids[1]] <<- sprintf("tftp_reboot_flag=%s", runif(1,0,1))
        archivo[ids[2]] <<- sprintf("wan0_pppoe_passwd=%s", input$pass)
        archivo[ids[3]] <<- sprintf("wan0_pppoe_username=%s", input$user)
        archivo[ids[4]] <<- sprintf("wl0_ssid=EMARTINEZ_%s", input$initials)
        archivo[ids[5]] <<- sprintf("wl_ssid=EMARTINEZ_%s", input$initials)
        archivo[ids[6]] <<- sprintf("wl_wpa_psk=emartinez%s", input$client)
        archivo[ids[7]] <<- sprintf("wl0_wpa_psk=emartinez%s", input$client)
        archivo[ids[8]] <- sprintf("wl0_akm=%s", "psk2")
        archivo[ids[9]] <- sprintf("wl_akm=%s", "psk2")
        archivo[ids[10]] <- "rm_web_ip=185.44.24.23"
        
        tryCatch({
          write(archivo, paste0("./archivos-samba/", MAC))
          system(sprintf("smbclient -U dhcp66 //X.X.X.X/tftpSRV PASSWORD -c \"put %s %s\"", 
                         sprintf("./archivos-samba/%s", MAC), MAC))
          output$ok <- renderText("0 Errores")
          print(Sys.time())
        }, warning = function(w) {
          output$ok <- renderText("Han habido errores en la conexión TFTP")
        })
      }  
    )
    
    ##################################################################################################################################
    ########################################################## ARCHIVOS DHCP66 GS ####################################################
    ##################################################################################################################################
    
    output$nombreGS <- renderText({ 
      paste("Nombre del cliente: ", input$nombreGS)
    })
    
    output$userGS <- renderText({ 
      paste("Usuario PPPoE: ", input$userGS)
    })
    
    output$passGS <- renderText({ 
      paste("Contraseña PPPoE: ", input$passGS)
    })
    
    output$telefonoGS <- renderText({ 
      paste("Número de teléfono: ", input$telefonoGS)
    })
    
    output$passSIPGS <- renderText({ 
      paste("Constraseña SIP: ", input$passSIPGS)
    })
    
    output$passSIPGS <- renderText({ 
      paste("Constraseña SIP: ", input$passSIPGS)
    })
    
    output$selectParticularEmpresaGS <- renderText({ 
      paste("Particular o Empresa: ", input$selectParticularEmpresaGS)
    })
    
    output$macGS <- renderText({ 
      paste("MAC: ", tolower(limpiaMAC(input$macGS)))
    })
    
    output$downloadDataGS <- downloadHandler(
      filename = function() {
        MACGS
      },
      content = function(file) {
        write(archivo, file)
      }
    )
    
    observeEvent(
      input$uploadGS,{
        
        archivo <<- scan("data/archivo-base-grandstreamHT502.xml", what = character(), sep = "\n")
        
        ids <<- vector(length=24)
        ids[1] <<- pmatch('\t\t<P8>', archivo)
        ids[2] <<- pmatch('\t\t<P30>', archivo) # NTP
        ids[4] <<- pmatch('\t\t<P38>', archivo)
        ids[5] <<- pmatch('\t\t<P57>', archivo)
        ids[6] <<- pmatch('\t\t<P64>', archivo) # Time Zone
        ids[8] <<- pmatch('\t\t<P109>', archivo)
        ids[9] <<- pmatch('\t\t<P133>', archivo)
        ids[10] <<- pmatch('\t\t<P189>', archivo)
        ids[11] <<- pmatch('\t\t<P190>', archivo)
        ids[12] <<- pmatch('\t\t<P231>', archivo)
        ids[13] <<- pmatch('\t\t<P243>', archivo)
        ids[14] <<- pmatch('\t\t<P246>', archivo) # Time Zone
        ids[15] <<- pmatch('\t\t<P258>', archivo)
        ids[16] <<- pmatch('\t\t<P277>', archivo)
        ids[17] <<- pmatch('\t\t<P401>', archivo)
        ids[18] <<- pmatch('\t\t<P854>', archivo)
        ids[19] <<- pmatch('\t\t<P901>', archivo) # Puerto 8080
        ids[20] <<- pmatch('\t\t<P2363>', archivo) # ANTES NO ESTABA ABAJO
        ids[21] <<- pmatch('\t\t<P4200>', archivo)
        ids[22] <<- pmatch('\t\t<P4567>', archivo)
        ids[23] <<- pmatch('\t\t<P5001>', archivo)
        ids[24] <<- pmatch('\t\t<P26003>', archivo) # ANTES NO ESTABA ABAJO
        ids[25] <<- pmatch('<mac>', archivo) #MAC del router en mayúsculas, SIN TABULADOR
        ids[26] <<- pmatch('\t\t<P58>', archivo) # Vocoder 2
        ids[27] <<- pmatch('\t\t<P59>', archivo) # Vocoder 3
        ids[28] <<- pmatch('\t\t<P60>', archivo) # Vocoder 4
        ids[29] <<- pmatch('\t\t<P61>', archivo) # Vocoder 5
        ids[30] <<- pmatch('\t\t<P62>', archivo) # Vocoder 6
        ids[31] <<- pmatch('\t\t<P46>', archivo) # Vocoder 7
        ids[32] <<- pmatch('\t\t<P98>', archivo) # Vocoder 8
        ids[33] <<- pmatch('\t\t<P92>', archivo) # DNS
        ids[34] <<- pmatch('\t\t<P93>', archivo) # DNS
        ids[35] <<- pmatch('\t\t<P94>', archivo) # DNS
        ids[36] <<- pmatch('\t\t<P95>', archivo) # DNS
        
        MACGS <<- sprintf("cfg%s.xml", tolower(limpiaMAC(input$macGS)))
        
        # Elegimos la IP según si es servidor de empresas o de clientes
        ipSIP <<- if(input$selectParticularEmpresaGS == "Particular") "X.X.X.X" else "Y.Y.Y.Y" 
        
        # Password LARGA del SIP
        passwordLargaSIP <<- input$passSIPGS
        
        # Password del PPPoE telefonía
        passwordPPPoETLFN <<- input$passGS
        
        # Hacemos hueco al final y añadimos los P-valores que no existían en el archivo de configuración
        l <<- length(archivo)
        
        archivo[l+9] <<- archivo[l]
        archivo[l+8] <<- archivo[l-1]
        # Añadimos las líneas inexistentes en la parte final del archivo
        archivo[l+7] <<- "\t\t<P196>emartinez</P196>"
        archivo[l+6] <<- sprintf("\t\t<P83>%s</P83>", passwordPPPoETLFN)
        archivo[l+5] <<- "\t\t<P2>PASSWORD</P2>"
        archivo[l+4] <<- sprintf("\t\t<P35>%s</P35>", input$telefonoGS)
        archivo[l+3] <<- sprintf("\t\t<P47>%s</P47>", ipSIP) # Puede ser de clientes o empresas
        
        # Añadir "-t" a la extensión si no existe
        extension <<- tail(unlist(strsplit(input$userGS, "-")), n=1)
        if (!(extension == "t")) numeroClienteGS <<- paste0(input$userGS, "t")
        else numeroClienteGS <<- input$userGS
        archivo[l+2] <<- sprintf("\t\t<P82>%s</P82>", numeroClienteGS)
        
        archivo[l+1] <<- "\t\t<P269>telefonia</P269>"
        archivo[l] <<- sprintf("\t\t<P34>%s</P34>", passwordLargaSIP)
        archivo[l-1] <<- sprintf("\t\t<P3>%s</P3>", input$nombreGS)
        
        # Añadimos el resto
        archivo[ids[1]] <<- "\t\t<P8>2</P8>" # PPPoE Option
        archivo[ids[2]] <<- "\t\t<P30>X.X.X.X</P30>"
        archivo[ids[4]] <<- "\t\t<P38>48</P38>"
        archivo[ids[5]] <<- "\t\t<P57>8</P57>"
        archivo[ids[6]] <<- "\t\t<P64>CET-1CEST-2,M3.5.0/02:00:00,M10.5.0/03:00:00</P64>" 
        archivo[ids[8]] <<- "\t\t<P109>0</P109>"
        archivo[ids[9]] <<- "\t\t<P133>0</P133>"
        archivo[ids[10]] <<- "\t\t<P189>1</P189>"
        archivo[ids[11]] <<- "\t\t<P190>1</P190>"
        archivo[ids[12]] <<- "\t\t<P231>1</P231>"
        archivo[ids[13]] <<- "\t\t<P243>1</P243>"
        archivo[ids[14]] <<- "\t\t<P246>GMT+0BST,M3.5.0,M10.5.0</P246>"
        archivo[ids[15]] <<- "\t\t<P258>1</P258>"
        archivo[ids[16]] <<- "\t\t<P277>1</P277>"
        archivo[ids[17]] <<- "\t\t<P401>0</P401>"
        archivo[ids[18]] <<- "\t\t<P854>10</P854>"
        archivo[ids[19]] <<- "\t\t<P901>8080</P901>"
        archivo[ids[20]] <<- "\t\t<P2363>0</P2363>"
        archivo[ids[21]] <<- "\t\t<P4200>{[6789]xxxxxxxx| x+ | *x+ | *xx*x+ }</P4200>" 
        archivo[ids[22]] <<- "\t\t<P4567>1</P4567>"
        archivo[ids[23]] <<- "\t\t<P5001>0</P5001>"
        archivo[ids[24]] <<- "\t\t<P26003>0</P26003>"
        archivo[ids[25]] <<- sprintf("<mac>%s</mac>", toupper(limpiaMAC(input$macGS)))
        archivo[ids[26]] <<- "\t\t<P58>8</P58>"
        archivo[ids[27]] <<- "\t\t<P59>8</P59>"
        archivo[ids[28]] <<- "\t\t<P60>8</P60>"
        archivo[ids[29]] <<- "\t\t<P61>8</P61>"
        archivo[ids[30]] <<- "\t\t<P62>8</P62>"
        archivo[ids[31]] <<- "\t\t<P46>8</P46>"
        archivo[ids[32]] <<- "\t\t<P98>8</P98>"
        archivo[ids[33]] <<- "\t\t<P98>185</P98>"
        archivo[ids[34]] <<- "\t\t<P98>44</P98>"
        archivo[ids[35]] <<- "\t\t<P98>24</P98>"
        archivo[ids[36]] <<- "\t\t<P98>1</P98>"
        
        tryCatch({
          write(archivo, paste0("./archivos-samba-http/", MAC_GS))
          system(sprintf("smbclient -U dhcp66 //X.X.X.X/httpSRV PASSWORD -c \"put %s %s\"", 
                         sprintf("./archivos-samba-http/%s", MAC_GS), MAC_GS))
          output$okGS <- renderText("0 Errores")
          print(Sys.time())
        }, warning = function(w) {
          output$ok <- renderText("Han habido errores en la creación del archivo")
        })
      }  
    )
    
    
    #####################################################################################################################################
    ########################################################## Prueba de Equipos ########################################################
    #####################################################################################################################################
    
    output$pruebaMAC <- renderUI({
      if(input$pruebaSelect == "Tenda W308R") textInput("pruebaTenda", "MAC: ", value = "C83A35") else textInput("pruebaGrandstream", "MAC: ", value = "000B82")
    })
    output$borraMAC <- renderUI({
      if(input$borraMACelige == "Tenda W308R") textInput("borraTenda", "MAC: ", value = "C83A35") else textInput("borraGrandstream", "MAC: ", value = "000B82")
    })
    
    observeEvent(input$borrarMAC,{
      
      if(input$pruebaSelect == "Tenda W308R"){
        
        MACapp <<- paste0(toupper(limpiaMAC(input$borraTenda)), '.cfg')
        system(sprintf("smbclient -U dhcp66 //X.X.X.X/tftpSRV PASSWORD -c \"rm %s\"", MACapp))
        system(sprintf("rm ./archivos-samba/%s", MACapp))
        output$okPRUEBA <- renderUI({ h1("Archivo Tenda Borrado") })
        
      } else if (input$pruebaSelect == "Grandstream HT502"){
      
        MAC_GS <<- sprintf("cfg%s.xml", tolower(limpiaMAC(input$borraGrandstream)))
        system(sprintf("rm ./archivos-samba-http/%s", MAC_GS))
        system(sprintf("smbclient -U dhcp66 //X.X.X.X/httpSRV PASSWORD -c \"rm %s\"", MAC_GS))
        output$okPRUEBA <- renderUI({ h1("Archivo Grandstream Borrado") })
        
      }
      
    })
    
    observeEvent(input$probarEquipo,{
      
      
      if(input$pruebaSelect == "Tenda W308R"){
          
          archivo <<- scan("data/archivo-base-tendaW308R.cfg", what = character(), skip = 1, sep = "\n")
          ids <- vector(length = 10)
          ids[[1]] <- pmatch('tftp_reboot_flag=', archivo) # Parámetro de reconexión
          ids[[2]] <- pmatch('wan0_pppoe_passwd=', archivo) # PPPoE Password
          ids[[3]] <- pmatch('wan0_pppoe_username=', archivo) # PPPoE User
          ids[[4]] <- pmatch('wl0_ssid=', archivo) # SSID
          ids[[5]] <- pmatch('wl_ssid=', archivo) # SSID
          ids[[6]] <- pmatch('wl_wpa_psk=', archivo) # WPA PASSWORD
          ids[[7]] <- pmatch('wl0_wpa_psk=', archivo) # WPA PASSWORD
          ids[[8]] <- pmatch('wl0_akm=', archivo) # WPA/WPA2 TYPE
          ids[[9]] <- pmatch('wl_akm=', archivo) # WPA/WPA2 TYPE
          ids[[10]] <- pmatch('rm_web_ip=', archivo) # Remote Web Management IP
          
          MACapp <<- paste0(toupper(limpiaMAC(input$pruebaTenda)), '.cfg')

          # tftp_reboot_flag necesita estar aquí ya que de otro modo, le asigna siempre la misma tftp_reboot_flag
          archivo[ids[1]] <- sprintf("tftp_reboot_flag=%s", runif(1,0,1)) # Random number para tiempo de reconexión a tftp
          archivo[ids[2]] <- sprintf("wan0_pppoe_passwd=%s", "ninguna")
          archivo[ids[3]] <- sprintf("wan0_pppoe_username=%s", "prueba")
          archivo[ids[4]] <- sprintf("wl0_ssid=EMARTINEZ_%s", "TEST")
          archivo[ids[5]] <- sprintf("wl_ssid=EMARTINEZ_%s","TEST")
          archivo[ids[6]] <- sprintf("wl_wpa_psk=emartinez%s", "prueba123")
          archivo[ids[7]] <- sprintf("wl0_wpa_psk=emartinez%s", "prueba123")
          archivo[ids[8]] <- sprintf("wl0_akm=%s", "psk2")
          archivo[ids[9]] <- sprintf("wl_akm=%s", "psk2")
          archivo[ids[10]] <- "rm_web_ip=185.44.24.23"
          
          write(archivo, paste0("./archivos-samba/", MACapp))
          system(sprintf("smbclient -U dhcp66 //X.X.X.X/tftpSRV PASSWORD -c \"put %s %s\"", sprintf("./archivos-samba/%s", MACapp), MACapp))

          output$okPRUEBA <- renderUI({ h1("Ok") })
      }

      
      else if (input$pruebaSelect == "Grandstream HT502"){
      
          library(XML)
          
          archivo <<- xmlParse("data/archivo-base-grandstreamHT502.xml")
          posicion <- xpathSApply(archivo, "//P8")
          xmlValue(posicion[[1]]) <- "2"
          posicion <- xpathSApply(archivo, "//P30")
          xmlValue(posicion[[1]]) <- "X.X.X.X"
          posicion <- xpathSApply(archivo, "//P38")
          xmlValue(posicion[[1]]) <- "48"
          posicion <- xpathSApply(archivo, "//P57")
          xmlValue(posicion[[1]]) <- "8"
          posicion <- xpathSApply(archivo, "//P64")
          xmlValue(posicion[[1]]) <- "CET-1CEST-2,M3.5.0/02:00:00,M10.5.0/03:00:00"
          posicion <- xpathSApply(archivo, "//P109")
          xmlValue(posicion[[1]]) <- "0"
          posicion <- xpathSApply(archivo, "//P133")
          xmlValue(posicion[[1]]) <- "0"
          posicion <- xpathSApply(archivo, "//P189")
          xmlValue(posicion[[1]]) <- "1"
          posicion <- xpathSApply(archivo, "//P190")
          xmlValue(posicion[[1]]) <- "1"
          posicion <- xpathSApply(archivo, "//P231")
          xmlValue(posicion[[1]]) <- "1"
          posicion <- xpathSApply(archivo, "//P243")
          xmlValue(posicion[[1]]) <- "1"
          posicion <- xpathSApply(archivo, "//P246")
          xmlValue(posicion[[1]]) <- "GMT+0BST,M3.5.0,M10.5.0"
          posicion <- xpathSApply(archivo, "//P258")
          xmlValue(posicion[[1]]) <- "1"
          posicion <- xpathSApply(archivo, "//P277")
          xmlValue(posicion[[1]]) <- "1"
          posicion <- xpathSApply(archivo, "//P854")
          xmlValue(posicion[[1]]) <- "10"
          posicion <- xpathSApply(archivo, "//P901")
          xmlValue(posicion[[1]]) <- "8080"
          
          posicion <- xpathSApply(archivo, "//P2363")
          xmlValue(posicion[[1]]) <- "0"
          posicion <- xpathSApply(archivo, "//P4200")
          xmlValue(posicion[[1]]) <- "{[6789]xxxxxxxx| x+ | *x+ | *xx*x+ }"
          posicion <- xpathSApply(archivo, "//P4567")
          xmlValue(posicion[[1]]) <- "1"
          posicion <- xpathSApply(archivo, "//P5001")
          xmlValue(posicion[[1]]) <- "0"
          posicion <- xpathSApply(archivo, "//P26003")
          xmlValue(posicion[[1]]) <- "0"
          posicion <- xpathSApply(archivo, "//mac")
          xmlValue(posicion[[1]]) <- toupper(limpiaMAC(input$pruebaGrandstream))
          posicion <- xpathSApply(archivo, "//P58")
          xmlValue(posicion[[1]]) <- "8"
          posicion <- xpathSApply(archivo, "//P59")
          xmlValue(posicion[[1]]) <- "8"
          posicion <- xpathSApply(archivo, "//P60")
          xmlValue(posicion[[1]]) <- "8"
          posicion <- xpathSApply(archivo, "//P61")
          xmlValue(posicion[[1]]) <- "8"
          posicion <- xpathSApply(archivo, "//P62")
          xmlValue(posicion[[1]]) <- "8"
          posicion <- xpathSApply(archivo, "//P46")
          xmlValue(posicion[[1]]) <- "8"
          posicion <- xpathSApply(archivo, "//P98")
          xmlValue(posicion[[1]]) <- "8"
          
          posicion <- xpathSApply(archivo, "//P92")
          xmlValue(posicion[[1]]) <- "185"
          posicion <- xpathSApply(archivo, "//P93")
          xmlValue(posicion[[1]]) <- "44"
          posicion <- xpathSApply(archivo, "//P94")
          xmlValue(posicion[[1]]) <- "24"
          posicion <- xpathSApply(archivo, "//P95")
          xmlValue(posicion[[1]]) <- "1"
          
        
          MAC_GS <<- sprintf("cfg%s.xml", tolower(limpiaMAC(input$pruebaGrandstream)))
        
          #Añadimos siblings
          posicion <- xpathSApply(archivo, "//P854") ##última posición, para que sea fácil buscar estos datos
          addSibling(posicion[[1]], newXMLNode("P196", "emartinez"))
          addSibling(posicion[[1]], newXMLNode("P83", "Qmh3f2XG"))
          addSibling(posicion[[1]], newXMLNode("P2", "PASSWORD"))
          addSibling(posicion[[1]], newXMLNode("P35", "22222"))
          addSibling(posicion[[1]], newXMLNode("P47", "X.X.X.X"))
          addSibling(posicion[[1]], newXMLNode("P269", "telefonia"))
          addSibling(posicion[[1]], newXMLNode("P34", "83d36f63f8c6a13fb54f759370e16638"))
          addSibling(posicion[[1]], newXMLNode("P3", "PRUEBA 22222"))
          addSibling(posicion[[1]], newXMLNode("P82", "grandstreamt"))
          
          
          ### FXS PORT 2 ###
          addSibling(posicion[[1]], newXMLNode("P735", "33333"))
          addSibling(posicion[[1]], newXMLNode("P703", "PRUEBA 33333"))
          addSibling(posicion[[1]], newXMLNode("P734", "4c5b8a31d08039f95e4a1afee3ceaf43"))
          posicion <- xpathSApply(archivo, "//P747")
          xmlValue(posicion[[1]]) <- "X.X.X.X"
          posicion <- xpathSApply(archivo, "//P401")
          xmlValue(posicion[[1]]) <- "1"
          
          saveXML(archivo, file=paste0("./archivos-samba-http/", MAC_GS), prefix = '<?xml version="1.0" encoding="UTF-8"?>')
          system(sprintf('cp /home/tecnico/WebApp/archivos-samba-http/%s /var/www/html/gs/', MAC_GS))
          saveXML(archivo, file=paste0("/var/www/html/gs/", MAC_GS), prefix = '<?xml version="1.0" encoding="UTF-8"?>')
          
          
          system(sprintf("smbclient -U dhcp66 //10.254.254.99/httpSRV PASSWORD -c \"put %s %s\"", sprintf("./archivos-samba-http/%s", MAC_GS), MAC_GS))
          output$okPRUEBA <- renderUI({ h1("Ok") })
      }
      

      
    })
    
})