# library(shiny)
#' @export myApp
# packages <- c("shiny", "shinymaterial", "shinyjs", "dplyr", "lubridate",
#               "data.table", "stringr", "readr", "DT")
# 
# sapply(packages, require, character.only = TRUE)

options(shiny.maxRequestSize = 30*1024^3)

# source("./functions/helpers.R") # Load all the code shiny::needed to show feedback on a button click
# source("./functions/how_to.R")
# source("./functions/analysis_tab.R")
# source("./functions/monitor_tab.R")
# source("./functions/DataCleaningScript.R")

myApp <- function(...) {
  
  ui <- shinymaterial::material_page(
    shinyjs::useShinyjs(),
    title = "Career PathFinder Data Cleaner",
    
    nav_bar_color = "orange",
    background_color = "white",
    nav_bar_fixed = TRUE,
    
    # Side-nav in the beginning of the UI
    shinymaterial::material_side_nav(
      fixed = TRUE,
      # Place side-nav tabs within side-nav
      shinymaterial::material_side_nav_tabs(
        side_nav_tabs = c(
          "How to Use" = "how_nav_tab",
          "Input/Output" = "output_nav_tab",
          "Monitor" = "monitor_tab",
          "User Data Analysis" = "analysis_nav_tab"
        ),
        icons = c("info_outline", "import_export", "desktop_windows", "insert_chart")
      )
    ),
    # CONTENT tab 1
    shinymaterial::material_side_nav_tab_content(
      side_nav_tab_id = "how_nav_tab",
      
      # How To Content Goes Here
      shinymaterial::material_row(
        shinymaterial::material_card(
          depth = 0,
          shiny::tags$div(
            how_to_content()
          )
        )
      )
    ),
    # CONTENT tab 2
    shinymaterial::material_side_nav_tab_content(
      side_nav_tab_id = "output_nav_tab",
      
      shinymaterial::material_card(
        depth = 0,
        shiny::tags$h3("Data for Career PathFinder"),
        
        shiny::tags$h6("Upload the required data using the button below."),
        
        shinymaterial::material_modal(
          modal_id = "example_modal",
          button_text = "Upload",
          floating_button = FALSE,
          button_icon = "attach_file",
          button_color = "orange",
          title = "Upload All Files Before Cleaning",
          shiny::tags$div(
            # Modal Content Goes Here
            shinymaterial::material_tabs(
              color = "orange",
              tabs = c(
                "Title Reference" = "title_tab",
                "PPRT" = "pprt_tab",
                "JPACT" = "jpact_tab"
              )
            ),
            shinymaterial::material_tab_content(
              tab_id = "title_tab",
              shiny::fileInput("import_title", "Import Title Reference",
                               accept = c("text/plain") ),
              shiny::uiOutput("titlePath")
            ),
            shinymaterial::material_tab_content(
              tab_id = "pprt_tab",
              shiny::fileInput("import_pprt", "Import PPRT",
                               accept = c("text/plain")),
              shiny::uiOutput("pprtPath")
            ),
            shinymaterial::material_tab_content(
              tab_id = "jpact_tab",
              shiny::fileInput("import_jpact", "Import JPACT",
                               accept = c("text/plain")),
              shiny::uiOutput("jpactPath"),
              shiny::textOutput("my_file_name")
            ),
            shiny::br(),
            
            shiny::div(class = "center-align",
                       shiny::tags$h6("Once all data is uploaded, you may click the button below to clean and prep the data."),
                       
                       shiny::tags$style(appCSS),
                       withBusyIndicatorUI(
                         shiny::actionButton("clean_data", " Clean Data", class = "btn-primary")
                       )
            ),
            shiny::uiOutput("test")
          )
        ),
        
        # shiny::br(),
        # 
        # shiny::tags$h6("The updated item pairs will be placed in the 'data' sub-folder when all the data cleaning rules have been applied. 
        #         Click the link on this page to open the folder."),
        shiny::tags$br(),
        shiny::tags$h6("Do not open the csv files before uploading them to the Career PathFinder admin site. Opening them causes leading
              zeroes to disappear and will prevent the tool from working correctly. If you shiny::need to open the csv files, open them after
              uploading them or just make a copy of them on your desktop.")
        # actionLink("openData", "Open Folder"),
        
        # tableOutput("testTable")
      )
      
    ),
    # CONTENT tab 3
    shinymaterial::material_side_nav_tab_content(
      side_nav_tab_id = "monitor_tab",
      
      shinymaterial::material_card( depth = 0,
                                    shiny::tags$div(
                                      monitor_tab_content()  
                                    )),
      
      shinymaterial::material_tabs(
        color = "orange",
        tabs = c(
          "30 Year Data" = "data_30",
          "15 Year Data" = "data_15") ),
      shinymaterial::material_tab_content(
        tab_id = "data_30",
        ## UI shiny::HTML content here
        shinymaterial::material_card( depth = 0,
                                      shiny::tags$div(
                                        shiny::tags$h4("Overview"),
                                        shinymaterial::material_row(
                                          shinymaterial::material_column( width = 6,
                                                                          DT::DTOutput("monitor_30_tbl") ) ),
                                        shiny::tags$br(),
                                        shiny::tags$h4("Missing Values"),
                                        shiny::tags$h6("The first 7 columns should always have 0% missing values.
                               If there's missing values in one of these, inspect the data."),
                                        shiny::plotOutput("monitor_30_missing"),
                                        shiny::tags$h4("Distribution of Values"),
                                        shiny::tags$h6("Only the continuous variables are plotted. This shows you the range
                               of values for each column. If any values seem unusual for a column,
                               inspect the data further."),
                                        shiny::plotOutput("monitor_30_histogram")
                                      )
        )
      ),
      shinymaterial::material_tab_content(
        tab_id = "data_15",
        ## UI shiny::HTML content here
        shinymaterial::material_card( depth = 0,
                                      shiny::tags$div(
                                        shiny::tags$h4("Overview"),
                                        shinymaterial::material_row(
                                          shinymaterial::material_column( width = 6,
                                                                          DT::DTOutput("monitor_15_tbl") ) ),
                                        shiny::tags$br(),
                                        shiny::tags$h4("Missing Values"),
                                        shiny::tags$h6("The first 7 columns should always have 0% missing values.
                               If there's missing values in one of these, inspect the data."),
                                        shiny::plotOutput("monitor_15_missing"),
                                        shiny::tags$h4("Distribution of Values"),
                                        shiny::tags$h6("Only the continuous variables are plotted. This shows you the range
                               of values for each column. If any values seem unusual for a column,
                               inspect the data further."),
                                        shiny::plotOutput("monitor_15_histogram")
                                      )
        )
      )
    ),
    # CONTENT tab 4
    shinymaterial::material_side_nav_tab_content(
      side_nav_tab_id = "analysis_nav_tab",
      
      shinymaterial::material_tabs(
        color = "orange",
        tabs = c(
          "Instructions" = "instructions",
          "Format Data" = "format") ),
      shinymaterial::material_tab_content(
        tab_id = "instructions",
        ## UI shiny::HTML content here
        shinymaterial::material_card( depth = 0,
                                      shiny::tags$div(
                                        analysis_tab_content()
                                      )
        )
      ),
      shinymaterial::material_tab_content(
        tab_id = "format",
        ## UI shiny::HTML content here
        shinymaterial::material_card( depth = 0,
                                      shiny::tags$div(
                                        shiny::tags$h4("Format Data from Word Press"),
                                        shiny::tags$p("Upload the csv file that you downloaded from the admin site, then
                              click the 'Format User Data' button.")
                                      ),
                                      shinymaterial::material_file_input("uploadUserData", "Upload User Data"),
                                      shiny::actionButton("clean_export", "Format User Data"),
                                      
                                      shiny::uiOutput("analysis_output_UI")
        )
      )
    )
  )
  
  server <- function(input, output, session) {
    
    
    output$jpactPath <- shiny::renderUI({
      
      # Check file name
      if ( !is.null(input$import_jpact$datapath) ) {
        
        if( grepl("JPACT", basename(input$import_jpact$name) ) ) {
          
          shiny::tagList(
            shiny::HTML(
              paste(
                "Upload of JPACT file was ", 
                shiny::tags$span( style="color:green", 
                                  "successful!" 
                ), sep = "" 
              )
            )
          )
        } else {
          shinyjs::alert("You tried uploading the wrong file. Please upload the JPACT text file.")
        }
      } else {
        shiny::tagList(
          shiny::HTML( 
            paste( 
              "Waiting for ", 
              shiny::tags$span(
                style="color:red", 
                "JPACT file"
              ), sep = ""
            ) 
          )
        )
      }
      
    })
    
    # onclick("openData", alert("Remember, don't open the csv files before uploading to the admin site!"))
    
    output$pprtPath <- shiny::renderUI({
      
      # Check file name
      if ( !is.null(  input$import_pprt$datapath ) ) {
        
        if( grepl(  "PAY_POLICY_RATE", basename( input$import_pprt$name ) ) ) {
          
          shiny::tagList(
            shiny::HTML(
              paste( 
                "Upload of PPRT file was ", 
                shiny::tags$span(
                  style="color:green", 
                  "successful!"
                ), sep = ""
              )
            )
          )
        } else {
          shinyjs::alert("You tried uploading the wrong file. Please upload the PAY POLICY RATE (PPRT) text file.")
        }
      } else {
        shiny::tagList(
          shiny::HTML( 
            paste(
              "Waiting for ", 
              shiny::tags$span(
                style="color:red", 
                "PPRT file"
              ), sep = ""
            ) 
          )
        )
      }
      
    })
    
    output$titlePath <- shiny::renderUI({
      
      # Check file name
      if ( !is.null( input$import_title$datapath ) ) {
        
        if( grepl( "TITLE_Reference_Extract", basename(input$import_title$name) ) ) {
          
          shiny::tagList(
            shiny::HTML(
              paste( 
                "Upload of Title Reference file was ", 
                shiny::tags$span(
                  style="color:green", 
                  "successful!"
                ), sep = ""
              )
            )
          )
        } else {
          shinyjs::alert("You tried uploading the wrong file. Please upload the TITLE Reference text file.")
        }
      } else {
        shiny::tagList(
          shiny::HTML( paste("Waiting for ", shiny::tags$span(style="color:red", "Title Reference file"), sep = "") )
        )
      }
      
    })
    
    # Read the Word Press Admin Export Data
    user_data_file <- shiny::reactive({
      shiny::validate( shiny::need( input$uploadUserData, message = FALSE))
      input$uploadUserData
    })
    
    # Enable the button to clean export data
    shiny::observe({
      if ( !is.null( input$uploadUserData ) ) {
        shinyjs::enable("clean_export")
      } else {
        shinyjs::disable("clean_export")
      }
    })
    
    # All data can be used for to find how many paths started and printed, etc.
    all_user_data <- shiny::reactive({
      events <- readr::read_csv( user_data_file()$datapath ) %>%
        dplyr::filter( !Time < lubridate::mdy("05012019") )
    })
    
    # This events data is cleaned up and only concerned with 'Printed' paths
    events <- shiny::eventReactive(input$clean_export, {
      events <- all_user_data() %>% 
        dplyr::mutate(Path = dplyr::lag(`Full Event Url`)) %>% 
        dplyr::mutate(PathDirection = ifelse( as.character(`Job Id`) == stringr::str_sub(Path, start = -4), "Forward", "Reverse")) %>%
        dplyr::filter(`Event Type` == "print") %>% 
        dplyr::filter(Path != "http://pathfinder.devcp.lacounty.gov/wp-json/careerpathfinder/v1/event?type=print") %>%
        dplyr::select(Time, `User Session Id`, PathDirection, Path)
      
      events <- events %>%
        dplyr::mutate(Path_Stripped = stringr::str_replace(events$Path, 
                                                           "http\\:\\/\\/pathfinder\\.devcp\\.lacounty\\.gov\\/wp\\-json\\/careerpathfinder\\/v1\\/event\\?type\\=step\\?steps\\=", "")) 
      
      events <- events %>%
        dplyr::mutate(Path_Stripped = stringr::str_replace(events$Path_Stripped,
                                                           "http\\:\\/\\/pathfinder\\.devcp\\.lacounty\\.gov\\/wp\\-json\\/careerpathfinder\\/v1\\/event\\?type\\=start\\?steps\\=", "")) %>%
        dplyr::select(Time, `User Session Id`, PathDirection, Path_Stripped)
      
      paths_df <- as.data.frame( stringr::str_split(
        events$Path_Stripped, '\";\"', simplify = TRUE) ) 
      
      paths_df <- as.data.frame( mapply(
        stringr::str_pad, 
        paths_df, 
        MoreArgs = list(
          width = 4, 
          side = "left", 
          pad = "0")) )
      
      paths_df <- as.data.frame( mapply( 
        gsub, 
        paths_df, 
        MoreArgs = list(
          pattern = "0000", 
          replacement = NA)) )
      
      paths_df$NumSteps <- as.data.frame( t( !apply(paths_df, 1, is.na) ) ) %>% 
        rowSums(na.rm = TRUE, dims = 1)
      
      names(paths_df) <- c(paste("Title", 1:5), "NumSteps")
      
      paths_df$`Title 1` <- stringr::str_extract(paths_df$`Title 1`, 
                                                 pattern = "=[0-9]{1,6}") %>% 
        stringr::str_extract(pattern = "[0-9]{1,6}") %>% 
        stringr::str_pad(width = 4, side = "left", pad = "0")
      
      events <- cbind(events, paths_df)
      
      events <- events %>% dplyr::select(-Path_Stripped)
      
      events
      
    })  
    
    output$analysis_output_UI <- shiny::renderUI({
      printed_avgSteps <- round( mean( events()$NumSteps, na.rm = TRUE), 2 )
      printed_totalPaths <- nrow( events() )
      
      printed_direction <- events() %>% 
        dplyr::filter(NumSteps > 1) %>% 
        dplyr::group_by(PathDirection) %>% 
        dplyr::summarise(Count = dplyr::n() ) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(Percent = round( Count/sum(Count)*100, 2) )
      
      starts <- all_user_data() %>% 
        dplyr::mutate( Path = dplyr::lag(`Full Event Url`)) %>% 
        # ADD HERE #  extract last 4 chars from event URL and compare to the JOB ID column on the next line. If same, it is a reverse path? Otherwise departure
        dplyr::mutate( PathDirection = ifelse( 
          as.character(`Job Id`) == stringr::str_sub(
            Path, start = -4), 
          "Forward", 
          "Reverse")) %>%
        dplyr::filter( `Event Type` == "start")
      
      help <- nrow( all_user_data() %>% 
                      dplyr::filter(`Event Type` == "help") 
      )
      
      
      shiny::tagList(
        shinymaterial::material_row(
          
          shinymaterial::material_column( width = 4,
                                          DT::datatable( events(),
                                                         options = list(dom = 'ftp') )
          ),
          shinymaterial::material_column( width = 8 )
        ),
        shinymaterial::material_row(
          shiny::downloadButton("downloadCleanedExport")
        ),
        
        shiny::br(),
        
        shinymaterial::material_row(
          shinymaterial::material_column( width = 4,
                                          
                                          DT::datatable( rbind(printed_avgSteps, 
                                                               format(printed_totalPaths, big.mark = ","), 
                                                               format(nrow(starts), big.mark = ",")),
                                                         options = list( dom = 't'),
                                                         width = "300px",
                                                         colnames = c("", ""),
                                                         rownames = c("Printed Paths Avg. Steps", "Total Paths Printed", "Total Paths Started"),
                                                         caption = "Statistics for Printed Career Paths")
          ),
          shinymaterial::material_column( width = 4, 
                                          offset = 1,
                                          shinymaterial::material_card( 
                                            shiny::renderText( 
                                              c( format( help, big.mark = ","), 
                                                 "visitors have used the 'Take-a-Tour' button")
                                            ) )
          )
        )
      )
    })
    
    output$downloadCleanedExport <- shiny::downloadHandler(
      filename = function() {
        paste('events', Sys.Date(), format( Sys.time(), "%I%M %p"), '.csv', sep = ' ')
      },
      content = function(con) {
        readr::write_csv(events(), con)
      }
    )
    
    # Read the JPACT file and show the user it has been read correctly
    jpact_file <- shiny::reactive({
      shiny::validate( shiny::need( input$import_jpact, message = FALSE))
      input$import_jpact
    })
    
    jpact_data <- shiny::reactive({
      
      job_specs = jpact_extract_specs[ complete.cases(jpact_extract_specs), ]
      
      adv <- data.table::setDT( readr::read_fwf( jpact_file()$datapath , skip = 1,
                                                progress = FALSE,
                                                col_types = paste(rep("c", length(job_specs$FieldName)), collapse = ""), # new
                                                readr::fwf_positions(
                                                  job_specs$Start,
                                                  job_specs$End,
                                                  col_names = job_specs$FieldName)))
      
      as.data.frame(adv) 
      
    })
    
    jpact_file_date <- shiny::reactive({
      
      data.table::setDT( readr::read_fwf( jpact_file()$datapath , n_max = 1,
                                          progress = FALSE,
                                          readr::fwf_positions(job_specs$Start,
                                                               job_specs$End,
                                                               col_names = job_specs$FieldName))) %>%
        dplyr::select(1) %>%
        lubridate::mdy()
      
    })
    
    title_file <- shiny::reactive({
      shiny::validate( shiny::need( input$import_title, message = FALSE))
      input$import_title
    })
    
    title_data <- shiny::reactive({
      
      title_specs = title_reference_specs[ complete.cases(title_reference_specs), ]
      
      title <- data.table::setDT( readr::read_fwf( title_file()$datapath,
                                                   progress = FALSE,
                                                   col_types = paste(rep("c", length(title_specs$FieldName)), collapse = ""), # new
                                                   readr::fwf_positions(
                                                     title_specs$Start,
                                                     title_specs$End,
                                                     col_names = title_specs$FieldName)) %>%
                                    dplyr::filter(ExpirationDate == "12319999")
      )
      
      as.data.frame(title)
      
    })
    
    pprt_file <- shiny::reactive({
      shiny::validate( shiny::need( input$import_pprt, message = FALSE))
      input$import_pprt
    })
    
    pprt_data <- shiny::reactive({
      
      pprt_specs = pprt_reference_specs[ complete.cases(pprt_reference_specs), ]
      
      salary <- data.table::setDT(readr::read_fwf( pprt_file()$datapath,
                                                   progress = FALSE,
                                                   col_types = "cccd", # new
                                                   readr::fwf_positions(
                                                     pprt_specs$Start,
                                                     pprt_specs$End,
                                                     col_names = pprt_specs$FieldName)))
      
      as.data.frame(salary)
      
    })
    
    # Enable the button to clean data
    shiny::observe({
      if ( !is.null( input$import_jpact ) & !is.null( input$import_title ) & !is.null( input$import_pprt )  ) {
        shinyjs::enable("clean_data")
      } else {
        shinyjs::disable("clean_data")
      }
    })
    
    shiny::observe({
      shinyjs::onclick("clean_data", hide("clean_data"))
    })
    
    # Hide/Show download button
    
    # shiny::observeEvent(input$clean_data, {
    # shinyjs::show("download_CP_data")
    # withBusyIndicatorServer("clean_data", {
    #   Sys.sleep(1)
    # })
    # })
    
    # Run DataCleaningScript.R
    # shiny::observeEvent(input$clean_data, {
    #   source("./functions/DataCleaningScript.R")
    #   dataCleaning( jpact_data(), pprt_data(), title_data() )
    # })
    
    datasetsOutput <- shiny::eventReactive(input$clean_data, {
      dat <- dataCleaning( jpact_data(), pprt_data(), title_data() )
      return( dat )
    })
    
    shiny::observeEvent(input$clean_data, {
      
      # New Hires/Promos counts are for Career PathFinder stats page updates
      start <- jpact_file_date() %m-% months(6)
      rptMonth <- lubridate::interval( start, jpact_file_date() )
      
      nh_by_class <- jpact_data() %>%
        dplyr::filter( is.na(APPOINTMENT_ID) ) %>%
        dplyr::filter(SUB_TITLE_CD %in% c("A", "D", "N", "L") &
                        !HOME_DEPT_CD %in% c("GJ", "NL", "SC") &
                        PERS_ACTN_CD %in% c("01", "44", "44A", "44B") &
                        lubridate::mdy(EFFECTIVE_DT) %within% rptMonth ) %>%
        dplyr::distinct(EMPLOYEE_ID, PERS_ACTN_CD, TITLE_CD, .keep_all = TRUE)
      
      withBusyIndicatorServer("clean_data", {
        # shinymaterial::material_spinner_show(session, "test")
        
        shiny::insertUI(
          selector = "#clean_data", where = "afterEnd",
          ui = shiny::tagList(
            shiny::tags$br(),
            shiny::downloadButton("download_CP_data"),
            shiny::tags$p("Your datasets, such as ",
                          toupper( names( datasetsOutput() )[1] ),
                          "are ready for download."),
            shiny::tags$br(),
            shiny::tags$p("There were ", format( nrow(nh_by_class), big.mark = ",") , 
                          "new hires and promotions in the last 6 months.")
          )
        )
        
        # shinymaterial::material_spinner_hide(session, "test")
        
      })
      
    })
    
    shiny::observeEvent(input$clean_data, {
      # When the button is clicked, wrap the code in a call to `withBusyIndicatorServer()`
      withBusyIndicatorServer("clean_data", {
        Sys.sleep(3)
      })
    })
    
    # datasetInput <- shiny::reactive({
    #   return(list(rock=rock, pressure=pressure, cars=cars))
    # })
    
    output$download_CP_data <- shiny::downloadHandler(
      filename = 'item_pairs.zip',
      content = function(fname) {
        tmpdir <- tempdir()
        setwd(tempdir())
        print(tempdir())
        
        fs <- c("item_pairs_15.csv", "item_pairs_15_rev.csv", 
                "item_pairs_30.csv", "item_pairs_30_rev.csv")
        readr::write_csv(datasetsOutput()$item_pairs_15, path = "item_pairs_15.csv", na = "")
        readr::write_csv(datasetsOutput()$item_pairs_15_rev, path = "item_pairs_15_rev.csv", na = "")
        readr::write_csv(datasetsOutput()$item_pairs_30, path = "item_pairs_30.csv", na = "")
        readr::write_csv(datasetsOutput()$item_pairs_30_rev, path = "item_pairs_30_rev.csv", na = "")
        print (fs)
        
        utils::zip( zipfile=fname, files=fs)
        
        if( file.exists( paste0( fname, ".zip" ) ) ) {
          file.rename( paste0( fname, ".zip" ), fname )
        }
        
      },
      contentType = "application/zip"
    )
    
    shinyjs::onclick("download_CP_data",
                     shinyjs::alert("Your files are downloading to your 'Downloads' folder")
    )
    
    shinyjs::onclick("downloadCleanedExport",
                     shinyjs::alert("Your file is downloading to your 'Downloads' folder"))
    
    
    # Monitor the Data --------------------------------------------
    
    # 30 Year Data Checks
    output$monitor_30_tbl <- DT::renderDataTable({
      tbl <- cbind(names( DataExplorer::introduce(datasetsOutput()$item_pairs_30)), 
                   transpose( DataExplorer::introduce(datasetsOutput()$item_pairs_30) ) )
      
      tbl <- tbl[c(1,2,5,6,7),]  # keep relevant rows
      
      tbl[,1] <- c("Rows", "Columns", "Empty Columns",
                   "Total Missing Values (Cells)", "Complete Rows")
      
      DT::datatable(
        tbl,  
        width = "500px",
        options = list( dom = 't'),
        colnames = c("", "")
      ) 
    })
    
    output$monitor_30_missing <- shiny::renderPlot({
      DataExplorer::plot_missing( datasetsOutput()$item_pairs_30,
                   ggtheme = ggplot2::theme_minimal(),
                   theme_config = list("legend.position" = "none")) 
    })
    
    output$monitor_30_histogram <- shiny::renderPlot({
      DataExplorer::plot_histogram( datasetsOutput()$item_pairs_30[, c("Incumbents", "ItemCt", "PairCt", "Prob", "Salary1", "Salary1Min", "Salary2", "Salary2Min", "SalaryDiff")],
                      ggtheme = ggplot2::theme_minimal()) 
    })
    
    # 15 Year Data Checks
    
    output$monitor_15_tbl <- DT::renderDataTable({
      tbl <- cbind(names( DataExplorer::introduce( datasetsOutput()$item_pairs_15)), 
                   transpose( DataExplorer::introduce( datasetsOutput()$item_pairs_15) ) )
      
      tbl <- tbl[c(1,2,5,6,7),]  # keep relevant rows
      
      tbl[,1] <- c("Rows", "Columns", "Empty Columns",
                   "Total Missing Values (Cells)", "Complete Rows")
      
      DT::datatable(
        tbl,  
        width = "500px",
        options = list( dom = 't'),
        colnames = c("", "")
      ) 
    })
    
    output$monitor_15_missing <- shiny::renderPlot({
      DataExplorer::plot_missing(datasetsOutput()$item_pairs_15,
                   ggtheme = ggplot2::theme_minimal(),
                   theme_config = list("legend.position" = "none")) 
    })
    
    output$monitor_15_histogram <- shiny::renderPlot({
      DataExplorer::plot_histogram( datasetsOutput()$item_pairs_15[, c("Incumbents", "ItemCt", "PairCt", "Prob", "Salary1", "Salary1Min", "Salary2", "Salary2Min", "SalaryDiff")],
                      ggtheme = ggplot2::theme_minimal()) 
    })
    
    if (!interactive()) {
      session$onSessionEnded(function() {
        stopApp()
        q("no")
      })
    }
    
  }
  
  shiny::shinyApp(ui, server, launch.browser = TRUE, ...)
  
}