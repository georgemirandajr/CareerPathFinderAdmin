

how_to_content <- function() {
  
  shiny::tagList(
    shiny::tags$h3("How to Use This App"),
    
    shiny::tags$p("The purpose of this application is to prepare the data for use in the Career PathFinder tool. 
Simply upload e-HR extract text files and click a button to apply data cleaning rules, create item pairs, and create export
           files. The exported .csv files are zipped in a folder within your 'Downloads' folder."),
    shiny::tags$h4("How To Use This Application"),
    shiny::tags$div(
      shiny::tags$p("Update the data for the Career PathFinder tool by following these steps:"),
      shiny::tags$ol(
        shiny::tags$li("Upload the needed files in the 'Input/Output' tab (i.e., EMPL_JPACT_EXTR, PAY_POLICY_RATE, TITLE_Reference_Extract)"), 
        shiny::tags$li("Clean the data by clicking the 'Clean Data' button"), 
        shiny::tags$li("Make a copy of the export files and review them for any anomalies."),
        shiny::tags$li("Upload the original 4 datasets (the ones you did not view) to WordPress.")
      )
    ),
    shiny::tags$h4("Behind the Scenes"),
    shiny::tags$div(
      shiny::tags$p("The DataCleaningScript.R makes use of clean_careers.R to process the system-extracted text files
             by applying the following data cleaning rules:"),
      shiny::tags$ol(
        shiny::tags$li("Delete expired classifications (using the 'EXPIRED' keyword in the TITLE_Reference_Extract)"),
        shiny::tags$li("Delete multiple lateral transfers"),
        shiny::tags$li("Delete lateral transfers within 30 days"),
        shiny::tags$li("Delete demotions that are beyond 5.5% decrease in salary")
      ),
      shiny::tags$p("Once the data rules have been applied, DataCleaningScript.R calls the functions make_pairs.R and make_pairs_rev.R. 
These functions create the final datasets containing summarized career movements. 
The forward and backward item pairings for 30 and 15 year datasets are exported to your 'Downloads' folder.")
    )
  )
}