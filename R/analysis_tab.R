analysis_tab_content <- function() {
  
  shiny::tagList(
    
    shiny::tags$h6("This page allows you to format the user-generated data exported from 
the Word Press admin site. It provides you a preview of what the data looks like 
           after it has been formatted and some helpful stats about the career paths people have created."),
    shiny::tags$h4("Export Data from Word Press"),
    shiny::tags$h6("You must have login credentials to download user data from the Word Press
           admin site. Assuming you have access, you can download all user data since inception and
                   upload it in this app for analysis using the Format Data tab."),
    shiny::tags$br()
  )
}