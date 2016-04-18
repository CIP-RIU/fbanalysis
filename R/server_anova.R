#' Server openbooks
#' 
#' @param input shinyserver input 
#' @param output shinyserver output
#' @param session shinyserver session
#' @param values reactive values
#' @author Omar Benites
#' @export

anova_server <- function(input, output, session, values){
  

  volumes <- shinyFiles::getVolumes()
  shinyFiles::shinyFileChoose(input, 'file', roots=volumes, session=session,
                  restrictions=system.file(package='base'),filetypes=c('xlsx'))
  
  
  hot_path <- reactive ({
    
    validate(
      need(input$file != "", label = "Please enter an XLSX file. XLS files are forbidden")
    )
    
    if(length(input$file)==0){return (NULL)}
    if(length(input$file)>0){
      hot_file <- as.character(parseFilePaths(volumes, input$file)$datapath)
    }
  })
  
  hot_bdata <- reactive({
    hot_file <- hot_path()
    if(length(hot_file)==0){return (NULL)}
    if(length(hot_file)>0){
      hot_bdata <- readxl::read_excel(path=hot_file , sheet = "Fieldbook")
    }
  })
  
  
    output$trait <- renderUI({
      selectInput('trait_anova', 'Select Trait', c(Choose='', anova_select_options(hot_bdata())),
                  selectize=TRUE)
    })
    
    output$rep <- renderUI({
      selectInput('rep_anova', 'Select Repetitions', c(Choose='', anova_select_options(hot_bdata())),
                  selectize=TRUE)
    })
    
    output$genotypes <- renderUI({
      selectInput('genotypes_anova', 'Select Genotypes', c(Choose='', anova_select_options(hot_bdata())), 
                  selectize=TRUE)
    })
    
  shiny::observeEvent(input$anova_button, {
    shiny::withProgress(message = "Opening Fieldbook...",value= 0,{

      #NOTE: To use pepa report package we need R 3.3.0 or more.
      #NOTE Finally, we always need pandoc installer.
            
      fieldbook <- as.data.frame(hot_bdata())
      trait <- input$trait_anova
      rep <- input$rep_anova
      genotypes <- input$genotypes_anova
      pepa::repo.rcbd(traits = trait, treat = genotypes, rep = rep, data = fieldbook)
      
      })
  })
  
} 


