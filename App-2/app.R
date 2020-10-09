# R Shiny App Bodega Bay SIMZ Data Plots
# DOUBLE PLOT GROUPS!
# This one allows side by side comparison of multiple taxon
# Credit to this example: https://github.com/rstudio/shiny-examples/blob/master/060-retirement-simulation/
# for help building out the function based approach to side-by-side input/output renderings
# and particularly for the server logic required to deal with the variable names.
rm(list=ls())
require(shiny)
require(ggplot2)
require(dplyr)
require(lazyeval)
load(file = 'data/bio.data.RData')
load(file = 'data/dor.data.RData')
load(file = 'data/bottoms.RData')
dor.data <- filter(dor.data, is.na(Transect) == FALSE)

# ================================================================================
# Function for Rendering Inputs
# ================================================================================
renderInputs <- function(prefix){
  wellPanel(
    fluidRow(
      column(5,
             h4("Larval Distribution Plot Inputs"),
             # Choose Taxanomic Resolution
             selectInput(inputId = paste0(prefix,"_taxres"),
                         label = "Choose Taxonomic Resolution",
                         choices = names(bio.data)[13:20],
                         selected = "Binomial"),
             
             # Choose Taxon
             uiOutput(paste0(prefix,"_taxsel")), #This is rendered from code in server ft.
             
             # Choose Additional Grouping Variables
             checkboxGroupInput(inputId = paste0(prefix,"_groups"),
                                label = "Select Grouping Variables",
                                choices = c("Transect" = "Transect",
                                            "Day" = "Day"),
                                selected = NULL)
      ),
      column(5, offset = 2,
             h4("Physical Variable Plot Inputs"),
             # Choose Physical Variable
             selectInput(paste0(prefix,"_phys.input"), "Choose Physical Parameter",
                         choices = levels(dor.data$parameter__name),
                         selected = "temperature")
      )))
}


# ================================================================================
# UI
# ================================================================================
ui <- fluidPage(
  headerPanel("Bodega Bay SIMZ 2014 Plankton Data"),
  fluidRow(
    column(6, h3("Plot Group A")),
    column(6, h3("Plot Group B"))
  ),
  
  fluidRow(
    column(6, renderInputs('a')),
    column(6, renderInputs('b'))),
  
  fluidRow(
    column(6, plotOutput("a_spp_plot")),
    column(6, plotOutput("b_spp_plot"))),
  br(),
  fluidRow(
    column(6, plotOutput("a_phy_plot")),
    column(6, plotOutput("b_phy_plot"))
  )
)
# ================================================================================
# SERVER
# ================================================================================
server <- function(input,output){
  # ================================================================================
  # FUNCTIONS FOR SERVER SECTION
  # ================================================================================
  # Dynamic UI generation: Provide list of taxa; User chooses which to plot
  # Choose Taxon [Conditional on above]
  taxSelui <- function(prefix){
    Choices <- sort(unique(as.character(eval( parse(text=paste0("bio.data$",eval(parse(text=paste0("input$",prefix,"_taxres")))))))))
    selectInput(inputId = paste0(prefix,"_spp"), 
                label = "Choose a Taxon", 
                choices = Choices,
                multiple = F) 
  }
  # *********************************************
  # Define paramter names: these are inputs.
  paramNames <- c('taxres','spp','groups','phys.input')
  # Server logic for getting parameter names
  getParams <- function(prefix) {
    input[[paste0(prefix, "_recalc")]]
    
    params <- lapply(paramNames, function(p) {
      input[[paste0(prefix, "_", p)]]
    }) # makes a list of param names with the plot group prefix appended

    names(params) <- paramNames
    params
  }
  # *********************************************
  # Summary data function
  sumFun <- function(taxres, spp, groups){
    # these are grouping variables used for all outputs regardless of input
    meta_criteria <- c("Depth.m","Stage.category","Distance.offshore")
    #interp returns formula for filtering based on two user inputs: taxres and spp
    filter_criteria <- interp(~ tax.res == taxon,
                              # *&^%*&^%*&^%*& START HERE ^%&*^%&*^%&*^%*&^%
                              tax.res = as.name(taxres), # input$a_taxres
                              taxon = as.character(spp))
    # filter_criteria <- interp(~ tax.res == taxon, tax.res = as.name("Binomial"),
    #                           taxon = as.character("Balanus glandula"))
    # Summarize Data, Compute Abundance using dplyr functions
    # Conditional on "groups" inputs. 'Groups' can be 'Transect' or 'Day'
    if(is.null((groups) == T)){eval(substitute(bio.data %>% group_by_(.dots= c(meta_criteria, var2)) %>%
                                                      summarize(Abundance = mean(Total.no, na.rm = TRUE)) %>%
                                                      filter_(filter_criteria),
                                                    list(var2 = taxres)))}
    else{eval(substitute(bio.data %>% group_by_(.dots = c(meta_criteria, var1, var2)) %>%
                           summarize(Abundance = mean(Total.no, na.rm = TRUE)) %>%
                           filter_(filter_criteria),
                         list(var1 = as.character(groups),
                              var2 = as.character(taxres))))}
  }
  # *********************************************
  # PLOT
  # Biological Plot Function
  plot_spp_ft <- function(sum.d,input){
    title <- as.character(input$spp) #Define title as name of taxon
    
    # Base plot
    p <- ggplot(data = sum.d,
                aes(x = Distance.offshore,
                    y = Depth.m,
                    size = Abundance)) +
      geom_point(alpha = 0.5) +
      scale_y_reverse() +
      xlab("Distance from Shore (meters)") + ylab("Depth (meters)") +
      ggtitle(title) + theme_bw() +
      theme(panel.grid.minor = element_line(color='grey',size=0.3),
            panel.grid.major = element_line(color='grey',size=0.5)) +
      scale_x_continuous(minor_breaks = seq(0,3500,100),
                         breaks = seq(0,3500,500))
    
    # Additional plot elements contingent on input values
    if(is.null((input$groups)) == T){p <- p + facet_grid(.~Stage.category)}
    else if(input$groups == "Transect" && length(input$groups) == 1){
      bio.data$Transect <- factor(bio.data$Transect, levels= c('Salmon Creek','Mussel Point', 'Bodega Head'))
      
      p <- p + facet_grid(Transect~Stage.category)
       }
    else if(length(input$groups) == 2){
      bio.data$Transect <- factor(bio.data$Transect, levels= c('Salmon Creek','Mussel Point', 'Bodega Head'))
      p <- p + aes(color = factor(Day)) + facet_grid(Transect~Stage.category) + guides(color=guide_legend(title="Day"))}
    else if(input$groups == "Day" && length(input$groups) == 1){
      p <- p + aes(color = factor(Day)) + facet_grid(.~Stage.category) + guides(color=guide_legend(title="Day"))}
    return(p)
  }
  # *********************************************
  # PLOT
  # Physical Plot Function
  plot_phy_ft <- function(input){
    filter_criteria_2 <- interp(~ par == par.in,
                                par = as.name("parameter__name"),
                                par.in = as.character(input$phys.input))
    dd <-  filter_(dor.data, filter_criteria_2)
    plot <- ggplot(data = dd,
                   aes(x = Distance.offshore,
                       y = measurement__depth,
                       color = datavalue)) +
      geom_point(size = 6) + facet_grid(Transect~.) +scale_y_reverse() +
      scale_color_gradient(low="blue",high="red")+
      xlab("Distance from Shore (meters)") + ylab("Depth (meters)") +
      ggtitle(paste('Dorado data: ', input$phys.input)) + 
      guides(color = guide_legend(title = paste(input$phys.input),
                                  reverse = T))
    return(plot)
  }
  
  
  # *********************************************
  # Generate UI based on user input: These are for taxon selectors based on taxres
  output$a_taxsel <- renderUI({
   taxSelui('a')
  })
  output$b_taxsel <- renderUI({
    taxSelui('b')
  })
  # *********************************************
  # Create data summaries
  sum.a <- reactive({do.call(sumFun, getParams('a')[1:3])
    })
  
  sum.b <- reactive({do.call(sumFun, getParams('b')[1:3])
    })
  # *********************************************
  # Generate output plots from data summaries
  output$a_spp_plot <- renderPlot({
    plot_spp_ft(sum.a(), getParams('a'))
  })
  
  output$b_spp_plot <- renderPlot({
    plot_spp_ft(sum.b(), getParams('b'))
  })
  
  output$a_phy_plot <- renderPlot({
    plot_phy_ft(getParams('a'))
  })
  
  output$b_phy_plot <- renderPlot({
    plot_phy_ft(getParams('b'))
  })
}

shinyApp(ui = ui, server = server)
