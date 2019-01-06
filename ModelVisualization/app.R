#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# source(paste(getwd(), '/app.R', sep=''))
# runApp()

#
# To run from an R command line
# library(shiny)
# shiny::runGitHub('ChicagoRTI/Modeling','don-morrison-2000', subdir='/ModelVisualization')

# Load packages
library(shiny)
library(shinythemes)
library(shinyjs)
library(ggplot2)
library(foreign)
library(nnet)
library(reshape2)
library(stringr)
library(Hmisc)



# Set this to 1 for fast multinomial testing (100 for production)
TEST <- 0
#MAX_ITER <- ifelse (TEST==0, 100, 1)
TOP_TEN <- ifelse (TEST==0, 10, 3)
#DO_P_VALUES <- ifelse (TEST==0, TRUE, FALSE)
DO_SCALE <- ifelse(TEST==0, TRUE, TRUE)
MAX_ABUNDANCE_LEVEL <- 10


g_land_use <- read.delim('https://don-morrison-2000.github.io/data/land_use.csv', as.is=TRUE, header=FALSE)
g_common_species <- sort(unique(read.delim('https://don-morrison-2000.github.io/data/common_species.csv', as.is=TRUE, header=FALSE)$V1))
g_species_sets <- c("Top 10 species", "Top 10 genera", "Common species")
g_usda_genus_to_family_map <- read.delim(file="https://don-morrison-2000.github.io/data/usda_genus_to_family_map.csv", sep=',', header=TRUE, row.names=1)
g_taxonomy_map = data.frame(Species=character(), Genus=character(), Family=character())
g_taxonomic_levels = c("Species", "Genus", "Family")

# Define all possible quantitative predictors
g_all_predictors_quantitative <- c (
      'Trunk diameter' =           'DBH_IN', 
      'Crown compactness' =        'CROWN_COMPACTNESS',
      'Max Height' =               'HEIGHT_MAX',
      'Mean Height' =              'HEIGHT_MEAN',
      'Crown area' =               'CROWN_AREA',
      'Building age' =             'BLDG_AGE',
      'Housing density' =          'HU_DENS',
      'Relative border' =          'RELBORDER_TREE',
      'NDVI max' =                 'NDVI_MAX',
      'NDVI mean' =                'NDVI_MEAN',
      'NDVI std'=                  'NDVI_STD'
      #      'Patch density' =            'PD',
      #      'Largest patch index' =      'LPI',
      #      'Landscape shape index' =    'LSI',
      #      'Distance to water' =        'DIST_WATER',
      #      'Distance to road (minor)' = 'DIST_MINOR',
      #      'Distance to road (major)' = 'DIST_MAJOR',
)
g_all_predictors_quantitative <- g_all_predictors_quantitative[order(names(g_all_predictors_quantitative))]

g_all_predictors_categorical <- c('Land use' = 'LU')
g_all_predictors <- c(g_all_predictors_quantitative, g_all_predictors_categorical)

g_data_descriptors_local_file <<- paste(getwd(), '/data/data_descriptors.rds', sep='') # Warning - getcwd() returns different results when call from "source"vs "runApp"
#g_data_descriptors_local_file <<- 'D:/CRTI/r_projects/ModelVisualization/data/data_descriptors.rds'

g_data_descriptors_http_url <- 'https://don-morrison-2000.github.io/source/shiny/chicago_tree_ui/data_descriptors.rds'
g_data_descriptors_fn <- if (file.exists(g_data_descriptors_local_file)) g_data_descriptors_local_file else gzcon(url(g_data_descriptors_http_url))

g_label_font <- element_text(family="sans", color='black', size=16)
g_data_font <- element_text(family="sans", face="bold", color='black', size=12)
g_hr <- tags$hr(style="height:1px;border:none;color:#333;background-color:#333;")



read_data <- function (fn)
{
      # Read in the data
      ctree <- read.delim(fn, as.is=TRUE, sep=',') 
      # Remove extraneous columns
      extra <- setdiff(names(ctree), c('GENUSSPECI',g_all_predictors))
      ctree <- ctree[, !(names(ctree) %in% extra)]
      # Add in missing columns, initialized with zeros
      missing <- setdiff(c('GENUSSPECI',g_all_predictors),names(ctree))
      missing_df <- data.frame(matrix(nrow=nrow(ctree), ncol=length(missing),0))
      names(missing_df) <- missing
      ctree <- cbind(ctree, missing_df)
      # Arrange predictor columns in alphabetical order (no special reason for doing this)
      ctree <- ctree[, c('GENUSSPECI',sort(g_all_predictors))]
      # Convert model categories to factors that match the UI names
      ctree$LU <- as.factor(g_land_use$V2[as.numeric(as.character(ctree$LU))])
      # Add a column with the genus name and convert taxanomic ranks to factors
      ctree$GENUSSPECI <- as.factor(ctree$GENUSSPECI)
      
      return (ctree)
}

filter_data_x <- function (ctree, filter_species_set, filter_species_set_others)
{
      # Convert columns back to non-factors so we can manipulate the contents better
      ctree$GENUSSPECI <- as.character(ctree$GENUSSPECI)
      
      if (filter_species_set == 'Top 10 species')
      {
            species_names_all <- sort(names(head(sort(table(factor(ctree$GENUSSPECI)), decreasing = TRUE), TOP_TEN)))
      }
      else if (filter_species_set ==  'Top 10 genera')
      {
            # Coerce all species names to genus only, then select the top 10
            ctree$GENUSSPECI <-  as.character(g_taxonomy_map[ctree$GENUSSPECI, 'Genus'])
            species_names_all <- sort(names(head(sort(table(factor(ctree$GENUSSPECI)), decreasing = TRUE), TOP_TEN)))
      }
      else 
      {
            # Default to common species
            species_names_all <- g_common_species
      }
      if (filter_species_set_others == 'Yes')
      {
            # Coerce all non-common species names to "Other"
            ctree$GENUSSPECI <- ifelse ((match(ctree$GENUSSPECI, species_names_all, nomatch = 0) > 0), ctree$GENUSSPECI, "Other")
            species_names_all <- c(species_names_all, "Other")
      }
      ctree <- subset(ctree, GENUSSPECI %in% species_names_all)
      
      # Convert the columns back to factors
      ctree$LU <- as.factor(ctree$LU)
      ctree$GENUSSPECI <- as.factor(ctree$GENUSSPECI)
      return (ctree)
}


update_taxonomy_map <- function (ctree)
{
      # Create a data frame - each row is a unique genus/species in the survey data
      taxonomy_map <- data.frame(Species=unique(as.character(ctree$GENUSSPECI)),stringsAsFactors=FALSE)
      taxonomy_map$Genus <-  as.factor(sapply(strsplit(taxonomy_map$Species," "),"[",1))
      
      # Add the family column to the dataframe and set unresolved mappings to 'Unknown'
      taxonomy_map$Family = g_usda_genus_to_family_map[as.character(taxonomy_map$Genus),'family']
      levels(taxonomy_map$Family) <- c(levels(taxonomy_map$Family), 'Unknown')
      taxonomy_map[is.na(taxonomy_map$Family),'Family'] <- 'Unknown'
      
      # Add in the existing entries in the global taxonomy map and return it (sorted by species)
      taxonomy_map <- unique(rbind(taxonomy_map, g_taxonomy_map))
      taxonomy_map <- taxonomy_map[order(taxonomy_map$Species),]
      row.names(taxonomy_map) <- taxonomy_map$Species
      return (taxonomy_map)
}


# Functions to allow prediction page to generated programatically
pred_row <- function (pred, data_descriptor_name) {return (list(cb(pred), sl(pred, data_descriptor_name)))}
cb <- function(pred) {checkboxInput (inputId = paste('predict_on_', pred, sep=''), label = strong(names(g_all_predictors_quantitative[g_all_predictors_quantitative==pred])), width = '600px', value = FALSE)} 
sl <- function(pred, data_descriptor_name) {conditionalPanel (condition = paste('input.predict_on_', pred, '==true', sep=''), sliderInput (inputId = paste('predict_', pred, sep=''), label = '', min = -999, max = -999, value = -999), g_hr)}



options(shiny.maxRequestSize=50*1024^2)

# Define UI
ui <- navbarPage("CRTI Tree Data", theme = shinytheme("cyborg"), selected = p(icon('eye-open', lib = "glyphicon"),'Model'),
                tags$head(tags$style(HTML("
                                          .shiny-notification {height:100px; width:600px; position:fixed; opacity:1.0; top:calc(50% - 50px);; left:calc(50% - 300px);;}
                                          .table.shiny-table > tbody > tr > td {padding-top: 0px; padding-bottom: 0px; line-height: 1;}
                                          .checkbox {margin-bottom: 0px;}
                                          .radio {margin-bottom: 0px;}
                                          "))),
                useShinyjs(),
                tabPanel(
                      title=p(icon('eye-open', lib = "glyphicon"),'Model'),
#                shinythemes::themeSelector(),
                            fluidRow
                            ( 
                                  column(3,selectInput(inputId = "data_descriptor_name", label = strong("Dataset"),  choices = '', selected = '')),
                                  column(2,selectInput(inputId = "filter_species_set", label = strong("Species Set"),  choices = g_species_sets, selected = g_species_sets[1])),
                                  column(2,selectInput(inputId = "filter_species_set_others", label = strong("Include others"),  choices = c('Yes','No'), selected = c('Yes')))
                            ),
                            g_hr,
                            tabsetPanel
                            (
                                  selected = p(icon("stats", lib = "glyphicon"),'Species ocurrence probability'),
                      

                                  tabPanel
                                  (
                                        id="model",
                                        title=p(icon("stats", lib = "glyphicon"),'Species ocurrence probability'), 
                                        fluidRow 
                                        (
                                              column 
                                              ( 
                                                    width=8,
                                                    wellPanel
                                                    (  
                                                          width=8, style = "max-width: 800px", 
                                                          plotOutput(outputId = "probability_plot", height = '300px', dblclick = "plot_dblclick",  brush = brushOpts(id = "plot_brush", resetOnNew = TRUE))
                                                    )
                                              ),
                                              column 
                                              ( 
                                                    width=2,
                                                    wellPanel
                                                    (  
                                                          checkboxInput (inputId = "plot_stack", label = strong("Stack"), value=FALSE),
                                                          checkboxInput (inputId = "plot_observations", label = strong("Observations"), value=FALSE),
                                                          checkboxInput (inputId = "show_statistics", label = strong("Show statistics"), value=FALSE),
                                                          checkboxInput (inputId = "show_p_values", label = strong("Show p-values"), value=FALSE)
                                                    )
                                              )
                                        ),
                                        fluidRow 
                                        (
                                              column 
                                              ( 
                                                    width=12,
                                                    (
                                                          conditionalPanel
                                                          ( 
                                                                condition = 'input.show_statistics == true', 
                                                                wellPanel
                                                                (  
                                                                      title='Statistics',
                                                                      width=12, style = "overflow-y:scroll; max-height: 350px",
                                                                      tableOutput(outputId = "outText")
                                                                )
                                                          )
                                                    )
                                              )
                                        ),
                                        fluidRow 
                                        (
                                              column 
                                              ( 
                                                    width=12,
                                                    (
                                                          conditionalPanel
                                                          ( 
                                                                condition = 'input.show_p_values == true', 
                                                                wellPanel
                                                                (  
                                                                      title='p-Values',
                                                                      width=12, style = "overflow-y:scroll; max-height: 350px",
                                                                      tableOutput(outputId = "out_p_values")
                                                                )
                                                          )
                                                    )
                                              )
                                        ),
                                        
                                        fluidRow 
                                        (
                                              column 
                                              ( 
                                                    width=3,
                                                    wellPanel
                                                    (  
                                                          radioButtons (inputId = "plot_predictor", label = strong("Plot Predictor"), choices = g_all_predictors_quantitative, selected = g_all_predictors_quantitative[1])
                                                    )
                                              ),
                                              column 
                                              ( 
                                                    width=3,
                                                    wellPanel
                                                    (  
                                                          checkboxGroupInput (inputId = "land_use", label = strong("Land Use"), choices = g_land_use$V2, selected = g_land_use$V2),
                                                          actionButton("land_use_none", label = strong("None")),
                                                          actionButton("land_use_all", label = strong("All"))
                                                    )
                                              ),
                                              column 
                                              ( 
                                                    width=3,
                                                    wellPanel
                                                    ( 
                                                          checkboxGroupInput (inputId = "species", label = strong("Species"), choices = NULL, selected = NULL),
                                                          actionButton("species_none", label = strong("None")),
                                                          actionButton("species_all", label = strong("All"))
                                                    )
                                              )
                                        )
                                        
                                  ),
                                  tabPanel
                                  (
                                        id="predict",
                                        title=p(icon("dashboard", lib = "glyphicon"),'Predict tree species'), 
                                        column 
                                        ( 
                                              width=3,
                                              wellPanel
                                              (
                                                    width=3,
                                                    lapply (g_all_predictors_quantitative, pred_row, data_descriptor_name=NULL),
                                                    selectInput("predict_LU", label = strong('Land Use'),  choices = NULL, selected = NULL)
                                              )
                                        ),
                                        column 
                                        ( 
                                              width=7,
                                              wellPanel
                                              (  
                                                    width=8, 
                                                    plotOutput(outputId = "out_piechart", height = "300px")
                                              ),
                                              wellPanel
                                              (  
                                                    width=5,
                                                    style="td line-height:0px; td padding-top: 0px, td padding-bottom: 0px; #shiny-html-output td color: #4d3a7d;",
                                                    tableOutput(outputId = "out_prediction")
                                              )
                                        )
                                  )
                            )
                  ),
                  tabPanel
                  (
                        title=p(icon('info-sign', lib = "glyphicon"),'Describe'),
                        fluidRow
                        ( 
                              column(3,selectInput(inputId = "ui_data_descriptor_name", label = strong("Dataset"),  choices = '', selected = '')),
                              column(3,radioButtons(inputId = "ui_taxonomic_unit", label = "",  choices = g_taxonomic_levels, selected = g_taxonomic_levels[1])),
                              column(1, offset=5, style = "margin-top: 25px;", actionButton("ui_help", "", icon=icon('question-sign', lib = "glyphicon")))
                        ),
                        g_hr,
                        fluidRow (
                              column (
                                    width=3,
                                    wellPanel (id="ui_taxa_panel",
                                          sliderInput (inputId = "ui_abundance_level", label = strong("Abundance level"),  min=1, max=MAX_ABUNDANCE_LEVEL, value=4),
                                          checkboxGroupInput (inputId = "ui_taxon", label = strong("Taxon"),  choices = "Acer platanoides", selected = "Acer platanoides"),
                                          actionButton("ui_taxon_none", label = strong("None")),
                                          actionButton("ui_taxon_all", label = strong("All"))
                                    )
                              ),
                              column (
                                    width=3,
                                    wellPanel (
                                          sliderInput (inputId = "ui_bins", label = strong("Quantiles"),  min=1, max=10, value=4),
                                          selectInput (inputId = "ui_var",  label="", choices=g_all_predictors_quantitative, selected=names(g_all_predictors_quantitative)[1])
                                    ),
                                    wellPanel (
                                          checkboxGroupInput (inputId = "ui_land_use", label = strong("Land use"),  choices = NULL, selected = NULL),
                                          actionButton("ui_land_use_none", label = strong("None")),
                                          actionButton("ui_land_use_all", label = strong("All"))
                                    )
                              ),
                              column (
                                    width=6,
                                    wellPanel (
                                          style = "overflow-y:scroll; min-height: 300px; max-height: 1000px",
                                          uiOutput(outputId = "ui_chart", width = '600px', height = "300px"),
                                          actionButton("ui_flip_chart", label = strong("Flip")),
                                          actionButton("ui_expand_collapse_chart", label = strong("Expand"))
                                    )
                              )
                        )
                  ),



                 tabPanel
                 (
                       id="admin",
                       title=p(icon("cog", lib = "glyphicon"),'Admin'),

                       fluidRow (
                             column (
                                   width=9,
                                   wellPanel
                                   (  
                                         title='Datasets', width=3, 
                                         tableOutput(outputId = "admin_datasets")
                                   )
                             ),
                             column(1, style = "margin-top: 25px;", actionButton("admin_help", "", icon=icon('question-sign', lib = "glyphicon")))
                       ),
                       g_hr,
                       
                       fluidRow (
                             column (
                                   width=3,
                                   selectInput (inputId = "admin_action_name", label = strong("Action"),  choices=c('Add dataset','Rebuild model', 'Delete dataset', 'Save configuration'), selected='Add dataset')
                             ),
                             column 
                             (
                                   width=6,
                                   conditionalPanel
                                   ( 
                                         condition = 'input.admin_action_name == "Add dataset"', 
                                         (
                                               wellPanel
                                               (
                                                     textInput(inputId='admin_new_data_name', label='Name', value = "", width = '250px', placeholder = 'Specify the dataset display name'),
                                                     fileInput(inputId = 'admin_new_data_file', label='Data file', multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"), width = NULL, buttonLabel = "Browse", placeholder = "Specify cleaned .csv data file"),
                                                     g_hr,
                                                     actionButton(inputId = "admin_new_data_add", label = strong('Add'), icon("plus", lib = "glyphicon"))
                                                     
                                               )
                                         )
                                   ),
                                   conditionalPanel
                                   ( 
                                         condition = 'input.admin_action_name == "Rebuild model"', 
                                         (
                                               wellPanel
                                               (
                                                     selectInput(inputId = "admin_update_name", label = 'Name',  choices = NULL, selected = NULL),
                                                     numericInput(inputId = "admin_update_iterations", label="Iterations", value=100, min = 1, max = 1000, width = '100px'),
                                                     checkboxInput (inputId = "admin_update_calculate_p_values", label = "Calculate p-values", value = FALSE),
                                                     g_hr,
                                                     actionButton(inputId="admin_update", label=strong("Rebuild"), icon("refresh", lib = "glyphicon"))
                                                )   
                                         )
                                   ),
                                   conditionalPanel
                                   ( 
                                         condition = 'input.admin_action_name == "Delete dataset"', 
                                         (
                                               wellPanel
                                               (
                                                     selectInput(inputId = "admin_delete_name", label = 'Name',  choices = NULL, selected = NULL),
                                                     g_hr,
                                                     actionButton(inputId="admin_delete", label=strong("Delete"), icon("trash", lib = "glyphicon"))
                                               )   
                                         )
                                   ),
                                   conditionalPanel 
                                   ( 
                                         condition = 'input.admin_action_name == "Save configuration"', 
                                         (
                                               wellPanel
                                               (
                                                     actionButton(inputId = "admin_save_configuration", label = strong('Save'), icon("save", lib = "glyphicon")) 
                                               )
                                         )
                                   )
                             ),
                             column(1, style = "margin-top: 25px;", actionButton("admin_help_action", "", icon=icon('question-sign', lib = "glyphicon")))
                       )
                 )


)

# Define server function
server <- function(input, output, session) 
{
      
      

      ################################################################################################################
      # Reactive values
      ################################################################################################################

      r_values  <- reactiveValues(
                  species_names_all = NULL,                 # List of species name in the current "species set" (after filtering)  
                  x = NULL,                                 # Zoomable x coordinate
                  y = NULL,                                 # Zoomable y coordinate
                  run_predict_go = FALSE,                   # Flip the setting to trigger an update on the predictions
                  abundance_level = 0,
                  display_taxon_list = vector("character"),   # List of species to display
                  selected_taxon_list = vector("character"),  # Selected species (including those not dislayed)
                  land_use_list = vector("character"),
                  selected_data_descriptor_name = NULL,          # Psuedo "parameter" passed to get_data
                  flip_chart = TRUE,
                  collapse_chart = TRUE,
                  selected_land_use = NA,                       # Save state to be used when data_descriptor changes
                  data_descriptors = list(readRDS(g_data_descriptors_fn))[[1]]
      )


      
      
      get_model_hash_id <- function (data_descriptor_name,     # Data_descriptor name
                                   land_use,                 # Set of land use names
                                   species_set,              # Set of species (or genera) names
                                   model_predictors,         # Predictor variables (both quantitative and categorical)
                                   model_type,               # Model type (binomial or multinomial)
                                   species_set_others)       # Fold all other species into "other"?
      {
            return (digest::digest(paste(data_descriptor_name, land_use, species_set, model_predictors, model_type, species_set_others)))
      }
      

      
      ################################################################################################################
      # 
      # Model tab 
      # 
      ################################################################################################################  
      
      
      # Inputs:
      #     cf: coefficients matrix (including intercept) from multinomial model
      #     fullx: matrix - one row per prediction, one column per prediction variable
      #     ref_spp: reference species name for the species categorical prediction variable
      # Output:
      #     matrix of probabilites - one row for each requested prediction, one column per species (all rows add up to 1)
      do_predict_multinomial <- function (cf, fullx, ref_spp)
      {
            X <- as.matrix(cbind(1,fullx))
            cf <- t(cf)
            denom <- rep(1,nrow(X))
            for (i in seq(1,ncol(cf)))
            {
                  denom <- denom + exp(X %*% cf[,i])
            }
            result <- matrix(sapply(1:ncol(cf), function(i) exp(X %*% cf[,i]) / denom),ncol=ncol(cf))
            result <- cbind(1 - rowSums(result), result)
            colnames(result) <- c(ref_spp, colnames(cf))
            return (result)
      }
      
      
      # Inputs
      #     model: list of model-related objects
      #     m_preds_q_mx: matrix - quantitative predictor values matrix - one row per prediction, one column per quantitative prediction variable. 
      #     land_use: land use categories to include in the predictions
      # Output:
      #     matrix of predictions - one row per prediction, one column per species
      predict_multinomial <- function (model, m_preds_q_mx, land_use, pred_q_range)
      {
            num_predictions <- nrow(m_preds_q_mx)
            prediction <- matrix(0, nrow=num_predictions, ncol=length(model$spps))
            colnames(prediction) <- model$spps
            
            # Scale predictors from 0 to 1
            if (DO_SCALE)
            {
                  for (pred_name_q in colnames(m_preds_q_mx))
                  {
                        m_preds_q_mx[,pred_name_q] <- as.vector(scale(m_preds_q_mx[,pred_name_q], center=pred_q_range[pred_name_q,'min'], scale=pred_q_range[pred_name_q,'diff']))
                  }
                  m_preds_q_mx[!is.finite(m_preds_q_mx)]=0  #Handle the edge case where scale returns NAN or infinite because there is no variation
            }
            
            if (length(model$pred_c_spec) == 0 )
            {
                  prediction  <- do_predict_multinomial (cf, m_preds_q_mx, model$ref_spp)
            }
            else if (length(model$pred_c_spec) == 1 )
            {
                  for (pred_c_spec in model$pred_c_specs)
                  {
                        prediction_input <- cbind(m_preds_q_mx, matrix(NA, nrow=num_predictions, ncol=length(pred_c_spec$cat_cf_names)))
                        colnames (prediction_input) <- c(colnames(m_preds_q_mx), pred_c_spec$cat_cf_names)
                        
                        for (lu_cat in land_use)
                        {
                              prediction_input[,pred_c_spec$cat_cf_names] <- 0
                              if (lu_cat != pred_c_spec$cat_name_ref)
                              {
                                    prediction_input[,paste(pred_c_spec$cat_pred_name, lu_cat, sep='')] <- 1
                              }
                              p <- do_predict_multinomial (model$cf, prediction_input, model$ref_spp)
                              prediction <- prediction + p * pred_c_spec$cat_weights[lu_cat]
                        }   
                  }
            }
            else 
            {
                  return (NULL) # Can't handle multiple categorical predictors yets
            }
            
            # VERIFICATION - run the same prediction through the multinom model object
            all_pred_df <- data.frame(m_preds_q_mx[,g_all_predictors_quantitative, drop=FALSE])
            result_df2 <- data.frame (x=numeric(), y=numeric(), species=character())
            prediction2 <- matrix(0, nrow=num_predictions, ncol=length(model$spps))
            colnames(prediction2) <- model$spps
            for (lu_cat in land_use)
            {
                  all_pred_df$LU <- lu_cat
                  prediction2 <- prediction2 + predict(model$model_reduced, newdata=all_pred_df, type="probs", se=TRUE) * pred_c_spec$cat_weights[lu_cat]
            }
            if ((sum(abs(prediction - prediction2) > .000001)) > 0) 
            {
                  print ("Warning. Prediction mismatch")
            }
            
            return (prediction)
      }
      
      
      get_model <- reactive({
            if (is.null(r_values$data_descriptors[[input$data_descriptor_name]]$ctree))
            {
                  return (NULL)
            }

            filter_data <- filter_data()
            model_hash_id <- get_model_hash_id(input$data_descriptor_name, levels(filter_data$LU), input$filter_species_set, g_all_predictors, input$filter_model_type, input$filter_species_set_others)
            
            if (nrow(filter_data) == 0)
            {
                  return (NULL)
            }
            
            for (data_descriptor in r_values$data_descriptors)
            {
                  if (!is.null(data_descriptor$models[[model_hash_id]]) )
                  {
                        return (data_descriptor$models[[model_hash_id]])
                  }
            }
            return (NULL)
      })
      
      
      get_data_r <- reactive({
            ctree <- r_values$data_descriptors[[r_values$selected_data_descriptor_name]]$ctree
            if (is.null(ctree))
            {
                  withProgress (message='Reading dataset', value=0, max=1, {
                        incProgress (amount=.5, detail=r_values$selected_data_descriptor_name)
                        fn <- paste(getwd(), '/data/', r_values$selected_data_descriptor_name, '.csv', sep='')
                        if (!file.exists(fn))
                        {
                              fn <- URLencode(paste('https://don-morrison-2000.github.io/source/shiny/chicago_tree_ui/', r_values$selected_data_descriptor_name, '.csv', sep=''))
                        }
                        ctree <- read_data(fn) 
                        r_values$data_descriptors[[r_values$selected_data_descriptor_name]]$ctree <- ctree
                        g_taxonomy_map <<- update_taxonomy_map(ctree)
                  })
            }
            return (ctree)
            
      })
      
      


      ################################################################################################################
      # 
      # Model -> Species occurence tab
      # 
      ################################################################################################################
      
      
      get_regression_coords = function(filter_model_type, ctree, model, m_preds, p_pred, spps, land_use, pred_q_range)
      {
            num_predictions = 101
            
            # Create a matrix with the means of all the quantitative predictors
            m_preds_q_means <- colMeans(ctree[g_all_predictors_quantitative], na.rm=TRUE)
            m_preds_q_mx <- t(matrix(rep(m_preds_q_means,num_predictions),nrow=length(g_all_predictors_quantitative)))
            colnames(m_preds_q_mx) <- g_all_predictors_quantitative
            m_preds_q_mx[,p_pred] <- seq(min(ctree[,p_pred],na.rm=TRUE),max(ctree[,p_pred],na.rm=TRUE),len=num_predictions)
            # Run the prediction and convert to long format
            result_mx <- predict_multinomial(model, m_preds_q_mx, land_use, pred_q_range)

            result_df <- melt(as.data.frame(cbind(x=m_preds_q_mx[,p_pred], result_mx[,spps[spps %in% colnames(result_mx)],drop=FALSE])),id.vars='x')
            colnames(result_df) <- (c('x','species','y'))
            return (result_df[c('x','y','species')])
      }
      
      
      get_occurrence_coords <- function (ctree, spps, predictor)
      {
            num_bins = 20
            occurrence_coords <- data.frame(x=numeric(), y=numeric(), species=character()) 
            
            for (spp in spps)
            {
                  ctree[,'occur'] <- ifelse(ctree$GENUSSPECI==spp, 1,0)
                  bins <- unique(quantile(ctree[,predictor], prob=seq(0,1,len=num_bins),na.rm=TRUE))
                  num_bins <- length(bins)
                  if (num_bins > 1)
                  {
                        bins[num_bins] <- bins[num_bins] + 1                  ## Necessary so the highest x falls in the last bin
                        x_binned <- cut(ctree[,predictor], breaks=bins, right=FALSE)
                        
                        mean_x <- tapply(ctree[,predictor], x_binned, mean)
                        mean_y <- tapply(ctree$occur, x_binned, mean)
                        occurrence_coords <- rbind (occurrence_coords, data.frame(x=mean_x, y=mean_y, species=rep(spp, length(mean_x))))
                  }
                  else
                  {
                        occurrence_coords <- rbind (occurrence_coords, data.frame(x=0, y=0, spp))  # Edge case where there is no variation
                  }
            }
            return (occurrence_coords)
            
      }
      

      # Observe the filter tab's species list
      observeEvent (r_values$species_names_all, {
            species_intersect <- input$species[input$species %in% r_values$species_names_all]
            updateCheckboxGroupInput (session, "species", choices = r_values$species_names_all, selected = (if (length(species_intersect)>0) species_intersect else r_values$species_names_all[1]))
      })
      
      
      # Observe a change to the data_descriptor name
      observeEvent (input$data_descriptor_name, {
            if (input$data_descriptor_name == '')
            {
                  # This in called on the initial load (assuming the tab is selected in the ui). It sets the simulated selecting 
                  # one of the data descriptors, which are initialized along with the other reactive variables
                  r_values$selected_data_descriptor_name <- names(r_values$data_descriptors)[1]
                  updateSelectInput (session, "data_descriptor_name", choices = names(r_values$data_descriptors), selected = names(r_values$data_descriptors)[1])   
            }
            else
            {
            
                  r_values$selected_data_descriptor_name <- input$data_descriptor_name
            }
            get_data_r()
            
            model <- get_model()
            land_use_intersect <- if(length(r_values$selected_land_use)==1 && is.na(r_values$selected_land_use)) model$land_use else intersect(model$land_use, r_values$selected_land_use)
            new_land_use <- if (length(land_use_intersect)>0) land_use_intersect else model$land_use[1]
            updateCheckboxGroupInput (session, "land_use", choices = model$land_use, selected = new_land_use)   
            r_values$selected_land_use <- new_land_use
      }) 

      # Observe a change to the selected land uses
      observeEvent (input$land_use, {
            r_values$selected_land_use <-input$land_use
      })
      
      # Observe the action button to select no land uses
      observeEvent (input$land_use_none, {
            model <- get_model()
            r_values$selected_land_use <- NULL
            updateCheckboxGroupInput (session, "land_use", choices = model$land_use, selected = NULL)   
      })
      
      # Observe the action button to select all land uses
      observeEvent (input$land_use_all, {
            model <- get_model()
            r_values$selected_land_use <- model$land_use
            updateCheckboxGroupInput (session, "land_use", choices = model$land_use, selected = model$land_use)   
      })
      
      
      # Observe the action button to select no species
      observeEvent (input$species_none, {
            model <- get_model()
            updateCheckboxGroupInput (session, "species", choices = r_values$species_names_all, selected = NULL)   
      })
      
      # Observe the action button to select all species
      observeEvent (input$species_all, {
            model <- get_model()
            updateCheckboxGroupInput (session, "species", choices = r_values$species_names_all, selected = r_values$species_names_all)   
      })
      
      
      
      # Observe double clicks on the plot.
      # check if there's a brush on the plot. If so, zoom to the brush bounds; if not, reset the zoom.
      observeEvent(input$plot_dblclick, {
            brush <- input$plot_brush
            if (!is.null(brush)) {
                  r_values$x <- c(brush$xmin, brush$xmax)
                  r_values$y <- c(brush$ymin, brush$ymax)
                  
            } else {
                  r_values$x <- NULL
                  r_values$y <- NULL
            }
      })
   

      # Plot the probabilities
      output$probability_plot <- renderPlot({ 
            model <- get_model()
            
            # Make sure all paramters are set
            if (is.null(model) || is.null(g_all_predictors) || is.null(input$plot_predictor) ||is.null(input$species) || !(input$plot_predictor %in% g_all_predictors) ||is.null(r_values$selected_land_use) || (sum(input$species %in% model$spps)==0) )
            {
                  return (NULL)     
            }

            filter_data <- filter_data()
            
            regression_coords <- get_regression_coords(input$filter_model_type, filter_data, model, g_all_predictors, input$plot_predictor, input$species, r_values$selected_land_use, r_values$data_descriptors[[input$data_descriptor_name]]$pred_q_range)
            occurrence_coords <- get_occurrence_coords (filter_data, input$species, input$plot_predictor)
            
            p <- ggplot () +
                  scale_x_continuous(name=names(g_all_predictors_quantitative[g_all_predictors_quantitative==input$plot_predictor])) +
                  scale_y_continuous(limits=c(0,1.1), name="Probability of occurrence") +
                  theme(title = g_label_font, axis.title = g_label_font, axis.text = g_data_font, legend.title = g_label_font, legend.text = g_data_font) +
                  coord_cartesian(xlim = r_values$x, ylim = r_values$y, expand = FALSE)
            if (input$plot_stack == TRUE)
            {
                  if (input$plot_observations == FALSE)
                  {
                        p <- p + 
                              geom_area(data=regression_coords, position='stack', aes(x=x, y=y, group=species, fill=species)) +
                              scale_fill_discrete(name="Species")
                  }
                  else
                  {
                        p <- p + 
                              geom_line(data=regression_coords, position='stack', aes(x=x, y=y, group=species, colour=species)) +
                              scale_colour_discrete(name="Species") 
                  }
            }
            else
            {
                  p <- p + 
                        geom_line(data=regression_coords, aes(x=x, y=y, group=species, colour=species)) +
                        scale_colour_discrete(name="Species") 
            }
            if (input$plot_observations == TRUE)
            {
                  p <- p + geom_point(data=occurrence_coords, show.legend=FALSE, aes(x=x, y=y, group=species, colour=species))
            }
            return (p)
      })
      
      output$outText <- renderTable({ 
            model <- get_model()
            # Make sure all paramters are set
            if (is.null(model) || is.null(g_all_predictors) || is.null(input$plot_predictor) ||is.null(input$species) || !(input$plot_predictor %in% g_all_predictors))
            {
                  return (NULL)     
            }
            stats <- data.frame("Multinomial", model$sample_size, signif(model$aic,4), signif(model$r2,4))
            colnames(stats) <- c('Type', 'Samples', 'AIC', ' R**2')
            return (stats)
            
      })
      
  
      output$out_p_values <- renderTable({ 
            model <- get_model()
            if (is.null(model))
            {
                  return (NULL)     
            }            
            df <- as.data.frame(t(model$p_values))
            df <- df[,which(colnames(df) %in% input$species),drop=FALSE]
            return (if (ncol(df) > 0 && nrow(df) > 0) df else NULL)
      }, rownames=TRUE, digits=3)
      
      
      
      ################################################################################################################
      # 
      # Model -> Predict tab
      # 
      ################################################################################################################
      
      
      # Observe updates to the checkboxes. When turned off, reset associated slider value to the mean
      lapply (X=g_all_predictors_quantitative, FUN=function (i)
      {
            observeEvent (input[[paste('predict_on_', i, sep='')]], {
                  check_box_id <- paste('predict_on_', i, sep='')
                  slider_id <- paste('predict_', i, sep='')
                  if (input[[check_box_id]] == FALSE)
                  {
                        new_val <- signif(mean(filter_data()[[i]],na.rm=TRUE),3)
                        updateSliderInput(session, slider_id, value = new_val)
                  }
            }, ignoreInit = TRUE)      
      })
      
      # Observe updates to the prediction sliders. 
      lapply (X=g_all_predictors_quantitative, FUN=function (i)
      {
            observeEvent (input[[paste('predict_', i, sep='')]], {
                  # Trigger the prediction update
                  r_values$run_predict_go <- !r_values$run_predict_go
            }, ignoreInit = TRUE)      
      })
      
      # Observe a change to the data_descriptor name - update the slide bar min/maxs and the land use selections
      observeEvent (input$data_descriptor_name, {
            r_values$selected_data_descriptor_name <- input$data_descriptor_name
            get_data_r()

            for (pred in g_all_predictors_quantitative)
            {
                  updateSliderInput(session, paste('predict_', pred, sep=''), step=(.1*(10^floor(log10(r_values$data_descriptors[[input$data_descriptor_name]]$pred_q_range[pred,'diff'])))), min = floor(r_values$data_descriptors[[input$data_descriptor_name]]$pred_q_range[pred,'min']), max = ceiling(r_values$data_descriptors[[input$data_descriptor_name]]$pred_q_range[pred,'max']), value = if (input[[paste('predict_', pred, sep='')]]==-999) signif(r_values$data_descriptors[[input$data_descriptor_name]]$pred_q_range[pred,'mean'],3) else input[[paste('predict_', pred, sep='')]] )
            }
            updateSelectInput(session, 'predict_LU',  choices = r_values$data_descriptors[[input$data_descriptor_name]]$lu_cats, selected=if (input$predict_LU %in% r_values$data_descriptors[[input$data_descriptor_name]]$lu_cats) input$predict_LU else r_values$data_descriptors[[input$data_descriptor_name]]$lu_cats[1])
      }, ignoreInit=TRUE)
      
      # This is triggered when the "run_predict_go" switch is flipped
      predict_go <- eventReactive(r_values$run_predict_go, {
            df <- data.frame(Predictor = character(), Value=numeric(), stringsAsFactors = FALSE)
            for (p in g_all_predictors_quantitative)
            {
                  if ( (input[[paste('predict_on_', p, sep='')]] == TRUE) && (p %in% g_all_predictors))
                  {
                        df <-rbind(df, data.frame(Predictor=p, Value=input[[paste('predict_',p,sep='')]], stringsAsFactors = FALSE))
                  }
                  else
                  {
                        df <-rbind(df, data.frame(Predictor=p, Value=mean(filter_data()[[p]],na.rm=TRUE), stringsAsFactors = FALSE))
                  }
            }
            colnames(df) <- c("Predictor", "Value")
            return (df)
      })
      
      
      get_species_prediction <- reactive ({
            model <- get_model()
            active_widgets <- predict_go()$Predictor %in% g_all_predictors
            model_input <- t(matrix(data=predict_go()$Value[active_widgets],dimnames=list(predict_go()$Predictor[active_widgets])))
            p <- predict_multinomial(model, model_input, input$predict_LU, r_values$data_descriptors[[input$data_descriptor_name]]$pred_q_range)
            lu_weight <- ifelse('LU' %in% g_all_predictors, model$pred_c_specs[['LU']]$cat_weights[[input$predict_LU]], 1)
            p <- data.frame(Probability=t(p/lu_weight))
            p$Species <- rownames(p)
            return (p)
      })
      
      output$out_prediction <- renderTable({ 
            if (!input$predict_LU %in% r_values$data_descriptors[[input$data_descriptor_name]]$lu_cats || is.null(r_values$data_descriptors[[input$data_descriptor_name]]$models))
            {
                  return (NULL)
            }
            prediction <- get_species_prediction()
            prediction <- prediction[order(prediction$Probability, decreasing = TRUE),]
            return (rbind(prediction, data.frame(Species='Total', Probability=sum(prediction$Probability))))
      }, spacing='xs')

      output$out_piechart <- renderPlot({
            if (!input$predict_LU %in% r_values$data_descriptors[[input$data_descriptor_name]]$lu_cats  || is.null(r_values$data_descriptors[[input$data_descriptor_name]]$models))
            {
                  return (NULL)
            }
            prediction <- get_species_prediction()
             
             blank_theme <- theme_minimal()+
                   theme(
                         axis.title = g_label_font, axis.text = g_data_font, legend.title = g_label_font, legend.text = g_data_font,
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(),
                         panel.border = element_blank(),
                         panel.grid=element_blank(),
                         axis.ticks = element_blank()
                   )
             
             p <- ggplot (prediction) +
                   aes(x='', y=Probability, fill=Species) +
                   geom_bar(width = 1, stat = "identity") +
                   coord_polar("y", start=0) + 
                   blank_theme + 
                   theme(axis.text.x=element_blank()) 

             return (p)
       })

       
       
       
       ################################################################################################################
       # 
       # Describe taxa by frequency by landuse tab
       # 
       ################################################################################################################
      
       get_taxa <- function(ctree, lu_cats, abundance_level, taxonomic_rank)
       {
            if (is.null(ctree))
            {
                  return (NULL)
            }
            # Add a column with the taxa for the specified taxonomic rank
            ctree[taxonomic_rank] <- as.factor(g_taxonomy_map[as.character(ctree$GENUSSPECI), taxonomic_rank])
            
            taxa <- vector('character')
            top_taxa <- r_values$data_descriptors[[input$ui_data_descriptor_name]]$top_taxa[[taxonomic_rank]]
            for (lu_cat in lu_cats)
            {
                  taxa <- unique(c(taxa, na.omit(rownames(top_taxa[top_taxa[,lu_cat]<=abundance_level,,drop=FALSE]))))
            }
            # Return the list of taxa sorted by abundance
            ctree <- ctree[ctree$LU %in% lu_cats,]
            return (names(sort(table(droplevels(ctree[ctree[[taxonomic_rank]] %in% taxa,][[taxonomic_rank]])), decreasing = TRUE)))
       }
      
      
             
       assign_quantiles <- function(full, var='HEIGHT_MEAN', num_bins=5)
       {
             # Calculate the break points - evenly spread acress the range
             val_range <- range(full[[var]], na.rm=TRUE)
             min <- as.integer(floor(val_range[1]))
             max <- as.integer(ceiling(val_range[2]))
             
             if (min == max)
             {
                   full$cat <- min   # Edge case where there is no variation
             }
             else if (num_bins == 1)
             {
                   full$cat[!is.na(full[[var]])] <- paste(min, '-' , max,sep='')
                   full$cat[is.na(full[[var]])] <- "Missing"
             }
             else
             {
                   # Bin the specified variable. The bin names are stored in a new column called "cat"
                   full$cat <- cut2(full[[var]], g=num_bins)
                   # Pretty up the bin names. Remove brackets and use only 3 significant digits 
                   x <- gsub('\\[||\\]||\\(||\\)','',levels(full$cat))
                   x <- strsplit(x, ",")
                   x <- lapply(lapply(lapply(lapply(x, as.numeric),signif, digits=3),as.character), paste, collapse="-")
                   levels(full$cat) <- unlist(x)
             }
             return(full)
       }
       
       # Observe the taxonomic unit radio button
       observeEvent(input$ui_taxonomic_unit, {
             update_taxon_list()
       })
       
       # Observe the abundance level slider
       observeEvent(input$ui_abundance_level, {
             r_values$abundance_level <- input$ui_abundance_level
             update_taxon_list()
       })
       
       # Observe the land use checkbox list
       observeEvent(input$ui_land_use, {
             r_values$land_use_list <- input$ui_land_use
             update_taxon_list()
       },ignoreNULL=FALSE)
       
       
       
       
       # Observe the button to clear the selected land use
       observeEvent(input$ui_land_use_none, {
             updateCheckboxGroupInput(session, "ui_land_use", choices = r_values$data_descriptors[[input$ui_data_descriptor_name]]$main_lu_cats, selected = NULL)
       })
       
       # Observe the button to select all land uses
       observeEvent(input$ui_land_use_all, {
             updateCheckboxGroupInput(session, "ui_land_use", choices = r_values$data_descriptors[[input$ui_data_descriptor_name]]$main_lu_cats, selected = r_values$data_descriptors[[input$ui_data_descriptor_name]]$main_lu_cats)
       })
       
       # Observe the taxon checkbox list
       observeEvent(input$ui_taxon, {
             selected <- input$ui_taxon
             not_selected <- setdiff(r_values$display_taxon_list, selected)
             r_values$selected_taxon_list <- setdiff(unique(c(r_values$selected_taxon_list, selected)), not_selected)
       },ignoreNULL=FALSE)
       
       # Observe the button to deselect all displayed taxa
       observeEvent(input$ui_taxon_none, {
             r_values$selected_taxon_list <- setdiff(r_values$selected_taxon_list, r_values$display_taxon_list)
             update_taxon_list()
       })

       # Observe the button to select all displayed taxa
       observeEvent(input$ui_taxon_all, {
             r_values$selected_taxon_list <- unique(c(r_values$display_taxon_list,r_values$selected_taxon_list))
             update_taxon_list()
       })
       
       # Observe the button to flip the chart
       observeEvent(input$ui_flip_chart, {
             r_values$flip_chart <- !r_values$flip_chart
       })
       
       # Observe the button to expand/collapse the chart
       observeEvent(input$ui_expand_collapse_chart, {
             r_values$collapse_chart <- !r_values$collapse_chart
             if (r_values$collapse_chart == FALSE)
             {
                   updateActionButton(session, inputId = "ui_expand_collapse_chart", label = "Collapse")
             }
             else
             {
                   updateActionButton(session, inputId = "ui_expand_collapse_chart", label = "Expand")
             }
       })
       
       # Observe the help button
       observeEvent(input$ui_help, {
             showModal(modalDialog(
                   title = "CRTI Tree Data - Describe",
                   HTML("
                        <dl>
                              <dt>Abundance level</dt><dd>Adjusts how many taxa to display in the list. Move the slider to increase or decrease the size of the list. When the slider is set to 'n', the taxon list represents the union of the 'n' most abundant taxa in each selected land use</dd>
                              <dt>Taxon</dt><dd>Which taxa to chart. The available taxa reacts to changes in the abundance level and the selected land uses</dd>
                              <dt>Quantiles</dt><dd>Quantiles are cut points dividing a set of observations in a sample into a number of equal sized groups (or as close as possible to equal) based on the value of a quantitative variable. The set of observations includes those records in the dataset where the taxon and land use are selected.</dd>
                              <dt>Land use</dt><dd>The land uses to chart</dd>
                        </dl>
                        ")
             ))
       })
       
       update_taxon_list <- reactive({
             ctree <- r_values$data_descriptors[[input$ui_data_descriptor_name]]$ctree
             r_values$display_taxon_list <- get_taxa(ctree, Reduce (intersect,list(r_values$land_use_list, r_values$data_descriptors[[input$ui_data_descriptor_name]]$main_lu_cats )), r_values$abundance_level, input$ui_taxonomic_unit)
             if (is.null(r_values$display_taxon_list))
             {
                   shinyjs::hide("ui_taxa_panel")
             }
             else
             {
                   shinyjs::show("ui_taxa_panel")
             }
             updateCheckboxGroupInput(session, "ui_taxon", choices = r_values$display_taxon_list, selected = r_values$selected_taxon_list)
       })
       
       # Observe a change in the data_descriptor name
       observeEvent(input$ui_data_descriptor_name, {
             if (input$ui_data_descriptor_name == '')
             {
                   # This is run only during initialization. 
                   updateSelectInput (session, "ui_data_descriptor_name", choices = names(r_values$data_descriptors), selected = names(r_values$data_descriptors)[1])   
             }
             else
             {
                  r_values$selected_data_descriptor_name <- input$ui_data_descriptor_name
                  get_data_r()
                  update_taxon_list()
                  updateCheckboxGroupInput(session, "ui_land_use", choices = r_values$data_descriptors[[input$ui_data_descriptor_name]]$main_lu_cats, selected = r_values$data_descriptors[[input$ui_data_descriptor_name]]$main_lu_cats)
             }
       })
       
       # Plot the chart (note that this is wrapped by renderUI to allow the height to vary)
       output$contents <- renderPlot({ 
             var_descs <- g_all_predictors_quantitative
             
             # Keep only the trees in the requested set of land uses and taxa
             ctree <- r_values$data_descriptors[[input$ui_data_descriptor_name]]$ctree
             ctree <- ctree[ctree$LU %in% r_values$land_use_list,]
             ctree$taxon <-  g_taxonomy_map[ctree$GENUSSPECI, input$ui_taxonomic_unit]
             ctree <- ctree[ctree$taxon %in% input$ui_taxon,]
             if(nrow(ctree)==0)
             {
                   return (NULL)
             }
             ctree$LU <- factor(ctree$LU)
             
             # Collapse taxon names if requested
             if (r_values$collapse_chart == TRUE)
             {
                   ctree$taxon = ""
                   taxa_to_chart = list("")
             }
             else
             {
                   taxa_to_chart = input$ui_taxon
             }

             # Categorize the trees by the requested variable. 
             ctree <- assign_quantiles (ctree, input$ui_var, input$ui_bins)

             # Create a data frame to collect the data
             df_cols <- c("Taxon", "LandUse", levels(ctree$cat))
             fits <- setNames(data.frame(matrix(ncol=length(df_cols), nrow = 0)), df_cols)
             for(taxon in taxa_to_chart)
             {
                   ctree[,'occur'] = ifelse(ctree$taxon==taxon, 1,0)
                   fit <- tapply(ctree$occur, list(ctree$LU, ctree$cat), sum, na.rm=TRUE)
                   fit <- cbind(Taxon=taxon, LandUse=rownames(fit), as.data.frame(fit))
                   fits <- rbind(fits,fit) 
             }
             if (nrow(fits) == 0)
             {
                   return (NULL)
             }
             # Get the range for the y axis
             fits <- replace(fits,is.na(fits),0)
             yrange <- range(as.numeric(as.matrix(fits[3:ncol(fits)])),na.rm=TRUE)
             
             # Melt the dataframe to prepare it for plotting. This results in 4 columns: 1) Taxon, 2) LandUse, 3)Category, 4) fit value
             fits <- melt(fits, c(id="Taxon","LandUse"), variable.name="Category")
             
             if (r_values$flip_chart)
             {
                   # Plot the results
                   g <- ggplot(fits,aes(LandUse,value, fill=as.factor(Category))) +
                         geom_bar(position="dodge", stat="identity") +
                         facet_wrap(~Taxon, ncol=1) +
                         xlab('Land Use') +
                         ylab('Ocurrences') +
                         scale_fill_discrete(name=names(var_descs)[which(var_descs==input$ui_var)]) +
                         theme(axis.text.x=element_text(angle = -30, hjust = 0))
                   # Add vertical separator lines if more than one land use category
                   if (length(levels(fits$LandUse)) > 1)
                   {
                         g <- g + geom_vline(xintercept = seq(1.5,length(unique(fits$LandUse))-0.5,1))
                   }
             }
             else
             {
                   g <- ggplot(fits,aes(Category,value, fill=as.factor(LandUse))) +
                         geom_bar(position="dodge", stat="identity") +
                         facet_wrap(~Taxon, ncol=1) +
                         xlab(names(var_descs)[which(var_descs==input$ui_var)]) +
                         ylab('Ocurrences') +
                         scale_fill_discrete(name="Land use") +
                         theme(axis.text.x=element_text(angle = -30, hjust = 0))
                   # Add vertical separator lines if more than one measurement category
                   if (length(levels(fits$Category)) > 1)
                   {
                         g <- g + geom_vline(xintercept = seq(1.5,length(unique(fits$Category))-0.5,1))
                   }
             }
             return (g)
       })
       
       
       # Update the widget height to match the number of facets, then call the function that performs the plot
       output$ui_chart <- renderUI({
             if (r_values$collapse_chart == TRUE)
             {
                   height <- ifelse (length(input$ui_taxon)==0, 0, 200)
             }
             else
             {
                   height <- ifelse (length(input$ui_taxon)==0, 0, 200+(length(input$ui_taxon)-1)*100)
             }
             plotOutput("contents", height = height, width = "100%")
       })
       
       
       
       ################################################################################################################
       # 
       # Admin tab
       # 
       ################################################################################################################
       
       # Build all the models for a single data_descriptor
       build_model_set <- function (data_descriptor_specs)
       {
             library(doParallel)
             library(foreach)
             cl<-makeCluster(min(detectCores(), length(data_descriptor_specs)))
             registerDoParallel(cl)

             # Make a local copy of global variables (required for parallel processing)
             DO_SCALE <- DO_SCALE
             all_predictors_quantitative <- g_all_predictors_quantitative
             model_set <- foreach (i=1:length(data_descriptor_specs), .packages='nnet') %dopar%
             {
                   spec <- data_descriptor_specs[[i]]

                   # Reduce categorical variables to the set that actually exist in the data
                   land_use <- levels(spec$data$LU)

                   pred_c_specs = list()
                   if ('LU' %in% spec$model_predictors)
                   {
                         cat_pred_name='LU'
                         cat_name_ref=land_use[1]
                         cat_names_non_ref=land_use[2:length(land_use)]
                         cat_cf_names=paste('LU',cat_names_non_ref,sep='')
                         cat_weights <- table(spec$data[[cat_pred_name]])/nrow(spec$data)
                         pred_c_specs[['LU']]  = list(cat_pred_name=cat_pred_name, cat_name_ref=cat_name_ref, cat_names_non_ref=cat_names_non_ref, cat_cf_names=cat_cf_names, cat_weights=cat_weights)
                   }

                   if (DO_SCALE)
                   {
                         for (pred_name_q in  all_predictors_quantitative)
                         {
                               spec$data[[pred_name_q]] <- as.vector(scale(spec$data[[pred_name_q]], center=spec$pred_q_range[pred_name_q,'min'], scale=spec$pred_q_range[pred_name_q,'diff']))
                               spec$data[[pred_name_q]] <- ifelse(is.nan(spec$data[[pred_name_q]]),0,spec$data[[pred_name_q]]) # Covers edge case where all numbers are the same and scale returns NaNs
                         }
                   }

                   predictors <- paste(spec$model_predictors,collapse='+')
                   model <- multinom(as.formula(paste('GENUSSPECI ~ ', predictors)), data=spec$data, maxit=spec$iterations)
                   model <- list(model_reduced=model,
                                 cf=coef(model, drop=FALSE),  # Warning - this returns a vector instead of a matrix if only 2 species exist
                                 aic=model$AIC,
                                 spps=model$lev,
                                 ref_spp=model$lev[1],
                                 sample_size=nrow(spec$data),
                                 land_use=land_use,
                                 pred_names_q=all_predictors_quantitative,
                                 pred_c_specs=pred_c_specs,
                                 model_hash_id=spec$model_hash_id,
                                 dataset_name=spec$dataset_name)
                              

                   model$p_values <- matrix()
                   if (spec$calculate_p_values)
                   {
                         s <- summary(model$model_reduced)
                         p_values <- (1 - pnorm(abs(s$coefficients/s$standard.errors), 0, 1)) * 2 # from https://stats.stackexchange.com/questions/63222/getting-p-values-for-multinom-in-r-nnet-package
                         model$p_values <- p_values
                   }

                   r2 <- (1-(model$model_reduced$deviance/(update(model$model_reduced, . ~ 1,trace=F)$deviance)))
                   model$r2 <- r2

                   model$model_reduced$fitted.values <- NULL
                   model$model_reduced$residuals <- NULL
                   model$model_reduced$weights <- NULL
                   model$model_reduced$family <- NULL
                   model$model_reduced$na.action <- NULL
                   attr(model$model_reduced$terms,".Environment") =c()
                   attr(model$model_reduced$formula,".Environment") =c()
                   model
             }
             stopCluster(cl)
             return (model_set)
       }
        
       # Subset the data
       filter_data <- reactive({
             data <- filter_data_x (r_values$data_descriptors[[input$data_descriptor_name]]$ctree,input$filter_species_set, input$filter_species_set_others)
             r_values$species_names_all <- levels(data$GENUSSPECI)
             return (data)
       })
       
       
       get_top_spps <- function (ctree, taxonomic_rank, lu_cats, limit)
       {
             # Add a column with the specified taxonomic rank
             ctree[taxonomic_rank] <- g_taxonomy_map[as.character(ctree$GENUSSPECI), taxonomic_rank]
             
             top_spps_names = NULL
             top_spps_lu = list()
             # Get the top x species for each land use, plus combine them into a single list
             for (lu_cat in lu_cats)
             {
                   t <- sort(table(ctree[[taxonomic_rank]][ctree$LU==lu_cat]), decreasing = TRUE)[1:limit,drop=FALSE]
                   for (i in seq(1:nrow(t))) {t[i]<-i}
                   top_spps_lu[[lu_cat]] <- t
                   top_spps_names <- unique(c(top_spps_names, names(t)))
             }
             
             top_spps <- matrix(NA, nrow=length(top_spps_names), ncol=length(lu_cats))
             rownames(top_spps) <- top_spps_names
             colnames(top_spps) <- lu_cats
             
             # Sum up each abundant species across all of the land uses
             for (lu_cat in lu_cats)
             {
                   for (spp in names(top_spps_lu[[lu_cat]]))
                   {
                         top_spps[spp,lu_cat] = top_spps_lu[[lu_cat]][[spp]] + ifelse(is.na(top_spps[spp,lu_cat]), 0, top_spps[spp,lu_cat])
                   }
             }
             return (top_spps)
       }
       
       # Observe the help button
       observeEvent(input$admin_help, {
             showModal(modalDialog(
                   title = "CRTI Tree Data - Admin",
                   HTML("
                        <p>
                              Each dataset is made up of 2 parts:
                              <ol>
                              <li>Observation data. Since this can be quite large, it is loaded in on-demand from its home GitHub location (https://don-morrison-2000.github.io/source/shiny/chicago_tree_ui/<dataset_name>.csv) 
                              <li>Descriptive information. This is in R object format and loaded at application startup time from its home GitHub location (https://don-morrison-2000.github.io/source/shiny/chicago_tree_ui/data_descriptor.rds) 
                              </ol>
                              To create and archive a new dataset:
                              <ol>
                              <li>Add dataset. 
                                    Reads the observation data from your local drive, adds missing columns, and writes the results out to the local cache at [current_working_directory]/data/<dataset_name>.csv.  
                                    Also generates in-memory R objects that provide descriptive information on the obeservation data
                              <li>Rebuild model. 
                                    Generates the multinomial regression model based on the observation data and updates the in-memory descriptive information. 
                              <li>Save configuration. 
                                    Writes the in-memory descriptive information to [current_working_directory]/data/data_descriptors.rds. At this point the .csv and the .rds files can be uploaded to GitHub. 
                                    To verify, delete the local .csv and .rds files, then restart the application.
                              </ol>
                        ")
                   ))
       })
       
       
       # Observe the action help button
       observeEvent(input$admin_help_action, {
             html <- character()
             if (input$admin_action_name == "Add dataset")
             {
                   html <- 
                         "<dt>Name</dt><dd>The display name to be assigned to the the dataset</dd>
                          <dt>Data file</dt><dd>Path to the local file</dd>"
             }
             else if (input$admin_action_name == "Rebuild model")
             {
                   html <- 
                         "<dt>Name</dt><dd>The display name of the dataset. The observation data is reloaded to ensure the model is built with the lastest data</dd>
                          <dt>Iterations</dt><dd>Creating a multinomial regression model is an iterative process. More iterations require more time but yield higher accuracy</dd>
                          <dt>Calculate p-values</dt><dd>The p-value helps determine the significance of a predictor variable. A p-values of less than .05 indicates that a predictor variable is statistically significant. Calculating p-values can take a lot of time</dd>"             
             }
             else if (input$admin_action_name == "Delete dataset")
             {
                   html <- 
                         "<dt>Name</dt><dd>The display name of the dataset to be deleted. The dataset and associated descriptions are deleted from memory and the local cache</dd>"
             }
             else if (input$admin_action_name == "Save configuration")
             {
                   html <- 
                         "Writes the in-memory descriptive information to [current_working_directory]/data/data_descriptors.rds"            
             }
             
             showModal(modalDialog(
                   title = paste ("CRTI Tree Data - Admin - ", input$admin_action_name),
                   HTML(paste("<dl>",html,"</dl"))
                   ))
       })
       
       # Observe a request to add a new dataset
       observeEvent (input$admin_new_data_add, {
             if (input$admin_new_data_name == "") {
                   showNotification('Dataset name is blank', duration = NULL, closeButton = TRUE, type = 'default')
                   return (NULL)
             } 
             if (!is.null(r_values$data_descriptors[[input$admin_new_data_name]])) 
             {
                   showNotification(paste('Dataset', input$admin_new_data_name, 'already exists', sep=' '), duration = NULL, closeButton = TRUE, type = 'default')
                   return (NULL)
             }
             
             withProgress (message=paste("Adding new dataset", input$admin_new_data_file$name, sep=' '),  value=0, max=3, {
                   
                   data_descriptor = list()
                   incProgress(1, detail="Reading data")
                   data_descriptor$ctree <- read_data (input$admin_new_data_file$datapath) 
                   g_taxonomy_map <<- update_taxonomy_map(data_descriptor$ctree)
                   
                   incProgress(1, detail="Creating objects")
                   data_descriptor$name <- input$admin_new_data_name
                   data_descriptor$file_name <- paste(getwd(), '/data/', input$admin_new_data_name, '.csv', sep='')
                   data_descriptor$file_name_original <- input$admin_new_data_file$name
                   data_descriptor$hash <- digest::digest(data_descriptor$ctree)
                   data_descriptor$records <- nrow(data_descriptor$ctree)
                   data_descriptor$top_ten_species <- sort(names(head(sort(table(factor(data_descriptor$ctree[['GENUSSPECI']])), decreasing = TRUE), TOP_TEN)))
                   data_descriptor$lu_cats <- levels(data_descriptor$ctree$LU)
                   data_descriptor$main_lu_cats <- names(which(table(data_descriptor$ctree$LU)>400))
                   data_descriptor$models <- NULL
                   
                   data_descriptor$top_taxa = list()
                   for (taxonomy_rank in c("Species", "Genus", "Family"))
                   {
                         data_descriptor$top_taxa[[taxonomy_rank]] <- get_top_spps (data_descriptor$ctree, taxonomy_rank, data_descriptor$main_lu_cats, MAX_ABUNDANCE_LEVEL)
                   }

                   data_descriptor$pred_q_range <- matrix(NA, nrow=length(g_all_predictors_quantitative), ncol=4)
                   dimnames(data_descriptor$pred_q_range) <- list(g_all_predictors_quantitative, c('min', 'max', 'diff', 'mean'))
                   for (predictor_quantitative in g_all_predictors_quantitative)
                   {
                         data_descriptor$pred_q_range[predictor_quantitative, 'min'] <- min(data_descriptor$ctree[[predictor_quantitative]], na.rm=TRUE)
                         data_descriptor$pred_q_range[predictor_quantitative, 'max'] <- max(data_descriptor$ctree[[predictor_quantitative]], na.rm=TRUE)
                         data_descriptor$pred_q_range[predictor_quantitative, 'diff'] <- data_descriptor$pred_q_range[predictor_quantitative, 'max'] - data_descriptor$pred_q_range[predictor_quantitative, 'min']
                         data_descriptor$pred_q_range[predictor_quantitative, 'mean'] <- mean(data_descriptor$ctree[[predictor_quantitative]], na.rm=TRUE)
                   }
                   # Add the new data descriptor to the global list
                   r_values$data_descriptors[[input$admin_new_data_name]] <<- data_descriptor
                  
                   updateSelectInput(session, inputId='admin_update_name', choices = c('All', names(r_values$data_descriptors)), selected = 'All')
                   updateSelectInput(session, inputId='admin_delete_name', choices = names(r_values$data_descriptors), selected = names(r_values$data_descriptors)[1])
                   updateSelectInput(session, inputId='data_descriptor_name', choices = names(r_values$data_descriptors))
                   updateSelectInput(session, inputId='ui_data_descriptor_name', choices = names(r_values$data_descriptors))
                   
                   file.copy (input$admin_new_data_file$datapath, data_descriptor$file_name, overwrite=TRUE )
                  
                   incProgress(1, detail="Success")
                   Sys.sleep(2)
             })
       })
       
       # Observe a request to delete a dataset
       observeEvent (input$admin_delete, {

             withProgress (message=paste("Deleting dataset", input$admin_delete_name, sep=' '),  value=0, max=1, {
                   incProgress(1, detail="Processing delete request")
                   # Delete the local file (if it exists)
                   if (file.exists(r_values$data_descriptors[[input$admin_delete_name]]$file_name))
                   {
                         file.remove (r_values$data_descriptors[[input$admin_delete_name]]$file_name)
                   }
                   # Remove data descriptor from the global list
                   r_values$data_descriptors[[input$admin_delete_name]] <<- NULL
                   updateSelectInput(session, inputId='admin_update_name', choices = c('All', names(r_values$data_descriptors)), selected = 'All')
                   updateSelectInput(session, inputId='admin_delete_name', choices = names(r_values$data_descriptors), selected = if(length(r_values$data_descriptors)>0) names(r_values$data_descriptors)[1] else NULL)                   
                   updateSelectInput(session, inputId='data_descriptor_name', choices = names(r_values$data_descriptors))
                   updateSelectInput(session, inputId='ui_data_descriptor_name', choices = names(r_values$data_descriptors))
                   incProgress(1, detail="Success")
                   Sys.sleep(2)
             })
       })
       

       
       # Observe the rebuild models button - PARALLEL
       observeEvent (input$admin_update, {
             dataset_names <- if (input$admin_update_name == 'All') names(r_values$data_descriptors) else input$admin_update_name
             data_specs <- list()
             # Collect all of the parameters up front
             withProgress (message='Building multinomial models', value=.1, max=1, {
                   for (dataset_name in dataset_names)
                   {
                         incProgress (amount=0, detail=paste("Preparing", dataset_name))
                         # Refresh the data
                         r_values$data_descriptors[[dataset_name]]$ctree <<- NULL
                         r_values$selected_data_descriptor_name <- dataset_name
                         ctree <- get_data_r()
                         
                         for (species_set in g_species_sets)
                         {
                               for (others in c('Yes', 'No'))
                               {
                                     spec <- list()
                                     spec$data <- filter_data_x (ctree, species_set, others)
                                     spec$data_nrow <- nrow(spec$data)
                                     spec$model_predictors <- g_all_predictors
                                     spec$pred_q_range <- r_values$data_descriptors[[dataset_name]]$pred_q_range
                                     spec$calculate_p_values <- input$admin_update_calculate_p_values
                                     spec$iterations <- input$admin_update_iterations
                                     spec$model_hash_id <- get_model_hash_id(dataset_name, levels(spec$data$LU), species_set, g_all_predictors, input$filter_model_type, others) 
                                     spec$dataset_name = dataset_name
                                     data_specs <- append(data_specs, list(spec))
                               }
                         }
                   }
                   incProgress (amount=.7, detail=paste(length(data_specs), "builds running on ", min(detectCores(), length(data_specs)), "processors"))
                   
                   # Reorder so the largest datasets are handled first (performance optimization)
                   data_specs <- data_specs[order(sapply(data_specs, function(x) x$data_nrow), decreasing=TRUE)]
                   # Build the models and stash the results in the proper data descriptors
                   for (model in build_model_set(data_specs))
                   {
                         r_values$data_descriptors[[model$dataset_name]]$models[[model$model_hash_id]] <<- model
                   }
             })
       })
       
      
       observeEvent (input$admin_update_name, {
            if (input$admin_update_name == '')
            {
                  # This is run only at initialization
                  updateSelectInput(session, inputId='admin_update_name', choices = c('All', names(r_values$data_descriptors)), selected = 'All')
            }
       })
       
       observeEvent (input$admin_delete_name, {
             if (input$admin_delete_name == '')
             {
                   # This is run only at initialization
                   updateSelectInput(session, inputId='admin_delete_name', choices = names(r_values$data_descriptors), selected = names(r_values$data_descriptors)[1])
             }
       })
       
       # Observe a request to save the current configuration to disk
       observeEvent (input$admin_save_configuration, {
             withProgress (message=paste("Saving datasets and models to disk", g_data_descriptors_local_file, sep=' '), value=0, max=1, {
                   # Remove tree data to reduce size of saved object. The tree data will be merged back in on demand
                   for (name in names(r_values$data_descriptors))
                   {
                         r_values$data_descriptors[[name]]$ctree <- NULL
                   }
                   incProgress(.2, detail=g_data_descriptors_local_file)
                   saveRDS (r_values$data_descriptors, file=g_data_descriptors_local_file)
                   incProgress(1, detail="Success")
                   Sys.sleep(2)
             })
       })
       
       # Output a summary table of dataset/model status
       output$admin_datasets <- renderTable(digits=0, { 
             if(length(r_values$data_descriptors) == 0) 
             {
                   return (NULL)
             }

             model_status <- character()
             data_load_status <- character()
             records <- numeric()
             for (data_descriptor in r_values$data_descriptors)
             {
                   if (is.null(data_descriptor$models))
                   {
                         model_status <- c(model_status, "Rebuld required")
                   }
                   else
                   {
                         model_status <- c(model_status, 'OK')
                   }
                   data_load_status <- c(data_load_status, if (is.null(nrow(data_descriptor$ctree))) "Pending" else "OK")
                   records <- c(records, data_descriptor$records)
             }
             df <- data.frame(names(r_values$data_descriptors), records, data_load_status, model_status)
             colnames(df) <- c('Name', 'Records', 'Data Load', 'Model Status')
             return (df)
             
       })
}

# Create Shiny object
shinyApp(ui = ui, server = server)


