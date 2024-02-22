#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

# Partie Server ## 
shinyServer(function(input, output) {
  ## importation de la data avec une fonction reactive
  data1 <- reactive({
    file1 <- input$dataFile
    if(is.null(file1)){return()
    } else {
      tryCatch(
        {
          if(stringr::str_ends(file1$datapath, "csv")) {
            as.data.frame(fread(file=file1$datapath, 
                                sep=input$sep, header = as.logical(input$header),
                                quote=input$quote, stringsAsFactors = input$stringAsFactors
                                ))
            
          } else if (stringr::str_ends(file1$datapath, "(xlsx|xls)")) {
            readxl::read_excel(file1$datapath,
                               col_names= as.logical(input$header),
                               sheet=input$sheet1
            )
          }
        })
    }
  })
  
   ##  lecture des 10 premieres lignes de données
  output$preview <-  renderDataTable({
    head(data1(),10)
  },  options = list(scrollX = TRUE , dom = 't')
  )
  
  # this reactive output contains the summary of the dataset and display the summary in table format
  output$filedf <- renderUI({
    tabItem(tabName = "readData",
            textInput("sheet1","Veuillez ecrire le nom de la feuille s'il s'agit d'un fichier excel",
                      value="un exemple"),
            fileInput("dataFile",label = NULL,
                      buttonLabel = "Browse...",
                      placeholder = "No file selected",
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv",".xlsx")),
            fluidRow(
              h3("Paramètres"),
              # Input: Checkbox if file has header
              box(width = 2,
                  height = 5,
                  radioButtons(inputId = "header",
                               label = "En tête",
                               choices = c("Yes" = TRUE,
                                           "No" = FALSE),
                               selected = TRUE, inline=T)),
              
              # Input: Select separator ----
              box(width = 3,
                  height = 5,
                  radioButtons(inputId = "sep",
                               label = "Separateur",
                               choices = c(Comma = ",",
                                           Semicolon = ";",
                                           Tab = "t"),
                               selected = "t", inline=T)),
              
              # Input: Select quotes ----
              box(width = 2,
                  height = 5,
                  radioButtons(inputId = "quote",
                               label= "Quote",
                               choices = c(None = "",
                                           "Double Quote" = '"',
                                           "Single Quote" = "'"),
                               selected = "", inline=T)),
              box(width = 3,
                  height = 5,
                  checkboxInput(inputId = "stringAsFactors",
                                "stringAsFactors", FALSE))
            ),
            fluidRow( br(),
                      br(),
                      br(),
                      br(),
                      h3("File preview"),
                      width = 12,
                      dataTableOutput(outputId = "preview")
            )
    )
  })
  
  # this reactive output contains the structure of the dataset and display the structure in table format
  output$str <- renderPrint({
    if(is.null(data1())){return ()}
    else{
      str(data1())
    }
  })
  
  # This reactive output contains the summary and display the dataset in table format
  output$sum <- renderPrint({
    if(is.null(data1())){return ()}
    else{
      summary(data1())
    }
  })
  
  # Lecture et description la data ## 
  output$tb <- renderUI({
    tabsetPanel(
      tabPanel("Lecture des données", uiOutput("filedf")),
      tabPanel("Structure", verbatimTextOutput("str")),
      tabPanel("Summary", verbatimTextOutput("sum"))
    )
  })
  
  output$visual <- renderUI({
    tabItem(tabName = "visualization",
            fluidRow(
              box(title="exploration des données",
                  solidHeader = TRUE,
                  collapsed = FALSE,
                  collapsible = TRUE,
                  status = "primary",
                  width = 10,
              ),
              
              box(title="Statistique descriptive",
                  solidHeader = TRUE,
                  collapsed = FALSE,
                  collapsible = TRUE,
                  status = "primary",
                  width = 10,
                  box(selectInput(inputId="cols", label="Variables",
                                  choices =unique(colnames(data1())), multiple = TRUE))
              )
            ))
  })
  
  ## graphique pour resumer la data ## 
  output$plot1 <- renderPlot({
    plot_intro(data1())
  } )
  
  ## statistique descriptive ##
  tb1 <- reactive({
    mycontrols  <- tableby.control(test=FALSE, total=TRUE,text = TRUE,
                                   numeric.test="wt", cat.test="chisq",
                                   numeric.stats=c("N","Nmiss","range" ,
                                                   "medianq1q3","meansd"),
                                   cat.stats=c("N","countpct","Nmiss"),
                                   date.stats = c("Nmiss","medianq1q3","meansd"),
                                   stats.labels=list(N='Available data', meansd="Mean(sd)", 
                                   medianq1q3='Median(IQR)',Nmiss="Missing", range="min-max"))
    validate(need(input$cols, "Veuillez selectionner la variable"))
    tableby(formulize(x = input$cols), data =data1(),
            control=mycontrols,digits = 2, digits.pct = 2)
  })
  
  ##  a commenter ##
  output$tab2 <- renderTable({
    as.data.frame(summary(tb1(), text = "html"))
  }, sanitize.text.function = identity)
  
  ##  a commenter ##
  output$tb2 <- renderUI({
    tabsetPanel(tabPanel("Exploration des données", plotOutput("plot1")),
                tabPanel("Statistique descriptive", 
                         selectInput(inputId="cols", 
                                     label="Variables", 
                                     choices =unique(colnames(data1())), 
                                     multiple = TRUE),tableOutput("tab2")))
  })
  
  ###----------- Regression PLS------------------### 
  output$fitpls <- renderUI({
    tabItem(tabName = "PLS-fit",
            tabBox(width = 100,height = 1000,
                   tabPanel(title="train/test",
                            box(sliderInput('train_size1', label='train_size',
                                            0.70, min = 0.70, max =1 )),
                            
                            box(verbatimTextOutput("tab3"),width=12)
                   ),
                   
                   tabPanel(title="Summary",
                            box(selectInput(inputId='col2',
                                            label ='Veuillez selectionner la variable target' ,
                                            choices=unique(colnames(data1()))
                                            ,multiple = FALSE),width=3),
                            
                            box(selectInput(inputId='col3',
                                            label ='Veuillez selectionner les variables explicatives',
                                            choices=unique(colnames(data1())),
                                            multiple = TRUE) ,width=3),
                            
                            box(numericInput(inputId='nbcomp', label='nombre de composants', 2, 
                                             min = 2, max = 5),width=3),
                            
                            radioButtons(inputId="result1", label="Veuillez choisir le resultat:",
                                         c("coefficients" = "coeff", 
                                           "Rcarré" = "R2",
                                           "corrélation" = "corr",
                                           "Qcarré" = "Q2")),
                            
                            box(verbatimTextOutput("tab4"),width=12)
                   ),
                   
                   tabPanel(title="Print",
                            radioButtons("result2", "Veuillez choisir le resultat:",
                                         c("RSS" = "RS",
                                           "PRESS" = "PR",
                                           "Features_Weights" = "Wh",
                                           "Coefs_yresiduals_comp" = "Ch",
                                           "Coefficients_xresiduals_components" = "Ph",
                                           "W_star" = "Wh*",
                                           "ah" = "ah"
                                         )),
                            
                           box(verbatimTextOutput("tab5"),width=12)
                   )
            ) )
  })
  
  ####----------------- Decomposition de la data en train et test------------- ###
  
  d <- reactive({
    train_test_split(data1(),train_size=input$train_size1)
  })
  
  ## creation de la partie train ## 
  train <- reactive({
    #train <- data1()%>%select(input$col3)
    train=d()$data_train
    return(train)
  })
 
  ## creation de la partie test ##
  test <- reactive({
    #test <- data1()%>%select(input$col3)
    test=d()$data_test
    return(test)
  })
  
  ## affichage de la repartition du train et test##
  output$tab3 <- renderText({
    if(is.null(data1())){return()}
    else{
      paste( paste("le nombre d'observations du train:",dim(train())[1],sep = " "),
             paste("le nombre d'observation du test:",dim(test())[1],sep=""),sep="\n")
    }
  })
  
  ###-------------- Partie fit-----------------###
  plss <-reactive({
    if(is.null(train())){return()}
    else{
      df= train() %>% dplyr::select(input$col3,input$col2)
      names(df)[names(df)==input$col2] <- "target"
      if(dim(df[,input$col3])[2]<2){return(print("Veuillez selectionner au moins deux variables explicatives"))}
      else{
        fit_pls(as.factor(target)~.,n_components=input$nbcomp,data=df)
      }
    }
  })
  
  ###---Sortie du summary -----##
  output$tab4 <- renderPrint({
    if(is.null(data1())){return()}
    else if(input$result1=="coeff"){
      return(summary(plss())[1]) 
    }
    else if (input$result1=="corr"){
      return(summary(plss())[2]) 
    }
    else if (input$result1=="R2"){
      return(summary(plss())[3])
    }
    else if (input$result1=="Q2"){
      return(summary(plss())[4])
    }
  })
  
  ###---Sortie du print-----##
  output$tab5 <- renderPrint({
    if(is.null(data1())){return()}
    else if(input$result2=="RS"){
      return(print(plss())[1])
    }
    else if(input$result2=="PR"){
      return(print(plss())[2])
    }
    else if(input$result2=="Wh"){
      return(print(plss())[3])
    }
    else if (input$result2=="Ch"){
      return(print(plss())[4])
    }
    else if (input$result2=="Ph"){
      return(print(plss())[5])
    }
    else if (input$result2=="Wh*"){
      return(print(plss())[6])
    }
    else if (input$result2=="ah"){
      return(print(plss())[7])
    }
  })
  
  ###------------- Partie Predict ------------------------------######
  output$predictpls <- renderUI({
    tabItem(tabName = "PLS-predict",
            #tabsetPanel(
            tabBox(width = 100,height = 3000,
                   tabPanel(title="Print",
                            radioButtons(inputId="result3", label="Veuillez choisir le resultat:",
                                         c("classe" = "class",
                                           "classe + proba d'appartenance" = "clproba")),
                            
                            box(verbatimTextOutput("tab7"),width=12)))) 
    })
  
  ## affichage de la prediction ## 
  output$tab7 <- renderPrint({
    df2= test() %>% dplyr::select(input$col3,input$col2)
    names(df2)[names(df2)==input$col2] <- "target"
    
    if(is.null(data1())){return()}

    else if(input$result3=="class"){
      return(print(predict_pls(plss(),newdata=df2,type=1))) 
    }
    else if (input$result3=="clproba"){
      return(print(predict_pls(plss(),newdata=df2,type=2))) 
    }
  })


##-------Partie graphique-----##
output$graphes <- renderUI({
  tabItem(tabName = "Graphique",
          #tabsetPanel(
          tabBox(width = 100,height = 200,
                 tabPanel(title="cercle de correlation",
                          plotOutput("tab8")),
                 
                 tabPanel(title="poids_explicatifs",
                          plotOutput("tab9")),
                 
                 tabPanel(title="Individus",
                          
                          box(numericInput(inputId='comp', label='choisir la composante', 2, 
                                           min = 2, max = 5),width=3),
                          box(selectInput(inputId='col4',
                                          label ='Veuillez selectionner la modalité',
                                          choices=levels(plss()[[1]]$Y_target),
                                          multiple = FALSE) ,width=5),
                          
                          plotOutput("tab10")),
                 
                 tabPanel(title="Composantes des individus",
                          box(selectInput(inputId='col5',
                                          label ='Veuillez selectionner la modalité',
                                          choices=levels(plss()[[1]]$Y_target),
                                          multiple = FALSE) ,width=5)
                          ,
                          plotOutput("tab11")),
                 
                 tabPanel(title="Q2",
                          plotOutput("tab12"))
                 )) 
})

output$tab8<- renderPlot({
  if(is.null(plss())){return()}
  else{
  plot_circle_correlation(plss())
  }
})

output$tab9<- renderPlot({
  plot_explanatories_weights(plss())
})

output$tab10<- renderPlot({
  list1=levels(plss()[[1]]$Y_target)
  plot_individuals(plss(),which(list1==input$col4),input$comp)
})

output$tab11<- renderPlot({
  list2=levels(plss()[[1]]$Y_target)
  plot_individuals_components(plss(),which(list2==input$col5))
  #plot_individuals_components(plss(),i=1)
})


output$tab12<- renderPlot({
  plot_Q2(plss())
})


})