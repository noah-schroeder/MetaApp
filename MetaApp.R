
library(shiny)

#This shiny app is licensed under GPLv3. Please refer to license.

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Random Effects Conventional Meta-Analysis Calculator"),

  navlistPanel(
    id = "tabset",
    "Conventional Meta-Analysis",
    tabPanel("Introduction",
             p("This app takes you through the process of running a conventional random effects meta-analysis using standardized mean difference effect sizes (Hedges g). The app will take you step by step through the data analysis process.
             This app was written by Noah L. Schroeder, Ph.D. using Shiny, metafor, and tidyverse packages for R. If you use this app for your analyses, please cite as:"),
             p("Schroeder, N. L. (2024). Random Effects Conventional Meta-Analysis Calculator."),
  ),
    tabPanel("Step 1: Calculate Effect Sizes", 
             h2("Calculating effect sizes"), p("In order for this app to calcuate your standardized mean difference effect size (Hedge's g) for each comparison, there are two requirements."),
             h3("Data File Requirements"),
             p("1. Your file must be in .csv format."),
             p("2. Your data must be organized in a specific way. You must have the treatment mean, treatment standard deviation, treatment sample size, control mean, control standard deviation, and control sample size each in their own columns."),
             h4("The respective columns must be labeled as follows:"),
             p("Treatment mean must be labeled", strong("intmean")),
             p("Treatment standard deviation must be labeled", strong("intsd")),
             p("Treatment sample size must be labeled", strong("intn")),
             p("Control mean must be labeled", strong("cmean")),
             p("Control standard deviation must be labeled", strong("csd")),
             p("Control sample size must be labeled", strong("cn")),
             p("When your file is properly formatted, you can upload your file and proceed."),
             fileInput("file", "Data", accept = ".csv"),
             actionButton("calc_es", "Calculate Effect Sizes and Variances"),
             conditionalPanel(
               condition = "input.calc_es > 0",
               h2("Understand the results"),
             p("Your results appear below. You will see your entire data file, but at the end are appended columns yi and vi, which are your effect size (Hedge's g) and variance, respectively. You are now ready to move forward to data analysis."), strong("You must download the result of this analysis to use for the rest of your analysis. Simply click 'download data' and save the file, then proceed to the next step."),
             downloadButton("download_button", label = "Download Data"),
             verbatimTextOutput("esresult_output")
             ),
  ),
    tabPanel("Step 2: Run the Meta-Analysis",
            h2("Run the Meta-Analysis"),
            p("Now you are ready to run your conventional meta-analysis. This app will help you run a random effects meta-analyis."),
            h3("Upload Your Dataset"),
            p("If you used Step 1 to calculate your effect sizes and variance, simply upload that file. If you did not use Step 1 of this app, ensure your effect sizes (Hedges' g) are in a column labeled yi and the effect size variances are in a column labeled vi. The file must be a .csv."),
            fileInput("cmafile", "Data", accept = ".csv"),
            
            h3("Run the Meta-Analysis"),
            p("As long as your data uploaded, press the Run Meta-Analysis button to run a random effects meta-analysis of standardized mean differences."),
            actionButton("run_cma", "Run Meta-Analysis"),
            conditionalPanel(
              condition = "input.run_cma > 0",
              #display result
            h3("Random Effects Meta-Analysis of Standardized Mean Differences Results"),
            downloadButton("cmadownload_button", label = "Download Results"),
            verbatimTextOutput("cmaresult_output"),
            h3("Forest Plot"),
            p("Note that within the app, the plot scales based on your window size. If you download the image it will be properly scaled."),
            # Button to copy forest plot
            downloadButton("forestplotdownload_button", label = "Download Forest Plot"),
            # Render the forest plot
            plotOutput("forest_plot"),
            h3("Need Help Understanding The Results?"),
            p("If you want help interpreting these results, please see ", HTML("<a href='https://noah-schroeder.github.io/reviewbook/meta.html#interpreting-the-results'>my open book</a>"),
            ),
            ),
          ),
    tabPanel("Step 3: Check for Outliers and Influence",
            h2("Check for Outliers and Influence"),
            p("Now you that you ran your meta-analysis, we need to make sure there isn't undue influence or outliers in the data set. We will do that using the influence function in metafor. You should upload the same dataset file as you used to run the meta-analysis."),
            h3("Upload Your Dataset"),
            p("If you used Step 1 to calculate your effect sizes and variance, simply upload that file. If you did not use Step 1 of this app, ensure your effect sizes (Hedges' g) are in a column labeled yi and the effect size variances are in a column labeled vi. The file must be a .csv."),
            fileInput("inffile", "Data", accept = ".csv"),
            actionButton("run_inf", "Run Outlier and Influence Analysis"),
            conditionalPanel(
              condition = "input.run_inf > 0",
              h3("Outlier and Influence Results"),
            downloadButton("infdownload_button", label = "Download Infuence Analysis Results"),
            verbatimTextOutput("resultforinf_output"),
            h3("Need Help Understanding The Results?"),
            p("If you want help interpreting these results, please see ", HTML("<a href='https://noah-schroeder.github.io/reviewbook/meta.html#checking-for-outliers-and-influence'>my open book</a>"),
            ),
            ),
  ),
    tabPanel("Step 4: Moderator Analysis",
             h2("Check for Moderating Variables"),
             p("This tool will help you check for", strong("categorical"), "moderating variables. Do not use this tool for continuous variables! The first step is to upload your data. This should be the same data you used to run the meta-analysis in Step 2."),
             fileInput("modfile", "Upload Moderator Analysis Data", accept = ".csv"),
             p("Once your file is uploaded, you can choose which column in your spreadsheet you want to examine as a moderator variable. Again,", strong("this is for categorical moderators only."), "After you choose your variable from the dropdown menu, click run and your results will be shown."),
             selectInput("dropdown", "Choose Column for Moderator Analysis", choices = NULL),
             actionButton("run_analysis", "Run Moderator Analysis"),
             conditionalPanel(
               condition = "input.run_analysis > 0",
             h3("Test of Moderators"),
             p("First, we conduct a test of moderators to see if there are significant differences between levels of the moderator."),
             p(strong("You must write down or copy paste the Qbetween results."), "Because of how I had to code the app to display the results in a format that users would be familiar with, you cannot download them directly at this time. Sorry!"),
             verbatimTextOutput("results_output"),
             h3("Effect Size Table"),
             p("Next, we see our table that shows the effect sizes and accompanying statistics for each level of the moderator. Remember, your Test of Moderators above tells you if there are significant differences between levels."),
             downloadButton("download_results", "Download Results *this will NOT save the test of moderators"),
             verbatimTextOutput("modtable_output"),
             h3("Need Help Understanding The Results?"),
             p("If you want help interpreting these results, please see ", HTML("<a href='https://noah-schroeder.github.io/reviewbook/meta.html#moderator-analysis'>my open book</a>"),
             ),
             ),
    ),
    tabPanel("Step 5: Publication Bias",
             h2("Publication Bias"),
             p("There are a variety of ways to evaluate publication bias. This tool provides a number of computational and graphic options."),
             h3("Upload Your Data"),
             p("First you need to upload your data. This is the same data file you used for the overall meta-analysis and moderator analysis. Only .csv files are accepted."),
             fileInput("pubbiasfile", "Upload Data", accept = ".csv"),
             actionButton("run_pub", "Run Publication Bias Analyses"),
             #display result
             conditionalPanel(
               condition = "input.run_pub > 0",
               h3("Funnel Plot"),
               p("Note that within the app, the plot scales based on your window size. If you download the image it will be properly scaled."),
               downloadButton("download_funnel", "Download Funnel Plot"),
               plotOutput("funnel_plot"),
               h3("Trim and Fill Analysis"),
               downloadButton("download_trim_fill", "Download Trim and Fill Analysis"),
               verbatimTextOutput("trim_fill_output"),
               h3("Egger's Regression"),
               downloadButton("download_eggers", "Download Egger's Regression"),
               verbatimTextOutput("eggers_output"),
               h3("Rosenthal's Fail Safe N"),
               downloadButton("download_rosenthal", "Download Rosenthal's Fail Safe N"),
               verbatimTextOutput("rosenthal_output"),
               h3("Orwin's Fail Safe N"),
               downloadButton("download_orwin", "Download Orwin's Fail Safe N"),
               verbatimTextOutput("orwin_output"),
               h3("Rosenberg's Fail Safe N"),
               downloadButton("download_rosenberg", "Download Rosenberg's Fail Safe N"),
               verbatimTextOutput("rosenberg_output"),
               h3("Need Help Understanding The Results?"),
               p("If you want help interpreting these results, please see ", HTML("<a href='https://noah-schroeder.github.io/reviewbook/meta.html#publication-bias'>my open book</a>")),
               ),
    ),
   tabPanel("How to Cite This App",
              h2("Please cite this app as:"),
              p("Schroeder, N. L. (2024). Random Effects Conventional Meta-Analysis Calculator."),
                      ),
   tabPanel("Need Help?",
            h2("Need Help with Meta-Analysis?"),
            p("If you want help learning meta-analysis or interpreting the results presented by this app, please see ", HTML("<a href='https://noah-schroeder.github.io/reviewbook/'>my open book</a>"), "."),
   ),
  tabPanel("Validation and Underlying Code",
           h2("Validation of This App"),
           p("In order to validate the this app, I have run a data set through the app and compared it to data directly from R and from", HTML("<a href='https://www.jamovi.org/'>JAMOVI</a>"), ". The results were consistent, as would be expected since all three sources are based on metafor."),
           p("Below are images from the app and JAMOVI that show the results of the overall meta-analysis and publication bias analyses."),
           h3("This App"),
           h5("Overall Meta-Analytic Result"),
           img(src = "appres.png", width = 700),
           h5("Fail Safe N Statistics"),
           img(src = "appfsn.png"),
           h3("JAMOVI"),
           h5("Overall Meta-Analytic Result"),
           img(src = "jamovires.png", width = 700),
           h5("Fail Safe N Statistics"),
           img(src = "jamovifsn.png", width = 700),
           img(src = "jamoviorwin.png", width = 700),
           img(src = "jamovirosenberg.png", width = 700),
           h3("R Code"),
           h5("Overall Meta-Analytic Result"),
           img(src = "bookres.png", width = 700),
           h5("Fail Safe N Statistics"),
           img(src = "bookfsn.png", width = 700),
           h2("Underlying Code"),
           p("The analysis code powering this app is all based off of ", HTML("<a href='https://noah-schroeder.github.io/reviewbook/'>my open book</a>"), "."),
           p("The underlying code for this application as a whole is available at", HTML("<a href='https://github.com/noah-schroeder/MetaApp'>my github repo</a>"),"."),
  ),
  tabPanel("References",
           h2("References"),
           p("Chang, W, Cheng, J, Allaire, J, Sievert, C, Schloerke, B, Xie, Y, Allen, J, McPherson, J, Dipert, A, Borges, B. (2023). _shiny: Web Application Framework for R_. R package version 1.8.0, <https://CRAN.R-project.org/package=shiny>."),
           p("Schroeder, N. L. (2024). A beginner’s guide to systematic review and meta-analysis. Available at https://noah-schroeder.github.io/reviewbook/"),
           p("Viechtbauer, W. (2010). Conducting meta-analyses in R with the metafor package. Journal of Statistical Software, 36(3), 1-48. https://doi.org/10.18637/jss.v036.i03"),
           p("Wickham, H., Averick, M., Bryan, J., Chang, W., McGowan, L. D. A., François, R., ... & Yutani, H. (2019). Welcome to the Tidyverse. Journal of open source software, 4(43), 1686.")
  ),
   tabPanel("Acknowledgments",
            h2("Acknowledgements"),
            p("Thank you to Dr. Vipin Verma, who helped me fix the code related to rendering png files."),
            h3("Generative AI Acknowledgment"),
            p("I was not famililar with Shiny when I began building this app, so a lot of the code was created with the assistance of ChatGPT 3.5."),
            ),
  tabPanel("Change Log",
           h2("Change Log"),
           p("March 28, 2024: Application Launched."),
  ),
  )
        
)  # Close navlistPanel

# Main Panel


          
#############################################################################
# Define server logic 
server <- function(input, output, session) {

  ###Conventional MA ES Calc
  # Define code to run when the button is clicked
 ## observeEvent(input$file, {
    # Check if a file is uploaded
 ##   req(input$file)})
 ###########################################
##conventional ES calc  
  # Reactive expression to read uploaded file and return result
  esresult <- eventReactive(input$calc_es, {
    req(input$file)  # Check if a file is uploaded
    
    # Read the uploaded CSV file
    data <- read.csv(input$file$datapath)
    #load metafor
    library(metafor)
    # Assuming the file contains columns 'n1', 'n2', and 'r'
    escalc(measure="SMD", m1i=intmean, sd1i=intsd, n1i=intn,
           m2i=cmean, sd2i=csd, n2i=cn, data=data)
  })
  # Render the result
  output$esresult_output <- renderPrint({
    esresult()
    
  })
    #download button
    # Download handler for the button
    output$download_button <- downloadHandler(
      filename = function() {
        "mydata.csv"  
      },
      content = function(file) {
        savedata <- esresult()
        # Write data to CSV file
        write.csv(savedata, file)
      })


  ############################################
## Conventional MA 
    #load metafor
    library(metafor)
   
# Reactive expression to run conventional MA
cmaresult <- eventReactive(input$run_cma, {
  req(input$cmafile)  # Check if a file is uploaded
 
    # Read the uploaded CSV file
  data <- read.csv(input$cmafile$datapath)
  # Run random effects MA
  result <- rma(yi, vi, data=data)
  return(result)
  # Create forest plot
  forest_plot <- ggforest(result)
})

# Render the result
output$cmaresult_output <- renderPrint({
  cmaresult()
})
#download button
# Download handler for the button
output$cmadownload_button <- downloadHandler(
  filename = function() {
    "myoverallresult.txt"  
  },
  content = function(file) {
    # Retrieve the result
    result <- cmaresult()
    
    # Convert result to text
    result_text <- capture.output(result)
    
    # Write result to text file
    writeLines(result_text, file)
  }
)
####### forest plot
# Reactive value to store forest plot
forest_plot <- reactive({
  # Read the uploaded CSV file
  data <- read.csv(input$cmafile$datapath)
  # Check if the "run_cma" button has been clicked
  if (input$run_cma > 0) {
    # Perform meta-analysis and create forest plot
    result <- cmaresult()
    forest(result, slab = data$studyauthor, main = "Forest Plot of Observed Effects", header="Author(s) and Year")
  }
})


# Render the forest plot
output$forest_plot <- renderPlot({
  # Check if the "run_cma" button has been clicked
  if (input$run_cma > 0) {
    # Display the forest plot
    print(forest_plot())
  }
})
# Download handler for the forest plot button
output$forestplotdownload_button <- downloadHandler(
  filename = function() {
    "forestplot.png"  # Specify file name
  },
  content = function(file) {
    # Capture the plot as a PNG file
    png(file, width = 2800, height = 2400, res = 300)
    data <- read.csv(input$cmafile$datapath)
    if (input$run_cma > 0) {
      # Perform meta-analysis and create forest plot
      result <- cmaresult()
      forest(result, slab = data$studyauthor, main = "Forest Plot of Observed Effects", header="Author(s) and Year")
    }
    dev.off()
}
)

############################################
#Outlier and Influence Analysis
#load metafor
library(metafor)

# Reactive expression to run conventional MA and influence analysis
infresult <- eventReactive(input$run_inf, {
  req(input$inffile)  # Check if a file is uploaded
  
  # Read the uploaded CSV file
  infdata <- read.csv(input$inffile$datapath)
  
  # Run random effects MA
  resultforinf <- rma(yi, vi, data=infdata)
  
  # Perform influence analysis
  infres <- influence(resultforinf)
  
  # Return both the MA result and influence analysis result
  return(list(resultforinf = resultforinf, infres = infres))
})

# Render the result on the screen
output$resultforinf_output <- renderPrint({
  # Check if the "run_inf" button has been clicked
  if (input$run_inf > 0) {
    # Display the influence analysis result
    inf_result <- infresult()
    print(inf_result$infres)
  }
})

#inf download button
# Download handler for the button
output$infdownload_button <- downloadHandler(
  filename = function() {
    "influenceresult.csv"  
  },
  content = function(file) {
    # Retrieve the influence analysis result from the reactive expression
    inf_result <- infresult()
    
    # Extract the influence statistics from the infres object
    inf_statistics <- inf_result$infres$inf
    
    # Convert the influence statistics into a data frame
    inf_table <- as.data.frame(inf_statistics)
    
    # Write data to CSV file
    write.csv(inf_table, file, row.names = FALSE)
  })
############################################
# Moderator Analysis
library(metafor)

# Reactive expression to read uploaded CSV file
uploaded_data <- reactive({
  req(input$modfile)
  read.csv(input$modfile$datapath)
})

# Update selectInput choices when file is uploaded
observeEvent(input$modfile, {
  updateSelectInput(session, "dropdown", choices = colnames(uploaded_data()))
})

# Run moderator analysis for q
mod <- reactive({
  req(input$run_analysis, input$dropdown)  # Ensure dropdown choice is available
  rma(yi, vi, mods = as.formula(paste("~ factor(", input$dropdown, ")")), data = uploaded_data())
})

# Display results of the test of moderators
output$results_output <- renderPrint({
  # Check if the "Run Moderator Analysis" button has been pressed
  if (input$run_analysis > 0) {
    # Run moderator analysis
    mod_result <- mod()
    
    # Extract QM statistics and p-value
    m <- mod_result$m
    QM <- round(mod_result$QM, 3)  # Round QM to three decimal places
    QMp <- round(mod_result$QMp, 3)  # Round QMp to three decimal places
    
    # Construct the output string
    output_str <- paste("Test of Moderators (df:", m, "):",
                        "\nQbetween(",m,")=", QM, ", p-val =", QMp)
    
    # Print the output string
    cat(output_str, sep = "\n")}
    
 # Run moderator analysis for table
    modfortable <- reactive({
      req(input$run_analysis, input$dropdown)  # Ensure dropdown choice is available
      # Construct the moderator formula excluding the intercept
      mod_formula <- as.formula(paste("~ -1 + factor(", input$dropdown, ")"))  # Create the formula without the intercept
      
      # Run the moderator analysis
      rma(yi, vi, mods = mod_formula, data = uploaded_data())
    })
    
    # Display results of the moderator analysis for table
    output$modtable_output <- renderPrint({
      # Check if the "Run Moderator Analysis" button has been pressed
      if (input$run_analysis > 0) {
        # Run moderator analysis
        mod_result <- modfortable()
        
        # Print the summary of the moderator analysis
        print(coef(summary(mod_result)))
      }
    })

    library(tidyverse)
    
    # Reactive expression to calculate participants in each group
    calculate_participants <- function(uploaded_data, mod_result, dropdown) {
      req(dropdown)  # Ensure dropdown choice is available
      
      # Get the actual data frame from the reactive expression
      data <- uploaded_data()
      
      # Group by the selected column and summarize participants in each group
      participants_summary <- data %>%
        group_by(!!sym(dropdown)) %>%
        summarise(nexp = sum(intn, na.rm = TRUE),
                  nctrl = sum(cn, na.rm = TRUE))
      
      # Extract coefficients and other relevant information from the mod_result object
      coef_table <- coef(summary(mod_result))
      
      # Combine the summary of participants with the coefficients table
      result_table <- cbind(participants_summary, coef_table)
      
      return(result_table)
    }
    
    
    observeEvent(input$run_analysis, {
      # Run moderator analysis
      mod_result <- modfortable()
      
      # Update download handler when data changes
      output$download_results <- downloadHandler(
        filename = function() {
          "Mod.Result.csv"
        },
        content = function(file) {
          # Convert the result to a data frame
          result_table <- calculate_participants(uploaded_data, mod_result, input$dropdown)
          
          # Write csv file
          write.csv(result_table, file, row.names = FALSE)
        }
      )
    })
    
 
    
    
    
})
######################################################
    ### publication bias 
# Reactive expression to perform meta-analysis and diagnostics
perform_meta_analysis <- eventReactive(input$run_pub, {
  req(input$pubbiasfile)  # Check if a file is uploaded
  
  # Read data
  data <- read.csv(input$pubbiasfile$datapath)
  
  # Run meta-analysis
  res <- tryCatch({
    rma(yi, vi, data = data)
  }, error = function(e) {
    NULL  # Return NULL if there's an error
  })
  
  if (!is.null(res)) {
    # Perform funnel plot
    funnel_plot <- tryCatch({
      funnel(res)
    }, error = function(e) {
      NULL
    })
    
    # Perform Egger's regression
    eggers <- tryCatch({
      regtest(res)
    }, error = function(e) {
      NULL
    })
    
    # Perform Rosenthal's fail-safe N
    rosenthal <- tryCatch({
      fsn(yi, vi, data = data)
    }, error = function(e) {
      NULL
    })
    
    # Perform Orwin's fail-safe N
    orwin <- tryCatch({
      fsn(yi, vi, data = data, type = "Orwin")
    }, error = function(e) {
      NULL
    })
    
    # Perform Rosenberg's fail-safe N
    rosenberg <- tryCatch({
      fsn(yi, vi, data = data, type = "Rosenberg")
    }, error = function(e) {
      NULL
    })
    
    # Return the results
    list(
      funnel_plot = funnel_plot,
      eggers = eggers,
      rosenthal = rosenthal,
      orwin = orwin,
      rosenberg = rosenberg
    )
  } else {
    # Return NULL if meta-analysis fails
    NULL
  }
})

# Reactive expression to perform trim and fill analysis
perform_trim_fill <- eventReactive(input$run_pub, {
  req(input$pubbiasfile)  # Check if a file is uploaded
  
  # Read data
  data <- read.csv(input$pubbiasfile$datapath)
  
  # Run meta-analysis
  res <- tryCatch({
    rma(yi, vi, data = data)
  }, error = function(e) {
    NULL  # Return NULL if there's an error
  })
  
  if (!is.null(res)) {
    # Perform trim and fill analysis
    trim_fill_result <- tryCatch({
      trimfill(res)
    }, error = function(e) {
      NULL
    })
    
    # Return the trim and fill result
    trim_fill_result
  } else {
    # Return NULL if meta-analysis fails
    NULL
  }
})


# Render the UI components
output$funnel_plot <- renderPlot({
  perform_meta_analysis()$funnel_plot
})


output$trim_fill_output <- renderPrint({
  perform_trim_fill()
})

output$eggers_output <- renderPrint({
  perform_meta_analysis()$eggers
})

output$rosenthal_output <- renderPrint({
  perform_meta_analysis()$rosenthal
})
output$orwin_output <- renderPrint({
  perform_meta_analysis()$orwin
})

output$rosenberg_output <- renderPrint({
  perform_meta_analysis()$rosenberg
})
##############################

# Download handler for the funnel plot button
output$download_funnel <- downloadHandler(
  filename = function() {
    "funnelplot.png"  # Specify file name
  },
  content = function(file) {
    # Capture the plot as a PNG file
    png(file, width = 2800, height = 2400, res = 300)
    if (!is.null(perform_meta_analysis()$funnel_plot)) {
      # Print the funnel plot to the PNG file
      # Read data
      data <- read.csv(input$pubbiasfile$datapath)
      
      # Run meta-analysis
      res <- tryCatch({
        rma(yi, vi, data = data)
      }, error = function(e) {
        NULL  # Return NULL if there's an error
      })
      funnel(res)
    }
    dev.off()
  }
)



# Download handler for the trim and fill analysis
output$download_trim_fill <- downloadHandler(
  filename = function() {
    "trim_fill_analysis.txt"
  },
  content = function(file) {
    # Convert result to text
    result_text <- capture.output(perform_trim_fill())
    
    # Write result to text file
    writeLines(result_text, file)
    
  }
)

# Download handler for Egger's regression
output$download_eggers <- downloadHandler(
  filename = function() {
    "eggers_regression.txt"
  },
  content = function(file) {
    # Convert result to text
    result_text <- capture.output(perform_meta_analysis()$eggers)
    
    # Write result to text file
    writeLines(result_text, file)
    
  }
)


# Download handler for Rosenthal's fail safe N
output$download_rosenthal <- downloadHandler(
  filename = function() {
    "rosenthal_failsafe_n.txt"
  },
  content = function(file) {
    # Convert result to text
    result_text <- capture.output(perform_meta_analysis()$rosenthal)
    
    # Write result to text file
    writeLines(result_text, file)
   
  }
)

# Download handler for Orwin's fail safe N
output$download_orwin <- downloadHandler(
  filename = function() {
    "orwin_failsafe_n.txt"
  },
  content = function(file) {
    # Convert result to text
    result_text <- capture.output(perform_meta_analysis()$orwin)
    
    # Write result to text file
    writeLines(result_text, file)
    
  }
)

# Download handler for Rosenberg's fail safe N
output$download_rosenberg <- downloadHandler(
  filename = function() {
    "rosenberg_failsafe_n.txt"
  },
  content = function(file) {
    # Convert result to text
    result_text <- capture.output(perform_meta_analysis()$rosenberg)
    
    # Write result to text file
    writeLines(result_text, file)
    
  }
)





    
    
    
#don't change below this line'         
    
}
##################################
# Run the application 
shinyApp(ui = ui, server = server)