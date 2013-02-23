library(shiny)

# Define UI for random distribution application 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Parametric and Non-parametric Models in Forecasting"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the br()
  # element to introduce extra vertical spacing
  sidebarPanel(
    
    helpText("This demo provides a graphical view over forecasting time series with parametric and non-parametric models.",  
             "Refer to Introduction tab for brief description."), 
    
    
    br(),
    
    sliderInput("b", 
                strong("Starting value:"), 
                value = 1597,
                min = 366, 
                max = 2142),
    br(),
    
    helpText("Note: choosing values below 1500 extends the loading time considerably."),
    
    HTML("<hr />"), 
    
    radioButtons("func_type", strong("Choose data type:"),
                 list("Dataset- Mortality in London"="d_set", 
                      "Simulation"="sim")),
    br(),
    conditionalPanel(
      condition="input.func_type=='sim'",
      selectInput("sim_func", strong("Choose simulation type:"), 
                  choices=c("linear function", "sine function", "cosine function", "polynomial", "non-linear type1", "non-linear type2", "[function of your choice]"="choice")),
      
      br(), 
      helpText("Note: go to Intro tab to find out more information about simulation types."),
      
      br(),
      conditionalPanel(
        condition="input.sim_func=='choice'",
        textInput("userchoice", strong("Type your own function:"), 
                  value="x+rnorm(N)+cumsum(rnorm(N))"),
        br()
      )), 
    
    
    HTML("<hr />"),
    
    radioButtons("f_type", strong("Fitting type:"),
                 list("Random Walk 1st order" = "rw1",
                      "Random Walk 2nd order" = "rw2",
                      "Parametric Model" = "para")),
    br(),
    
    conditionalPanel(
      condition="input.f_type=='para'",
      textInput("para_sin_func", "Fit harmonics: sine component", 
                value="1000*sin(2*pi * time.predict/365)"),
      textInput("para_cos_func", "Fit harmonics: cosine component", 
                value="0.01*cos(2*pi * time.predict/365)"),
      
      helpText("Note: Use 'time.predict' as a variable.")
      
      
    ), 
    
    HTML("<hr />"),
    
    sliderInput("h", 
                strong("Prediction horizon:"), 
                value = 50,
                min = 1, 
                max = 100),
    
    br(),
    
    helpText("Note: while the data view will show only the",
             "specified horizon (number of prediction values),",
             " the summary will still be based on the full dataset."),
    
    
    HTML("<hr />"),    
    
    #downloadLink("downloadData", "Download the sample dataset."),
    helpText(HTML("To download instructions, sample dataset or summary in the form of report/ poster/ slides, click <a href=\"/incontext/download\" target=_blank>this link</a>.")),
    
    br(), 
    
    helpText(HTML("All source available on <a href = \"http://anothercontext.co.uk\" target=_blank>Github</a>"))
    
    
  ),
  
  # Show a tabset that includes a plot, summary, and table view
  # of the generated distribution
  #conditional panel to be introduced: 1panel: intro, 2. general stuff ie acf, pacf, spread, 3. prediction using inla
  
  mainPanel(
    tabsetPanel(
      tabPanel("Introduction",  h5("Project description"), ("This project will compare the performance of parametric and non-parametric time series trend models, by means of simulation. The models to be compared include simple parametric trends such as polynomials and sinusoidal functions, as well as more flexible random walk processes, and finally a real dataset example: London mortality rates for years 1st Jan 2000- 31st Dec 2005."), br(),br(),h5("Method"), ("Integrated Nested Laplace approximation and R-INLA were used to estimate the trends and make predictions on the generated time series data with trends."), HTML("<hr />"), h5("Demo guide"), em(">> Starting point"), br(), ("Choose the starting point from the dataset. The slider's minimum corresponds to 1 year and 1 day value, while its maximum is the dataset size reduced by horizon's maximum value."), br(), br(), em(">> Data type"), br(), ("There are 2 data types: sample dataset of London mortality and simulations with soem 'build-up' noise (Gaussian noise rnorm(N) and integrated noise- random walk- cumsum(rnorm(N)), where N=2192 . After choosing simulation option, additional panel will pop out showing 7 types of time series, in no particular order. These are:"), br(), br(), ("linear function >> 4 + .01* (1:N) + rnorm(N)"), br(), br(), ("sine function >> 5*sin(2*pi*(1:N))+rnorm(N)"), br(), br(),("cosine function >> cos(2*pi*(1:N)/10) + rnorm(N) + cumsum(rnorm(N))"), br(), br(), ("polynomial >> sin(2*pi*(1:N)) + rnorm(N) + cumsum(rnorm(N))"), br(), br(), ("non-linear type1 >> 4 + sqrt(.001*log10(1:N)) + rnorm(N) + cumsum(rnorm(N))"), br(), br(), ("non-linear type2 >> -.05*cos(2*pi*x) -.05*sin(2*pi*x) + cos(4*pi*x) + sin(4*pi*x) + cumsum(cumsum(rnorm(N)))
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "), br(), br(), ("function of your choice >> you can type your own function in the additional text field that will pop out after choosing this option. As a variable, use either 'x' or  type '1:N'."), br(), br(), br(), em(">> Fitting Type >> Parametric Model"), br(), ("Fit harmonics by typing sine and cosine components into appopriate text fields. Use 'time.predict' as your variable."), br(), br(), strong("All data are updated immediately after the inputs change."), HTML("<hr />"), ("For more detailed explanation and conclusions, please refer to instractions or project report which is soon going to be available for download.")),
      tabPanel("Prediction", h5("Plot 1. Time Sequence Plot for chosen Method"), plotOutput("prediction"), br(), br(), h5("Plot 2. Time Sequence Plots from Non-Parametric and Parametric Methods"), plotOutput("prediction.all_facets"), br(), br(), h5("Tab 1. Prediction values"), tableOutput("prediction_summary")), 
      tabPanel("Residuals",h5("Plot 3. Residuals using Non-parametric (RW1, RW2) and Parametric Metods"), plotOutput("res.facets"), h5("Plot 4. Histogram, Density and Correlogram Plots for Residuals"), plotOutput("acf.facets")),
      tabPanel("Errors", h5("Plot 5. Root Mean Square, Mean Absolute Percentage Errors and Bias plots"), plotOutput("sd.facets"), plotOutput("error.facets"), br(), br(), h5("Tab 3. Prediction errors"), tableOutput("prediction_errors")),
      tabPanel("Summary", h5("The project is still to be concluded."), br(), br(), h5("Tab 4. Deviance Information Criterium"), br(), tableOutput("dic_crit"), h5("Tab 5. The estimated precision (1/variance) parameters for the gaussian data"), verbatimTextOutput("hyperpar_summary"),br(), h5("Tab 6. The estimated covariate effects with uncertainty intervals"), verbatimTextOutput("fixed_summary"))
      )
  )
))