library(shiny)

# Define UI 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Parametric and Non-parametric Models in Forecasting"),
  
  # Sidebar with controls
  sidebarPanel(
    
      helpText("This demo provides a graphical view over forecasting time series with parametric and non-parametric models.",  
               "Refer to Introduction tab for brief description."),       
      
      h6("Note: After changing any setting or defining a new function, data are updated immediately after the inputs change."),
      br(),
     
      sliderInput("b", 
                strong("Starting value:"), 
                value = 1095,
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
                    choices=c("linear function", "sine function", "cosine function", "polynomial", "non-linear", "harmonic", "[function of your choice]"="choice")),
        
        br(), 
        helpText("Note: go to Intro tab to find out more information about simulation types."),
       
        br(),
        conditionalPanel(
        condition="input.sim_func=='choice'",
        textInput("userchoice", strong("Type your own function:"), 
                  value="2*sin(2*pi*x/365)+rnorm(N)"),
        
        helpText("Note: Use 'x' as a variable or, alternatively '1:N', and rnorm(N) to add some noise.")
       
      )), 
        
  
    HTML("<hr />"),
    
    radioButtons("f_type", strong("Fitting type:"),
                 list("Random Walk 1st order (RW1)" = "rw1",
                      "Random Walk 2nd order (RW2)" = "rw2",
                      "Parametric Model (PRM)" = "para")),
      br(),
      
    conditionalPanel(
        condition="input.f_type=='para'",
        radioButtons("harm_fit", strong("Harmonics fitting:"),
                 list("Automatic"="auto",
                      "User Defined"="harm_user_def")),
        conditionalPanel(    
            condition="input.harm_fit=='harm_user_def'",
            textInput("para_sin_func", "Fit harmonics: sine component", 
                        value="sin(2*pi*time.predict/365)"),
            textInput("para_cos_func", "Fit harmonics: cosine component", 
                      value="cos(2*pi*time.predict/365)"),
           
            helpText("Note: Use 'time.predict' as a variable.")
        )
        
      ), 
   
    HTML("<hr />"),
    
    sliderInput("h", 
                strong("Prediction horizon (in days):"), 
                value = 50,
                min = 1, 
                max = 100),
    
    br(),
    
    helpText("Note: Data are updated immediately after the inputs change."),
    
    
    HTML("<hr />"),    
    
    #downloadLink("downloadData", "Download the sample dataset."),
    helpText(HTML("To download the summary in the form of report or poster, click <a href=\"/incontext/download\" target=_blank>this link</a>.")),
      
    br(), 
    
    helpText(HTML("All source available on <a href = \"https://github.com/AnotherContext/stats.git\" target=_blank>Github</a>"))
    
    
  ),
  
  #Show a tabset that includes a plots, summary, and tables view
  #conditional panel to be introduced: 1panel: intro, 2. general stuff ie acf, pacf, spread, 3. prediction using inla
  
  mainPanel(
    tabsetPanel(
      tabPanel("Introduction",  h6("Note: The application was tested using Firefox and Google Chrome browsers only. It will not work loaded in IE as it does not correctly render IFRAME."), br(), h5("Project description"), ("Prediction of the next values of a time series before they are observed is difficult because of extrapolating beyond the limits of available data. A forecast is intended to capture the genuine patterns and relationships which exist in the historical data"), br(), br(), ("The aim of this project is to compare the performance of parametric and non-parametric time series models, by means of simulation, in predicting the trends into the future. The models to be compared include simple functions such as polynomials and sinusoidal functions, as well as a real dataset example: London mortality rates for years 1st Jan 2000- 31st Dec 2005."), br(),br(),h5("Method"), ("The method used to estimate trends and predictions was the Bayesian inference based on the Integrated Nested Laplace Approximation approach. The computational interface to INLA was delivered by the R-INLA program. More details can be found on www.r-inla.org."), br(), br(), ("The non-parametric models were estimated using first and second-order random walks. Due to regular seasonal shape of the real data, the parametric model was restricted to the harmonic regression model. Although, this choice is questionable for linear, polynomial and non-linear simulations, the other options were not implemented due to the analysis that was carried out in the context of seasonal data and complexity of the application in place. However, the possible extention is not excluded in the future."), HTML("<hr />"), h5("Demo guide"), em(">> Starting point"), br(), ("Choose the starting point from the dataset. The slider's minimum corresponds to 1 year and 1 day value, while its maximum is the dataset size reduced by horizon's maximum value."), br(), br(), em(">> Data type"), br(), ("There are 2 data types: sample dataset of London mortality and simulations with some 'build-up' noise (Gaussian noise rnorm(N) and integrated noise- random walk- cumsum(rnorm(N)), where N=2192 . After choosing simulation option, additional panel will pop out showing 7 types of time series in no particular order. These are:"), br(), br(), ("linear function >> 4 + .01* (1:N) + rnorm(N)"), br(), br(), ("sine function >> 0.5 + 0.5*sin(2*pi*x/365) +  0.8*sin(4*pi*x/365) + rnorm(N)"), br(), br(),("cosine function >> cos(2*pi*(1:N)/356) + 0.5*cos(4*pi*x/365)+ rnorm(N)"), br(), br(), ("polynomial >> 0.01 + 5*(.001*(1:N))^3 + rnorm(N)"), br(), br(), ("non-linear>> 4 + sqrt(.001*log10(1:N)) + rnorm(N) + cumsum(rnorm(N))"), br(), br(), ("harmonic >> 0.5 + 0.5*sin(2*pi*x/365) + 0.5*cos(2*pi*x/365) + 0.8*cos(4*pi*x/365) + 0.8*sin(4*pi*x/365) + cos(8*pi*x/365) + rnorm(N)
  "), br(), br(), ("function of your choice >> you can type your own function in the additional text field that will pop out after choosing this option. As a variable, use either 'x' or  type '1:N'. You could also add some noise 'rnorm(N)' and/or random walk 'cumsum(rnorm(N))'."), br(), br(), br(), em(">> Fitting Type >> Parametric Model"), br(), ("There are 2 options: you can leave fitting to the program or fit harmonics by choosing 'User defined' option, and then typing sine and cosine components into appopriate text fields. Use 'time.predict' as your variable."), br(), br(), strong("All data are updated immediately after the inputs change."), ("This may cause an error message to appear but don't worry, just make sure the typed function is correct; there are no missing signs, parentheses etc., and be patient while waiting for the result. INLA is fast and R-INLA program is computationally powerful but it may take few more seconds! Obviously, the speed of your broadband connection matters as well."), HTML("<hr />"), ("For more detailed explanation and conclusions, please refer to the report which is soon going to be available for download.")),
      tabPanel("Prediction", h5("Plot 1. Time series plot for chosen fitting method and horizon"), plotOutput("prediction"), br(), br(), h5("Plot 2. Time series plots generated using all 3 fitting methods and chosen horizon"), plotOutput("prediction.all_facets")), #, br(), br(), h5("Tab 1. Prediction values"), tableOutput("prediction_summary")), 
      tabPanel("Residuals",h5("Plot 3. Residuals of Non-parametric (RW1, RW2) and Parametric Models"), plotOutput("res.facets"), h5("Plot 4. Histogram, Density and Correlogram Plots for Residuals"), plotOutput("acf.facets")),
      tabPanel("Errors", h5("Plot 5. Root Mean Square, Mean Absolute Percentage Errors and Bias plots"), plotOutput("sd.facets"), plotOutput("error.facets"), br(), br(), h5("Tab 3. Prediction errors"), tableOutput("prediction_errors")),
      tabPanel("Summary", br(), br(), h5("Tab 4. Deviance Information Criterium"), br(), tableOutput("dic_crit"), h5("Tab 5. The estimated precision (1/variance) parameters for the gaussian data"), verbatimTextOutput("hyperpar_summary"),br(), h5("Tab 6. The estimated covariate effects with uncertainty intervals"), verbatimTextOutput("fixed_summary"))
      )
   )
))

