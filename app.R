


rm(list=ls())

####################
library(shiny)
library(haven)
library(shinythemes)
library(ggplot2)
library(tidyr)
library(dplyr)
library(hrbrthemes)
library(formattable)
library(shinyjs)
library(rintrojs)
library(shiny.info)
library(shiny.i18n)
library(sjlabelled)
library(ggthemes)
library(robustHD)




url <- a("Open Psychometrics, 'Development of the Nerdy Personality Attributes Scale. 20 December 2015.' ", href="http://openpsychometrics.org/tests/NPAS/development/")
url2 <- a("Nigel Gervas Meek, (2017). The Association between personality and social attitudes in a British context", href="http://doi.org/10.5255/UKDA-SN-852860/")
url3 <- a("Wicherts, J. M., Veldkamp, C. L., Augusteijn, H. E., Bakker, M., Van Aert, R., & Van Assen, M. A. (2016). Degrees of freedom in planning, running, analyzing, and reporting psychological studies: A checklist to avoid p-hacking. Frontiers in psychology, 7, 1832.", href="https://doi.org/10.3389/fpsyg.2016.01832")
url4 <- a("o.e.c.alagoz@tilburguniversity.edu", href="mailto:o.e.c.alagoz@tilburguniversity.edu")


#################################################

#import data
# for some reason, the spss file that I imported was acting weird in analyses. Then I noticed all values were assigned as factors, and a different type of labelling was preventing me from indexing.
# so I removed all labels and made the whole data numeric. then ordered the data for %based outlier removal
poldat <- read_sav("data1.sav")

poldat <- remove_all_labels(poldat)

poldat <- as_numeric(poldat)

poldat<- poldat[order(-poldat$sum_per, na.last=NA), ]


## it is for rendering the table even no predictors are selected.

empty_table<- data.frame(c("Select","Some","Variables"), c("To", "Start", "Analysis"))
names(empty_table)<- c(" ", " ")


direction<- TRUE
f_pol<- vector()

coefs_poldat<-matrix(nrow=4, ncol=4)

poldat$left<-poldat$Left_Right
poldat$welfare<-poldat$Welfarism
poldat$immigrant<-poldat$Immigration
poldat$libertarianism<-poldat$Libertarianism
poldat$religiousity<-poldat$Religiosity
poldat$theocratism<-poldat$Theocratism


#below is for passing variables into left-wing variable's computation, and passing control variables into regression model

reg_for_poldat <- function (input1, input2, input3, input4, input5, input6, control1, control2, control3, out_r_poldat, out_l_poldat, out_sd_poldat, plot=FALSE) {
    
  #the same as npas data but now I modified it to use them simultaneously. before, it was only sd-based or only percentage-based.   
  
    upperbound <- mean(poldat$sum_per) + out_sd_poldat * sd(poldat$sum_per) 
    lowerbound <- mean(poldat$sum_per) - out_sd_poldat * sd(poldat$sum_per)
    
    if (out_sd_poldat != 0) {poldat<- poldat[poldat$sum_per< upperbound & poldat$sum_per> lowerbound, ]
    poldat<- poldat[(1 + out_l_poldat * nrow(poldat) / 100) : (nrow(poldat) - out_r_poldat * nrow(poldat) / 100), ]}
    
    else {poldat<- poldat[(1 + out_l_poldat * nrow(poldat) / 100) : (nrow(poldat) - out_r_poldat * nrow(poldat) / 100), ]}
    
    
    # empty containers for passing variables into formulas
    
    predictors_poldat<<- vector()
    controllers<<- vector()
    
    # any selected variable is passed into predictors_poldat vector
    all_predictors <- c("left", "welfare", "immigrant", "libertarianism", "religiousity", "theocratism")
    
    select <- c(input1, input2, input3, input4, input5, input6)
    
    predictors_poldat <<- all_predictors[select]
    
    # any selected control variable is passed into controllers vector
    
    all_control_vars <- c("Sex","Age","Education")
    
    select_cont<- c(control1, control2, control3)
    
    controllers <<- all_control_vars[select_cont]
    
    
    outcome<-"poldat$sum_per"
    
    # if variables are selected from both option, 
    #first it calculates the sum score of leftism according to variables selected on that panel.
    
    if (length(predictors_poldat)!=0 & length(controllers)!=0) {
      
        #this results in, for instance, poldat$politics<- poldat$left+poldat$welfare etc.   
      
        func<<-paste(paste("poldat$Politics <-"), paste(paste(paste("poldat", predictors_poldat, sep="$"),collapse = "+")), sep="")
        
        #this evaluates the command above
        eval(parse(text = func))
        
        #to show it on the table in a better way, I assigned it to Leftism variable
        Left_Wing <- poldat$Politics 
        
        
        #with this one, I created a formula to use it in lm function. as I did in the npas data before, it creates a fromula with personality as the outcome, leftism as the main IV, and controllers as control variables
        
        c<- c(paste("Left_Wing", paste(paste(paste("poldat", controllers, sep="$"),collapse = "+")), sep="+"))
        
        f_pol<<-as.formula(paste(outcome, c, sep= "~"))
        
        effect_poldat<<-summary(lm(f_pol)) 
        
        coefs_poldat<<- as.data.frame(effect_poldat$coefficients)
        coefs_poldat<<-signif(coefs_poldat, digits = 4)
        
        #this one creates the direction variable which will be used in the verdict section. It takes the "estimate" value from the coefficients and if it's a positive value, it assigns "positive" to it, vice versa.
        if(coefs_poldat[2,1]>0) { direction<<- "positive"} else {direction<<- "negative"}
        
        # if one does not select control variables, then this section kicks in and do the regression with only Leftism variable
    } else if (length(predictors_poldat)!=0 & length(controllers)==0)  { 
        
        func<<-paste(paste("poldat$Politics <-"), paste(paste(paste("poldat", predictors_poldat, sep="$"),collapse = "+")), sep="")
        
        eval(parse(text = func))
        
        Left_Wing <- poldat$Politics 
        
        
        f_pol<<- as.formula(paste(outcome, "~", "Left_Wing"))
        
        effect_poldat<<-summary(lm(f_pol)) 
        
        coefs_poldat<<- as.data.frame(effect_poldat$coefficients)
        coefs_poldat<<-signif(coefs_poldat, digits = 4)
        
        if(coefs_poldat[2,1]>0) { direction<<- "positive"} else {direction<<- "negative"}
        
        
    }
    
    
    
    names(coefs_poldat)<<- c("Estimate", "Standard Error", "t Value", "p Value")
    
   
  
    # here, I added this statement to create the plots only in plot output. The first one is used if any predictor is selected. The second one is used if no predictors is selected.
    # the second one contains the message of "select variable to print the plot".
    if(plot==TRUE & length(predictors_poldat)!=0) {
        
        ggplot()+aes(x=poldat$Politics, y=poldat$sum_per) +
            geom_point()+ geom_rangeframe() +
        labs(x="Left-Wing Opinions", y="How good is one's personality")+
            stat_smooth(method = "lm", formula = y ~ x, size = 1) + theme_tufte()
    } else {
        
        plot.new() 
        mtext( "Select some variables to print the plot.", side = 3)}
    
}

# I modified the verdict from the npas data. I added some html codes to print direction, significance and p-value in different colors.
verdict_poldat <- function(x)
    
{ 
    
    
    
    if(coefs_poldat$`p Value`[2]< .001) {paste("You have found a significant and ","<font color=\"#685fff\"><b>", direction, "</b></font>", " association between left-wing opinions and positive personality attributes!<font color=\"#685fff\"><b> (p<.001) </b></font>")} 
    
    else if (coefs_poldat$`p Value`[2]>= .001 & coefs_poldat$`p Value`[2]< .05) {paste0("You have found a significant and ","<font color=\"#685fff\"><b>", direction, "</b></font>", " association between left-wing opinions and positive personality attributes! The p-value you obtained is: ", "<font color=\"#685fff\"><b>", round(coefs_poldat$`p Value`[2], 3),"</b></font>", ". Did you know that you can try to improve your effect size by removing some outliers or controlling for some variables?")} 
    
    else if (coefs_poldat$`p Value`[2]>= .05 & coefs_poldat$`p Value`[2]< .10) {paste0("You <font color=\"#d10007\"><b> couldn't get </b></font> a significant p-value. 
                                                                                       But, you are so close to find a ", "<font color=\"#d10007\"><b>", direction,"</b></font>", " association between left-wing opinions and positive personality attributes! Your p-value is: ", "<font color=\"#685fff\"><b>", round(coefs_poldat$`p Value`[2], 3),"</b></font>", ". Maybe you can catch a significant one if you add/remove another control variables or remove some outliers." )}
    
    else {paste0("That is unlucky. You <font color=\"#d10007\"><b> couldn't find </b></font> any significant association (", "<font color=\"#d10007\"><b>", round(coefs_poldat$`p Value`[2], 3),"</b></font>", "). Try harder! Maybe you can catch one if you add some other predictors or remove some outliers.")}
    
}


#import data

## below is the same as I send it to you before. I only changed the outlier removal and added colors to verdict section.
data <- read_sav("data.sav")



data<- data [order(-data$sumnerd, na.last=NA), ]

multiple_regression <- function (input1, input2, input3, out_r, out_l, out_sd, graphoption="FALSE") {
    

    upperbound <- mean(data$sumnerd) + out_sd * sd(data$sumnerd) 
    lowerbound <- mean(data$sumnerd) - out_sd * sd(data$sumnerd)
    

    if (out_sd != 0) {data<- data[data$sumnerd< upperbound & data$sumnerd> lowerbound, ]
    data<- data[(1 + out_l * nrow(data) / 100) : (nrow(data) - out_r * nrow(data) / 100), ] }
    
    else {data<- data[(1 + out_l * nrow(data) / 100) : (nrow(data) - out_r * nrow(data) / 100), ]}
    

    all_predictors <- c("gender", "orientation", "marriagestatus", "livingarea")
    
    select <- c(TRUE, input1, input2, input3)
    
    predictors <- all_predictors[select]
    
    outcome <- "data$sumnerd"
    
    
    f <- as.formula(
        
        paste(outcome, "~",
              
              paste(predictors, collapse = " + ")
              
        ))
    
    lm(data=data, f)
    
    effect<-summary(lm(data=data, f))
    
    coefs<<- as.data.frame(effect$coefficients)
    
    coefs<<-round(coefs,3)
    
    names(coefs)<<- c("Estimate", "Standard Error", "t Value", "p Value")
    
    
    if (graphoption==T) { data$gender<- as.factor(data$gender)
    
    levels(data$gender)<- c("Male", "Female")
    
    
    data<- na.omit(subset(data, select = c(sumnerd, gender)))
    
    data %>%
        
        ggplot(aes(x=sumnerd, fill=gender)) +
        
        geom_histogram( color="lightsalmon4", alpha=0.3, position = 'identity') +
        
        scale_fill_manual(values= c("blue", "lightsalmon4")) +
        
        labs(x="Total Score of Nerdy Personality", y="Frequency")+
        
        xlim(25,130)+
        
        theme_ipsum()  }
}



verdict <- function(x)
    
{ 
    
    if(coefs$`p Value`[2]< .001) {print("You have found a <font color=\"#685fff\"><b> significant </b></font> association between gender and nerdy personality! <font color=\"#685fff\"><b> (p<.001) </b></font>")} 
    
    else if (coefs$`p Value`[2]>= .001 & coefs$`p Value`[2]< .05) {paste0("You have found a <font color=\"#685fff\"><b> significant </b></font> association between gender and nerdy personality! The p-value you obtained is: ","<font color=\"#685fff\"><b>", round(coefs$`p Value`[2], 3), "</b></font>", ". Did you try to improve your effect size by removing some outliers?")} 
    
    else if (coefs$`p Value`[2]>= .05 & coefs$`p Value`[2]< .10) {paste0("You are so close to find a <font color=\"#d10007\"><b> significant </b></font> association between gender and nerdy personality! Your p-value is: ","<font color=\"#d10007\"><b>", round(coefs$`p Value`[2], 3), "</b></font>" ,". Maybe you can catch a significant one if you add/remove another predictor or remove some outliers." )}
    
    else {paste0("That is unlucky. You <font color=\"#d10007\"><b> couldn't </b></font> find any <font color=\"#d10007\"><b> significant </b></font> association (","<font color=\"#d10007\"><b>", round(coefs$`p Value`[2], 3), "</b></font>", "). Try harder! Maybe you can catch one if you add some other predictors or remove some outliers.")}
    
}

########################

# I used navbarPage to combine two datasets in one app. the first tabPanel is for NPAS data, and the second is for political psyc data.

# "intro.box"es are for the introduction thingy that the detective talks. The rest is the same as I sent before, so I am passing to new dataset.
ui <- navbarPage(
    
    
    title = 
      div(icon("user-secret", lib="font-awesome"),"P-Hack the Personality",icon("user-secret", lib="font-awesome")),
    windowTitle = HTML("P-hack the Personality"),
    
    theme=shinytheme("yeti"),
    fluid = TRUE,
    
    tabPanel(  
               title= "Nerdy Personality Attributes Data", icon=icon("user-tag", lib="font-awesome"), useShinyjs(), introjsUI(), theme = shinytheme("paper"), style=" align:right ",
               
               introBox(
               div(img(src = "logo.png", style="height: 140px")),
               
               data.step=1,
               
               data.intro = "Hello, there! I am the Detective P. Many scientists call me p-hacker, but I use some strategies to turn a non-significant result into a significant one. With this application, I will tell you my top secret recipe for finding significant results. Let's try to explain the nerdy personality attributes with gender!)"),
               
              
               
               sidebarLayout(                 
                   
                 sidebarPanel( 
                   
                   
                   
                   
                   actionButton("intro", icon= icon("cogs"), "How it works?" ),
                   
                   
                   introBox(
                     
                     h3("Predictors"), 
                     hr(),
                     actionButton("info1", icon = icon("question-circle"), "Why it is problematic?" ),
                     
                     hr(),
                     
                     checkboxInput("orientation", "Control for the sexual orientation", value= FALSE),
                     br(),
                     
                     checkboxInput("marriagestatus", "Control for the marital status", value= FALSE),
                     br(),
                     
                     checkboxInput("livingarea", "Control for the living area", value=FALSE),
                     
                     data.step=2,
                     
                     data.intro = "From this panel, you can select one or more predictors to include in your regression analysis. Remember, If you add lots of variables into your research, you would have more options make your results significant! Cool, right?"),
                   
                   
                   hr(),
                   
                   
                   introBox(
                     
                     h3("Outlier Options"),
                     hr(),
                     actionButton("info2", icon= icon("question-circle"), "Why it is problematic?"),
                     hr(),
                     
                     
                     introBox(
                       
                       sliderInput("outlier_r", label = h5("Remove outliers from the right tail (%)"), min = 0, max = 25, 
                                   value = 0),
                       data.step= 4,
                       
                       data.intro= "Use this slider to remove data from the right tail."),
                     
                     introBox(
                       
                       br(),
                       sliderInput("outlier_l", label = h5("Remove outliers from the left tail (%)"), min = 0, max = 25, value = 0),
                       
                       data.step = 5,
                       
                       data.intro= "Use this slider to remove data from the left tail."),
                     
                     
                     br(),
                     introBox(
                       
                       sliderInput("outlier_sd", label = h5("Remove outliers from both tails (SD)"), min = 0, max = 2, value = 0),
                       
                       data.step = 6,
                       
                       data.intro= "If you don't want to use percentages, you can easily select a data range based on standard deviation."),
                     
                     data.step = 3,
                     
                     data.intro = "From this panel, you can use these sliders to adjust your sample's range."))
                 
                 ,          
                   
                   
                 mainPanel( 
                   h4(tags$i("In order to see why these actions are considered as p-hacking, click on the buttons on the left panel.")),
                   
                   
                   
                   hidden(
                     
                     div(id="info_pre",
                         
                         h4(htmlOutput("info1text") ) 
                     )
                   ),
                
                   
                   
                   
                   hidden(
                     
                     div(id="info_out",
                         
                         h4( htmlOutput("info2text")))),
                   
                   hr(),               
                   h3("Results of the Analysis"), 
                   hr(),
                   introBox( 
                     
                     h3("Is your finding significant?"), 
                     hr(),  
                     
                     h4( htmlOutput("stats")), 
                     
                     data.step = 7,
                     
                     data.intro = "Here, you will see what the p-value and whether it is significant."),
                 
                   hr(), 
                   
                   introBox(  
                     
                     h4(icon("file-contract"), "Summary Statistics"), 
                     
                     formattableOutput("results"),
                     
                     data.step = 8,
                     
                     data.intro = "And here, you will see a bit more detailed information regarding the results of the analysis."),
                   br(),
                   hr(), 
                   
                   introBox(
                     
                     h4(icon("chart-bar"),"Histogram of Nerdy Personality Score Distribution"),
                     
                     
                     br(),
                     
                     br(),            
                     plotOutput(outputId = "hist_nerd"),
                     
                     data.step = 9,
                     
                     data.intro = "With this awesome-looking histogram, you can see how your data look as you remove some data via sliders."))
                 
               ),
               
               tabName = "tabone",
               tagList("Data is obtained from:", url),
               br(),
               tagList("Reference: ", url3),
               id = "",
               
               
               
    ),
    
    tabPanel(title="Political Psychology Data",icon=icon("galactic-republic", lib="font-awesome"),useShinyjs(), introjsUI() ,
             
             div(img(src = "logo.png", style="height: 140px")),
             
                                fluidPage(
             
             # I used fluid row to separate the sidebar thing because if I give it in one column, it becomes too long and people have to scroll-up or down to change inputs and to see outputs.
            fluidRow( 
              
              
              column(2,   
                     
                     
                     
                     h4("How do you want to define Left-wing tendency? Select a set of variables to measure it:"),
                     
                     hr(),
                     
                     actionButton("info1_poldat", icon = icon("question-circle"), "Why it is problematic?" ),
                     
                     hr(),
                     
                     checkboxInput("left","Attitudes towards relations between socioeconomic classes", value= FALSE),
                     br(),
                     
                     checkboxInput( "welfare", "Attitudes towards government provision of welfare", value= FALSE),
                     br(),
                     
                     checkboxInput( "immigrant", "Attitudes towards immigrants/immigration", value= FALSE),
                     
                     hr(),
                     
                     checkboxInput( "libertarianism", "Opinions about social traditionalism ", value= FALSE),
                     br(),
                     
                     checkboxInput( "religiousity","How religious is someone", value= FALSE),
                     br(),
                     
                     checkboxInput( "theocratism", "Opinions about theocracy", value=FALSE)),
                 
              column(8, 
                     h4(tags$i("In order to see why these actions are considered as p-hacking, click on the buttons on the left panel.")),
                     
                     hidden(
                       
                       div(id="info_pre_poldat",
                           
                           h4(htmlOutput("info1_poldattext") ) 
                       )
                     ),  busy(loader = "dots", position = "top right"),
                     
                     
                     hidden(
                       
                       div(id="info_out_poldat",
                           
                           h4( htmlOutput("info2_poldattext")))),
                     
                     
                     hidden(
                       
                       div(id="info_cont_poldat",
                           
                           h4( htmlOutput("info3_poldattext")))),
                     
                     hr(),               
                     h3("Results of the Analysis"), 
                     hr(),
                     
                     
                     h3("Is your finding significant?"), 
                     
                     #used htmloutput to render text with colored words 
                     h4( htmlOutput("stats_poldat")), 
                     
                     
                     br(), 
                     hr(), 
                     
                     
                     h4(icon("file-contract"), "Summary Statistics"), 
                     
                     formattableOutput("results1"),
                     
                     br(),
                     hr(), 
                     
                     
                     
                     h4(icon("chart-bar"),"Association Between Left-Wing Politics and Better Personality"),
                     
                     
                     br(),
                     
                     br(),            
                     plotOutput(outputId = "plot_per")
                     
                     
                     
              ),
                 
              column(2,
                     
                     
                     hr(),
                     
                     h4("Control for some variables:"),
                     
                     
                     actionButton("info2_poldat", icon = icon("question-circle"), "Why it is problematic?" ),
                     
                     hr(),
                     
                     checkboxInput("Sex","Sex", value= FALSE),
                     br(),
                     
                     checkboxInput( "Age", "Age", value= FALSE),
                     br(),
                     
                     checkboxInput( "Education", "Education", value= FALSE),
                     
                     
                     
                     hr(),
                     
                     
                     
                     h4("Outlier Options"),
                     
                     actionButton("info3_poldat", icon= icon("question-circle"), "Why it is problematic?"),  
                     
                     hr(),
                     
                     hr(),
                     
                     sliderInput("outlier_r_poldat", label = h5("Remove outliers from the right tail (%)"), min = 0, max = 25, 
                                 value = 0),
                     
                     br(),
                     sliderInput("outlier_l_poldat", label = h5("Remove outliers from the left tail (%)"), min = 0, max = 25, value = 0),
                     
                     
                     br(),
                     
                     
                     sliderInput("outlier_sd_poldat", label = h5("Remove outliers from both tails (SD)"), min = 0, max = 2, value = 0),
                     
                     
              )
             )),
             
             tabName = "tabtwo",
             tagList("The data is obtained from UK Data Service:", url2),
            br(),
            tagList("Reference: ", url3),
             id = "secondtab"
            ),
    
            tabPanel(title="About", icon=icon("info-circle"), lib="font-awesome",
    
    h4(htmlOutput("about")),
    
  
    
    
            )
    )



server <- function(input, output,session) {
    
    
    
    
    
    output$stats_poldat <- renderText({
        
        # I had to specify all situations (only leftism variables are selected or also the control vars. are selected ) with if-else if. otherwise it wouldn't have rendered the output.
        
        reg_for_poldat(input$left, input$welfare, input$immigrant, input$libertarianism, input$religiousity, input$theocratism, input$Sex, input$Age, input$Education, input$outlier_r_poldat, input$outlier_l_poldat, input$outlier_sd_poldat)
        
        if( length(predictors_poldat)!=0 & length(controllers)!=0) {
            
            verdict_poldat()
        } 
        else if (length(predictors_poldat)!=0 & length(controllers)==0) {
            
            verdict_poldat()  
        } 
       
      #if no input is selected, then it tells people to select some variables to start analysis 
        else if (length(predictors_poldat)==0 & length(controllers)==0) {  
            paste("Start with selecting some variables on the left panel.")  }
        
    })
    
    
    
    output$results1<- renderFormattable({
        
        reg_for_poldat(input$left, input$welfare, input$immigrant, input$libertarianism, input$religiousity, input$theocratism, input$Sex,  input$Age, input$Education,  input$outlier_r_poldat, input$outlier_l_poldat, input$outlier_sd_poldat)
        
        # with else statement, I showed another table to tell people select some variables, if they haven't selected any
        if(length(predictors_poldat)>0) {
            formattable(coefs_poldat, align = c( rep("r", 4)),
                        
                        
                        list(`p Value` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), area(col = 1) ~ color_tile("#DeF7E9", "#71CA97")))
        } else { formattable(empty_table, align = c( rep("c", 4)),
                             
                             
                             list(`p Value` = formatter("span", style = ~ style(color = "grey",font.weight = "bold"))))}
    })
    
    
    
    
    
    
    output$plot_per<- renderPlot({
        
        
        reg_for_poldat(input$left, input$welfare, input$immigrant, input$libertarianism, input$religiousity, input$theocratism, input$Sex, input$Age, input$Education,  input$outlier_r_poldat, input$outlier_l_poldat, input$outlier_sd_poldat, TRUE)
        
    })
    
    # below is the outputs for NPAS data. they are the same as before.
    output$stats <- renderText({
        
        
        
        multiple_regression(input$orientation, input$marriagestatus, input$livingarea, input$outlier_l, input$outlier_r, input$outlier_sd, FALSE)
        
        
        verdict() 
        
    })
    
    
    
    
    
    output$results<- renderFormattable({
        
        multiple_regression(input$orientation, input$marriagestatus, input$livingarea, input$outlier_l, input$outlier_r, input$outlier_sd, FALSE)
        
        
        formattable(coefs, align = c( rep("r", 4)),
                    
                    
                    list(`p Value` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), area(col = 1) ~ color_tile("#DeF7E9", "#71CA97")))
    })
    
    
    
    
    
    
    output$hist_nerd<- renderPlot({
        
        
        multiple_regression(input$orientation, input$marriagestatus, input$livingarea, input$outlier_l, input$outlier_r, input$outlier_sd, TRUE)
        
    })
    
    output$about<- renderText( {
    
     paste("
      <center><font color=\"#d10007\"><h2><b> About the Application</b></h2></font>
      <br></br><hr></hr>
      <h5>This web application was created by <i><b>Ömer Emre Can Alagöz</b></i> under the supervision of <i><b>Michèle Nuijten</b></i>.</h5>

      <hr></hr>
      <b>Aim:</b>
      <br></br>
<h4>Aim of this project was providing an application for social sciences students/researchers to show them how some basic and common behaviours could dramatically change the statistical results. Using <b>real-world datasets</b> and also providing some brief information regarding when and why these behaviours could be harmful for the research is thought to strengthen the instructive feature of the application. People, especially students, who do not know much about these issues are expected to be intrigued with this introductory real-life examples; ones with some basic knowledge are expected to comprehend their effect through practising these <b>p-hacking</b> behaviors.
</h4>
      
      <hr></hr>
      Contact Information - Ömer Emre Can Alagöz:
      <br></br>",
           icon=icon("paper-plane", lib="font-awesome"),"
      <h5><b><a href=mailto:o.e.c.alagoz@tilburguniversity.edu>Send an Email</a></b></h5>", icon=icon("github", lib = "font-awesome"),
     " <h5><b><a href=https://github.com/oecalagoz>GitHub</a></b></h5>",
     icon=icon("twitter", lib="font-awesome"),
      "<h5><b><a href=https://www.twitter.com/oec_alagoz <>Twitter</a></b></h5>
      <hr></hr>
      <h5><b><a href=https://www.tilburguniversity.edu/about/schools/socialsciences/organization/departments/methodology-statistics>Tilburg University, Department Methodology Statistics</a></b></h5>
      <h5><b>Tilburg, The Netherlands </b></h5>",img(src="tiu.png", style="height: 140px", align="center"),"
      </center>
      ")
    })
    
    # this one makes the introduction part start if "how it works" is clicked on. 
    
    observeEvent(input$intro,
                 introjs(session, options = list("nextLabel"=("Next"),
                                                 "prevLabel"="What? Hold on, I couldn't catch that!",
                                                 "skipLabel"="I am impatient.")
                         
                 )
    )
    
    #these show some brief information regarding the why these behaviours are bad. it works with "toggle" logic. the first click shows it, the second one hides it.
    
    observeEvent(input$info1, {
        
        toggle('info_pre')
        
        output$info1text<- renderText({
            
          "<hr></hr> Researcher should decide on variables that a theory or the literature suggests an association beforehand the study. Without a proper rationale, measuring many variables to try out if they moderate an association or any association becomes significant after controlling them is not a good practice. Including arbitrary variables in the model could spuriously strengthen a relationship by explaining more variance. It is even worse if the researcher, later on, modify a hypothesis accordingly. For more detailed discussion: (Wicherts et al., 2016)"
        })
    })
    
    
    
    observeEvent(input$info2, {
        
        toggle('info_out')
        
        output$info2text<- renderText({
            
            
" <hr></hr> There are many different ways to deal with outliers or influential data points. For instance, one can run Dixon's Q test, examine the quartiles, and treat cases as outliers if they fall outside of a specific standard deviation. One should not automatically employ one of these options because the important thing about treating a case as outlier is basing this decision on substantial arguments. Removing a data point may turn a non-significant p-value to a significant one (vice versa). Here the SD-based option gives you a chance to remove data beyond 1 or 2 SD deviations far from the mean. This is totally an arbitrary decision. Also, you can play with the data range via the percentage option, so that you can see how outlier removal is influential on results." })
    })
    
    

    
    observeEvent(input$info1_poldat, {
        
        toggle('info_pre_poldat')
        
        output$info1_poldattext<- renderText({
            
            "<hr></hr> There could be more than one aspect to define a variable. For instance, while some could measure left-wing opinions with attitudes towards redistribution of resources, others could measure it with attitudes towards welfare policies. Or, one could combine more than one aspect. Each different definition may lead to not only different p-values but also opposite directions of the association. Again, using substantial theories and a good literature review are critical steps to define a variable."
        })
    })
    
    
    
    observeEvent(input$info2_poldat, {
        
        toggle('info_cont_poldat')
        
        output$info3_poldattext<- renderText({
            
            "<hr></hr> Researcher should decide on variables that a theory or the literature suggests an association beforehand the study. Without a proper rationale, measuring many variables to try out if they moderate an association or any association becomes significant after controlling them is not a good practice. Including arbitrary variables in the model could spuriously strengthen a relationship by explaining more variance. It is even worse if the researcher, later on, modify a hypothesis accordingly. For more detailed discussion: (Wicherts et al., 2016) "
        })
    })
    
    observeEvent(input$info3_poldat, {
        
        toggle('info_out_poldat')
        
        output$info2_poldattext<- renderText({
            
          "<hr></hr> There are many different ways to deal with outliers or influential data points. For instance, one can run Dixon's Q test, examine the quartiles, and treat cases as outliers if they fall outside of a specific standard deviation. One should not automatically employ one of these options because the important thing about treating a case as outlier is basing this decision on substantial arguments. Removing a data point may turn a non-significant p-value to a significant one (vice versa). Here the SD-based option gives you a chance to remove data beyond 1 or 2 SD deviations far from the mean. This is totally an arbitrary decision. Also, you can play with the data range via the percentage option, so that you can see how outlier removal is influential on results." })
    
    })
    
}

shinyApp(ui = ui, server = server)

