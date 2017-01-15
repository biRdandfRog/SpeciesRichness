library(shinydashboard)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Species Richness",tabName="sprich",icon=icon("star",lib="glyphicon")),
    menuItem("Example",tabName="example",icon=icon("twitter", lib="font-awesome")),
    menuItem("Exercises",tabName="exercises",icon=icon("line-chart", lib="font-awesome")),
    menuItem("Learn More",tabName="lit",icon=icon("book", lib="glyphicon"))
    ,br(),br()
    
  ) 
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName="sprich",
            fluidRow(
              column(width=4,
                     box(title=tags$b("Measuring species richness"), width=NULL,collapsible = TRUE,collapsed=TRUE,
                         p("Species richness is the total number of unique species in a defined area. Measuring species richness is a core
                           objective of ecological research and conservation, and provides a foundation for understanding biodiversity."),
                         p("This application demonstrates how species richness is estimated and compared between two sites that differ in the number of 
                           individuals and sampling effort.")),
                     box(title = tags$b("Community Metrics"),solidHeader = TRUE,width=NULL,
                         p("Use the sliders to set the species richness of two hypothetical communities. Pressing 'Rarefy!' will generate a species accumulation curve
                           showing the number of new species observed in the communities (y-axis), with increasing individuals or sampling effort (x-axis)."),
                         column(width=6,
                               tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #CD950C}")),
                               tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #79B791}")),
                               h4(tags$b("Community A"),align="center"),br(),
                               sliderInput("srA","Species Richness",min=0,max=100,round=TRUE,value=25,width=200,ticks=FALSE),br(),
                               actionButton("rarefy","Rarefy!"),br()),
                         column(width=6,
                                h4(tags$b("Community B"),align="center"),br(),
                                sliderInput("srB","Species Richness",min=0,max=100,round=TRUE,value=45,width=200,ticks=FALSE),br(),br(),br(),br()),
                         p("The accumulation curve is used to estimate overall species richness and compare among sites when the number of individuals observed and/or sampling effort differs.
                           Change the number of individuals observed and sampling effort for each community to see how measured species richness is affected."),br(),
                         column(width=6,
                                tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #CD950C}")),
                                tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: #CD950C}")),
                                tags$style(HTML(".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar {background: #79B791}")),
                                tags$style(HTML(".js-irs-5 .irs-single, .js-irs-5 .irs-bar-edge, .js-irs-5 .irs-bar {background: #79B791}")),

                                sliderInput("abun_A","Individuals",min=0,max=300,round=TRUE,value=50,width=200,ticks=FALSE),
                                sliderInput("eff_A","Sampling Effort",min=0,max=200,round=TRUE,value=50,width=200,ticks=FALSE,post=" samples")),
                         column(width=6,
                                sliderInput("abun_B","Individuals",min=0,max=300,round=TRUE,value=50,width=200,ticks=FALSE),
                                sliderInput("eff_B","Sampling Effort",min=0,max=200,round=TRUE,value=50,width=200,ticks=FALSE,post=" samples")),
                         p("Or, upload a .'csv' file that contains a sample x species matrix of species abundances for 2 communities. The first column of the data file must
                           be the grouping variable to indicate the 2 communities."),
                         fileInput("getcsv","Upload data",multiple=FALSE,accept=c("text/csv","text/comma-separated-values,text/plain",".csv"))
                     ),
                     box(title=tags$b("Evenness"),width=NULL,collapsible = TRUE,
                         p("Species evenness, or the relative abundance of species, affects the number of species observed via sampling.
                           If all species are present in the community in equal proportion, the likelihood of observing each is similar and thus more species
                           may be detected with lower sampling effort. However, if species abundances are highly uneven (a few common species and many rare ones, for example) 
                           most individuals observed will be of common species rather than rare ones which may go undetected. Use the sliders to see how species evenness
                           implicates the species accumulation curve."),
                         column(width=6,
                                tags$style(HTML(".js-irs-6 .irs-single, .js-irs-6 .irs-bar-edge, .js-irs-6 .irs-bar {background: #CD950C}")),
                                tags$style(HTML(".js-irs-7 .irs-single, .js-irs-7 .irs-bar-edge, .js-irs-7 .irs-bar {background: #79B791}")),
                                sliderInput("evennessA","A:",min=0,max=1,value=1,ticks=FALSE,width=200)),
                         column(width=6,
                                sliderInput("evennessB","B:",min=0,max=1,value=1,ticks=FALSE,width=200)),
                         h5(tags$b("(0 = uneven, 1 = all species equally common)"),align="center")
                     )##mouse over sampling effort that defines it- can be the number of smaples/observations to the amount of area surveyed or total time.
                     #depending on the taxa of interest, the degree of sampling effort and methodlogy will vary
                         ),
              column(width=8,
                     box(title = tags$b("Species Accumulation Curve"),width=NULL,
                         plotOutput("rarefaction"),
                         p(""),
                         radioButtons("xvar","Change x-axis:",choices=c("Individual-based rarefaction curve"="ind","Sample-based rarefaciton curve"="site"),selected="ind")
                     ),
                     column(width=6,
                            box(title=tags$b("Estimating total species richness"),width=NULL, collapsible=TRUE,
                                p("The amount of sampling within a community heavily influences the number of species and individuals detected. Further, some species
                           may go undetected or be very rare. Species accumulation curves allow us to assess the adequacy of sampling, and make estimates of 
                           overall species richness in the face of these challenges."),br(),
                                p("Once the majority of species have been observed, the curve tapers to a flat line (or asymptote), which suggests the overall species richness.
                           Using this concept, the species accumulation curve can be used estimate the overall richness of the community, including species not observed.
                           Various asymptotic estimators have been developed for this purpose."),
                                p("Check the box below to visualize 'Chao2' estimated richness"),
                                checkboxInput("chao2","Show estimated species richness (Chao +/- SE)", value=FALSE)
                            )),
                     column(width=6,
                            box(title=tags$b("Comparing species richness?"), width=NULL,collapsible = TRUE,
                                p("Richness is inherently sample-size dependent, meaning that a site with a greater number of individuals or sampling effort, has a greater likelihood of including more species.
                           This makes it difficult to compare species richness among sites that differ in abundance of effort."),br(),
                                p(tags$b("Interpolation vs Extrapolation:"), "When the communities differ in the number of individuals or sampling effort, comparison is made at a constant value.  
                           Interpolation compares the communities at the total number of individuals of the smaller community.  Extrapolation projects richness 
                           at a value greater than the smallest level."),
                                radioButtons("compare","Compare richness:",choices=c("None selected","Interpolate","Extrapolate"),selected="None selected",inline=TRUE)
                            ))
              )
             
            )
    ),
    tabItem(tabName="lit",
            fluidRow(
              column(width=6,
                     box(title=tags$b("Literature"),width=NULL,
                         p("Gotelli, N.J., Colwell, R.K. 2010. Biological diversity: frontiers in measurement and assessment. A. E. Magurran and B. J. McGill, eds. pages 39-54"),
                         a("See pdf", href="http://www.uvm.edu/~ngotelli/manuscriptpdfs/Chapter%204.pdf",target="_blank"),
                         p("Gotelli, N.J., and Colwell. R.K. 2001. Quantifying biodiversity: procedures and pitfalls in the 
                           measurement and comparison of species richness. Ecology. 4:379-391."),
                         a("See paper",href="http://onlinelibrary.wiley.com/doi/10.1046/j.1461-0248.2001.00230.x/full",target="_blank"),
                         p("Magurran, A. E. 1988. Ecological Diversity and its Measurement. Princeton University Press, Princeton, NJ.")),
                     box(title=tags$b("This app"), solidHeader=TRUE,status="warning",width=NULL,
                         p("This app was made by",a("Colleen Nell",href="www.collnell.com",target="_blank"),"with support from the", a("UCI Data Science Initiative summer fellowship (2016)",
                                                                                                                                       href="http://datascience.uci.edu/2016/06/27/2016-data-science-summer-fellow/",target="_blank")),
                         p("It is one part to a series of applications on", tags$a("Measuring and comparing ecological communities",href="www.bionerdz.com",target="_blank"), "using R."),
                         p(tags$b("Get R script for these analyses:")),br(),
                         a("Comparing Species Richness",href="https://raw.githubusercontent.com/biRdandfRog/SpeciesRichness/master/SR_code.R",target="_blank"),br(),
                         p("What is a",tags$a("site x species community matrix?",href="https://collnell.shinyapps.io/diversity/",target="_blank")),br(),
                         p("If you are new to R, check out this",a("Intro to R cookbook for Ecologists",href="http://rpubs.com/mooneyk/213152",target="_blank")),
                         a("See code for application",href="https://github.com/biRdandfRog/SpeciesRichness",target="_blank"))),
              column(width=6,
                  
                     box(title=tags$b("R packages"),width=NULL,
                         p("Several R packages have been developed for comparing, extrapolating, and estimating species richness.
                           The following were used in the development of this app."),
                         h4("vegan"),
                         p("Ordination methods, diversity analysis and other functions for community and vegetation ecologists."),
                         a("Documentation",href="https://cran.r-project.org/web/packages/vegan/vegan.pdf",target="_blank"),
                         h4("iNEXT"),
                         p("Interpolation and Extrapolation for Species Diversity"),
                         a("Documentation",href="https://cran.r-project.org/web/packages/iNEXT/iNEXT.pdf",target="_blank"),br(),
                         a("Intro Guide to iNEXT",href="https://cran.r-project.org/web/packages/iNEXT/vignettes/Introduction.html",target="_blank"),
                         h4("EcoSimR"),
                         p("Gotelli, N.J., Hart, E., and A.M. Ellison. 2015. EcoSimR. Version 0.1.0. http://www.uvm.edu/~ngotelli/EcoSim/EcoSim.html"),
                         p("Used in this app to generate null community matrices"),
                         a("Website",href="http://ecosimr.org/",target="_blank"),br(),
                         a("Documentation",href="https://cran.r-project.org/web/packages/EcoSimR/EcoSimR.pdf",target="_blank"),br(),
                         h4("rich"),
                         p("An R package to analyze species richness"),
                         a("Introduction to rich",href=""))
                     )
            )
    ),
    tabItem(tabName="exercises",
            column(width=6,
                   box(title=tags$b("Using the app to understand species richness"),width=NULL,
                       h5(tags$b("1. Set individuals to 50 and sampling effort to 50 for both communities. Set Community A species richness to 25, and Community B 
                         to 60. Press 'Rarefy!'.") ),
                       p("What does the species accumulation curve look like?"),
                       p("What is the estimated species richness (chao) for each community?"),
                       p("Was sampling sufficient in each community?"),
                       p("Are the estimated values for species richness the same as we set in the community metrics? Why or why not?"),
                       p("Are we able to conclude these communities differ in species richness based on our sampling?"),br(),
                       h5(tags$b("2. Change the number of individuals and sampling effort to 120 and press 'Rarefy!'.")),
                       p("How did the plot change?"),
                       p("What happened to the estimates of total species richness?"),
                       p("What happens to estimates of species richness as the number of individuals and/or sampling effort increases?"),br(),
                       h5(tags$b("3. Now change the richness for both communities to 100 and press 'Rarefy!'.")),
                       p("What changed? How are the diversity estimates affected?"),
                       p("What could be changed to improve the accuracy of the diversity estaimtes when there is higher species richness?"),br(),
                       h5(tags$b("4. Look at the 'Evenness' box. Set evenness to 0 for Community A.")),
                       p("What happens to the species accumulation curve when evenness is set to 0? How does this compare to an evenness of 1?"),
                       p("What is community evenness? What does an evenness of 1 mean?"),
                       p("How does evenness affect estimates of species richness?"),br(),
                       h5(tags$b("5. Go to the 'Example' tab")),
                       p("Which taxa is species richness being measured for? What is the sampling unit for this data? What are the treatments being compared?"),
                       p("Why is a species accumulation curve appropriate to compare the momoculture and polyculture abundance data?"),
                       p("Why might it be predicted that there is higher species richness of birds in polyculture plots?"),
                       p("What is the estimated species richness for each tree diversity?"),
                       p("Do you think this is enough to conclude that species richness differs between tree monocultures and polycultures?"),
                       p("What do you conclude from the data shown? Why?"))),
            column(width=6,
                   box(title=tags$b("Questions"),width=NULL,
                       p("What is a site x species abundance matrix?"),
                       p("What is the difference between species richness and abundance?"),
                       p("Describe one way that species richness is measured or sampled."),
                       p("How does abundance affect measured species richness?"),
                       p("How does sampling effort affect measured species richness?"),
                       p("What is evenness? Describe an 'even community'."),
                       p(" ")),
                   box(title=tags$b("R-based"),width=NULL,
                       p("The following questions pertain to using the 'BCI' dataset. To access this dataset run 'data(BCI)' in your R console.
                         To answer the questions use the R script provided on the 'Learn More' tab under 'This app' to look at the data in R. Read the ",
                         a("documentation",href="http://cc.oulu.fi/~jarioksa/softhelp/vegan/html/BCI.html",target="_blank"), "on the BCI data set to learn more"),br(),br(),
                       p(""),
                       p("How many species and how many sites are in the BCI abundance matrix?"),
                       p("What are the species being measured in this data set?"),
                       p("What is the total observed species richness?"),
                       p("What is the total observed abundance?"),
                       p("Describe the species accumulation curve look like for BCI. Do you think there was adequate sampling?"),
                       p("What is the estimated overall species richness of BCI, including species not observed?"),
                       p("How similar is the estimated species richness to the overall species richness?"),
                       p(" ")
                       ))),
    tabItem(tabName="example",
            fluidRow(
              column(width=5,
                     box(title=tags$b("Experimental Design"),width=NULL,
                     p("The data shown are from bird surveys conducted in the Yucatan peninsula of Mexico. A series of 15-minute surveys were conducted, in which the
                       the number of individual birds and the species observed were recorded. Surveys were conducted in plots of either 1 tree species 
                       (monoculture) or 4 tree species (polyculture). The goal of these surveys was to compare bird species richness between tree monocultures and polycultures."),br(),
                     h4(tags$b("Question:")), h4("Does tree diversity (monoculture vs polyculture) had an effect on the species richness 
                       of birds."),
                     h4(tags$b("Hypothesis:")), h4("Polyculture plots contain more species of birds than monoculture plots")
                     ), ##incldue plot images
                     box(title=tags$b("Estimate Bird Species Richness"),width=NULL,br(),br(),
                         p("This table shows several estimates (Chao, Jack1, Jack2 and Boot) for bird species richness (and standard error). These 
                           estimators represent different methods to project overall richness based on your sampling effort. Plotted values are the Chao estimate,"),
                         verbatimTextOutput("richests"),br(),
                         checkboxInput("chao","Plot estimated species richness (+/- SE)", value=FALSE)
                     )
            ),
              column(width=7,
                     box(title=tags$b("Bird species richness in tree monoculture and polycultures"),width=NULL,
                         plotOutput("divplots"),br(),
                         radioButtons("xvar_ex","Change x-axis:",choices=c("Individual-based rarefaction curve"="ind","Sample-based rarefaciton curve"="site"),selected="ind")
                         )
                    
                     )
            )
    )
    
    
  )
)
# Put them together into a dashboardPage
dashboardPage(skin = "purple",
              dashboardHeader(title = "Measuring Species Richness", titleWidth=350),
              sidebar,
              body
)