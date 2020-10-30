library(shiny)
library(gstat)
library(sp)
library(sf)
library(ggplot2)
library(rasterVis)
library(RColorBrewer)
library(rgdal)
library(raster)
library(rgeos)
library(ggspatial)
library(scales)
library(grDevices)
library(colorRamps)
library(tidyverse)
library(shinyBS)


#SETUP
proj4proj <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +ellps=sphere +units=m +no_defs"
proj1 <- "+proj=longlat +datum=NAD83 +no_defs"

load("data/allsinfo.rda")


newmap <- readOGR('data/cbmap/cb_2018_us_state_20m.shp')
contUS = newmap[newmap$STATEFP < 60 & newmap$NAME != "Alaska" & newmap$NAME != "Hawaii", ]

transUS <- contUS
transUS <- spTransform(transUS, crs(proj4proj))
outlineUS <- aggregate(transUS)

urjudgetxt <- "\nyour rating"
questionsn <- 15

alignCenter <- function(el) {
  htmltools::tagAppendAttributes(el,
                                 style="margin-left:auto;margin-right:auto;"
  )
}

ui <- fluidPage(title = "Personal Dialect Quiz",
  tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
  br(),
  br(),
  titlePanel(
    h2(strong("Alls we know about your personal dialect!"), align = "center", style = "font-size:29px; font-family:Arial")
  ),
  column(id = "introtextcol", width = 8, offset = 2,
         div(id = "introtext", 
          titlePanel(
           h5("What does your grammar say about where you're from? Rate whether you accept the following sentences from 1 to 5 to see your personal dialect map:", align = "left"),
            )
          )
         ),
  column(id = "qcol", width = 8, offset = 2, style = "background-color:#F6F7EB; border: 1px solid #9C9583; border-radius:8px",
        h5(textOutput("qnum"), align = "center", style = "color: gray, font-size: 10px; font-weight: 400;"),
         titlePanel(h4(textOutput("sentence"), align = "left")),
         splitLayout(id = "btns", align = 'center', cellWidths = c('48.25%','48.25%'),
                     h5("Unacceptable", align = 'left', style = 'font-size:12px; font-weight: 400;'),
                     h5("Acceptable", align = "right", style = 'font-size:12px; font-weight: 400;')
         ),
         splitLayout(id = "btns", cellWidths = c('19%','19%','19%','19%','19%'),
           bsButton(inputId = "c1", block = T,
                        HTML("1")),
           bsButton(inputId = "c2", block = T,
                        HTML("2")),
           bsButton(inputId = "c3", block = T,
                        HTML("3")),
           bsButton(inputId = "c4", block = T,
                        HTML("4")),
           bsButton(inputId = "c5", block = T,
                        HTML("5")),
           align = "center"
         ),
         br(),
         fluidRow(align = "center",
                  bsButton(inputId = "gnext", label = "Next", disabled = T),
         ),
         br()
  ),
  br(),
  column(id = "prevQ", width = 8, offset = 2,
    textOutput("oldsentence"),
      plotOutput("sentenceMap", width = "100%", height = "57%"),
    br(),
    alignCenter(div(style = "max-width: 80vw; width: 469px; height: 88px;",
      plotOutput("legend"),
    )),
    br(),
    htmlOutput("moreinfo"),
    br(),
    br(),
    br(),
    br(),
  ),
  column(width = 8, offset = 2,
         hr(),
         h5("This quiz and its maps are built upon data from the Yale Grammatical 
    Diversity Project’s database of over 250,000 ratings for over 180 sentences,
    the vast majority of which are included in this quiz. The surveys were 
    conducted on Amazon Mechanical Turk between 2015 and 2019.", 
            br(), br(), 
            HTML(paste0('The YGDP was founded by Raffaella Zanuttini in 2010 to research syntactic 
    variation in American English, which remains severely understudied. More 
    details about the project may be found at ', a(href = 'https://ygdp.yale.edu', 'ygdp.yale.edu'),'.')),
            br(),br(),
            HTML(paste0("I would like to extend thanks to Kaija Gahm and Jim Wood for their 
            support and advice in making this quiz, as well as the NYTimes
            for the ", a(href="https://www.nytimes.com/interactive/2014/upshot/dialect-quiz-map.html",
                   "inspiration", target="_blank"), ".")),
            br(), br(), 
            "Cheers,",br(),
            "Ian Neidel",br(),br(),br())
         )
)

server <- function(input, output, session) {
  myallsinfo <- allsinfo
  allconstr <- unique(sapply(allsinfo$constructionName, function(x) (strsplit(x, " -"))[[1]][1]))
  nc <- sample(allconstr, 1)
  allconstr <<- allconstr[allconstr != nc]
  currSid <- sample((filter(allsinfo, stringr::str_detect(constructionName, nc)))$sentenceID, 1)
  curridw <- raster(paste0("data/prerenderedMaps/r",currSid,".grd"))
  prevctxt <- ""
  currsTxt <- filter(allsinfo, sentenceID == currSid)$sentenceText
  dfrpr <- "ToBeFilled"
  
  personalRaster <- "TBA"
  rv <- reactiveValues(num = 3)
  rv <- reactiveValues(ggplt = 0)
  output$sentence <- renderText({paste0('"',currsTxt,'"')})
  output$oldsentence <- renderText({prevctxt})
  output$moreinfo <- renderText({""})
  output$qnum <- renderText({paste0("Question 1 of ",questionsn)})

  observeEvent(input$c1, {rv$num <- 1})
  observeEvent(input$c2, {rv$num <- 2})
  observeEvent(input$c3, {rv$num <- 3})
  observeEvent(input$c4, {rv$num <- 4})
  observeEvent(input$c5, {rv$num <- 5})
  observeEvent(input$c1 | input$c2 | input$c3 | input$c4 | input$c5, {
    if(!is.null(rv$num)){
      print("ggplotting")
      rv$ggplt <- list(src = paste0("data/prerenderedMaps/F",currSid,"c",rv$num,".svg"))
      updateButton(session, inputId = "gnext", disabled = F)
      updateButton(session, inputId = "c1", style = "default")
      updateButton(session, inputId = "c2", style = "default")
      updateButton(session, inputId = "c3", style = "default")
      updateButton(session, inputId = "c4", style = "default")
      updateButton(session, inputId = "c5", style = "default")
      updateButton(session, inputId = paste0("c",rv$num), style = "btn btn-primary")
    }
    
  })
  observeEvent(input$gnext, {
    output$legend <- renderImage({list(src = paste0("data/legends/l",isolate({rv$num}),".png"), width = "100%")})
    output$sentenceMap <- renderImage({
      if (!is.null(input$gnext)){
        print("redoing map")
        isolate({rv$ggplt})
      }
    })
    if (rv$num==4){
      curridw[curridw>4] <- curridw[curridw>4]/2+2
    }else if (rv$num==2){
      curridw[curridw<2] <- curridw[curridw<2]/2+1
    }
    
    if (input$gnext == 1){
      insertUI(
        selector = "#oldsentence",
        where = "beforeBegin",
        ui = h5("How your rating of the previous question compared to people across the country:", align = "left")
      )
      personalRaster <<- (curridw-rv$num)^2
    }else{
      personalRaster <<- personalRaster+(curridw-rv$num)^2
    }
    if (!is.null(input$gnext)){
      updateButton(session, inputId = "gnext", disabled = T)
      updateButton(session, inputId = "c1", style = "default")
      updateButton(session, inputId = "c2", style = "default")
      updateButton(session, inputId = "c3", style = "default")
      updateButton(session, inputId = "c4", style = "default")
      updateButton(session, inputId = "c5", style = "default")
    }
    if (input$gnext == questionsn){
      insertUI(selector = "#gnext",
               where = "afterEnd",
               ui = actionButton(inputId = "gfin", label = "Finish"))
      output$qnum <- renderText({paste0("Question ",  (input$gnext+1))})
    }else if (input$gnext > questionsn){
      output$qnum <- renderText({paste0("Question ",  (input$gnext+1))})
    }else if (!is.null(input$gnext)){
      output$qnum <- renderText({paste0("Question ",  (input$gnext+1), " of ",questionsn)})
    }
    if (length(allconstr) == 0){
      allconstr <<- sapply(myallsinfo$constructionName, function(x) (strsplit(x, " -"))[[1]][1])
    }
    newconstr <- sample(allconstr, 1)
    allconstr <<- allconstr[allconstr != newconstr]
    newS <- sample((filter(myallsinfo, stringr::str_detect(constructionName, newconstr)))$sentenceID, 1)
    print(newS)
    
    output$sentence <- renderText({paste0('"',currsTxt,'"')})
    output$oldsentence <- renderText({paste0('"',prevctxt,'"')})
    cUrl <- (allsinfo %>% filter(sentenceID == currSid))$pageYGDP
    cName <- strsplit((allsinfo %>% filter(sentenceID == currSid))$constructionName, " -")[[1]][1]
    if (cUrl != "NA"){
      output$moreinfo <- renderText({paste0("<p>Find out more about the <a href=",cUrl, ' target="_blank"','>"', cName,'" construction</a>!</p>')})
    }else{
      output$moreinfo <- renderText({""})
    }
    prevctxt <<- currsTxt
    currSid <<- newS
    print(currsTxt)
    currsTxt <<- filter(allsinfo, sentenceID == newS)$sentenceText
    print(currsTxt)
    curridw <- raster(paste0("data/prerenderedMaps/r",currSid,".grd"))
    myallsinfo <<- filter(myallsinfo, sentenceID != newS)
    if (length(myallsinfo$sentenceID) == 0){
      removeUI(selector = "#gnext")
    }
  })
  observeEvent(input$gfin, {
    output$persMap <- renderPlot({
      negRaster <- calc(personalRaster, fun = rank)*-1
      rfocal = focal(negRaster, w=matrix(1,3,5), mean)
      rdis = disaggregate(rfocal, 14, method = "bilinear")
      
      maskedrdis <- mask(rdis, contUS)
      reprojrast <- projectRaster(maskedrdis, crs=proj4proj)
      
      ptsrpr <- rasterToPoints(reprojrast, spatial = TRUE)
      localdfrpr <- data.frame(ptsrpr)
      
      print(minValue(negRaster))
      print(maxValue(negRaster))
      
      finalOut <- ggplot() +
        geom_raster(data = localdfrpr, aes(x = x, y = y, fill = layer))+
        layer_spatial(data = transUS, fill = NA, color = "black", size = .3)+
        layer_spatial(data = outlineUS, fill = NA, color = "black", size = .8)+
        scale_fill_gradientn(colors = c("#2166ac","#2166ac","#4393c3","#92c5de","#d1e5f0","#f7f7f7","#f7f7f7","#fddbc7","#f4a582", "#d6604d","#d6604d"),
                             guide = guide_colorbar(title.hjust = 0.5, title.position = "top"), 
                             breaks = c(minValue(negRaster), maxValue(negRaster)),
                             labels=c("Least similar","Most similar"),
                             limits=c(minValue(negRaster)-0.1,maxValue(negRaster)+0.1))+
        theme(plot.background = element_rect(fill = "transparent"),
              panel.background = element_rect(fill = "transparent"),
              panel.grid.major = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.title.y=element_blank(),
              axis.title.x=element_blank(),
              legend.background = element_rect(fill = "transparent"),
              legend.position="bottom",
              legend.key.width = unit(1.3, "cm")
        )+
        labs(fill = "Similarity to you")
      finalOut
    }, bg="transparent")
    
    insertUI(selector = "#introtextcol", where = "afterEnd", 
             div(
          plotOutput("persMap", height = "62.5vw", width = "100vw") %>% 
      shinycssloaders::withSpinner(type = 8, color="#2166ac"), br(),br(),br(),br())
    )
    insertUI(selector = "#introtextcol", where = "beforeEnd", 
             div(
              h3("Your Map", align = "center")
             )
    )
    
    removeUI(selector = "#qcol")
    removeUI(selector = "#prevQ")
    removeUI(selector = "#btns")
    removeUI(selector = "#gfin")
    removeUI(selector = "#gnext")
    removeUI(selector = "#sentence")
    removeUI(selector = "#moreinfo")
    removeUI(selector = "#legend")
  })
}

shinyApp(ui = ui, server = server)