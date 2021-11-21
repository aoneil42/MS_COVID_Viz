#app.R
#load libraries
library(shiny)
library(xts)
library(TTR)
library(jsonlite)

#Selected States
selstates <- c("Colorado","Texas","Missouri")

#Function to return data for individual states, aggregated
Stateagg <- function(State,Death = FALSE,pop = 0) {
    if (Death == FALSE) {
        url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
        df = read.csv(url, check.names=FALSE)
        df = df[df$Province_State == State,]
        df = df[df$Lat > 0,]
        agg = t(aggregate(. ~ Province_State, df, sum)[-c(2,3,4,5,6,7,8,9,10,11)])
    } else {
        url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
        df = read.csv(url, check.names=FALSE)
        df = df[df$Province_State == State,]
        df = df[df$Lat > 0,]
        agg = t(aggregate(. ~ Province_State, df, sum)[-c(2,3,4,5,6,7,8,9,10,11,12)])
    }
    agg.df = as.data.frame(agg[-1,])
    colnames(agg.df) = agg[1,]
    agg.df$Date = as.Date(rownames(agg.df), format = "%m/%e/%y")
    agg.df = agg.df[,c(ncol(agg.df),1:(ncol(agg.df)-1))]
    if (pop == 0) {
        agg.df[,2] = as.numeric(as.character(agg.df[,2]))
        agg.xts = xts(agg.df[,2], order.by=agg.df[,1])
        if (Death == FALSE) {
            names(agg.xts) <- "Cases"
        } else {
            names(agg.xts) <- "Deaths"
        }
    } else {
        agg.df[,2] = as.numeric(as.character(agg.df[,2]))
        agg.df[,2] <- (agg.df[,2]/pop)*100000
        agg.xts = xts(agg.df[,2], order.by=agg.df[,1])
        if (Death == FALSE) {
            names(agg.xts) <- "Cases_per_100k"
        } else {
            names(agg.xts) <- "Deaths_per_100k"
        }
    }    
    
    
    #agg.diff = diff(agg.xts)
    #agg.diff.ma = SMA(agg.diff,l)
    #results = list("df" = agg.df, "xts" = agg.xts, "diffs" = agg.diff, "sma" = agg.diff.ma)
    return(agg.xts)
}


#Function to return county level data for individual states
State <- function(State,Death = FALSE) {
    if (Death == FALSE) {
        url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
    } else {
        url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
    }
    df = read.csv(url, check.names=FALSE)
    df = df[df$Province_State == State,]
    df = df[df$Lat > 0,]
    df.t = t(df[-c(1,2,3,4,5,6,7,8,9,10,11)])
    df.df = as.data.frame(df.t[-1,])
    colnames(df.df) = df$Admin2
    df.df$Date = as.Date(rownames(df.df), format = "%m/%e/%y")
    df.df = df.df[,c(ncol(df.df),1:(ncol(df.df)-1))]
    #df.df[,c(-1)] = as.numeric(as.character(df.df[,c(-1)]))
    #df.xts = xts(df.df[,-1], order.by=df.df[,1])
    #df.diff = diff(df.xts)
    #df.diff.ma = SMA(df.diff,l)
    #results = list("df" = df.df, "xts" = df.xts, "diffs" = df.diff, "sma" = df.diff.ma)
    return(df.df)
}

#Function to return county xts
County <- function(State,County,Death = FALSE, pop = 0) {
    sdf <- State(State, Death)
    if (pop == 0) {
        cdf <- sdf[,c("Date",County)]
        cdf.xts = xts(cdf[,2], order.by=cdf[,1])
        if (Death == FALSE) {
            names(cdf.xts) <- "Cases"
        } else {
            names(cdf.xts) <- "Deaths"
        }
    } else {
        cdf <- sdf[,c("Date",County)]
        cdf[,2] <- (cdf[,2]/pop)*100000
        cdf.xts = xts(cdf[,2], order.by=cdf[,1])
        if (Death == FALSE) {
            names(cdf.xts) <- "Cases_per_100k"
        } else {
            names(cdf.xts) <- "Deaths_per_100k"
        }
    }
    
    
    return(cdf.xts)
}

#function to calculate change difference
diffcalc <- function(df.xts) {
    diff = diff(df.xts)
    return(diff)
}

#function to calculate moving average
smacalc <- function(diff,l=7) {
    diff.ma = SMA(diff,l)
    return(diff.ma)
}

#Funtion to return df from xts
itemtodf <- function(x) {
    df = as.data.frame(x)
    df <- cbind(Date = rownames(df), df)
    rownames(df) <- 1:nrow(df)
    return(df)
}

#Loop to pull most recent summary data
for (i in 0:30) {
    USSUM <- tryCatch({
        date = as.character(format(as.Date(Sys.Date()-i), format="%m-%d-%Y"))
        url = paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/",
                     date,".csv")
        read.csv(url, check.names=FALSE)
    }, warning = function(w) {w})
    if(inherits(USSUM, "warning")) next else break
}

USSUM.sel <- USSUM[USSUM$Province_State %in% selstates,]

#Function to make 7 and 14 day MAs for graphing
graphfunc <- function(x) {
    graphdf <- na.omit(cbind(x, SMA(x,7),SMA(x,14)))
    colnames(graphdf) <- c("Count","7DayMA","14DayMA")
    return(graphdf)
}



ui <- navbarPage(theme="bootstrap.css",
                 "COVID-19 Dashboard",
                 tabPanel("Map and Charts",
                          fluidRow(
                              column(6,
                                        # put your tags here for Leaflet
                                        tags$head(tags$link(rel = "stylesheet", type = "text/css", href =
                                        "https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.3.1/leaflet.css")),
                    
                                        tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.3.1/leaflet.js"),
                                        tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/leaflet-ajax/2.1.0/leaflet.ajax.min.js"),
                                        # create the div element as a placeholder for your map
                                        tags$div(id="my_map",style="width: 600px; height: 600px;"),
                    
                                        # load your javascript code
                                        tags$script(src="my_map_script.js"),
                                     h4("Click on state or county for time series graph")
                                       ),#end column
                              column(6,
                                     tabsetPanel(
                                         tabPanel("Totals", fluid = TRUE,
                                        # load plotly.js from cdnjs
                                        tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/plotly.js/1.54.5/plotly.min.js"),
                                        tags$div(id="stplotdiv", style="height:600px;width:600px;"),
                                        # load our javascript code 
                                        tags$script(src="my_plotly.js")
                                                 ), #end totals panel
                                         tabPanel("Rate of Change", fluid = TRUE,
                                                  tabsetPanel(
                                                      tabPanel("Cases", fluid = TRUE,
                                                     # load plotly.js from cdnjs
                                                     tags$script(src=
                                                         "https://cdnjs.cloudflare.com/ajax/libs/plotly.js/1.54.5/plotly.min.js"),
                                                     tags$div(id="diffplotdiv",
                                                              style="height:600px;width:600px;"),
                                                     # load our javascript code 
                                                     tags$script(src="my_plotly.js")
                                                              ), #end cases panel
                                                      
                                                      tabPanel("Deaths", fluid = TRUE,
                                                     # load plotly.js from cdnjs
                                                     tags$script(src=
                                                         "https://cdnjs.cloudflare.com/ajax/libs/plotly.js/1.54.5/plotly.min.js"),
                                                     tags$div(id="diffplotdivd",
                                                              style="height:600px;width:600px;"),
                                                     # load our javascript code 
                                                     tags$script(src="my_plotly.js")
                                                              ) #end deaths panel
                                                )#end tabset panel
                                            )#end ROC Panel
                                         )#end tabset panel
                               )#end column
                           ),#end fluidrow
                          "Author: Austin Stevens; Source: John's Hopkins, US Census"
                          ),#end maps and charts tab
                 tabPanel("CO/MO/TX Comparison",
                    headerPanel("Comparison of Colorado, Missouri, and Texas"),
                    fluidRow(
                        column(6,
                               mainPanel(
                                        
                                        tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/canvasjs/1.7.0/canvasjs.js"),
                                        tags$div(id="my_chart",style="width: 500px; height: 500px"),
                                        #tags$div(id="my_chart2",style="width: 500px; height: 300px"),
                                        tags$script(src="my_chart_script.js")
                                        )
                               ),
                        column(6,
                               mainPanel(
                                        
                                        tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/canvasjs/1.7.0/canvasjs.js"),
                                        #tags$div(id="my_chart",style="width: 500px; height: 300px"),
                                        tags$div(id="my_chart2",style="width: 500px; height: 500px"),
                                        tags$script(src="my_chart_script.js")
                                        )
                               )
                        ), #end fluidRow
                    fluidRow(
                        column(6,
                               mainPanel(
                                   # load plotly.js from cdnjs
                                   tags$script(src=
                                               "https://cdnjs.cloudflare.com/ajax/libs/plotly.js/1.54.5/plotly.min.js"),
                                   tags$div(id="diffplotdivcomptot",
                                            style="height:600px;width:600px;"),
                                   # load our javascript code 
                                   tags$script(src="my_plotly.js")
                                   )
                               ),
                        column(6,
                               mainPanel(
                                   # load plotly.js from cdnjs
                                   tags$script(src=
                                               "https://cdnjs.cloudflare.com/ajax/libs/plotly.js/1.54.5/plotly.min.js"),
                                   tags$div(id="diffplotdivcomptotroc",
                                            style="height:600px;width:600px;"),
                                   # load our javascript code 
                                   tags$script(src="my_plotly.js")
                                   )
                               )
                        ),#end fluidRow
                    fluidRow(
                        column(6,
                               mainPanel(
                                   # load plotly.js from cdnjs
                                   tags$script(src=
                                               "https://cdnjs.cloudflare.com/ajax/libs/plotly.js/1.54.5/plotly.min.js"),
                                   tags$div(id="diffplotdivcomptotd",
                                            style="height:600px;width:600px;"),
                                   # load our javascript code 
                                   tags$script(src="my_plotly.js")
                                   )
                               ),
                        column(6,
                               mainPanel(
                                   # load plotly.js from cdnjs
                                   tags$script(src=
                                               "https://cdnjs.cloudflare.com/ajax/libs/plotly.js/1.54.5/plotly.min.js"),
                                   tags$div(id="diffplotdivcomptotrocd",
                                            style="height:600px;width:600px;"),
                                   # load our javascript code 
                                   tags$script(src="my_plotly.js")
                                   ) #end main
                               ) # end column
                        ), #end fluidRow
                         "Author: Austin Stevens; Source: John's Hopkins, US Census" 
                       )#end tabpanel
                 
                 ) #end navbarpage



server <- function(input, output, session) {
    jlist <- list()
    USSUM.sel.sub <- USSUM.sel[-c(2,3,4,5,8,10,13,15,16,18)]
    USSUM.sel.sub$Province_State <- as.character(USSUM.sel.sub$Province_State)
    jlist[[1]] <- c("Province_State",USSUM.sel.sub$Province_State)
    for (i in 2:length(colnames(USSUM.sel.sub))) {
        item = colnames(USSUM.sel.sub)[i]
        jlist[[i]] <- c(item,as.numeric(USSUM.sel.sub[,item]))
    }
    jdf <- toJSON(jlist)
    bar_msg <- toString(jdf)
    session$sendCustomMessage(type="sum_msg",bar_msg)
    session$sendCustomMessage(type="sum_all",toString(toJSON(USSUM)))
    co = Stateagg("Colorado")
    mo = Stateagg("Missouri")
    tx = Stateagg("Texas")
    stcases <- cbind(co,mo,tx)
    names(stcases) <- c("Colorado","Missouri","Texas")
    stcases.js = toString(toJSON(itemtodf(stcases)))
    session$sendCustomMessage(type="stcases",stcases.js)
    cod = Stateagg("Colorado", T)
    mod = Stateagg("Missouri", T)
    txd = Stateagg("Texas", T)
    stdeaths <- cbind(cod,mod,txd)
    names(stdeaths) <- c("Colorado","Missouri","Texas")
    stdeaths.js = toString(toJSON(itemtodf(stdeaths)))
    session$sendCustomMessage(type="stdeaths",stdeaths.js)
    stcasdiff = diffcalc(stcases)
    stcasdiffsma7 = as.data.frame(lapply(stcasdiff,function(x){SMA(x,7)}))
    names(stcasdiffsma7) <- c("Colorado","Missouri","Texas")
    stcasdiff.js = toString(toJSON(itemtodf(stcasdiffsma7)))
    session$sendCustomMessage(type="stcasdiff",stcasdiff.js)
    stdeaddiff = diffcalc(stdeaths)
    stdeaddiffsma7 = as.data.frame(lapply(stdeaddiff,function(x){SMA(x,7)}))
    names(stdeaddiffsma7) <- c("Colorado","Missouri","Texas")
    stdeaddiff.js = toString(toJSON(itemtodf(stdeaddiffsma7)))
    session$sendCustomMessage(type="stdeaddiff",stdeaddiff.js)
    observeEvent(input$clicked_st,
                 {
                     clicked_NAME = isolate(input$clicked_st[1])
                     pop_ST = as.numeric(isolate(input$clicked_st[2]))
                     # find the row in the data frame
                     #dfrow = paste("Station ID: ",clicked_ID)
                     #output$txt <- renderText({ clicked_NAME })
                     session$sendCustomMessage(type="st_NAME",clicked_NAME)
                     st = Stateagg(clicked_NAME)
                     std = Stateagg(clicked_NAME, T)
                     stpop = Stateagg(clicked_NAME, F, pop_ST)
                     stdpop = Stateagg(clicked_NAME, T, pop_ST)
                     stcd = cbind(st, std, stpop, stdpop)
                     st.js = toString(toJSON(itemtodf(stcd)))
                     session$sendCustomMessage(type="st_msg",st.js)
                     st_diff = diffcalc(st)
                     st_diff.js = toString(toJSON(itemtodf(graphfunc(st_diff))))
                     session$sendCustomMessage(type="st_diff_msg",st_diff.js)
                     st_diff.d = diffcalc(std)
                     st_diff.d.js = toString(toJSON(itemtodf(graphfunc(st_diff.d))))
                     session$sendCustomMessage(type="st_diff_d_msg",st_diff.d.js)
                     
                     
                 })
    observeEvent(input$clicked_co,
                 {
                     clicked_CO = isolate(input$clicked_co[1])
                     clicked_ST = isolate(input$clicked_co[2])
                     pop_CO = as.numeric(isolate(input$clicked_co[3]))
                     NAME = paste0(clicked_CO," County, ",clicked_ST)
                     # find the row in the data frame
                     #dfrow = paste("Station ID: ",clicked_ID)
                     #output$txt <- renderText({ clicked_CO })
                     #output$txt2 <- renderText({ clicked_ST })
                     session$sendCustomMessage(type="st_NAME",NAME)
                     stcnty = County(clicked_ST,clicked_CO)
                     stcntyd = County(clicked_ST,clicked_CO,T)
                     stcntyp = County(clicked_ST,clicked_CO,F,pop_CO)
                     stcntydp = County(clicked_ST,clicked_CO,T,pop_CO)
                     stcntym = cbind(stcnty,stcntyd,stcntyp,stcntydp)
                     #names(stcntym) = c("Cases", "Deaths")
                     stcn.js = toString(toJSON(itemtodf(stcntym)))
                     session$sendCustomMessage(type="st_msg",stcn.js)
                     stcn_diff = diffcalc(stcnty)
                     stcn_diff.js = toString(toJSON(itemtodf(graphfunc(stcn_diff))))
                     session$sendCustomMessage(type="st_diff_msg",stcn_diff.js)
                     stcn_diff.d = diffcalc(stcntyd)
                     stcn_diff.d.js = toString(toJSON(itemtodf(graphfunc(stcn_diff.d))))
                     session$sendCustomMessage(type="st_diff_d_msg",stcn_diff.d.js)
                     
                     
                 })
  
}

shinyApp(ui=ui, server=server)