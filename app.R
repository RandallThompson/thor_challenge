#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

bartype <- as.data.frame(list(type=c("Mens", "Womens", "Training"), barweight=c(45,35,15), barlength=c(86.75, 79.13, 69.5), sleeveweight=c(11.25, 8.75, 3.75), sleevelength=c(18.4, 15, 9)))

platetype <- as.data.frame(list(size=c("None", "Metal 5", "Metal 10", "Metal 25", "Metal 45", "Bumper 10", "Bumper 15", "Bumper 25", "Bumper 45"), platelength=c(0, .75, 1, 1.25, 1.5, 1, 1.37, 2, 3.25), plateweight=c(0, 5, 10, 25, 45, 10, 15, 25, 45)))

# platetype3 <- merge(platetype, merge(platetype, platetype))
# colnames(platetype3) <- c("size1", "platelength1", "plateweight1", "size2", "platelength2", "plateweight2", "size3", "platelength3", "plateweight3")                    

hand <- as.data.frame(list(position=c("Bar", "Sleeve"), handlength=c(-5, 0)))

data <- merge(bartype, merge(platetype, hand)) #platetype3 to add all 3 plate inputs


library(shiny)
library(tidyverse)
library(png)

ui <- fluidPage(
    headerPanel("Calculating the Thor Challenge"),
    
    fluidPage( 
        fluidRow(column(3, textOutput('totalintro')),
        column(6,textOutput('texttotal'))),
    
    sidebarLayout(
        sidebarPanel( 
       verticalLayout(selectInput('bar', "Bar Type", bartype$type, selected = 'Mens'),
        selectInput('hand', 'Hand Position', hand$position, selected = 'Bar'),
        selectInput('plate1', 'Plate 1', platetype$size, selected = "None"),
              selectInput('plate2', 'Plate 2', platetype$size, selected = "None"),
              selectInput('plate3', 'Plate 3', platetype$size, selected = "None")
        )),
       mainPanel(
        fluidRow(
            column(6, imageOutput("image2")),
            column(2, imageOutput("image3")),
            column(2, imageOutput("image4")),
            column(2, imageOutput("image5"))
        ),
        fluidRow(imageOutput("image1"))
       )),
       
    p(strong("Howdy Partner! Looks like yer fixina lift some weights! 
             The first thing I want to draw your attention to is sleeve length. 
             You might expect that as the weight of the bar gets lighter, this exercise gets easier.
             Well, yes and no. Without plates, the Thor Challenge is easier with a lighter bar, BUT, 
             when you start adding plates, the shorter loadable sleeve length gives you less leverage and will 
             eventually make the exercise harder. Second, according to the Rogue Barbell website, the training bar shown here
             is not meant for forces of higher than 250 pounds. I see you thinking about it. Don't break your gym's one and only training bar.
             A number of assumptions and estimations went into this calculation. The bar and plate measurements were taken from the Rogue Barbell website. 
             The measure of sleeve length in the pictures above only show the Loadable Sleeve length. I added 2 inches for plate stoppers on each sleeve for our calculations.
             The sleeves on the Men's 20kg bar weigh 5kg each. I kept that ratio and made the assumption that each sleeve weighs 1/4 the total weight of the bar. 
             To calculate lever distance, I assumed the center of gravity was 1/2 the length. For hand position, I used the middle finger as the center of gravity
             and used a ruler to measure the distance between hand positions. Lastly, I hope you enjoyed this demo! I'm always looking for feedback so send an email with comments to RandyThompsonDC@gmail.com")),
        tableOutput('table'),
       
        
        fluidRow(column(3, textOutput('text1'),
        textOutput('text2'),
        textOutput('text3')),
        column(3, textOutput('text4'),
        textOutput('text5'),
        textOutput('text6')),
        column(3, textOutput('text7'),
        textOutput('text8'),
        textOutput('text9'))),
        
        fluidRow(column(3, textOutput('text10'),
        textOutput('text11'),
        textOutput('text12')),
        column(3, textOutput('text13'),
        textOutput('text14'),
        textOutput('text15')),
        column(3, textOutput('text16'),
        textOutput('text17'),
        textOutput('text18')),
        column(3, textOutput('text19'),
        textOutput('text20'),
        textOutput('text21'))),       
               
        tags$head(tags$style("#texttotal{color: red;
                                 font-size: 40px;
                                 font-style: italic;
                                 }
                             #totalintro{font-size: 17px;}
                             #text1{font-size: 17px; font-style: bold;}
                             #text4{font-size: 17px; font-style: bold;}
                             #text7{font-size: 17px; font-style: bold;}
                             #text10{font-size: 17px; font-style: bold;}
                             #text13{font-size: 17px; font-style: bold;}
                             #text16{font-size: 17px; font-style: bold;}
                             #text19{font-size: 17px; font-style: bold;}
                             #text3{color: red; font-size: 17px; font-style: bold;}
                             #text6{color: red; font-size: 17px; font-style: bold;}
                             #text9{color: red; font-size: 17px; font-style: bold;}
                             #text12{color: red;font-size: 17px; font-style: bold;}
                             #text15{color: red;font-size: 17px; font-style: bold;}
                             #text18{color: red;font-size: 17px; font-style: bold;}
                             #text21{color: red;font-size: 17px; font-style: bold;}"))
        #                      #text6{color: green;
        #                          font-size: 20px;
        #                          font-style: italic;
        #                          },
        #                      #text9{color: red;
        #                          font-size: 20px;
        #                          font-style: italic;
        #                          },
        #                      #text12{color: blue;
        #                          font-size: 20px;
        #                          font-style: italic;
        #                          },
        #                      #text15{color: blue;
        #                          font-size: 20px;
        #                          font-style: italic;
        #                          },
        #                      #text18{color: blue;
        #                          font-size: 20px;
        #                          font-style: italic;
        #                          },
        #                      #text21{color: red;
        #                          font-size: 20px;
        #                          font-style: italic;
        #                          }") )
    #),
    
    
))

server <- function(input, output, session) {
    
    total <- reactive({
        table <- data %>%
            filter(type == input$bar & position == input$hand & size == input$plate1) 
        
        if (input$plate2 != "None") {
            table <- cbind(table, filter(platetype, size == input$plate2))
            colnames(table) <- c("type"     ,    "barweight"  ,  "barlength"  ,  "sleeveweight", "sleevelength", "size" ,        "platelength" , "plateweight" , "position" ,    "handlength","size2", "platelength2", "plateweight2")
        }

        if (input$plate3 != "None") {
            table <- cbind(table, filter(platetype, size == input$plate3))
            colnames(table) <- c("type"     ,    "barweight"  ,  "barlength"  ,  "sleeveweight", "sleevelength", "size" ,        "platelength" , "plateweight" , "position" ,    "handlength","size2", "platelength2", "plateweight2",
            "size3", "platelength3", "plateweight3")
        }
        
    sleeve1 <- ((table$sleevelength + table$handlength) * .5 * table$sleeveweight) 
    bar1 <- .5 * table$barlength * table$barweight 
    sleeve2 <- (table$barlength - (table$sleevelength * .5)) * table$sleeveweight 
    plate1 <- (table$barlength - table$sleevelength + table$platelength) * table$plateweight  
    sleeveh <- (table$sleevelength - table$handlength)
    plate2 <- (table$barlength - table$sleevelength + table$platelength + table$platelength2) * table$plateweight2
    plate3 <- (table$barlength - table$sleevelength + table$platelength + table$platelength2 + table$platelength3) * table$plateweight3 
    
    total <- (sleeve1 + bar1 + sleeve2 + plate1)/sleeveh
        # (((table$sleevelength + table$handlength) * .5 * table$sleeveweight) +
        # .5 * table$barlength * table$barweight +
        # (table$barlength - (table$sleevelength * .5)) * table$sleeveweight +
        # (table$barlength - table$sleevelength + table$platelength) * table$plateweight) / 
        # (table$sleevelength - table$handlength)
        
        if (input$plate2 != "None") {
            total <- total + (plate2/sleeveh)
                #((table$barlength - table$sleevelength + table$platelength + table$platelength2) * table$plateweight2/ 
                 #                 (table$sleevelength - table$handlength))
            }
        
        
        if (input$plate3 != "None") {
            total <- total + (plate3/sleeveh)
                #((table$barlength - table$sleevelength + table$platelength + table$platelength2 + table$platelength3) * table$plateweight3/ 
                 #             (table$sleevelength - table$handlength))
            } 
    
        return(paste(round(total, 2), "lbs"))    
    })
    
    
    sleeve1 <- reactive({
        table <- bartext()
        ((table$sleevelength + table$handlength) * .5 * table$sleeveweight)}) 
    bar1 <- reactive({
        table <- bartext()
        .5 * table$barlength * table$barweight })
    sleeve2 <- reactive({
        table <- bartext()
        (table$barlength - (table$sleevelength * .5)) * table$sleeveweight })
    plate1 <- reactive({
        table <- bartext()
        (table$barlength - table$sleevelength + table$platelength) * table$plateweight  })
    sleeveh <- reactive({
        table <- bartext()
        (table$sleevelength - table$handlength)})
    plate2 <- reactive({
        table <- bartext()
        (table$barlength - table$sleevelength + table$platelength + table$platelength2) * table$plateweight2})
    plate3 <- reactive({
        table <- bartext()
        (table$barlength - table$sleevelength + table$platelength + table$platelength2 + table$platelength3) * table$plateweight3 })
    
    output$texttotal <- renderText({ 
        paste(total())})
    
    selectedData <- reactive({
            table <- data %>%
            filter(type == input$bar & position == input$hand & size == input$plate1) 
            
            if (input$plate2 != "None") {
                table <- cbind(table, filter(platetype, size == input$plate2))
                colnames(table) <- c("type"     ,    "barweight"  ,  "barlength"  ,  "sleeveweight", "sleevelength", "size" ,        "platelength" , "plateweight" , "position" ,    "handlength","size2", "platelength2", "plateweight2")
            }
            
            if (input$plate3 != "None") {
                table <- cbind(table, filter(platetype, size == input$plate3))
                colnames(table) <- c("type"     ,    "barweight"  ,  "barlength"  ,  "sleeveweight", "sleevelength", "size" ,        "platelength" , "plateweight" , "position" ,    "handlength","size2", "platelength2", "plateweight2",
                                     "size3", "platelength3", "plateweight3")
            }
            return(table)
    })
    
    output$table <- renderTable(
        selectedData()#, style="margin-top:-100em"
    )    
    
    
    
    #####function to make dataset of choices for text of calculation########
    
    bartext <- function(){
    text <- data %>%
        filter(type == input$bar & position == input$hand & size == input$plate1) 
    
    if (input$plate2 != "None") {
        text <- cbind(text, filter(platetype, size == input$plate2))
        colnames(text) <- c("type"     ,    "barweight"  ,  "barlength"  ,  "sleeveweight", "sleevelength", "size" ,        "platelength" , "plateweight" , "position" ,    "handlength","size2", "platelength2", "plateweight2")
    }
    
    if (input$plate3 != "None") {
        text <- cbind(text, filter(platetype, size == input$plate3))
        colnames(text) <- c("type"     ,    "barweight"  ,  "barlength"  ,  "sleeveweight", "sleevelength", "size" ,        "platelength" , "plateweight" , "position" ,    "handlength","size2", "platelength2", "plateweight2",
                             "size3", "platelength3", "plateweight3")
    }
    
    #text <- as.character(text)
    text$position <- as.character(text$position)
    
    return(text)
    }
    ######end bartext function#######
    
    barweight <- reactive({
        bash <- bartext()
        bash$barweight
    })
    
    barlength <- reactive({
        bash <- bartext()
        bash$barlength
    })
    
    sleeveweight <- reactive({
        bash <- bartext()
        bash$sleeveweight
    })
    
    sleevelength <- reactive({
        bash <- bartext()
        bash$sleevelength
    })
    
    platelength1 <- reactive({
        bash <- bartext()
        bash$platelength
    })
    plateweight1 <- reactive({
        bash <- bartext()
        bash$plateweight
    })
    platelength2 <- reactive({
        bash <- bartext()
        bash$platelength2
    })
    plateweight2 <- reactive({
        bash <- bartext()
        bash$plateweight2
    })
    platelength3 <- reactive({
        bash <- bartext()
        bash$platelength3
    })
    plateweight3 <- reactive({
        bash <- bartext()
        bash$plateweight3
    })
    handlength <- reactive({
        bash <- bartext()
        bash$handlength
    })
###########################text for calculation############    
    output$totalintro <- renderText({
        paste("Total Force on Lifting Arm When Bar is Parallel to the Ground =")})
    output$text1 <- renderText({ 
        paste("Sleeve 1 +")})
    output$text2 <- renderText({ 
        paste("(sleevelength + handposition) * .5 * sleeveweight")})
    output$text3 <- renderText({ 
        paste(sleeve1())})
    
    output$text4 <- renderText({ 
        paste("Bar +")})
    output$text5 <- renderText({ 
        paste(".5 * barlength * barweight")}) 
    output$text6 <- renderText({ 
        paste(bar1())})
    
    output$text7 <- renderText({ 
        paste("Sleeve 2 +")})
    output$text8 <- renderText({ 
        paste("(barlength - (sleevelength * .5)) * sleeveweight")}) 
    output$text9 <- renderText({ 
        paste(sleeve2())})
    
    output$text10 <- renderText({ 
        paste("Plate1 +")})
    output$text11 <- renderText({ 
        paste("(barlength - sleevelength + platelength) * plateweight")})  
    output$text12 <- renderText({ 
        paste(plate1())})
    
    output$text13 <- renderText({ 
        paste("Plate2 +")})
    output$text14 <- renderText({ 
        paste("(barlength - sleevelength + platelength + platelength2) * plateweight2")})
    output$text15 <- renderText({ 
        paste(plate2())})
    
    output$text16 <- renderText({ 
        paste("Plate3")})
    output$text17 <- renderText({ 
        paste("(barlength - sleevelength + platelength + platelength2 + platelength3) * plateweight3")}) 
    output$text18 <- renderText({ 
        paste(plate3())})
    
    output$text19 <- renderText({ 
        paste("/ Hand Placement")})
    output$text20 <- renderText({ 
        paste("(sleevelength - handposition)")})
    output$text21 <- renderText({ 
        paste(sleeveh())})
    
        # output$text1 <- renderText({ 
        #     paste("Total weight lifted = ( Bar weight ")}) 
        # output$text2 <- renderText({ 
        #     paste(barweight())})
        # output$text3 <- renderText({ 
        #     paste(" x ( Bar length ")}) 
        # output$text4 <- renderText({ 
        #     paste(barlength())}) 
        # output$text5 <- renderText({ 
        #     paste(" + an adjustment for hand position ")}) 
        # output$text6 <- renderText({ 
        #     paste(handlength())}) 
        # output$text7 <- renderText({ 
        #     paste(") ) cog + ( Plate weight ")}) 
        # output$text8 <- renderText({ 
        #     paste(plateweight1())}) 
        # output$text9 <- renderText({ 
        #     paste(" x ( Plate length")}) 
        # output$text10 <- renderText({ 
        #     paste(platelength1())}) 
        # output$text11 <- renderText({ 
        #     paste(" + platelength2 and 3 + hand position")}) 
        # output$text12 <- renderText({ 
        #     paste(handlength())}) 
        # output$text13 <- renderText({ 
        #     paste("for hand position.")}) 
            
            # bar weight <barcat> * (bar len <barcat> + hand <handcat>) * cog + 
            #     {plate weight * (platenum <plateweight> + platecat <platecat>} + hand <handcat>) * cog 
         #selectedText(),
        
    

    
    # output$barweight <- renderText(
    # input$bar == 
    # )
    
    
    output$image1 <- renderImage({
        if (input$bar == "Mens"){
            return(
        list(
            src = "www/mensbar.png",
            style="width: 100%; margin-top:-18em",
            filetype = "image/png",
            alt = "mens"
        ))}
        if (input$bar == "Womens") {
            return(list(
                src = "www/bellabar.png",
                style="width: 100%; margin-top:-18em",
                contentType = "image/png",
                alt = "womens"
            ))
        } else if (input$bar == "Training") {
            return(list(
                src = "www/trainingbar.png",
                style="width: 100%; margin-top:-18em",
                contentType = "image/png",
                alt = "training"
            ))
        }
        
    }, deleteFile = FALSE)

    
    output$image2 <- renderImage({
        if (input$hand == "Bar"){
            return(
                list(
                    src = "www/handbar.jpg",
                    style="width: 100%",
                    filetype = "image/jpg",
                    alt = "mens"
                ))}
        
         else if (input$hand == "Sleeve") {
            return(list(
                src = "www/handsleeve.jpg",
                style="width: 100%",
                contentType = "image/jpg",
                alt = "training"
            ))
        }
        
    }, deleteFile = FALSE)
    
    observeEvent(input$plate1, {
    output$image3 <- renderImage({
        if (input$plate1 == "Bumper 10"){
            return(
                list(
                    src = "www/bumper10.jpg",
                    style="display: inline-block; width: 150%; ",
                    filetype = "image/jpg",
                    alt = "mens"
                ))}
        
        if (input$plate1 == "Bumper 15") {
            return(list(
                src = "www/bumper15.jpg",
                style="width: 150%",
                contentType = "image/jpg",
                alt = "womens"
            ))}
        
        if (input$plate1 == "Bumper 25"){
            return(
                list(
                    src = "www/bumper25.jpg",
                    style="width: 150%",
                    filetype = "image/jpg",
                    alt = "mens"
                ))}
        
        if (input$plate1 == "Bumper 45") {
            return(list(
                src = "www/bumper45.jpg",
                style="width: 150%",
                contentType = "image/jpg",
                alt = "womens"
            ))}
        
        if (input$plate1 == "Metal 5"){
            return(
                list(
                    src = "www/metal5.jpg",
                    style="width: 150%",
                    filetype = "image/jpg",
                    alt = "mens"
                ))}
        
        if (input$plate1 == "Metal 10") {
            return(list(
                src = "www/metal10.jpg",
                style="width: 150%",
                contentType = "image/jpg",
                alt = "womens"
            ))}
        
        if (input$plate1 == "Metal 25"){
            return(
                list(
                    src = "www/metal25.jpg",
                    style="width: 150%",
                    filetype = "image/jpg",
                    alt = "mens"
                ))}
        
        else if (input$plate1 == "Metal 45"){
            return(list(
                src = "www/metal45.jpg",
                style="width: 150%",
                contentType = "image/jpg",
                alt = "training"
            ))}
        
    }, deleteFile = FALSE)
    }, ignoreInit = TRUE)
    

    observeEvent(input$plate2, {
    output$image4 <- renderImage({
        if (input$plate2 == "Bumper 10"){
            return(
                list(
                    src = "www/bumper10.jpg",
                    style="width: 150%",
                    filetype = "image/jpg",
                    alt = "mens"
                ))}
        
        if (input$plate2 == "Bumper 15") {
            return(list(
                src = "www/bumper15.jpg",
                style="width: 150%",
                contentType = "image/jpg",
                alt = "womens"
            ))}
        
        if (input$plate2 == "Bumper 25"){
            return(
                list(
                    src = "www/bumper25.jpg",
                    style="width: 150%",
                    filetype = "image/jpg",
                    alt = "mens"
                ))}
        
        
        if (input$plate2 == "Bumper 45") {
            return(list(
                src = "www/bumper45.jpg",
                style="width: 150%",
                contentType = "image/jpg",
                alt = "womens"
            ))}
        
        if (input$plate2 == "Metal 5"){
            return(
                list(
                    src = "www/metal5.jpg",
                    style="width: 150%",
                    filetype = "image/jpg",
                    alt = "mens"
                ))}
        
        if (input$plate2 == "Metal 10") {
            return(list(
                src = "www/metal10.jpg",
                style="width: 150%",
                contentType = "image/jpg",
                alt = "womens"
            ))}
        
        if (input$plate2 == "Metal 25"){
            return(
                list(
                    src = "www/metal25.jpg",
                    style="width: 150%",
                    filetype = "image/jpg",
                    alt = "mens"
                ))}
        
        
        else if (input$plate2 == "Metal 45"){
            return(list(
                src = "www/metal45.jpg",
                style="width: 150%",
                contentType = "image/jpg",
                alt = "training"
            ))}
        
        
    }, deleteFile = FALSE)
    }, ignoreInit = TRUE)
    
    observeEvent(input$plate3, {
    output$image5 <- renderImage({
        if (input$plate3 == "Bumper 10"){
            return(
                list(
                    src = "www/bumper10.jpg",
                    style="width: 150%",
                    filetype = "image/jpg",
                    alt = "mens"
                ))}
        
        if (input$plate3 == "Bumper 15") {
            return(list(
                src = "www/bumper15.jpg",
                style="width: 150%",
                contentType = "image/jpg",
                alt = "womens"
            ))}
        
        if (input$plate3 == "Bumper 25"){
            return(
                list(
                    src = "www/bumper25.jpg",
                    style="width: 150%",
                    filetype = "image/jpg",
                    alt = "mens"
                ))}
        
        
        if (input$plate3 == "Bumper 45") {
            return(list(
                src = "www/bumper45.jpg",
                style="width: 150%",
                contentType = "image/jpg",
                alt = "womens"
            ))}
        
        if (input$plate3 == "Metal 5"){
            return(
                list(
                    src = "www/metal5.jpg",
                    style="width: 150%",
                    filetype = "image/jpg",
                    alt = "mens"
                ))}
        
        if (input$plate3 == "Metal 10") {
            return(list(
                src = "www/metal10.jpg",
                style="width: 150%",
                contentType = "image/jpg",
                alt = "womens"
            ))}
        
        if (input$plate3 == "Metal 25"){
            return(
                list(
                    src = "www/metal25.jpg",
                    style="width: 150%",
                    filetype = "image/jpg",
                    alt = "mens"
                ))}
        
        
        else if (input$plate3 == "Metal 45"){
            return(list(
                src = "www/metal45.jpg",
                style="width: 150%",
                contentType = "image/jpg",
                alt = "training"
            ))}
        
        
    }, deleteFile = FALSE)    
    }, ignoreInit = TRUE)
    
    # src = "www/bumper10.jpg",
    # contentType = "image/jpg",
    
# output$image2 <- renderImage({
#     if (input$bar == "Mens"){
#         return(
#             list(
#                 src = "www/mensbar.png",
#                 filetype = "image/png",
#                 alt = "mens"
#             ))}
#     
#     
#     if (input$bar == "Womens") {
#         return(list(
#             src = "www/bumper10.jpg",
#             contentType = "image/jpg",
#             alt = "womens"
#         ))
#     } else if (input$bar == "Training") {
#         return(list(
#             src = "www/trainingbar.png",
#             contentType = "image/png",
#             alt = "training"
#         ))
#     }
#     
# }, deleteFile = FALSE)
# output$image1 <- renderImage({
#     if (input$bar == "Mens"){
#         return(
#             list(
#                 src = "www/mensbar.png",
#                 filetype = "image/png",
#                 alt = "mens"
#             ))}
#     
#     
#     if (input$bar == "Womens") {
#         return(list(
#             src = "www/bellabar.png",
#             contentType = "image/png",
#             alt = "womens"
#         ))
#     } else if (input$bar == "Training") {
#         return(list(
#             src = "www/trainingbar.png",
#             contentType = "image/png",
#             alt = "training"
#         ))
#     }
#     
# }, deleteFile = FALSE)
}
# server <- function(input, output, session) {
#     
#     output$image2 <- renderImage({
#         # if (input$bar == "Mens")
#         #     return(
#         list(
#             src = "www/mensbar.png",
#             filetype = "image/png",
#             alt = "Face"
#         )})}

# Run the application 
shinyApp(ui = ui, server = server)
