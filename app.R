library(shiny)
library(shinyWidgets)
library(shinycssloaders)

load_tweets <- function() {
    readRDS("covidtweets_2020-10-15.rds")
}

censor_names <- function(text) {
    gsub("@\\w+", "@█████", text)
}

list_topics <- c("Topic 1" = "topic1",
                 "Topic 2" = "topic2",
                 "Topic 3" = "topic3",
                 "Topic 4" = "topic4",
                 "Topic 5" = "topic5")

ui <- fluidPage(

    includeCSS("extra.css"),

    titlePanel("Classify COVID-19 tweets"),

    fluidRow(

        column(6,
            checkboxGroupButtons(
               "topics", "Topics",
               list_topics,
               justified = TRUE, status = "primary",
               checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                no = icon("remove", lib = "glyphicon"))
            ),
            sliderTextInput(
               "feeling", "Sentiment:",
               choices = c("Negative", "Neutral", "Positive"),
               selected = "Neutral",
               force_edges = TRUE
            ),
            actionButton("do", "Next tweet",
                        width = "50%", class = "btn-primary btn-block"),
            ),
        column(6,
            withSpinner(
                htmlOutput("twtext",
                           container = tags$blockquote,
                           class = "twitter-tweet"),
            ),
        )
    )

)

server <- function(input, output, session) {

    alltweets <- load_tweets()

    user_id <- stringi::stri_rand_strings(1, 10)

    outdate <- file.path("out", Sys.Date())

    if (!dir.exists(outdate)) {
        dir.create(outdate)
    }

    outfile <- paste0(file.path(outdate, user_id), ".csv")

    write(
        paste(c("id", list_topics, "feeling"), collapse = ","),
        file = outfile
    )

    observeEvent(input$do, ignoreNULL = FALSE, {

        # Reset UI state
        updateCheckboxGroupButtons(session, "topics", selected = character(0))

        updateSliderTextInput(session, "feeling", selected = "Neutral")

        # Select and display tweet
        d <- sample(nrow(alltweets), 1)

        output$twtext <- renderUI({
                censor_names(alltweets$texte[d])
        })

        # Save result in an easily readable format
        topics_tf <- list_topics %in% input$topics

        write(
            paste(c(alltweets$id[d], topics_tf, input$feeling), collapse = ","),
            file = outfile,
            append = TRUE
        )

    })

}

shinyApp(ui = ui, server = server)
