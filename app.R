library(shiny)
library(shinyWidgets)
library(shinyBS)

alltweets <- readRDS("covidtweets.rds")

censor_names <- function(text) {
    gsub("@\\w+", "@█████", text)
}

icon_with_title <- function(..., title) {

    i <- icon(...)
    i$attribs$title <- title

    return(i)
}

bullet_list <- function(...) {
    paste("&bull;", paste(..., sep = "<br> &bull; "))
}

if (!dir.exists("out")) {
    dir.create("out")
}


library(dplyr)
list_topics <- tribble(
    ~ id, ~ label, ~ choices,
    "topic1", "Connaissance et propagation de la maladie", c("Rassuré", "Neutre", "Doute", "Inquiet"),
    "topic2", "Mesures de contrôle, action gouvernementale", c("Adhésion à la mesure", "Neutre", "Incompréhension de la mesure", "Rejet de la mesure"),
    "topic3", "Isolement des malades et des personnes contact", c("Adhésion à la mesure", "Neutre", "Incompréhension de la mesure", "Rejet de la mesure"),
    "topic4", "Dépistage et traitement", c("Rassuré", "Neutre", "Doute", "Inquiet"),
    "topic5", "Vaccination", c("Adhésion à la mesure", "Neutre", "Incompréhension de la mesure", "Rejet de la mesure"),
    "topic6", "Impacts sociétaux", c("Positif", "Neutre", "Négatif")
)

ui <- fluidPage(

    includeCSS("extra.css"),

    fluidRow(

        column(6,
               wellPanel(
                   fluidRow(
                       column(6,
                              img(src="logo_IRD.png", width = "50%"),
                       ),
                       column(6,
                              img(src="logo_ANR.svg", width = "50%"),
                       ),
                   ),
                   br(),
                   includeMarkdown("text_intro.md"),
               )
        ),
        column(6,
               h4("Tweet à classer:"),
               htmlOutput("twtext",
                          container = tags$blockquote,
                          class = "twitter-tweet"),
               br(),
               actionButton("cancel", "Ce tweet ne parle pas du COVID-19", class = "btn-warning"),
               lapply(seq_along(list_topics$id), function(i) {
                   sliderTextInput(
                       list_topics$id[i],
                       list_topics$label[i],
                       choices = c(list_topics$choices[[i]], "N/A"),
                       selected = "N/A",
                       force_edges = TRUE,
                       grid = TRUE,
                       width = "100%"
                   )
               }),
               actionButton("skip", "Aucun thème ne convient"),
               actionButton("do", "Valider", class = "btn-success"),
        ),
    )

)

server <- function(input, output, session) {

    user_id <- stringi::stri_rand_strings(1, 10)

    outdate <- file.path("out", Sys.Date())

    if (!dir.exists(outdate)) {
        dir.create(outdate)
    }

    outfile <- paste0(file.path(outdate, user_id), ".csv")

    write(
        paste(c("id", list_topics$id), collapse = ","),
        file = outfile
    )

    # Select and display tweet
    d <- sample(nrow(alltweets), 1)

    output$twtext <- renderUI({
        censor_names(alltweets$texte[d])
    })

    observeEvent(input$do, {

        # Save result in an easily readable format
        topics_feelings <- vapply(list_topics$id, function(i) input[[i]], character(1))

        if (identical(unique(topics_feelings), "N/A")) {
            show_alert(
                title = "Erreur",
                text = 'Sélectionnez au moins un thème avec une perception négative/neutre/positive ou cliquez sur le bouton "Aucun thème ne convient"',
                type = "error"
            )
        } else {

            write(
                paste(c(alltweets$id[d], topics_feelings), collapse = ","),
                file = outfile,
                append = TRUE
            )

            # Reset UI state
            for (i in list_topics$id) {
                updateSliderTextInput(session, i, selected = "N/A")
            }

            # Select and display tweet
            d <<- sample(nrow(alltweets), 1)

            output$twtext <- renderUI({
                censor_names(alltweets$texte[d])
            })

        }

    })

    observeEvent(input$skip, {

        # Save result in an easily readable format
        topics_feelings <- rep_len("OtherTopic", length(list_topics$id))

        write(
            paste(c(alltweets$id[d], topics_feelings), collapse = ","),
            file = outfile,
            append = TRUE
        )

        # Reset UI state
        for (i in list_topics$id) {
            updateSliderTextInput(session, i, selected = "N/A")
        }

        # Select and display tweet
        d <<- sample(nrow(alltweets), 1)

        output$twtext <- renderUI({
            censor_names(alltweets$texte[d])
        })

    })

    observeEvent(input$cancel, {

        # Save result in an easily readable format
        topics_feelings <- rep_len("OffTopic", length(list_topics$id))

        write(
            paste(c(alltweets$id[d], topics_feelings), collapse = ","),
            file = outfile,
            append = TRUE
        )

        # Reset UI state
        for (i in list_topics$id) {
            updateSliderTextInput(session, i, selected = "N/A")
        }

        # Select and display tweet
        d <<- sample(nrow(alltweets), 1)

        output$twtext <- renderUI({
            censor_names(alltweets$texte[d])
        })

    })

}

shinyApp(ui = ui, server = server)
