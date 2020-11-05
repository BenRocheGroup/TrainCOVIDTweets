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
    ~ id, ~ label, ~ description,
    "topic1", "Diffusion et propagation de la maladie", bullet_list("Foyers et zones de circulation", "Niveaux de risque", "Chiffres en France et dans le monde : décès, hospitalisations, cas, situation épidémiologique", "Modélisation"),
    "topic2", "Connaissance de la maladie", bullet_list("Origine du virus, de la maladie (y compris les hypothèses discutées&nbsp;: Bill Gates, 5G…)", "Mode de transmission", "Symptômes (dont asymptomatiques)", "Évolution et mutation du virus"),
    "topic3", "Mesures de contrôle, action gouvernementale", bullet_list("Confinement", "Quarantaine", "Couvre-feu", "Masques", "Fermetures (école, magasins, bar…)", "télétravail", "distanciation sociale", "attestations de déplacement", "gel hydro-alcoolique", "application de suivi (StopCovid, TousAntiCovid)", "aides aux entreprises"),
    "topic4", "Prise en charge et traitement", bullet_list("Dépistage (tests, stratégie)", "Médicaments", "Protocoles thérapeutiques", "Vaccin"),
    "topic5", "Impacts sociétaux", bullet_list("Impacts économiques", "Inégalités sociales (santé, scolaires…)", "Tension hospitalière et impact sur les soignants", "Déprogrammations hospitalières", "Ruptures de stock", "Achats de panique", "Racisme", "Santé mentale (ennui, stress, solitude, dépression, anxiété)")
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
                        span(tagList(list_topics$label[i], tipify(icon("question-circle"), title = list_topics$description[i], placement = "right"))),
                        choices = c("Negative" = "<div class='text-danger'>Négatif</div>",
                                    "Neutral" = "<div class='text-info'>Neutre</div>",
                                    "Positive" = "<div class='text-success'>Positif</div>",
                                    "N/A"),
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

    observeEvent(input$do, ignoreNULL = FALSE, {

        # Reset UI state
        for (i in list_topics$id) {
            updateSliderTextInput(session, i, selected = "N/A")
        }

        # Select and display tweet
        d <- sample(nrow(alltweets), 1)

        output$twtext <- renderUI({
                censor_names(alltweets$texte[d])
        })

        # Save result in an easily readable format
        topics_feelings <- vapply(list_topics$id, function(i) input[[i]], character(1))

        write(
            paste(c(alltweets$id[d], topics_feelings), collapse = ","),
            file = outfile,
            append = TRUE
        )

    })

    observeEvent(input$skip, ignoreNULL = FALSE, {

        # Reset UI state
        for (i in list_topics$id) {
            updateSliderTextInput(session, i, selected = "N/A")
        }

        # Select and display tweet
        d <- sample(nrow(alltweets), 1)

        output$twtext <- renderUI({
            censor_names(alltweets$texte[d])
        })

        # Save result in an easily readable format
        topics_feelings <- rep_len("N/A", length(list_topics$id))

        write(
            paste(c(alltweets$id[d], topics_feelings), collapse = ","),
            file = outfile,
            append = TRUE
        )

    })

    observeEvent(input$cancel, ignoreNULL = FALSE, {

        # Reset UI state
        for (i in list_topics$id) {
            updateSliderTextInput(session, i, selected = "N/A")
        }

        # Select and display tweet
        d <- sample(nrow(alltweets), 1)

        output$twtext <- renderUI({
            censor_names(alltweets$texte[d])
        })

        # Save result in an easily readable format
        topics_feelings <- rep_len("OT", length(list_topics$id))

        write(
            paste(c(alltweets$id[d], topics_feelings), collapse = ","),
            file = outfile,
            append = TRUE
        )

    })

}

shinyApp(ui = ui, server = server)
