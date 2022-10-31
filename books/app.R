library(shiny)
library(dplyr)
library(DT)

load("data/book_transactions.Rdata")
popularity <- read.csv("data/book_popularity.csv")

# TODO: dedup books by title*author not just isbn
popularity$book_name <- paste(
  popularity$book_title,
  "by",
  popularity$book_author,
  "(", popularity$isbn, ")"
)

make_book_recs <- function(selected_isbns, transactions, exclude = character(0),
                           min_support = 2, min_confidence = 0.1, min_len = 2,
                           max_time = 0) {
  book_rules <- apriori(
    transactions,
    parameter = list(
      supp = min_support / length(transactions),
      conf = min_confidence,
      minlen = min_len,
      maxtime = max_time
    ),
    appearance = list(
      none = exclude,
      lhs = selected_isbns,
      default = "rhs"
    ),
    control = list(
      verbose = FALSE
    )
  )

  if (length(book_rules) > 0) {
    book_rules <- book_rules[!is.redundant(book_rules)]
    book_rules <- book_rules[is.significant(book_rules, transactions)]

    recs_df <- DATAFRAME(
      book_rules,
      itemSep = " + ", setStart = "", setEnd = ""
    )

    recs_df <- recs_df %>%
      filter(!(RHS %in% selected_isbns)) %>%
      group_by(RHS) %>%
      summarise(confidence = 100 * max(confidence)) %>%
      rename(isbn = RHS) %>%
      arrange(-confidence)
  } else {
    recs_df <- NULL
  }

  recs_df
}

ui <- fluidPage(
  titlePanel("Book recommender"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "selected_books",
        label = "Select books to base recs on:",
        choices = sort(popularity$book_name),
        multiple = TRUE
      ),
      sliderInput(
        inputId = "min_confidence",
        label = "Minimum confidence for recs:",
        min = 0,
        max = 100,
        value = 25,
        step = 1
      ),
      actionButton(
        inputId = "make_recs_button",
        label = "Give me good books!",
        icon = icon("book")
      )
    ),
    mainPanel(
      DT::dataTableOutput("rec_dt")
    )
  )
)


server <- function(input, output) {
  recommendations <- eventReactive(input$make_recs_button, {
    selected_rows <- popularity[popularity$book_name %in% input$selected_books, ]

    make_book_recs(
      selected_isbns = selected_rows$isbn,
      transactions = book_transactions,
      min_support = 2,
      min_confidence = input$min_confidence / 100,
      min_len = 1,
      exclude = character(0)
    )
  })

  output$rec_dt <- DT::renderDataTable({
    if (is.null(recommendations())) {
      no_recs <- data.frame(`No recs` = "Try adjusting the inputs")
      names(no_recs) <- "No recs"

      no_recs
    } else {
      recommendations() %>%
        left_join(popularity, by = "isbn") %>%
        select(
          book_title, book_author, confidence,
          n_readers, perc_readers, avg_rating
        ) %>%
        mutate(
          confidence = round(confidence, 1),
          perc_readers = round(100 * perc_readers, 1),
          avg_rating = round(perc_readers, 1),
        )
    }
  })
}


shinyApp(ui = ui, server = server)
