library(shiny)
library(ggplot2)
library(dplyr)
library(DBI)
library(RSQLite)
source(file.path("R", "sentiment.R"))

# -----------------------------------------------------------------------------
# SCREEN / SENSE — an interactive movie ratings & review analysis in R
# Local sample data is prepared from a public educational IMDb review dataset.
# -----------------------------------------------------------------------------

seed_file <- function(name) file.path("data", name)
if (!all(file.exists(seed_file("movies.csv"), seed_file("reviews.csv")))) {
  stop("Real sample data is missing. Run Rscript scripts/prepare_sample_data.R first.")
}
seed_movies <- read.csv(seed_file("movies.csv"), stringsAsFactors = FALSE)
seed_reviews <- read.csv(seed_file("reviews.csv"), stringsAsFactors = FALSE)

# SQLite is the persistent data layer. The prepared source sample is inserted
# only when the database is first created; future launches keep imported rows.
db_file <- file.path(getwd(), "movie_insights_expanded.sqlite")
db <- dbConnect(SQLite(), db_file)
dbExecute(db, "PRAGMA foreign_keys = ON")
dbExecute(db, "CREATE TABLE IF NOT EXISTS movies (
  id INTEGER PRIMARY KEY AUTOINCREMENT, title TEXT NOT NULL, genre TEXT NOT NULL,
  year INTEGER NOT NULL, rating REAL NOT NULL, votes INTEGER NOT NULL DEFAULT 0)")
dbExecute(db, "CREATE TABLE IF NOT EXISTS reviews (
  id INTEGER PRIMARY KEY AUTOINCREMENT, movie_id INTEGER NOT NULL, review TEXT NOT NULL,
  sentiment TEXT NOT NULL, score REAL NOT NULL,
  FOREIGN KEY(movie_id) REFERENCES movies(id))")
if (dbGetQuery(db, "SELECT COUNT(*) AS n FROM movies")$n[1] == 0) {
  dbWriteTable(db, "movies", seed_movies, append = TRUE, row.names = FALSE)
  dbWriteTable(db, "reviews", seed_reviews %>% select(movie_id, review, sentiment, score), append = TRUE, row.names = FALSE)
}
load_movies <- function() dbGetQuery(db, "SELECT id, title, genre, year, rating, votes FROM movies")
load_reviews <- function() {
  dbGetQuery(db, "SELECT r.movie_id, r.review, r.sentiment, r.score,
    m.title, m.genre, m.year, m.rating
    FROM reviews r JOIN movies m ON m.id = r.movie_id")
}
movies <- load_movies()
reviews <- load_reviews()

fmt_num <- function(x) format(round(x), big.mark = ",", scientific = FALSE)
fmt_pct <- function(x) paste0(round(x * 100), "%")

ui <- fluidPage(
  tags$head(
    tags$title("SCREEN / SENSE — Movie audience intelligence"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$script(HTML("$(document).on('keydown', '#movie_query', function(e) { if (e.key === 'Enter') { e.preventDefault(); $('#movie_search').click(); } });")),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = "anonymous"),
    tags$link(href = "https://fonts.googleapis.com/css2?family=DM+Mono:wght@400;500&family=DM+Sans:wght@400;500;600;700&family=Playfair+Display:wght@500;600;700&display=swap", rel = "stylesheet"),
    tags$style(HTML('
      :root { --ink:#172b35; --ink2:#38515b; --muted:#829096; --paper:#f5f4ef; --white:#fffefa; --line:#e7e5dd; --teal:#347c78; --teal-lite:#e4efeb; --coral:#d78266; --gold:#c4a565; --nav:#152a34; }
      * { box-sizing:border-box; }
      body { margin:0; background:var(--paper); color:var(--ink); font-family:"DM Sans",sans-serif; font-size:14px; }
      .container-fluid { padding:0; max-width:1600px; }
      .topbar { height:70px; display:flex; align-items:center; justify-content:space-between; padding:0 42px; background:var(--nav); color:#f8f7f1; border-bottom:1px solid rgba(255,255,255,.09); }
      .brand { display:flex; align-items:center; gap:13px; font-family:"DM Mono",monospace; font-size:13px; letter-spacing:1.7px; }
      .brand-mark { width:31px;height:31px;border:1px solid #80aaa1;border-radius:50%;display:grid;place-items:center;color:#b6d1c8;font-family:serif;font-size:17px; }
      .top-meta { color:#a9b8b8; font-family:"DM Mono",monospace; font-size:10px; letter-spacing:1.2px; text-transform:uppercase; }
      .shell { display:grid;grid-template-columns:246px 1fr;min-height:calc(100vh - 70px); }
      .sidebar { background:var(--white);border-right:1px solid var(--line);padding:30px 22px; }
      .side-kicker,.eyebrow,.section-kicker { color:var(--muted);font-family:"DM Mono",monospace;font-size:10px;letter-spacing:1.5px;text-transform:uppercase; }
      .side-kicker { margin:2px 0 22px; }
      .filter-label { color:#53666c;font-size:11px;font-weight:700;letter-spacing:.45px;margin:20px 0 8px;text-transform:uppercase; }
      .sidebar .form-group { margin-bottom:5px; }
      .sidebar .selectize-input,.sidebar .form-control { border:1px solid var(--line);border-radius:5px;box-shadow:none;background:#fffefa;color:var(--ink);font-size:12px;min-height:38px; }
      .sidebar .irs--shiny .irs-bar,.sidebar .irs--shiny .irs-from,.sidebar .irs--shiny .irs-to,.sidebar .irs--shiny .irs-single { background:var(--teal);border-color:var(--teal); }
      .sidebar .irs--shiny .irs-handle { border-color:var(--teal); }
      .filter-note { margin-top:24px;padding:14px;border-radius:6px;background:#f2f5f1;color:#687a78;font-size:11px;line-height:1.6; }
      .filter-note strong { display:block;color:var(--teal);font-family:"DM Mono",monospace;font-size:10px;letter-spacing:1px;margin-bottom:5px;text-transform:uppercase; }
      .main { padding:32px 42px 52px;min-width:0; }
      .hero { display:flex;justify-content:space-between;align-items:flex-end;margin-bottom:24px; }
      .eyebrow { color:var(--teal);margin-bottom:10px; }
      h1 { font-family:"Playfair Display",serif;font-size:34px;font-weight:500;letter-spacing:-.6px;line-height:1.12;margin:0;color:var(--ink); }
      .hero-sub { color:#768489;font-size:12px;margin-top:9px; }
      .period-tag { border:1px solid #d8dfd9;border-radius:3px;color:#59716d;padding:8px 11px;font-family:"DM Mono",monospace;font-size:10px;letter-spacing:.7px;text-transform:uppercase;white-space:nowrap; }
      .nav-tabs { border-bottom:1px solid var(--line);margin-bottom:22px;display:flex;gap:5px; }
      .nav-tabs>li>a { border:0!important;border-radius:0!important;color:#829096;padding:12px 17px;margin:0!important;font-size:12px; }
      .nav-tabs>li.active>a,.nav-tabs>li.active>a:hover,.nav-tabs>li.active>a:focus { color:var(--teal);background:transparent;border-bottom:2px solid var(--teal)!important;font-weight:700; }
      .nav-tabs>li>a:hover { background:transparent;color:var(--ink); }
      .kpi-grid { display:grid;grid-template-columns:repeat(4,minmax(0,1fr));gap:13px;margin-bottom:15px; }
      .kpi { background:var(--white);border:1px solid var(--line);border-radius:5px;padding:17px 18px 15px;min-height:104px; }
      .kpi-label { color:#78878b;font-family:"DM Mono",monospace;font-size:9px;letter-spacing:1.15px;text-transform:uppercase; }
      .kpi-value { color:var(--ink);font-family:"Playfair Display",serif;font-size:28px;line-height:1;margin-top:13px; }
      .kpi-foot { color:#91a09e;font-size:10px;margin-top:7px; }
      .panel { background:var(--white);border:1px solid var(--line);border-radius:5px;padding:18px 19px 12px;margin-bottom:15px;min-width:0; }
      .panel-head { display:flex;justify-content:space-between;align-items:flex-start;margin-bottom:4px; }
      .panel-title { font-family:"Playfair Display",serif;font-size:17px;color:var(--ink); }
      .panel-sub { color:#92a0a0;font-size:10px;margin-top:4px; }
      .panel-unit { color:#92a0a0;font-family:"DM Mono",monospace;font-size:9px;letter-spacing:.7px;text-transform:uppercase;padding-top:4px; }
      .chart-row { display:grid;grid-template-columns:1.35fr 1fr;gap:15px; }
      .chart-row.equal { grid-template-columns:1fr 1fr; }
      .plot-box { height:264px; }
      .plot-box.tall { height:340px; }
      .plot-box .shiny-plot, .plot-box .plotly { width:100%!important;height:100%!important; }
      .insight { background:var(--nav);color:#e8efea;border-radius:5px;padding:18px 20px;margin:0 0 15px;display:flex;gap:17px;align-items:flex-start; }
      .insight-mark { color:#a9c8ba;font-family:"Playfair Display",serif;font-size:22px;line-height:1; }
      .insight-label { color:#9bb7ad;font-family:"DM Mono",monospace;font-size:9px;letter-spacing:1.4px;text-transform:uppercase;margin-bottom:6px; }
      .insight-text { color:#e7eeea;font-size:12px;line-height:1.65; }
      .review-card { background:#f6f6f0;border-left:2px solid #85a99c;padding:13px 15px;margin:8px 0;border-radius:0 4px 4px 0; }
      .review-quote { font-family:"Playfair Display",serif;font-style:italic;font-size:15px;color:#32484e;line-height:1.45; }
      .review-meta { display:flex;justify-content:space-between;color:#899694;font-family:"DM Mono",monospace;font-size:9px;letter-spacing:.5px;margin-top:10px;text-transform:uppercase; }
      .review-chip { color:var(--teal); }
      .local-movie { display:block; }
      .online-title { font-family:"Playfair Display",serif;font-size:28px;font-weight:500;margin:4px 0 12px;color:var(--ink); }
      .online-score { display:flex;gap:10px;align-items:center;margin-bottom:14px;color:#829096;font-family:"DM Mono",monospace;font-size:10px; }
      .online-overview { color:#52636a;font-size:13px;line-height:1.75;max-width:820px; }
      .online-kpi .kpi-value { font-size:24px;color:var(--teal); }
      .data-table { width:100%;border-collapse:collapse;font-size:11px; }
      .data-table th { color:#849294;font-family:"DM Mono",monospace;font-size:9px;letter-spacing:.9px;text-align:left;text-transform:uppercase;padding:10px;border-bottom:1px solid var(--line); }
      .data-table td { padding:10px;border-bottom:1px solid #efeee9;color:#52636a; }
      .data-table tr:last-child td { border:0; }
      .data-table .movie-name { color:var(--ink);font-weight:600; }
      .rating-pill { background:#edf3ee;color:var(--teal);border-radius:3px;padding:3px 7px;font-family:"DM Mono",monospace; }
      .footer { border-top:1px solid var(--line);margin-top:24px;padding-top:15px;color:#99a3a2;font-family:"DM Mono",monospace;font-size:9px;letter-spacing:.6px;display:flex;justify-content:space-between; }
      .shiny-input-container { width:100%; }
      @media(max-width:1000px) { .shell{grid-template-columns:210px 1fr}.main{padding:27px 24px}.topbar{padding:0 24px}.chart-row{grid-template-columns:1fr}.kpi-grid{grid-template-columns:repeat(2,1fr)} }
      @media(max-width:650px) { .shell{display:block}.sidebar{border-right:0;border-bottom:1px solid var(--line);padding:20px}.main{padding:23px 16px}.topbar{padding:0 16px;height:60px}.top-meta{display:none}h1{font-size:29px}.hero{display:block}.period-tag{display:inline-block;margin-top:15px}.kpi-grid{gap:8px}.kpi{padding:14px;min-height:95px}.kpi-value{font-size:24px}.chart-row,.chart-row.equal{grid-template-columns:1fr}.nav-tabs>li>a{padding:11px 9px;font-size:11px}.footer{display:block;line-height:1.8}.online-title{font-size:22px} }
    '))
  ),
  div(class = "topbar",
      div(class = "brand", div(class = "brand-mark", "S"), span("SCREEN / SENSE")),
      div(class = "top-meta", "AUDIENCE INTELLIGENCE  ·  R ANALYTICS")),
  div(class = "shell",
      div(class = "sidebar",
          div(class = "side-kicker", "Movie dataset"),
          div(class = "filter-note", tags$strong("Search the sample"), "Each result shows its source rating and sampled user reviews. ", tags$a(href = "https://huggingface.co/datasets/MansaT/Movie-Dataset", target = "_blank", "Dataset source")),
          div(class = "filter-note", tags$strong("Sentiment method"), "A small word lexicon counts positive and negative terms. A negator in the prior three words flips a match. Scores become Positive, Mixed or Critical; the polarity index is an estimate, not a trained model.")),
      div(class = "main",
          div(class = "hero",
              div(div(class = "eyebrow", "THE AUDIENCE, IN THEIR OWN WORDS"),
                  h1("What makes a movie\n"), tags$span(style="font-family:'Playfair Display',serif;font-size:34px;font-weight:500;letter-spacing:-.6px;line-height:1.12;color:#347c78;", "stay with us?"),
                  div(class = "hero-sub", "Ratings, genres & the feeling behind every review.")),
              div(class = "period-tag", "●  LOCAL MOVIE DATA  /  01")),
          tabsetPanel(id = "tabs", type = "tabs", selected = "lookup",
              tabPanel("Find a movie", value = "lookup",
                  div(class="panel",div(class="panel-head",div(div(class="panel-title","Search this movie dataset"),div(class="panel-sub","Type a title from the included sample to see its rating, reviews and sentiment.")),div(class="panel-unit",paste(nrow(movies),"MOVIES · LOCAL DATA"))),
                      textInput("movie_query", "Movie title", placeholder="Try Parasite, Inception, or The Matrix"),
                      actionButton("movie_search", "Search movies", class="btn btn-primary"),
                      uiOutput("search_status")),
                  uiOutput("movie_picker"),uiOutput("local_movie_card"),
                  div(class="chart-row equal",div(class="panel",div(class="panel-head",div(div(class="panel-title","Review sentiment"),div(class="panel-sub","Lexicon estimate · sampled dataset reviews"))),uiOutput("movie_review_summary"),div(class="plot-box",plotOutput("movie_sentiment_plot",height="100%"))),
                      div(class="panel",div(class="panel-head",div(div(class="panel-title","Audience reviews"),div(class="panel-sub","Sampled reviews for the selected movie"))),uiOutput("movie_review_cards")))),
              tabPanel("Overview", value = "overview",
                  uiOutput("overview_kpis"),
                  div(class = "chart-row",
                      div(class = "panel", div(class="panel-head",div(div(class="panel-title","The genre landscape"),div(class="panel-sub","Average audience rating and review sentiment by genre")),div(class="panel-unit","RATING / 10")),div(class="plot-box",plotOutput("genre_plot",height="100%"))),
                      div(class = "panel", div(class="panel-head",div(div(class="panel-title","A changing audience"),div(class="panel-sub","Ratings and review sentiment across release years")),div(class="panel-unit","YEARLY TREND")),div(class="plot-box",plotOutput("trend_plot",height="100%")))),
                  uiOutput("overview_insight"),
                  div(class = "chart-row equal",
                      div(class="panel",div(class="panel-head",div(div(class="panel-title","How audiences feel"),div(class="panel-sub","Share of reviews in each sentiment group"))),div(class="plot-box",plotOutput("sentiment_plot",height="100%"))),
                      div(class="panel",div(class="panel-head",div(div(class="panel-title","The films finding their audience"),div(class="panel-sub","Highest-rated titles in the sample"))),uiOutput("top_movies")))),
              tabPanel("Genre study", value = "genres",
                  div(class="panel",div(class="panel-head",div(div(class="panel-title","A closer look at genre"),div(class="panel-sub","Mean rating, review tone and audience reach for every genre")),div(class="panel-unit","COMPARATIVE VIEW")),div(class="plot-box tall",plotOutput("genre_detail",height="100%"))),
                  div(class="chart-row equal",div(class="panel",div(class="panel-head",div(div(class="panel-title","Rating distribution"),div(class="panel-sub","Where each genre's scores tend to land"))),div(class="plot-box",plotOutput("rating_dist",height="100%"))),div(class="panel",div(class="panel-head",div(div(class="panel-title","Genre scorecard"),div(class="panel-sub","A compact read on audience response"))),uiOutput("genre_table")))),
              tabPanel("Audience voice", value = "voice",
                      div(class="panel",div(class="panel-head",div(div(class="panel-title","How sentiment is estimated"),div(class="panel-sub","The same rule is applied to every included review"))),
                      p(style="font-size:11px;color:#52636a;line-height:1.7;margin:8px 0 4px;",
                        "The app tokenizes English review text and counts matches in a fixed positive and negative word list. A negator among the previous three words flips a match. Polarity = (positive matches − negative matches) ÷ all matches; the result is mapped to 0–100%. Scores ≥57.5% are Positive, ≤42.5% are Critical, and the rest are Mixed. Reviews with no lexicon matches receive a neutral 50% score."),
                      p(style="font-size:10px;color:#829096;line-height:1.6;margin:0;","This explainable baseline can miss context, sarcasm, intensifiers, and vocabulary outside the lexicon; treat the result as an estimate.")),
                  div(class="chart-row equal",div(class="panel",div(class="panel-head",div(div(class="panel-title","Sentiment by genre"),div(class="panel-sub","Positive, mixed and critical review share"))),div(class="plot-box tall",plotOutput("sentiment_genre",height="100%"))),div(class="panel",div(class="panel-head",div(div(class="panel-title","What viewers are saying"),div(class="panel-sub","A sample of audience language from these films"))),uiOutput("review_cards"))),
                  div(class="panel",div(class="panel-head",div(div(class="panel-title","Review tone over time"),div(class="panel-sub","Mean lexicon polarity index by release year"))),div(class="plot-box",plotOutput("sentiment_time",height="100%")))),
              tabPanel("Movie explorer", value = "movies",
                  div(class="panel",div(class="panel-head",div(div(class="panel-title","All movies in the sample"),div(class="panel-sub","Ratings and vote counts from the source dataset"))),uiOutput("movie_table")))) ,
          div(class="footer",span("SCREEN / SENSE  ·  MOVIE RATINGS & GENRE SENTIMENT ANALYSIS"),span("BUILT WITH R  /  SHINY  /  GGPLOT2"))
      )
  )
)

server <- function(input, output, session) {
  onStop(function() if (dbIsValid(db)) dbDisconnect(db), session = session)

  search_results <- reactiveVal(NULL)
  selected_local_movie <- reactiveVal(NULL)
  search_message <- reactiveVal("Type a title from the sample and select Search movies.")

  load_local_movie <- function(movie_id) {
    row <- movies[movies$id == as.integer(movie_id), , drop = FALSE]
    if (!nrow(row)) {
      selected_local_movie(NULL)
      return(invisible(NULL))
    }
    movie_reviews <- reviews[reviews$movie_id == row$id[1], , drop = FALSE]
    review_rows <- lapply(seq_len(nrow(movie_reviews)), function(i) list(
      content = movie_reviews$review[i],
      sentiment = movie_reviews$sentiment[i],
      score = movie_reviews$score[i],
      author = "IMDb user review"
    ))
    selected_local_movie(list(
      movie = list(title = row$title[1], genre = row$genre[1], year = row$year[1],
                   rating = row$rating[1], votes = row$votes[1]),
      reviews = review_rows
    ))
    invisible(NULL)
  }

  observeEvent(input$movie_search, {
    query <- trimws(input$movie_query %||% "")
    if (!nzchar(query)) {
      selected_local_movie(NULL)
      search_results(NULL)
      search_message("Enter a movie title first.")
      return()
    }
    matches <- movies[grepl(tolower(query), tolower(movies$title), fixed = TRUE), , drop = FALSE]
    if (!nrow(matches)) {
      selected_local_movie(NULL)
      search_results(NULL)
      search_message(paste0("No title matching ‘", query, "’ is in this sample. Try a title from Movie explorer."))
      return()
    }
    search_results(matches)
    load_local_movie(matches$id[1])
    search_message(if (nrow(matches) == 1) {
      paste("Showing", matches$title[1], "from the local sample.")
    } else {
      paste(nrow(matches), "titles match. Showing the first result; choose another below.")
    })
  })

  output$movie_picker <- renderUI({
    matches <- search_results()
    if (is.null(matches) || nrow(matches) < 2) return(NULL)
    choices <- stats::setNames(matches$id, paste0(matches$title, " (", matches$year, ")"))
    selectInput("movie_pick", "Choose a matching title", choices = choices, selected = matches$id[1])
  })

  observeEvent(input$movie_pick, {
    req(input$movie_pick)
    load_local_movie(input$movie_pick)
  })

  output$search_status <- renderUI({
    msg <- search_message()
    if (is.null(msg)) return(NULL)
    div(style = "font-size:11px;color:#59716d;margin-top:10px;line-height:1.6;", msg)
  })

  output$local_movie_card <- renderUI({
    result <- selected_local_movie()
    if (is.null(result)) return(NULL)
    movie <- result$movie
    div(class = "panel local-movie",
      div(class = "eyebrow", paste(movie$genre, "·", movie$year)),
      h2(class = "online-title", movie$title),
      div(class = "online-score",
        span(class = "rating-pill", paste0(sprintf("%.1f", movie$rating), " / 10")),
        span(paste(fmt_num(movie$votes), "IMDb ratings")),
        span(paste(length(result$reviews), "reviews in this sample")))
    )
  })

  output$movie_review_summary <- renderUI({
    result <- selected_local_movie()
    if (is.null(result)) return(div("Search a movie to see its review analysis."))
    if (!length(result$reviews)) return(div("No sampled reviews are available for this title."))
    classes <- vapply(result$reviews, `[[`, character(1), "sentiment")
    div(div(class = "online-kpi", div(class = "kpi-value", fmt_pct(mean(classes == "Positive"))),
            div(class = "kpi-foot", "of sampled reviews were classified Positive by the lexicon")),
        div(style = "font-size:10px;color:#829096;margin:7px 0 0;",
            paste(length(classes), "reviews analyzed · Positive / Mixed / Critical")))
  })

  output$movie_sentiment_plot <- renderPlot({
    result <- selected_local_movie()
    req(!is.null(result), length(result$reviews) > 0)
    counts <- table(factor(vapply(result$reviews, `[[`, character(1), "sentiment"),
                           levels = c("Positive", "Mixed", "Critical")))
    sentiment_data <- data.frame(sentiment = names(counts), count = as.integer(counts))
    ggplot(sentiment_data, aes(x = sentiment, y = count, fill = sentiment)) +
      geom_col(width = .62) +
      geom_text(aes(label = count), vjust = -.4, color = "#38515b", size = 3.5) +
      scale_fill_manual(values = c("Positive" = "#347c78", "Mixed" = "#c4a565", "Critical" = "#d78266")) +
      scale_y_continuous(expand = expansion(mult = c(0, .18))) +
      theme_screen() + theme(legend.position = "none", panel.grid.major.y = element_blank())
  })

  output$movie_review_cards <- renderUI({
    result <- selected_local_movie()
    if (is.null(result)) return(div("Search a movie to see its sampled reviews."))
    if (!length(result$reviews)) return(div("No reviews are available for this title."))
    lapply(head(result$reviews, 5), function(r) {
      content <- r$content %||% "Review text not available."
      excerpt <- if (nchar(content) > 440) paste0(substr(content, 1, 440), "…") else content
      div(class = "review-card", div(class = "review-quote", paste0("“", excerpt, "”")),
          div(class = "review-meta", span("IMDb user review"),
              span(class = "review-chip", paste("Lexicon ·", r$sentiment))) )
    })
  })

  filtered_movies <- reactive(movies)
  filtered_reviews <- reactive(reviews)
  theme_screen <- function() theme_minimal(base_family = "sans", base_size = 10) + theme(
    plot.background = element_rect(fill = "#fffefa", color = NA), panel.background = element_rect(fill = "#fffefa", color = NA),
    panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(color = "#eeede7", linewidth = .45),
    axis.title = element_blank(), axis.text = element_text(color = "#879493", size = 9), plot.margin = margin(10, 10, 4, 4),
    legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(color = "#788784", size = 9)
  )

  output$overview_kpis <- renderUI({
    d <- filtered_movies(); r <- filtered_reviews()
    avg <- if (nrow(d)) mean(d$rating) else 0
    pos <- if (nrow(r)) mean(r$sentiment == "Positive") else 0
    cards <- list(c("FILMS IN SAMPLE", fmt_num(nrow(d)), "Included in this local dataset"),
                  c("AVERAGE RATING", sprintf("%.1f", avg), "Audience score · out of 10"),
                  c("AUDIENCE REVIEWS", fmt_num(nrow(r)), "A voice for every point of view"),
                  c("POSITIVE SENTIMENT", fmt_pct(pos), "Share of reviews with a warm response"))
    div(class="kpi-grid", lapply(cards, function(x) div(class="kpi",div(class="kpi-label",x[1]),div(class="kpi-value",x[2]),div(class="kpi-foot",x[3]))))
  })

  output$genre_plot <- renderPlot({
    d <- filtered_movies(); req(nrow(d)>0)
    stats <- d %>% group_by(genre) %>% summarise(rating=mean(rating), .groups="drop") %>%
      left_join(filtered_reviews() %>% group_by(genre) %>% summarise(sentiment=mean(sentiment=="Positive")*100,.groups="drop"),by="genre")
    ggplot(stats,aes(x=reorder(genre,rating),y=rating)) + geom_col(fill="#347c78",width=.62) + geom_text(aes(label=sprintf("%.1f",rating)),hjust=-.35,color="#38515b",size=3.1) + coord_flip(ylim=c(0,10)) + theme_screen()
  })
  output$trend_plot <- renderPlot({
    d <- filtered_movies(); req(nrow(d)>0)
    yearly <- d %>% group_by(year) %>% summarise(rating=mean(rating),.groups="drop")
    ggplot(yearly,aes(year,rating)) + geom_line(color="#347c78",linewidth=1) + geom_point(color="#d78266",size=1.7) + scale_y_continuous(limits=c(4,9),breaks=4:9) + theme_screen()
  })
  output$sentiment_plot <- renderPlot({
    r <- filtered_reviews(); req(nrow(r)>0)
    s <- r %>% count(sentiment) %>% mutate(sentiment=factor(sentiment,levels=c("Positive","Mixed","Critical")))
    ggplot(s,aes(x="",y=n,fill=sentiment)) + geom_col(width=.52,color="#fffefa",linewidth=1) + coord_polar("y") + scale_fill_manual(values=c("Positive"="#347c78","Mixed"="#c4a565","Critical"="#d78266")) + theme_void() + theme(legend.position="bottom",legend.title=element_blank(),legend.text=element_text(size=9,color="#788784"),plot.background=element_rect(fill="#fffefa",color=NA))
  })
  output$top_movies <- renderUI({
    d <- filtered_movies() %>% arrange(desc(rating),desc(votes)) %>% head(5)
    if (!nrow(d)) return(div("The movie sample is empty."))
    tags$table(class="data-table",tags$thead(tags$tr(tags$th("TITLE"),tags$th("GENRE"),tags$th("YEAR"),tags$th("SCORE"))),tags$tbody(lapply(seq_len(nrow(d)),function(i)tags$tr(tags$td(class="movie-name",d$title[i]),tags$td(d$genre[i]),tags$td(d$year[i]),tags$td(span(class="rating-pill",sprintf("%.1f",d$rating[i])))))) )
  })
  output$overview_insight <- renderUI({
    d <- filtered_movies(); r <- filtered_reviews()
    if (!nrow(d)) return(NULL)
    best <- d %>% group_by(genre) %>% summarise(score=mean(rating),n=n(),.groups="drop") %>% filter(n>=3) %>% arrange(desc(score)) %>% head(1)
    if (!nrow(best)) return(NULL)
    div(class="insight",div(class="insight-mark","✳"),div(div(class="insight-label","A note from the data"),div(class="insight-text",paste0("In this selection, ",best$genre," earns the strongest average rating at ",sprintf("%.1f",best$score),"/10. ",if(nrow(r)) paste0(fmt_pct(mean(r$sentiment=="Positive"))," of audience reviews are positive — a useful reminder that a genre's reputation is built one reaction at a time.") else ""))) )
  })
  output$genre_detail <- renderPlot({
    d <- filtered_movies(); req(nrow(d)>0)
    stats <- d %>% group_by(genre) %>% summarise(rating=mean(rating),films=n(),.groups="drop")
    ggplot(stats,aes(x=rating,y=reorder(genre,rating),size=films)) + geom_point(color="#347c78",alpha=.85) + geom_text(aes(label=sprintf("%.1f",rating)),nudge_x=.17,color="#50666a",size=3) + scale_x_continuous(limits=c(4,9.5),breaks=4:9) + scale_size(range=c(3,11),guide="none") + theme_screen()
  })
  output$rating_dist <- renderPlot({
    d <- filtered_movies(); req(nrow(d)>0)
    ggplot(d,aes(x=rating,fill=genre)) + geom_density(alpha=.5,linewidth=.35) + scale_fill_manual(values=rep(c("#347c78","#d78266","#c4a565","#7387a2","#9d7894","#819b7e","#d2a575","#658e95","#bb7968","#899aaf"),4)) + theme_screen()
  })
  output$genre_table <- renderUI({
    d <- filtered_movies(); r <- filtered_reviews()
    s <- d %>% group_by(genre) %>% summarise(films=n(),rating=mean(rating),.groups="drop") %>% left_join(r %>% group_by(genre) %>% summarise(positive=mean(sentiment=="Positive"),.groups="drop"),by="genre") %>% arrange(desc(rating))
    if (!nrow(s)) return(div("The movie sample is empty."))
    tags$table(class="data-table",tags$thead(tags$tr(lapply(c("GENRE","FILMS","AVG SCORE","POSITIVE"),tags$th))),tags$tbody(lapply(seq_len(nrow(s)),function(i)tags$tr(tags$td(class="movie-name",s$genre[i]),tags$td(s$films[i]),tags$td(span(class="rating-pill",sprintf("%.1f",s$rating[i]))),tags$td(ifelse(is.na(s$positive[i]),"—",fmt_pct(s$positive[i])))))))
  })
  output$sentiment_genre <- renderPlot({
    r <- filtered_reviews(); req(nrow(r)>0)
    s <- r %>% count(genre,sentiment) %>% group_by(genre) %>% mutate(share=n/sum(n)) %>% ungroup()
    ggplot(s,aes(x=reorder(genre,share,FUN=mean),y=share,fill=sentiment)) + geom_col(position="fill",width=.68) + coord_flip() + scale_y_continuous(labels=function(x)paste0(x*100,"%")) + scale_fill_manual(values=c("Positive"="#347c78","Mixed"="#c4a565","Critical"="#d78266")) + theme_screen()
  })
  output$review_cards <- renderUI({
    r <- filtered_reviews() %>% arrange(desc(score)) %>% distinct(movie_id,.keep_all=TRUE) %>% head(4)
    if (!nrow(r)) return(div("No reviews in this selection."))
    lapply(seq_len(nrow(r)),function(i)div(class="review-card",div(class="review-quote",paste0("“",r$review[i],"”")),div(class="review-meta",span(r$title[i]),span(class="review-chip",paste(r$sentiment[i],"· polarity",sprintf("%.0f%%",r$score[i]*100))))))
  })
  output$sentiment_time <- renderPlot({
    r <- filtered_reviews(); req(nrow(r)>0)
    s <- r %>% group_by(year) %>% summarise(score=mean(score),.groups="drop")
    ggplot(s,aes(year,score)) + geom_ribbon(aes(ymin=0,ymax=score),fill="#e4efeb",alpha=.8) + geom_line(color="#347c78",linewidth=1) + geom_point(color="#d78266",size=1.5) + scale_y_continuous(limits=c(0,1),labels=function(x)paste0(x*100,"%")) + theme_screen()
  })
  output$movie_table <- renderUI({
    d <- filtered_movies() %>% arrange(desc(rating))
    if (!nrow(d)) return(div("The movie sample is empty."))
    tags$table(class="data-table",tags$thead(tags$tr(lapply(c("DB ID","TITLE","GENRE","YEAR","AUDIENCE RATING","VOTES"),tags$th))),tags$tbody(lapply(seq_len(nrow(d)),function(i)tags$tr(tags$td(d$id[i]),tags$td(class="movie-name",d$title[i]),tags$td(d$genre[i]),tags$td(d$year[i]),tags$td(span(class="rating-pill",sprintf("%.1f",d$rating[i]))),tags$td(fmt_num(d$votes[i]))))))
  })
}

shinyApp(ui, server)
