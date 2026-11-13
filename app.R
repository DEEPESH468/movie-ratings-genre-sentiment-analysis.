library(shiny)
library(ggplot2)
library(dplyr)
library(DBI)
library(RSQLite)
library(jsonlite)

# -----------------------------------------------------------------------------
# SCREEN / SENSE — an interactive movie ratings & review analysis in R
# The included data is generated reproducibly for a self-contained demo.
# Replace make_movies() and make_reviews() with your own CSV imports as needed.
# -----------------------------------------------------------------------------

set.seed(42)

genre_profiles <- data.frame(
  genre = c("Drama", "Comedy", "Thriller", "Romance", "Action", "Sci-Fi", "Animation", "Documentary", "Horror", "Adventure"),
  rating_center = c(7.35, 6.85, 7.10, 6.95, 6.75, 7.20, 7.45, 7.55, 6.35, 7.00),
  vote_center = c(18000, 14500, 22000, 12000, 35000, 28000, 24000, 6500, 16500, 26000),
  sentiment = c(.69, .73, .62, .77, .65, .71, .82, .80, .54, .74),
  stringsAsFactors = FALSE
)

movie_titles <- c(
  "The Quiet Between", "A Map of Tomorrow", "Small Hours", "The Last Lightkeeper", "Blue Sunday",
  "After the Rain", "Paper Moons", "Somewhere, Still", "A Kind of Ordinary", "North of June",
  "The Long Way Home", "All the Bright Rooms", "A Soft Landing", "The Other Shore", "Golden Hour",
  "Nothing Like Before", "The Shape of Us", "Halfway to Morning", "Wildflower Season", "In Good Company",
  "The Far Side of Summer", "Room for Wonder", "A Borrowed Sky", "The Night Archive", "One More Spring",
  "What We Leave Behind", "The Sunday Club", "When We Were Elsewhere", "The Unfinished Story", "Open Water"
)

make_movies <- function(n = 720) {
  genres <- sample(genre_profiles$genre, n, replace = TRUE, prob = c(1.2, 1, 1, .9, 1.1, .9, .65, .45, .8, .85))
  gp <- genre_profiles[match(genres, genre_profiles$genre), ]
  year <- sample(1995:2025, n, replace = TRUE)
  data.frame(
    id = seq_len(n),
    title = paste(sample(movie_titles, n, replace = TRUE), year),
    genre = genres,
    year = year,
    rating = round(pmin(9.8, pmax(2.1, rnorm(n, gp$rating_center, .82))), 1),
    votes = pmax(210, round(rlnorm(n, log(gp$vote_center), .78))),
    stringsAsFactors = FALSE
  )
}

review_bits <- list(
  positive = c("beautifully made", "quietly moving", "the performances feel effortless", "a wonderful surprise", "stays with you long after", "visually stunning", "genuinely funny", "a lovely story", "the pacing is perfect", "warm and thoughtful"),
  negative = c("hard to stay invested", "the story feels predictable", "a little too long", "the characters feel thin", "the ending misses the mark", "uneven pacing", "not as memorable as it should be", "a disappointing sequel", "the dialogue feels forced", "beautiful but emotionally distant"),
  mixed = c("an interesting idea with uneven execution", "some lovely moments, though it loses its way", "the cast is strong but the story drifts", "not perfect, but there is plenty to like", "a promising start with a quieter finish", "good intentions and a few rough edges", "visually rich, if a little slow", "the ending may divide audiences", "a mixed bag with real highlights", "worth a watch, with reservations")
)

make_reviews <- function(movies, per_movie = 3L) {
  movie_i <- rep(seq_len(nrow(movies)), each = per_movie)
  n <- length(movie_i)
  sentiment_class <- sample(c("Positive", "Mixed", "Critical"), n, TRUE, c(.59, .27, .14))
  sentences <- vapply(seq_len(n), function(i) {
    bucket <- switch(sentiment_class[i], Positive = "positive", Mixed = "mixed", Critical = "negative")
    paste(review_bits[[bucket]][sample.int(length(review_bits[[bucket]]), 1)],
          "", "A", tolower(movies$genre[movie_i[i]]), "that knows its audience.")
  }, character(1))
  data.frame(movie_id = movies$id[movie_i], review = sentences,
             sentiment = sentiment_class,
             score = ifelse(sentiment_class == "Positive", runif(n, .68, .98),
                            ifelse(sentiment_class == "Critical", runif(n, .04, .34), runif(n, .38, .64))),
             stringsAsFactors = FALSE)
}

seed_movies <- make_movies()
seed_reviews <- make_reviews(seed_movies)

# SQLite is the persistent data layer. The demo catalog is inserted only when
# the database is first created; future launches keep imported records.
db_file <- file.path(getwd(), "movie_insights.sqlite")
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

tmdb_request <- function(path, token, query = list()) {
  query_string <- if (length(query)) {
    paste0("?", paste(vapply(names(query), function(key) {
      paste0(utils::URLencode(key, reserved = TRUE), "=", utils::URLencode(as.character(query[[key]]), reserved = TRUE))
    }, character(1)), collapse = "&"))
  } else ""
  url <- paste0("https://api.themoviedb.org/3", path, query_string)
  body_file <- tempfile(fileext = ".json")
  on.exit(unlink(body_file), add = TRUE)
  utils::download.file(url, body_file, method = "libcurl", mode = "wb", quiet = TRUE,
                       headers = c(Authorization = paste("Bearer", token)))
  response <- jsonlite::fromJSON(paste(readLines(body_file, warn = FALSE), collapse = ""), simplifyVector = FALSE)
  if (!is.null(response$status_code) && response$status_code >= 400) {
    stop(response$status_message %||% paste("TMDB returned", response$status_code))
  }
  response
}

score_review <- function(review_text) {
  positive <- c("amazing", "beautiful", "best", "brilliant", "charming", "enjoyed", "excellent", "fantastic", "fun", "funny", "good", "great", "impressive", "love", "loved", "masterpiece", "moving", "perfect", "powerful", "strong", "wonderful")
  negative <- c("annoying", "awful", "bad", "boring", "confusing", "disappointing", "dull", "fail", "flat", "forgettable", "hate", "hated", "mess", "poor", "predictable", "slow", "terrible", "weak", "waste", "worst")
  words <- tolower(unlist(strsplit(gsub("[^[:alpha:]']", " ", review_text), "\\s+")))
  pos <- sum(words %in% positive)
  neg <- sum(words %in% negative)
  if (pos > neg) "Positive" else if (neg > pos) "Critical" else "Mixed"
}

tmdb_server_token <- function() {
  token <- Sys.getenv("TMDB_API_READ_ACCESS_TOKEN", unset = "")
  if (!nzchar(trimws(token))) NULL else trimws(token)
}

ui <- fluidPage(
  tags$head(
    tags$title("SCREEN / SENSE — Movie audience intelligence"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
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
      .online-movie { display:grid;grid-template-columns:180px 1fr;gap:22px;align-items:start; }
      .online-poster { width:100%;max-width:180px;border-radius:4px;box-shadow:0 8px 22px rgba(21,42,52,.14); }
      .online-title { font-family:"Playfair Display",serif;font-size:28px;font-weight:500;margin:4px 0 12px;color:var(--ink); }
      .online-score { display:flex;gap:10px;align-items:center;margin-bottom:14px;color:#829096;font-family:"DM Mono",monospace;font-size:10px; }
      .online-overview { color:#52636a;font-size:13px;line-height:1.75;max-width:820px; }
      .online-kpi .kpi-value { font-size:24px;color:var(--teal); }
      .tmdb-credit {display:flex;align-items:center;gap:16px;color:#758387;font-size:10px;line-height:1.7;}
      .tmdb-credit img {width:44px;height:44px;object-fit:contain;}
      .tmdb-credit strong {color:#52636a;}
      .data-table { width:100%;border-collapse:collapse;font-size:11px; }
      .data-table th { color:#849294;font-family:"DM Mono",monospace;font-size:9px;letter-spacing:.9px;text-align:left;text-transform:uppercase;padding:10px;border-bottom:1px solid var(--line); }
      .data-table td { padding:10px;border-bottom:1px solid #efeee9;color:#52636a; }
      .data-table tr:last-child td { border:0; }
      .data-table .movie-name { color:var(--ink);font-weight:600; }
      .rating-pill { background:#edf3ee;color:var(--teal);border-radius:3px;padding:3px 7px;font-family:"DM Mono",monospace; }
      .footer { border-top:1px solid var(--line);margin-top:24px;padding-top:15px;color:#99a3a2;font-family:"DM Mono",monospace;font-size:9px;letter-spacing:.6px;display:flex;justify-content:space-between; }
      .shiny-input-container { width:100%; }
      @media(max-width:1000px) { .shell{grid-template-columns:210px 1fr}.main{padding:27px 24px}.topbar{padding:0 24px}.chart-row{grid-template-columns:1fr}.kpi-grid{grid-template-columns:repeat(2,1fr)} }
      @media(max-width:650px) { .shell{display:block}.sidebar{border-right:0;border-bottom:1px solid var(--line);padding:20px}.main{padding:23px 16px}.topbar{padding:0 16px;height:60px}.top-meta{display:none}h1{font-size:29px}.hero{display:block}.period-tag{display:inline-block;margin-top:15px}.kpi-grid{gap:8px}.kpi{padding:14px;min-height:95px}.kpi-value{font-size:24px}.chart-row,.chart-row.equal{grid-template-columns:1fr}.nav-tabs>li>a{padding:11px 9px;font-size:11px}.footer{display:block;line-height:1.8}.online-movie{grid-template-columns:95px 1fr;gap:14px}.online-title{font-size:22px}.online-poster{width:95px}.tmdb-credit{align-items:flex-start} }
    '))
  ),
  div(class = "topbar",
      div(class = "brand", div(class = "brand-mark", "S"), span("SCREEN / SENSE")),
      div(class = "top-meta", "AUDIENCE INTELLIGENCE  ·  R ANALYTICS")),
  div(class = "shell",
      div(class = "sidebar",
          div(class = "side-kicker", "Refine your view"),
          div(class = "filter-label", "Release year"),
          sliderInput("year", NULL, min = 1995, max = 2025, value = c(2005, 2025), sep = ""),
          div(class = "filter-label", "Genre"),
          selectInput("genre", NULL, choices = c("All genres", sort(unique(movies$genre))), selected = "All genres"),
          div(class = "filter-label", "Minimum rating"),
          sliderInput("rating", NULL, min = 2, max = 10, value = 4.0, step = .5),
          div(class = "filter-label", "Add your data"),
          selectInput("upload_type", NULL, choices = c("Movies CSV" = "movies", "Reviews CSV" = "reviews")),
          fileInput("upload_csv", NULL, accept = ".csv", buttonLabel = "Choose CSV", placeholder = "No file selected"),
          actionButton("import_csv", "Import into database", class = "btn btn-default btn-sm"),
          uiOutput("import_status"),
          div(class = "filter-note", tags$strong("The dataset"), "A reproducible, self-contained sample of 720 films and 2,160 audience reviews. Explore the analysis, then swap in your own movie data.")),
      div(class = "main",
          div(class = "hero",
              div(div(class = "eyebrow", "THE AUDIENCE, IN THEIR OWN WORDS"),
                  h1("What makes a movie\n"), tags$span(style="font-family:'Playfair Display',serif;font-size:34px;font-weight:500;letter-spacing:-.6px;line-height:1.12;color:#347c78;", "stay with us?"),
                  div(class = "hero-sub", "Ratings, genres & the feeling behind every review.")),
              div(class = "period-tag", "●  LIVE ANALYSIS  /  01")),
          tabsetPanel(id = "tabs", type = "tabs", selected = "online",
              tabPanel("Live movie lookup", value = "online",
                  div(class="panel",div(class="panel-head",div(div(class="panel-title","Search the real movie catalog"),div(class="panel-sub","Look up a title online, then analyze its audience reviews with an R sentiment lexicon.")),div(class="panel-unit","LIVE · TMDB")),
                      textInput("tmdb_query", "Movie title", placeholder="Try: Everything Everywhere All at Once"),
                      actionButton("tmdb_search", "Search TMDB", class="btn btn-default btn-sm"),
                      tags$small(style="display:block;color:#829096;margin-top:10px;line-height:1.6;", "Search by title and choose the right release. API access is configured once by the app owner; visitors do not need a token."),
                      uiOutput("tmdb_status")),
                  uiOutput("tmdb_picker"),uiOutput("online_movie_card"),
                  div(class="chart-row equal",div(class="panel",div(class="panel-head",div(div(class="panel-title","Review sentiment"),div(class="panel-sub","R lexicon estimate · up to 60 TMDB reviews"))),uiOutput("online_review_summary"),div(class="plot-box",plotOutput("online_sentiment_plot",height="100%"))),
                      div(class="panel",div(class="panel-head",div(div(class="panel-title","Audience reviews"),div(class="panel-sub","Recent reviews returned by TMDB"))),uiOutput("online_review_cards"))),
                  div(class="panel tmdb-credit",tags$img(src="https://www.themoviedb.org/assets/2/v4/logos/v2/blue_square_2-d537fb228cf3ded904ef09b136fe3fec72548ebc1fea3fbbd1ad9e36364db38b.svg",alt="TMDB logo"),
                      div(tags$a(href="https://www.themoviedb.org/",target="_blank","The Movie Database (TMDB)")," provides the movie metadata, ratings, artwork and reviews in this view.",tags$br(),tags$strong("This product uses the TMDB API but is not endorsed or certified by TMDB.")))),
              tabPanel("Overview", value = "overview",
                  uiOutput("overview_kpis"),
                  div(class = "chart-row",
                      div(class = "panel", div(class="panel-head",div(div(class="panel-title","The genre landscape"),div(class="panel-sub","Average audience rating and review sentiment by genre")),div(class="panel-unit","RATING / 10")),div(class="plot-box",plotOutput("genre_plot",height="100%"))),
                      div(class = "panel", div(class="panel-head",div(div(class="panel-title","A changing audience"),div(class="panel-sub","Ratings and review sentiment across release years")),div(class="panel-unit","YEARLY TREND")),div(class="plot-box",plotOutput("trend_plot",height="100%")))),
                  uiOutput("overview_insight"),
                  div(class = "chart-row equal",
                      div(class="panel",div(class="panel-head",div(div(class="panel-title","How audiences feel"),div(class="panel-sub","Share of reviews in each sentiment group"))),div(class="plot-box",plotOutput("sentiment_plot",height="100%"))),
                      div(class="panel",div(class="panel-head",div(div(class="panel-title","The films finding their audience"),div(class="panel-sub","Highest-rated titles in the current selection"))),uiOutput("top_movies")))),
              tabPanel("Genre study", value = "genres",
                  div(class="panel",div(class="panel-head",div(div(class="panel-title","A closer look at genre"),div(class="panel-sub","Mean rating, review tone and audience reach for every genre")),div(class="panel-unit","COMPARATIVE VIEW")),div(class="plot-box tall",plotOutput("genre_detail",height="100%"))),
                  div(class="chart-row equal",div(class="panel",div(class="panel-head",div(div(class="panel-title","Rating distribution"),div(class="panel-sub","Where each genre's scores tend to land"))),div(class="plot-box",plotOutput("rating_dist",height="100%"))),div(class="panel",div(class="panel-head",div(div(class="panel-title","Genre scorecard"),div(class="panel-sub","A compact read on audience response"))),uiOutput("genre_table")))),
              tabPanel("Audience voice", value = "voice",
                  div(class="chart-row equal",div(class="panel",div(class="panel-head",div(div(class="panel-title","Sentiment by genre"),div(class="panel-sub","Positive, mixed and critical review share"))),div(class="plot-box tall",plotOutput("sentiment_genre",height="100%"))),div(class="panel",div(class="panel-head",div(div(class="panel-title","What viewers are saying"),div(class="panel-sub","A sample of audience language in this selection"))),uiOutput("review_cards"))),
                  div(class="panel",div(class="panel-head",div(div(class="panel-title","Review tone over time"),div(class="panel-sub","Average sentiment score by release year"))),div(class="plot-box",plotOutput("sentiment_time",height="100%")))),
              tabPanel("Movie explorer", value = "movies",
                  div(class="panel",div(class="panel-head",div(div(class="panel-title","Find a film"),div(class="panel-sub","Search the titles in your current selection"))),textInput("search",NULL,placeholder="Type a movie title or genre..."),uiOutput("movie_table")))) ,
          div(class="footer",span("SCREEN / SENSE  ·  MOVIE RATINGS & GENRE SENTIMENT ANALYSIS"),span("BUILT WITH R  /  SHINY  /  GGPLOT2"))
      )
  )
)

server <- function(input, output, session) {
  onStop(function() if (dbIsValid(db)) dbDisconnect(db), session = session)
  tmdb_results <- reactiveVal(NULL)
  online_movie <- reactiveVal(NULL)
  tmdb_message <- reactiveVal("Enter a movie title and a TMDB API token to search the live catalog.")
  last_loaded_id <- reactiveVal(NULL)
  load_online_movie <- function(movie_id, token) {
    detail <- tmdb_request(paste0("/movie/", movie_id), token, list(language = "en-US"))
    review_response <- tmdb_request(paste0("/movie/", movie_id, "/reviews"), token, list(language = "en-US", page = 1))
    review_rows <- review_response$results %||% list()
    pages <- min(as.integer(review_response$total_pages %||% 1), 3L)
    if (pages > 1L) for (page in 2:pages) {
      more <- tmdb_request(paste0("/movie/", movie_id, "/reviews"), token, list(language = "en-US", page = page))
      review_rows <- c(review_rows, more$results %||% list())
    }
    if (length(review_rows)) review_rows <- lapply(review_rows, function(r) {
      r$sentiment <- score_review(r$content %||% "")
      r
    })
    online_movie(list(movie = detail, reviews = review_rows))
    last_loaded_id(as.character(movie_id))
    tmdb_message(paste("Live TMDB data ·", length(review_rows), "reviews analyzed by the R word lexicon."))
  }
  observeEvent(input$tmdb_search, {
    token <- tmdb_server_token()
    query <- trimws(input$tmdb_query %||% "")
    if (is.null(token)) { tmdb_message("Live search is not configured by the app owner yet. No token is needed from movie search users."); return() }
    if (!nzchar(query)) { tmdb_message("Enter a movie title first."); return() }
    tmdb_message("Searching TMDB…")
    online_movie(NULL)
    last_loaded_id(NULL)
    tryCatch({
      response <- tmdb_request("/search/movie", token, list(query = query, include_adult = "false", language = "en-US"))
      results <- response$results %||% list()
      if (!length(results)) {
        tmdb_results(NULL)
        tmdb_message(paste0("No movie matches found for ‘", query, "’. Try another title."))
      } else {
        tmdb_results(results)
        labels <- vapply(results, function(m) paste0(m$title, " (", ifelse(nzchar(m$release_date %||% ""), substr(m$release_date, 1, 4), "year unknown"), ")"), character(1))
        choices <- stats::setNames(vapply(results, function(m) as.character(m$id), character(1)), labels)
        updateSelectInput(session, "tmdb_pick", choices = choices, selected = as.character(results[[1]]$id))
        load_online_movie(results[[1]]$id, token)
      }
    }, error = function(e) tmdb_message(paste("TMDB search failed:", conditionMessage(e))))
  })
  output$tmdb_status <- renderUI({
    msg <- tmdb_message()
    if (is.null(msg)) return(NULL)
    div(style="font-size:11px;color:#59716d;margin-top:10px;line-height:1.6;",msg)
  })
  output$tmdb_picker <- renderUI({
    results <- tmdb_results()
    if (is.null(results) || !length(results)) return(NULL)
    choices <- stats::setNames(vapply(results, function(m) as.character(m$id), character(1)),
      vapply(results, function(m) paste0(m$title, " · ", ifelse(nzchar(m$release_date %||% ""), substr(m$release_date, 1, 4), "year unknown")), character(1)))
    selectInput("tmdb_pick", "Choose the matching release", choices = choices,
      selected = as.character(results[[1]]$id))
  })
  observeEvent(input$tmdb_pick, {
    req(input$tmdb_pick)
    token <- tmdb_server_token()
    if (is.null(token)) { tmdb_message("Live search is not configured by the app owner yet."); return() }
    if (identical(as.character(input$tmdb_pick), last_loaded_id())) return()
    tmdb_message("Loading movie details and reviews…")
    online_movie(NULL)
    tryCatch(load_online_movie(input$tmdb_pick, token),
      error = function(e) tmdb_message(paste("Could not load this movie:", conditionMessage(e))))
  })
  output$online_movie_card <- renderUI({
    result <- online_movie()
    if (is.null(result)) return(NULL)
    movie <- result$movie
    poster <- if (!is.null(movie$poster_path) && nzchar(movie$poster_path)) paste0("https://image.tmdb.org/t/p/w500", movie$poster_path) else NULL
    genres <- if (length(movie$genres)) paste(vapply(movie$genres, `[[`, character(1), "name"), collapse = " · ") else "Genre not listed"
    release_year <- if (nzchar(movie$release_date %||% "")) substr(movie$release_date, 1, 4) else "Release year not listed"
    div(class="panel online-movie",
      if (!is.null(poster)) tags$img(class="online-poster",src=poster,alt=paste(movie$title,"poster")),
      div(div(class="eyebrow",paste(genres," · ",release_year)),h2(class="online-title",movie$title),
        div(class="online-score",span(class="rating-pill",paste0(sprintf("%.1f",movie$vote_average %||% 0)," / 10")),span(paste(fmt_num(movie$vote_count %||% 0),"TMDB ratings"))),
        div(class="online-overview",movie$overview %||% "No synopsis available."),
        div(class="panel-sub",paste("TMDB movie ID",movie$id,"· Original language",toupper(movie$original_language %||% "unknown")))))
  })
  output$online_review_summary <- renderUI({
    result <- online_movie()
    if (is.null(result)) return(div("Search for a title to see its review analysis."))
    if (!length(result$reviews)) return(div("TMDB returned no audience reviews for this title."))
    classes <- vapply(result$reviews, `[[`, character(1), "sentiment")
    div(div(class="online-kpi",div(class="kpi-value",fmt_pct(mean(classes == "Positive"))),div(class="kpi-foot","of fetched reviews read positive in the R lexicon scan")),
      div(style="font-size:10px;color:#829096;margin:7px 0 0;",paste(length(classes),"reviews scanned · Positive / Mixed / Critical word counts")))
  })
  output$online_sentiment_plot <- renderPlot({
    result <- online_movie(); req(!is.null(result)); req(length(result$reviews) > 0)
    counts <- table(factor(vapply(result$reviews, `[[`, character(1), "sentiment"), levels=c("Positive","Mixed","Critical")))
    sentiment_data <- data.frame(sentiment=names(counts), count=as.integer(counts))
    ggplot(sentiment_data,aes(x=sentiment,y=count,fill=sentiment)) + geom_col(width=.62) + geom_text(aes(label=count),vjust=-.4,color="#38515b",size=3.5) + scale_fill_manual(values=c("Positive"="#347c78","Mixed"="#c4a565","Critical"="#d78266")) + scale_y_continuous(expand=expansion(mult=c(0,.18))) + theme_screen() + theme(legend.position="none",panel.grid.major.y=element_blank())
  })
  output$online_review_cards <- renderUI({
    result <- online_movie()
    if (is.null(result)) return(div("Search for a movie to load audience reviews."))
    if (!length(result$reviews)) return(div("No TMDB reviews are available for this title."))
    lapply(head(result$reviews,5),function(r) {
      content <- r$content %||% "Review text not available."
      excerpt <- if (nchar(content) > 440) paste0(substr(content,1,440),"…") else content
      div(class="review-card",div(class="review-quote",paste0("“",excerpt,"”")),div(class="review-meta",span(r$author %||% "TMDB member"),span(class="review-chip",paste("R lexicon ·",r$sentiment))))
    })
  })
  data_version <- reactiveVal(0L)
  import_message <- reactiveVal(NULL)
  observeEvent(input$import_csv, {
    req(input$upload_csv)
    tryCatch({
      incoming <- read.csv(input$upload_csv$datapath, stringsAsFactors = FALSE, check.names = FALSE)
      if (identical(input$upload_type, "movies")) {
        required <- c("title", "genre", "year", "rating", "votes")
        missing <- setdiff(required, names(incoming))
        if (length(missing)) stop(paste("Movies CSV is missing:", paste(missing, collapse = ", ")))
        incoming <- incoming %>% select(all_of(required))
        incoming$year <- as.integer(incoming$year)
        incoming$rating <- as.numeric(incoming$rating)
        incoming$votes <- as.integer(incoming$votes)
        if (anyNA(incoming[, c("title", "genre", "year", "rating", "votes")])) stop("Movie fields must be filled with valid values.")
        dbWriteTable(db, "movies", incoming, append = TRUE, row.names = FALSE)
        movies <<- load_movies()
        updateSelectInput(session, "genre", choices = c("All genres", sort(unique(movies$genre))))
        updateSliderInput(session, "year", min = min(movies$year), max = max(movies$year), value = range(c(input$year, movies$year)))
        import_message(paste(nrow(incoming), "movies added to SQLite."))
      } else {
        required <- c("movie_id", "review", "sentiment", "score")
        missing <- setdiff(required, names(incoming))
        if (length(missing)) stop(paste("Reviews CSV is missing:", paste(missing, collapse = ", ")))
        incoming <- incoming %>% select(all_of(required))
        incoming$movie_id <- as.integer(incoming$movie_id)
        incoming$score <- as.numeric(incoming$score)
        if (anyNA(incoming[, required])) stop("Review fields must be filled with valid values.")
        unknown <- setdiff(unique(incoming$movie_id), movies$id)
        if (length(unknown)) stop(paste("Unknown movie_id values:", paste(head(unknown, 8), collapse = ", ")))
        if (any(!incoming$sentiment %in% c("Positive", "Mixed", "Critical"))) stop("sentiment must be Positive, Mixed, or Critical.")
        if (any(incoming$score < 0 | incoming$score > 1)) stop("score must be between 0 and 1.")
        dbWriteTable(db, "reviews", incoming, append = TRUE, row.names = FALSE)
        reviews <<- load_reviews()
        import_message(paste(nrow(incoming), "reviews added to SQLite."))
      }
      data_version(data_version() + 1L)
    }, error = function(e) import_message(paste("Import failed:", conditionMessage(e))))
  })
  output$import_status <- renderUI({
    msg <- import_message()
    if (is.null(msg)) return(NULL)
    div(style = "font-size:10px;color:#347c78;margin:7px 0;line-height:1.5;", msg)
  })
  filtered_movies <- reactive({
    data_version()
    d <- movies %>% filter(year >= input$year[1], year <= input$year[2], rating >= input$rating)
    if (!is.null(input$genre) && input$genre != "All genres") d <- d %>% filter(genre == input$genre)
    d
  })
  filtered_reviews <- reactive({ reviews %>% semi_join(filtered_movies(), by = c("movie_id" = "id")) })
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
    cards <- list(c("FILMS IN VIEW", fmt_num(nrow(d)), "Across the selected release window"),
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
    if (!nrow(d)) return(div("No films match these filters."))
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
    if (!nrow(s)) return(div("No films match these filters."))
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
    lapply(seq_len(nrow(r)),function(i)div(class="review-card",div(class="review-quote",paste0("“",r$review[i],"”")),div(class="review-meta",span(r$title[i]),span(class="review-chip",paste(r$sentiment[i],"·",sprintf("%.0f%%",r$score[i]*100))))))
  })
  output$sentiment_time <- renderPlot({
    r <- filtered_reviews(); req(nrow(r)>0)
    s <- r %>% group_by(year) %>% summarise(score=mean(score),.groups="drop")
    ggplot(s,aes(year,score)) + geom_ribbon(aes(ymin=0,ymax=score),fill="#e4efeb",alpha=.8) + geom_line(color="#347c78",linewidth=1) + geom_point(color="#d78266",size=1.5) + scale_y_continuous(limits=c(0,1),labels=function(x)paste0(x*100,"%")) + theme_screen()
  })
  output$movie_table <- renderUI({
    d <- filtered_movies(); q <- trimws(if (is.null(input$search)) "" else input$search)
    if (nzchar(q)) d <- d %>% filter(grepl(q,title,ignore.case=TRUE) | grepl(q,genre,ignore.case=TRUE))
    d <- d %>% arrange(desc(rating)) %>% head(60)
    if (!nrow(d)) return(div("No films found. Try a different title or adjust the filters."))
    tags$table(class="data-table",tags$thead(tags$tr(lapply(c("DB ID","TITLE","GENRE","YEAR","AUDIENCE RATING","VOTES"),tags$th))),tags$tbody(lapply(seq_len(nrow(d)),function(i)tags$tr(tags$td(d$id[i]),tags$td(class="movie-name",d$title[i]),tags$td(d$genre[i]),tags$td(d$year[i]),tags$td(span(class="rating-pill",sprintf("%.1f",d$rating[i]))),tags$td(fmt_num(d$votes[i]))))))
  })
}

shinyApp(ui, server)
