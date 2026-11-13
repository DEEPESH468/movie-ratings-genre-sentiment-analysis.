# SCREEN / SENSE

An R full stack project for exploring movie ratings, genres, and audience review sentiment. The first tab searches the live TMDB catalog by movie title, retrieves the selected film's metadata and available audience reviews, and analyzes review tone in R. The remaining dashboard tabs explore a local SQLite sample catalog for genre comparisons.

## Run the project

Install R, then install the packages used by the app:

```r
install.packages(c("shiny", "ggplot2", "dplyr", "DBI", "RSQLite", "jsonlite"))
```

From this folder, start the application with:

```r
shiny::runApp()
```

Or open `app.R` in RStudio and click **Run App**.

## What you can explore

- Filter the catalog by release year, genre, and minimum audience rating.
- Search TMDB by movie title, choose the right release, and view its overview, genres, audience rating and reviews.
- Run an R lexicon sentiment estimate on up to 60 available TMDB reviews for the selected film.
- Compare average ratings and review tone across genres.
- Follow rating and sentiment patterns over time.
- Browse review excerpts and search the selected movie catalog.

## Data and analysis

The **Live movie lookup** tab uses TMDB's title search, movie details, and review API endpoints. Visitors only enter a title; the app owner configures the TMDB API Read Access Token once on the server as `TMDB_API_READ_ACCESS_TOKEN` (for example in a private `.Renviron` file). The app never displays or stores the token in SQLite. TMDB requires application authentication for API requests; there is no official keyless endpoint for its live movie reviews. The app fetches metadata and reviews when a title is searched; the TMDB rating is its published average and vote count, not an independent rating study. Results depend on what TMDB has for that title, and some movies have no written reviews. The review sentiment is an explainable, small English word-lexicon estimate, not a trained sentiment model; sarcasm, negation, and non-English reviews can be misclassified.

The local dashboard also creates `movie_insights.sqlite` on first launch and seeds it with a deterministic demonstration catalog of 720 generated films and 2,160 generated reviews. Those rows exist to demonstrate multi-film genre comparisons and are not real movie records or audience research. The database persists imported CSV data across restarts.

Use **Add your data** in the sidebar to append CSV data to SQLite. A movies CSV must have `title`, `genre`, `year`, `rating`, and `votes` columns. A reviews CSV must have `movie_id`, `review`, `sentiment`, and `score` columns. Sentiment must be `Positive`, `Mixed`, or `Critical`, and score must be between 0 and 1. Import films first. The Movie Explorer shows each record's database ID; use that ID as `movie_id` when attaching reviews. Sentiment groups in the starter data are generated labels; for real reviews, add and document your sentiment method before interpreting the results.

The database file is local and untracked data: back it up before deleting it. Removing it resets the app to its starter dataset the next time the app launches.

TMDB asks non-commercial API users to attribute the source and show its logo. This product uses the TMDB API but is not endorsed or certified by TMDB. See TMDB's [API FAQ and attribution rules](https://developer.themoviedb.org/docs/faq).

## Stack

- **R + Shiny** for the interactive frontend, API calls, and server-side analysis
- **SQLite + DBI/RSQLite** for persistent movies and reviews tables
- **TMDB API** for live title search, movie metadata, ratings, artwork, and audience reviews
- **jsonlite** for decoding API responses
- **dplyr** for data preparation and summaries
- **ggplot2** for statistical visualizations
- Custom CSS for the responsive dashboard interface
