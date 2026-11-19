# SCREEN / SENSE

An R Shiny project for analyzing movie ratings, genres, and audience review sentiment. Search the included local movie dataset by title to see its rating, sampled reviews, and sentiment analysis. Dashboard tabs summarize the complete sample.

## Run the project

Install R, then install the packages used by the app:

```r
install.packages(c("shiny", "ggplot2", "dplyr", "DBI", "RSQLite", "jsonlite"))
```

From this folder, prepare the local sample data once, then start the application:

```r
source("scripts/prepare_sample_data.R")
shiny::runApp()
```

The preparation script downloads the source files and writes `data/movies.csv` and `data/reviews.csv`. These local files and the SQLite database are ignored by Git. The script uses a fixed random seed, so it selects the same review sample each time.

Alternatively, in a terminal:

```r
Rscript scripts/prepare_sample_data.R
Rscript -e 'shiny::runApp()'
```

Or open `app.R` in RStudio and click **Run App**.

## What you can explore

- Search locally by movie title (for example, `Parasite`) and see its rating, review sentiment, and review excerpts. Type a title in **Find a movie** and click **Search movies** (or press Enter).
- Analyze a reproducible sample of 200 real IMDb films, with up to 30 user reviews per film.
- Compare average ratings and review tone across genres.
- Follow rating and sentiment patterns over time.
- Browse the complete movie catalog in **Movie explorer**.

## Data and analysis

The local sample comes from the [MansaT IMDb Movie Dataset](https://huggingface.co/datasets/MansaT/Movie-Dataset), which contains IMDb movie metadata and user-written reviews. Its dataset card declares an MIT license; the source text consists of IMDb user reviews, so check upstream terms before redistributing review text. The preparation script pins dataset revision `68771d0e2dacbbdc76f202158bffb7e69e2b52da`, selects 200 titles across available primary genres, reads their IMDb ratings, vote counts, years, and genres, and samples up to 30 reviews per film with a fixed random seed. This selected sample is not representative of all movies or viewers. The project is for classroom demonstration and is not affiliated with or endorsed by IMDb. Downloaded source files are kept local and are not tracked in Git. When the app starts, it adds newly prepared movies and their reviews to the existing SQLite database while preserving previously imported rows.

The sentiment estimate is a transparent lexicon baseline implemented in `R/sentiment.R`. It tokenizes English review text, counts matches to fixed positive and negative word lists, and flips a matched word if a negator occurs within the prior three tokens. Polarity is `(positive matches - negative matches) / all matched words`, mapped to 0–100%. Scores at or above 57.5% are labeled Positive, scores at or below 42.5% Critical, and scores between them Mixed. Text with no lexicon matches receives a neutral 50% score and Mixed label. This is an explainable heuristic rather than a trained or validated model; sarcasm, context, intensifiers, non-English text, and lexicon gaps can lead to errors.

The SQLite file `movie_insights_expanded.sqlite` is local and untracked. It loads the prepared sample when first created and adds newly prepared movies on later starts while keeping existing rows. Back it up before deleting it.

## Stack

- **R + Shiny** for the interactive frontend and server-side analysis
- **SQLite + DBI/RSQLite** for persistent movies and reviews tables
- **dplyr** for data preparation and summaries
- **ggplot2** for statistical visualizations
- Custom CSS for the responsive dashboard interface
