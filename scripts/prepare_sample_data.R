# Download a reproducible 50-film sample from MansaT/Movie-Dataset.
# The source files contain IMDb user reviews and movie metadata; see README.md.

prepare_sample_data <- function() {
options(stringsAsFactors = FALSE)
source(file.path("R", "sentiment.R"))
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Install jsonlite before preparing the movie sample.")
}

source_revision <- "68771d0e2dacbbdc76f202158bffb7e69e2b52da"
base_url <- paste0("https://huggingface.co/datasets/MansaT/Movie-Dataset/resolve/", source_revision, "/")
genres <- c(
  "Action", "Adventure", "Animation", "Biography", "Comedy", "Crime", "Drama",
  "Fantasy", "History", "Horror", "Music", "Mystery", "Romance", "Sci-Fi",
  "Sport", "Thriller", "War"
)
sample_movie_count <- 50L
review_limit <- 30L
set.seed(20261118)
download_dir <- tempfile("screen-sense-source-")
dir.create(download_dir)
on.exit(unlink(download_dir, recursive = TRUE), add = TRUE)

download_csv <- function(relative_path, destination) {
  url <- paste0(base_url, utils::URLencode(relative_path, reserved = FALSE))
  utils::download.file(url, destination, mode = "wb", quiet = TRUE, method = "libcurl")
  if (!file.exists(destination) || file.info(destination)$size == 0) {
    stop("Could not download source file: ", relative_path)
  }
}

metadata <- lapply(genres, function(genre) {
  relative <- paste0("data/1_movies_per_genre/", genre, ".csv")
  destination <- file.path(download_dir, paste0("metadata-", gsub("[^A-Za-z]", "", genre), ".csv"))
  download_csv(relative, destination)
  read.csv(destination, stringsAsFactors = FALSE, check.names = FALSE)
})
metadata <- unique(do.call(rbind, metadata))
metadata$year <- suppressWarnings(as.integer(metadata$year))
metadata <- metadata[!duplicated(paste(metadata$name, metadata$year)), , drop = FALSE]

tree_file <- file.path(download_dir, "review-files.json")
tree_url <- paste0("https://huggingface.co/api/datasets/MansaT/Movie-Dataset/tree/",
                   source_revision, "/data/2_reviews_per_movie_raw?recursive=true&expand=false&limit=1000")
utils::download.file(tree_url, tree_file, mode = "wb", quiet = TRUE, method = "libcurl")
review_tree <- jsonlite::fromJSON(tree_file)
review_paths <- review_tree$path[grepl("^data/2_reviews_per_movie_raw/.* [0-9]{4}\\.csv$", review_tree$path)]
review_files <- sub("^.*/", "", review_paths)
available <- data.frame(
  title = sub(" [0-9]{4}\\.csv$", "", review_files),
  year = as.integer(sub("^.* ([0-9]{4})\\.csv$", "\\1", review_files)),
  file = review_paths,
  stringsAsFactors = FALSE
)
catalog <- merge(metadata, available, by.x = c("name", "year"), by.y = c("title", "year"))
if (nrow(catalog) < sample_movie_count) {
  stop("Only found ", nrow(catalog), " movies with review files; expected at least ", sample_movie_count, ".")
}
catalog$primary_genre <- sub(";.*$", "", catalog$genres)
catalog$votes <- suppressWarnings(as.integer(catalog$num_raters))
catalog$num_reviews <- suppressWarnings(as.integer(catalog$num_reviews))
catalog$rating <- suppressWarnings(as.numeric(catalog$rating))
catalog <- catalog[!is.na(catalog$votes) & !is.na(catalog$rating) & !is.na(catalog$primary_genre), , drop = FALSE]
catalog <- catalog[order(-catalog$votes, -catalog$num_reviews, catalog$name), , drop = FALSE]

# Round-robin across primary genres, favoring titles with more source votes.
genre_levels <- sort(unique(catalog$primary_genre))
selected_rows <- integer(0)
while (length(selected_rows) < sample_movie_count) {
  added <- FALSE
  for (genre in genre_levels) {
    eligible <- which(catalog$primary_genre == genre & !(seq_len(nrow(catalog)) %in% selected_rows))
    if (length(eligible)) {
      selected_rows <- c(selected_rows, eligible[1])
      added <- TRUE
    }
    if (length(selected_rows) >= sample_movie_count) break
  }
  if (!added) break
}
selected <- catalog[selected_rows, , drop = FALSE]
selected <- selected[order(selected$name, selected$year), , drop = FALSE]
if (nrow(selected) < sample_movie_count) {
  stop("Only prepared ", nrow(selected), " movies after filtering source metadata; expected ", sample_movie_count, ".")
}
selected$id <- seq_len(nrow(selected))
selected$title <- selected$name
selected$genre <- selected$primary_genre
movies <- selected[, c("id", "title", "genre", "year", "rating", "votes")]

reviews <- list()
for (i in seq_len(nrow(selected))) {
  title_year <- paste(selected$title[i], selected$year[i])
  relative <- selected$file[i]
  destination <- file.path(download_dir, paste0("reviews-", i, ".csv"))
  download_csv(relative, destination)
  raw <- read.csv(destination, stringsAsFactors = FALSE, check.names = FALSE)
  if (!all(c("review", "rating") %in% names(raw))) {
    stop("Unexpected review schema for ", title_year)
  }
  raw$review <- gsub("<br\\s*/?>", " ", raw$review, ignore.case = TRUE)
  raw$review <- trimws(gsub("\\s+", " ", raw$review))
  raw <- raw[!is.na(raw$review) & nzchar(raw$review), , drop = FALSE]
  if (nrow(raw) > review_limit) raw <- raw[sample.int(nrow(raw), review_limit), , drop = FALSE]
  scored <- lapply(raw$review, score_review)
  reviews[[i]] <- data.frame(
    movie_id = selected$id[i], review = raw$review,
    sentiment = vapply(scored, `[[`, character(1), "sentiment"),
    score = vapply(scored, `[[`, numeric(1), "score"),
    stringsAsFactors = FALSE
  )
}
reviews <- do.call(rbind, reviews)

dir.create("data", showWarnings = FALSE)
write.csv(movies, file.path("data", "movies.csv"), row.names = FALSE, na = "")
write.csv(reviews, file.path("data", "reviews.csv"), row.names = FALSE, na = "")
cat("Prepared", nrow(movies), "real IMDb movie records and", nrow(reviews),
    "sampled user reviews across", length(unique(movies$genre)), "genres in data/\n")
}

prepare_sample_data()
