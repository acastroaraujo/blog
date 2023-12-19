
# https://themockup.blog/posts/2022-11-08-use-r-to-generate-a-quarto-blogpost/

## Add this line of code to your project R profile:
## source("helper-functions.R") 

## To modify your R profile enter this code into the console:
## usethis::edit_r_profile(scope = "project")

# library(stringr) # easy string manipulation 
# library(glue)    # easy adding of string together
# library(fs)      # easy file manipulation
# library(cli)     # easy and beautiful messages/warnings

new_post <- function(
    title,
    file = "index.qmd",
    description = "",
    author = "andrés castro araújo",
    draft = FALSE,
    date = Sys.Date(),
    title_limit = 40,
    open_file = TRUE
) {
  
  # convert to kebab case and remove non space or alphanumeric characters
  title_kebab <- stringr::str_to_lower(title) |>
    stringr::str_remove_all("[^[:alnum:][:space:]]") |>
    stringr::str_replace_all(" ", "-")
  
  # warn if a very long slug
  if (nchar(title_kebab) >= title_limit) {
    cli::cli_alert_warning("Warning: Title slug is longer than {.val {title_limit}} characters!")
  }

  slug <- glue::glue("posts/{date}-{title_kebab}")
  
  # create and alert about directory
  
  if (slug %in% dir("posts", full.names = TRUE)) {
    cli::cli_alert_danger("You are about to replace a post that already has this name.")
    cli::cli_alert_danger("Do you want to proceed?")
    proceed <- readLines(n = 1L)
    
    if (tolower(proceed) == "yes") fs::dir_create(path = slug) else return(invisible(NULL))
  
  } else {
    fs::dir_create(path = slug)
  }
  
  cli::cli_alert_success("Folder created at {.file {slug}}")
  
  # wrap description at 77 characters
  description <- stringr::str_wrap(description, width = 77) |> 
    stringr::str_replace_all("[\n]", "\n  ")
  
  # start generating file
  new_post_file <- glue::glue("{slug}/{file}")
  
  # build yaml core
  new_post_core <- c(
    "---",
    glue::glue("title: \"{title}\""),
    if (nchar(description) == 0) "#description: |" else "description: |",
    glue::glue("  {description}"),
    glue::glue("author: {author}"),
    glue::glue("date: {date}")
  )
  
  # add draft if draft
  if (draft) {
    new_post_text <- c(
      new_post_core,
      "draft: true",
      "---\n"
    )
  } else {
    new_post_text <- c(
      new_post_core,
      "---\n"
    )
  }
  
  # finalize new post text
  new_post_text <- paste0(
    new_post_text,
    collapse = "\n"
  )
  
  # create file and alert

  fs::file_create(new_post_file)
  cli::cli_alert_success("File created at {.file {new_post_file}}")
  
  # print new post information
  cat(new_post_text)
  
  writeLines(
    text = new_post_text,
    con = new_post_file
  )
  
  invisible(rstudioapi::documentOpen(new_post_file, line = length(new_post_text)))
  
}
