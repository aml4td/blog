new_post <- function(path, site = ".", open = rlang::is_interactive(), ...) {
  require(cli)
  if (!rlang::is_installed("hugodown")) {
    cli::cli_abort('Please install: {.code devtools::install_github("r-lib/hugodown")}')
  }

  data <- list(...)

  path <- gsub("[[:space:]]", "-", path)
  path_chr <- as.character(fs::path_abs(path))
  site <- fs::path_dir(path_chr)

  tld <- fs::path_dir(path)
  if (!fs::dir_exists(fs:::path(site, "posts", tld))) {
    cli::cli_abort("Can't find {.code tld} directory in 'posts/'")
  }

  dest <- fs::path(site, "posts", path)
  if (fs::file_exists(dest)) {
    cli::cli_abort("`path` already exists")
  } else {
    fs::dir_create(dest)
  }

  qmds <- fs::dir_ls(dest, glob = "*.qmd")
  defaults <- list(
    slug = fs::path_file(path),
    title = tools::toTitleCase(hugodown:::unslug(fs::path_file(path))),
    author = tools::toTitleCase(whoami::fullname("Your name")),
    date = strftime(Sys.Date(), "%Y-%m-%d")
  )
  data <- utils::modifyList(defaults, data)

  res <- make_template(data, dest)

  index <- fs::dir_ls(dest, pattern = "index")
  usethis::edit_file(index, open = open)

  invisible(dest)
}


make_template <- function(data, dest) {
  dest <- fs::path(dest, "index.qmd")
  data <- data[names(data) != "slug"]
  categories <- paste0("categories:\n  - category1\n  - category2")
  entries <- paste0(names(data), ": '", data, "'")
  res <- paste0(c("---", entries, categories, "---", "\n<hr>\n"),
                collapse = "\n")
  cat(res, collapse = "\n", file = dest)
}


