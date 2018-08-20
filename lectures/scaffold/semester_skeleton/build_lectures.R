require(slidify)
require(yaml)
require(stringr)

source('fix_rmd.R')
source('fix_file_refs.R')

set.slidify.options <- function(extras = c()) {
  options <- unique(c(getOption('rhoedown.HTML.options'), rhoedown::markdownHTMLOptions(TRUE), extras))
  options <- options[! is.null(options)]
  options(rhoedown.HTML.options = options)
  invisible(options)
}

set.slidify.extensions <- function(extras = c()) {
  exts <- unique(c(getOption('rhoedown.extensions'), rhoedown::markdownExtensions(TRUE), extras))
  exts <- exts[! is.null(exts)]
  options(rhoedown.extensions = exts)
  invisible(exts)
}

find_semester_dir <- function(dir = '.', up = TRUE) {
  dir = normalizePath(path.expand(dir))
  if(basename(dir) == 'semester') return(dir)
  dirs = list.dirs(dir, recursive = FALSE)
  for (d in dirs) {
    if (! is.null(find_semester_dir(d, FALSE))) return(d)
  }
  if (up) {
    while( basename(dir) != 'semester') {
      d = dirname(dir)
      if (d == dir) return(NULL)
      dir = d
    }
    if (basename(dir) == 'semester') return(dir)
  }
  NULL
}


make_pageurl <- function(lecture_no, lecture_dir = 'Lectures') {
  server <- 'mingus.noip.us'
  head_dir <- 'EES_2110'
  URLencode(paste0('http://', file.path(server, head_dir, lecture_dir, 
                                        sprintf('Lecture_%02d', lecture_no))))
}

add_class_number <- function(lecture_number, file = 'index.Rmd') {
  lines <- readLines(file)
  for (i in 2:length(lines)) {
    if (grepl('^\\s*class_no\\s*:\\s*$', lines[i])) {
      lines[i] <- paste(str_trim(lines[i], 'right'), lecture_number)
      break
    }
    if (grepl('^---', lines[i]))
      break
  }
  writeLines(lines, file)
}

make_qr <- function(slide_dir, semester_dir = '.') {
  message("semester_dir = ", semester_dir)
  cmdline <- paste("python", file.path(semester_dir,'lecture_qr.py'), slide_dir)
  message("Running \"", cmdline, "\" in directory ", getwd())
  system(cmdline)
}

add_qrimage <- function(image_file = 'qrcode.png', file = 'index.Rmd') {
  lines <- readLines(file)
  for (i in 2:length(lines)) {
    if (grepl('^\\s*qrimage\\s*:\\s*$', lines[i])) {
      lines[i] <- paste(str_trim(lines[i], 'right'), image_file)
      break
      }
    if (grepl('^---', lines[i]))
      break
  }
  writeLines(lines, file)
}

add_pageurl <- function(lecture_no, lecture_dir = "Lectures", file = 'index.Rmd') {
  lines <- readLines(file)
  for (i in 2:length(lines)) {
    if (grepl('^\\s*pageurl\\s*:\\s*$', lines[i])) {
      lines[i] <- paste(str_trim(lines[i], 'right'), 
                        make_pageurl(lecture_no, lecture_dir))
      break
    }
    if (grepl('^---', lines[i]))
      break
  }
  writeLines(lines, file)
}


author_lecture <- function(lecture_number, semester_dir = '.', 
                           lecture_dir = file.path(semester_dir, 'Lectures'),
                           scaffold_dir = file.path(semester_dir, 'scaffold'),
                           open_rmd = TRUE) {
  semester_dir <- normalizePath(semester_dir)
  message("semester_dir = ", semester_dir)
  if (! dir.exists(lecture_dir)) dir.create(lecture_dir, recursive = TRUE)
  if (is.numeric(lecture_number))
    new_lecture <- sprintf('Lecture_%02d', lecture_number)
  else 
    new_lecture <- paste0('Lecture_', lecture_number)
  if (dir.exists(file.path(lecture_dir, new_lecture)) && 
      file.exists(file.path(lecture_dir, new_lecture, 'index.Rmd'))) {
    stop('Lecture ', lecture_number, ' already exists.')
  } else {
    author(file.path(lecture_dir, new_lecture),
           use_git = FALSE, open_rmd = FALSE,
           scaffold = file.path(scaffold_dir, 'lecture_skeleton'))
    make_qr(file.path(lecture_dir, new_lecture), semester_dir)
    add_class_number(lecture_number)
    dir.create('images')
    add_qrimage()
    add_pageurl(lecture_number, basename(lecture_dir))
    if (open_rmd)
      file.edit('index.Rmd')
  }
}

em_en_dash <- function(infile, outfile) {
  src <- file(infile, 'r')
  text <- readLines(src, warn=FALSE)
  close(src)
  text <- gsub('([[:alnum:]])---([[:alnum:]])', '\\1&mdash;\\2', text)
  text <- gsub('([[:alnum:]])--([[:alnum:]])', '\\1&ndash;\\2', text)
  dest <- file(outfile,'w')
  writeLines(text, dest)
  close(dest)
  invisible(text)
}


build_lecture <- function(inputFile, knit_deck = TRUE,
                          return_page = FALSE, save_payload=FALSE,
                          envir = parent.frame()) {
  if (is.numeric(inputFile)) {
    inputFile <- file.path(semester.dir, 'Lectures', sprintf('Lecture_%02d', inputFile))
  }
  if (dir.exists(inputFile)) {
    inputFile <- file.path(inputFile, 'index.Rmd')
  }
  set.slidify.options()
  set.slidify.extensions()
  ## REMOVE LINES AFTER KNITR IS UPDATED ------
  options('knitr.in.progress' = TRUE)
  on.exit(options('knitr.in.progress' = FALSE))
  cwd <- getwd()
  on.exit(setwd(cwd), add = TRUE)
  ## -------------------------------------------
  
  .SLIDIFY_ENV <<- new.env()
  site = list()
  inputFile <- normalizePath(inputFile)
  d <- inputFile
  while(dirname(d) != d) {
    d <- dirname(d)
    semester_file <- file.path(d, 'semester.yml')
    if (file.exists(semester_file)) {
      site <- yaml.load_file(semester_file)
      break
    }
    site_file <- file.path(d, 'site.yml')
    if (file.exists(site_file)) {
      site <- yaml.load_file(site_file)
      break
    }
  }
  if (file.exists(semester_file))
    setwd(dirname(semester_file))
  else if (file.exists(site_file))
    setwd(dirname(site_file))
  page = slidify:::parse_page(inputFile, knit_deck, envir = envir)
  
  page = modifyList(page, as.list(.SLIDIFY_ENV))
  render_page(page, payload = list(site = site), return_page, save_payload)
  outputFile <- file.path(dirname(page$file),sprintf('%s.html', page$filename))
  message("Processing ", outputFile, " in ", getwd())
  em_en_dash(outputFile, outputFile)
}

build_semester_index <- function(semester_dir = '.', last_lecture = NA, envir = parent.frame()) {
  site = yaml.load_file('semester.yml')
  last_lecture_file <- file.path(semester_dir, 'last_lecture.yml')
  if (is.na(last_lecture)) {
    if (file.exists(last_lecture_file)) file.remove(last_lecture_file)
  } else {
    cat('last_lecture: ', last_lecture, '\n', sep='', append=FALSE, file = last_lecture_file)    
  }
  semester_file = dir(".", recursive = FALSE, pattern = "*.Rmd")
  pages = slidify:::parse_pages(semester_file, envir = envir)
  tags = slidify:::get_tags(pages)
  pages = render_pages(pages, site, tags, return_page = TRUE)
  for (page in pages) {
    outputFile <- file.path(dirname(page$file),sprintf('%s.html', page$filename))
    message("Processing ", outputFile)
    em_en_dash(outputFile, outputFile)
  }
  return(invisible(list(pages = pages, site = site, tags = tags)))
}


build_lectures <- function (semester_dir = ".", last_lecture = NA, envir = parent.frame()) 
{
  cwd = getwd()
  on.exit(setwd(cwd))
  setwd(semester_dir)
  if (! file.exists('semester.yml') && ! dir.exists('libraries') && dir.exists('semester')) setwd('semester')
  build_semester_index(last_lecture = last_lecture, envir = new.env())
  LectureFiles <- dir('Lectures', pattern="*.Rmd", recursive = TRUE, 
                      full.names = TRUE)
  p <- c()
  for(f in LectureFiles) {
    message("Processing ", f)
    p <- c(p, build_lecture(f, envir = new.env()))
  }
  message("Lecture Building Successful :-)")
  #  return(invisible(list(pages = pages, site = site, tags = tags)))
}

semester.dir <- find_semester_dir()
data.dir <- file.path(semester.dir, 'data')
script.dir <- file.path(semester.dir, 'util_scripts')
