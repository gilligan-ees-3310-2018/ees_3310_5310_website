require(slidify)
require(yaml)
require(stringr)

copy_newer <- function(source_dir, dest_dir, omit = NULL) {
  if (! dir.exists(dest_dir)) dir.create(dest_dir, recursive=TRUE)
  s_dirs <- list.dirs(source_dir, full.names=FALSE, recursive=FALSE)
  if (! is.null(omit)) {
  s_dirs <- s_dirs[! tolower(s_dirs)  %in% c(unlist(omit))]
  }
  
  s_files <- list.files(source_dir, full.names=FALSE, recursive=FALSE, include.dirs=FALSE)
  s_files <- s_files[! file.info(file.path(source_dir, s_files))$isdir]
  d_files <- file.path(dest_dir, s_files)
  mask <- ! (file.exists(d_files) & 
               (file.mtime(d_files) >= file.mtime(file.path(source_dir, s_files))))
  s_files <- s_files[mask]
  for (f in s_files) {
    file.copy(file.path(source_dir, f), file.path(dest_dir, f), 
              overwrite=TRUE, recursive=FALSE, copy.date=TRUE)
  }
  
  for (d in s_dirs) {
    copy_newer(file.path(source_dir, d), file.path(dest_dir, d), omit)
  }
}

copy_libraries <- function(source_dir, dest_dir) {
  copy_newer(source_dir, dest_dir, 'node_modules')
}

copy_scaffold <- function(semester_dir = './semester', scaffold_dir = './scaffold') {
  dest_dir <- file.path(semester_dir, 'scaffold', 'lecture_skeleton')
  source_dir <- file.path(scaffold_dir, 'lecture_skeleton')
  copy_newer(source_dir, dest_dir)
}

initialize_semester <- function(semester_dir = './semester', scaffold_dir = './scaffold') {
  cwd <- getwd()
  author(semester_dir, use_git = FALSE, open_rmd = FALSE,
         scaffold = file.path(scaffold_dir,'semester_skeleton'))
  setwd(cwd)
  if (! dir.exists(semester_dir)) dir.create(semester_dir)
  copy_libraries(file.path(scaffold_dir, 'libraries'), file.path(semester_dir, 'libraries'))
  copy_scaffold(semester_dir, scaffold_dir)
  setwd(semester_dir)
}
