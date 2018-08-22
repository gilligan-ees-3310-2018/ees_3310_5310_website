library(tidyverse)
library(stringr)

format_md = function(x, digits = NULL, scientific = NULL, comma = FALSE,
                     significance = FALSE,
                     output_format = c('markdown', 'latex')) {
  output_format <- match.arg(output_format)
  if (x != 0) {
    lx <- log10(abs(x))
    ex <- floor(lx)
    mx <- sign(x) * 10^(lx %% 1)
  } else {
    ex = 0
    mx = 0
  }
  if (is.null(scientific)) {
    scientific = (ex > 3) || (ex < -2)
  }

  if (scientific) {
    if (output_format == 'markdown') {
      output <- paste0(format(mx, digits = digits + 1, drop0trailing = FALSE),
                       " &times; 10^", ex, "^")
    } else if (output_format == 'latex')  {
      output <- paste0(format(mx, digits = digits + 1, drop0trailing = FALSE),
                       " \\times 10^{", ex, "}")
    } else {
      error("Unknown output_format: ", output_format)
    }
  } else {
    if (comma) mark = ','
    else mark = ''
    if (significance) {
      x <- signif(x, digits + 1)
      digits = max(0, digits - ex)
    }
    output <- formatC(x, digits = digits, format = 'f', big.mark = mark, drop0trailing = FALSE)
  }
  output
}
