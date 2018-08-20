library(pacman)
p_load(tidyverse)
p_load_gh("wilkox/treemapify")

theme_set(theme_bw(base_size = 20))

nations <- data_frame(country=c('USA','Japan','EU','Canada','Australia','Other Enthusiastic',
                                'Other Reluctant', 'Brazil', 'China','India','Indonesia',
                                'Small Islands', 'Other Vulnerable',
                                'Russia', 'Other Exporters'),
                      group = c(rep_len('Enthusiastic', 6), rep_len('Reluctant',5),
                                rep_len('Vulnerable',2), rep_len('Exporters',2)),
                      emissions = c(17.3, 3.9, 14.2, 2.0, 1.7, 0.6,
                                    5.1, 5.10001, 20.5, 5.4, 1.7,
                                    0.9, 7.3,
                                    6.1, 8.1))

nations <- nations %>%  
  mutate(group = ordered(group, levels = c('Enthusiastic', 'Reluctant', 'Exporters', 'Vulnerable')),
         color = NA) %>%
  arrange(group, desc(emissions), country) %>%
  mutate(country = ordered(country, levels = rev(country)))

ug <- levels(nations$group)
lug <- length(ug)

for (i in 1:lug) {
  g <- ug[i]
#  cat('group = ', as.character(g), ',')
  h <- 1 - ((i - 1) / lug)
#  cat('hue = ', h, '\n')
  nj <- sum(nations$group == g)
  for(j in 1:nj) {
    c <- nations$country[nations$group == g][j]
#    cat("country = ", as.character(c), ',')
    v <- j / nj
#    cat('hue = ', h, ' value = ', v, '\n')
    nations$color[nations$country == c] <- hsv(h, 0.75 - 0.5 * v, v)
  }
}

plot_gridlock <- function(nations) {
  colors <- nations$color
  names(colors) <- as.character(nations$country)
  
  breaks <- nations$country[order(nations$group,nations$emissions,nations$country)]
  
  p <- ggplot(nations, aes(y=emissions,fill=country,x=group)) +
    geom_bar(stat='identity') +
    geom_bar(stat='identity', color='black',show.legend=FALSE) +
    scale_y_continuous(limits=c(0,41), expand=c(0,0)) +
    labs(x="Group",y="% Emissions") +
    scale_fill_manual(name="Country", values=colors, breaks=breaks) +
    theme(panel.grid.major.x = element_blank(),
          legend.position = c(0.99,0.99), legend.justification = c(1,1))
  p
}

jgplotify <- function (treeMap, label.colour = "white", label.size.factor = 1, 
          label.size.threshold = NULL, label.size.fixed = NULL, label.groups = TRUE, 
          group.label.colour = "darkgrey", group.label.size.factor = 1, 
          group.label.size.threshold = NULL, group.label.size.fixed = NULL) 
{
  fill <- group <- label <- labelx <- labely <- labelsize <- alpha <- NULL
  if (missing(treeMap) || is.data.frame(treeMap) == FALSE) {
    stop("Must provide a data frame")
  }
  if (!missing(label.size.fixed) && !missing(label.size.factor)) {
    warning("label.sized.fixed overriding label.size.factor")
    label.size.factor <- 1
  }
  if (!missing(group.label.size.fixed) && !missing(group.label.size.factor)) {
    warning("group.label.sized.fixed overriding group.label.size.factor")
    group.label.size.factor <- 1
  }
  xlim <- c(min(treeMap["xmin"]), max(treeMap["xmax"]))
  ylim <- c(min(treeMap["ymin"]), max(treeMap["ymax"]))
  Plot <- ggplot(treeMap)
  Plot <- Plot + coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE)
  Plot <- Plot + geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, 
                               ymax = ymax, fill = fill))
  Plot <- Plot + geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, 
                               ymax = ymax), fill = NA, colour = "grey", size = 0.2)
  Plot <- Plot + theme(axis.ticks = element_blank(), axis.title = element_blank(), 
                       axis.text = element_blank(), panel.background = element_blank(), 
                       panel.border = element_blank())
  Plot <- Plot + guides(fill = guide_legend(title = attributes(treeMap)$fillName))
  if ("group" %in% colnames(treeMap)) {
    groupRects <- treeMap %>% group_by(group) %>% 
      summarise(xmin = min(xmin), xmax = max(xmax), ymin = min(ymin), ymax = max(ymax)) %>%
      ungroup()
    names(groupRects) <- c("group", "xmin", "xmax", "ymin", 
                           "ymax")
    Plot <- Plot + geom_rect(data = groupRects, mapping = aes(xmin = xmin, 
                                                              xmax = xmax, ymin = ymin, ymax = ymax), colour = "grey", 
                             fill = NA, size = 1.2)
  }
  if (label.groups == TRUE && "group" %in% colnames(treeMap)) {
    if ("label" %in% colnames(treeMap)) {
      groupLabels <- treeMap %>% group_by(group) %>% 
        summarize(x = max(xmax) - ((max(xmax) - min(xmin)) * 0.5), 
                  y = min(ymin) + 2, size = (max(xmax) - min(xmin))/nchar(as.character(group[1]))) %>%
        ungroup()
    }
    else {
      groupLabels <- treeMap %>% group_by(group) %>% 
        summarize(x = max(xmax) - ((max(xmax) - min(xmin)) * 0.5), 
                  y = max(ymax) - ((max(ymax) - min(ymin)) * 0.5), 
                  size = (max(xmax) - min(xmin))/nchar(as.character(group[1]))) %>%
        ungroup()
    }
    groupLabels$size <- groupLabels$size * group.label.size.factor
    if (!missing(group.label.size.fixed)) {
      groupLabels$size <- rep(group.label.size.fixed, nrow(groupLabels))
    }
    if (!missing(group.label.size.threshold)) {
      groupLabels$alpha <- ifelse(groupLabels$size < group.label.size.threshold, 
                                  0, 1)
    }
    else {
      groupLabels$alpha <- rep(1, nrow(groupLabels))
    }
    Plot <- Plot + annotate("text", x = groupLabels$x, y = groupLabels$y, 
                            label = groupLabels$group, size = groupLabels$size, 
                            colour = group.label.colour, alpha = groupLabels$alpha, 
                            fontface = "bold", hjust = 0.5, vjust = 0)
  }
  if ("label" %in% colnames(treeMap)) {
    treeMap <- treeMap %>% group_by(label) %>% 
      mutate(labelx = xmin + 1, labely = ymax - 1, 
             labelsize = (xmax - xmin)/(nchar(as.character(label)))) %>%
      ungroup()
    if (!missing(label.size.fixed)) {
      treeMap$labelsize <- rep(label.size.fixed, nrow(treeMap))
    }
    if (!missing(label.size.threshold)) {
      treeMap$alpha <- ifelse(treeMap$labelsize * label.size.factor < 
                                label.size.threshold, 0, 1)
    }
    else {
      treeMap$alpha <- rep(1, nrow(treeMap))
    }
    Plot <- Plot + geom_text(data = treeMap, aes(label = label, colour = label.colour,
                                                 x = labelx, y = labely, size = labelsize, alpha = alpha), 
                             hjust = 0, vjust = 1, show.legend = FALSE)
    if (missing(label.size.fixed)) {
      Plot <- Plot + scale_size(range = c(1, 8) * label.size.factor, 
                                guide = FALSE)
    }
    else {
      Plot <- Plot + scale_size(range = c(1, label.size.fixed), 
                                guide = FALSE)
    }
  }
  return(Plot)
}

treemap_gridlock <- function(nations) {
  colors <- nations$color
  names(colors) <- as.character(nations$country)
  
  tm <- treemapify(nations, area = "emissions", fill = "country", 
                   label = "country", group = "group")
  tm <- tm %>% as_data_frame() %>%
    mutate(label.colour = ifelse(label %in% c('Australia', 'Other Enthusiastic', 
                                                 'Other Reluctant', 'Indonesia',
                                              'Russia', 'Small Islands'),
                                 'black', 
                                 ifelse(group %in% c('Exporters', 'Vulnerable'),
                                        'white', '#FFFF00'))
           )
  
  lc <- unique(tm$label.colour)
  
  jgplotify(tm, label.size.factor = 2, 
            group.label.size.factor = 2,
            group.label.colour = '#F0F000'
            ) + 
    scale_fill_manual(values = colors, guide = FALSE) + 
    scale_color_manual(values = setNames(lc, lc), guide = FALSE) +
    guides(fill=FALSE, colour = FALSE) +
    theme(panel.grid = element_blank())
}
