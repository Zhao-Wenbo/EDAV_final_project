library(tidyverse)
library(patchwork)

plot_missing <- function(mydata, percent=FALSE) {
  original_names <- names(mydata)
  shorter_names <- original_names
  for (i in 1:length(original_names))
    shorter_names[i] = substr(original_names[i], 1, 5)
  names(mydata) <- shorter_names
  
  missing_patterns <- data.frame(is.na(mydata)) %>%
    group_by_all() %>%
    count(name = "count", sort = TRUE) %>%
    ungroup()
  
  missing_patterns_r <- missing_patterns %>% 
    mutate(complete=ifelse(rowSums(select(missing_patterns, -count)) > 0, FALSE, TRUE)) %>% 
    rownames_to_column("id") %>% 
    mutate(count_p = count / dim(mydata)[1] * 100)
  
  missing_patterns_r$id <- factor(as.integer(missing_patterns_r$id))
  
  # mycars_t <- as.data.frame(t(mycars))
  # row.names(mycars_t) <- names(mycars)
  
  tidy_missing_pattern <- missing_patterns_r %>% 
    pivot_longer(-c("id", "count", "complete", "count_p"), names_to="key", values_to="missing") %>% 
    mutate(missing2 = factor(ifelse(missing, 1, ifelse(complete, 2, 0))))
  
  xpos = (length(names(mydata)) + 1) / 2
  ypos = ifelse(sum(missing_patterns_r$complete) > 0, 
                sum(dim(missing_patterns_r)[1]:1 * missing_patterns_r$complete), -1)
  
  pattern_plot <- ggplot(tidy_missing_pattern) +
    geom_tile(aes(x = fct_reorder(key, -count * missing, sum), 
                  y = fct_rev(id), 
                  fill = missing2), color = "grey90", show.legend = FALSE) + 
    xlab("variable") + ylab("missing pattern") +
    scale_fill_manual(values=c("#CBCBCB", "#B69FE6", "#B3B3B3")) +
    theme_bw()+
    {if(ypos > 0)  annotate(geom = "text", x = xpos, y = ypos, label="complete cases")}
  
  col_count_plot <- {if(percent) ggplot(tidy_missing_pattern, 
                                        aes(x = fct_reorder(key, -count * missing, sum), 
                                            y = count_p * missing))
                     else ggplot(tidy_missing_pattern, 
                                 aes(x = fct_reorder(key, -count * missing, sum), 
                                     y = count * missing))} +
    geom_col(show.legend = FALSE, fill = "#6495ED", alpha=0.6) +
    xlab("") + 
    ylab(ifelse(percent, "% rows\n missing:", "num rows\n missing:")) + 
    {if (percent) ylim(0, 100)
      else scale_y_continuous(n.breaks = 3)} +
    ggtitle("Missing Value Patterns")
    
  
  row_count_plot <- ggplot(missing_patterns_r) +
    geom_col({if(percent) aes(x=count_p, y=fct_rev(id), alpha=complete)
      else aes(x=count, y=fct_rev(id), alpha=complete)}, fill = "#6495ED", show.legend = FALSE) +
    ylab("") + xlab(ifelse(percent, "% rows", "row count")) +
    {if (percent) xlim(0, 100)
      else scale_x_continuous(n.breaks = 3)} +
    scale_alpha_ordinal(range = c(0.6, 1.0))
  
  col_count_plot + plot_spacer() + pattern_plot + row_count_plot +
    plot_layout(heights = c(1,3), widths = c(4, 1))
}
