library(tidyverse)
stroop_stim <- data.frame( stimuli = length(16),
                           word = rep(c("red","green","blue","yellow"), each=4),
                           color = rep(c("red","green","blue","yellow"), 4),
                           response = rep(c("r","g","b","y"), 4),
                           congruency = length(16),
                           id = "stroop_stim",
                           fontsize = "16pt") %>%
  mutate(congruency = as.numeric(word==color)) %>%
  mutate(congruency = recode(congruency, `1` = "Con", `0` = "Inc")) %>%
  mutate(stimuli = html_stimuli(df = .,
                                html_content = "word",
                                html_element = "p",
                                column_names = c("color","fontsize"),
                                css = c("color", "font-size"),
                                id = "id"))


column_names <- c("color","fontsize")
css <- c("color", "font-size")
df<-stroop_stim
html_element <- "p"
html_content <- "word"

html_stimuli(df = stroop_stim, 
             html_content = "word",
             html_element = "p",
             column_names = c("color","fontsize"),
             css = c("color", "font-size"),
             id = "id"
             )

html_stimuli <- function(df, html_content, html_element, column_names, css=NULL, id="my_stim"){
  
  # assign ids
  if(!is.null(df$id)){
    write_id <- paste("id = '", df[,id],"'", sep="")}
  else{
    write_id <- paste("id = '", id,"'", sep="")
  }
  
  
  # if css is NULL assume column_names are valid css attributes
  if(is.null(css)) css <- column_names
  
  # write style definitions for each css attribute
  css_df <- data.frame(row.names=1:dim(df)[1])
  for(i in 1:length(column_names)){
   css_df[,i] <- paste(css[i],": ", df[,column_names[i]], ";", sep="")
  }
  
  # paste together in one style definition
  css_df <- tidyr::unite(css_df, col="style", sep=" ")
  write_style <- paste("style = '",css_df$style,"'",sep="")
  
  #write the html element definition
  write_html <- paste("<",html_element," ", 
                      write_id, " ",
                      write_style,">",
                      df[,html_content],
                      "</",html_element,">", sep="")
  
  return(write_html)
  
}


stroop_stim <- data.frame(stimuli = length(16),
                          word = rep(c("red","green","blue","yellow"), each=4),
                          color = rep(c("red","green","blue","yellow"), 4),
                          response = rep(c("r","g","b","y"), 4),
                          id = "stroop_stim",
                          fontsize = "16pt")

html_stimuli(df = stroop_stim, 
             html_content = "word",
             html_element = "p",
             column_names = c("color","fontsize"),
             css = c("color", "font-size"),
             id = "id"
)

stroop_stim <- data.frame(stimuli = length(16),
                          word = rep(c("red","green","blue","yellow"), each=4),
                          color = rep(c("red","green","blue","yellow"), 4),
                          response = rep(c("r","g","b","y"), 4),
                          id = "stroop_stim",
                          fontsize = "16pt") %>%
  mutate(stimuli = html_stimuli(df = .,
                                html_content = "word",
                                html_element = "p",
                                column_names = c("color","fontsize"),
                                css = c("color", "font-size"),
                                id = "id"))

######### convert df to json


