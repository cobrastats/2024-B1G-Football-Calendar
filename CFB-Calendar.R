#Load Libraries
library(cfbfastR)
library(dplyr)
library(gt)
library(gtExtras)
library(stringr)
library(glue)
library(tidyverse) 

#Create master schedule of 2024 NCAAF Season
masterschedule = data.frame()
for(i in c(1:14)){
  schedule = espn_cfb_schedule(year=2024,week=i)
  schedule$week = i
  masterschedule = rbind(masterschedule,schedule)
}

#Create list of B1G Teams - must be done manually
b1g = c("Illinois","Indiana","Iowa","Nebraska","Northwestern","Michigan","Michigan State","Minnesota",
        "Ohio State","Penn State","Rutgers","Wisconsin","Purdue","Maryland","Washington","USC","UCLA","Oregon")

#Filter schedule to include only B1G Games
masterschedule = masterschedule %>% filter(home_team_location %in% b1g | away_team_location %in% b1g)

#Create new schedule to include each individual game for each team
newschedule = data.frame()
for(i in b1g){
  teamschedule = masterschedule %>% filter(home_team_location == i | away_team_location == i) %>% 
    mutate(team = i)
  newschedule = rbind(newschedule,teamschedule)
}

#Format new schedule to prep for pivot
newschedule = newschedule %>% 
  select(season,week,game_id,game_date,team,
         home_team_location,home_team_color,home_team_logo,home_team_abb,
         away_team_location,away_team_color,away_team_logo,away_team_abb) %>% 
  mutate(location = if_else(home_team_location==team,"HOME","AWAY"),
         team_color = if_else(location=="AWAY",away_team_color,home_team_color),
         team_logo = if_else(location=="AWAY",away_team_logo,home_team_logo),
         team_abb = if_else(location=="HOME",home_team_abb,away_team_abb),
         opp = if_else(location=="HOME",away_team_location,home_team_location),
         opp_color = if_else(location=="HOME",away_team_color,home_team_color),
         opp_logo = if_else(location=="HOME",away_team_logo,home_team_logo)) %>% 
  select(week,location,team,team_color,team_logo,opp,opp_color,opp_logo,team_abb) %>% 
  mutate(team_logo = paste0("<img src='", team_logo, "' style='height: 60px; width: auto; vertical-align: middle;'>"),
         opp_logo = paste0("<img src='", opp_logo, "' style='height: 50px; width: auto; vertical-align: middle;'>","<br>",location)) %>% 
  select(week,location,team,team_color,team_logo,opp,opp_color,opp_logo) %>% arrange(team,week) %>% select(week,team_logo,opp_logo)
                                    
#Pivot wider with logos to prep for table
newpivot = newschedule %>% pivot_wider(names_from = team_logo,values_from = opp_logo) %>% arrange(week)

#Transpose to make team naames as rows
newpivot = as.data.frame(t(newpivot))

#Convert all NAs (bye weeks) to dashes
newpivot = newpivot %>%
  mutate_all(
    function(x) {
      ifelse(
        is.na(x),"--",x
      )
    }
  )

#Convert row names (team logos) to first column
new_col = rownames(newpivot)
newpivot = cbind(new_col,newpivot)
rownames(newpivot) = 1:nrow(newpivot)

#Remove "week" label from row 1
newpivot$new_col[1]=""

#Create header for table
title_header <- glue(
  "<div style='display: flex; justify-content: space-between; align-items: center;'>
    <div>
        <img src='https://i.imgur.com/8oFuQOu.png' style='height: 120px; width: auto; vertical-align: left;'>
    </div>
    <div style='text-align: center;padding-right: 130px;'>
        <span style='font-weight: bold; font-size: 50px; line-height: 1.2;'>2024 Football<br>Calendar</span>
    </div>
    <div style='display: flex; flex-direction: column; align-items: flex-end;'>
        <div style='margin-bottom: 5px;'>
            <pre style='display: inline-block; background-color: #cce7f5; border: 1px solid black; width: 60px; height: 40px; line-height: 40px; vertical-align: top; text-align: center; margin: 0;'>HOME</pre>
        </div>
        <div style='margin-bottom: 5px;'>
            <pre style='display: inline-block; background-color: #ffffff; border: 1px solid black; width: 60px; height: 40px; line-height: 40px; vertical-align: top; text-align: center; margin: 0;'>AWAY</pre>
        </div>
        <div style='margin-bottom: 5px;'>
            <pre style='display: inline-block; background-color: #e0e0e0; border: 1px solid black; width: 60px; height: 40px; line-height: 40px; vertical-align: top; text-align: center; margin: 0;'>BYE</pre>
        </div>
    </div>
  </div>"
)

#Create Table
plot = newpivot %>% 
  gt() %>%  gt::fmt_markdown() %>% 
  tab_header(title = html(title_header)) %>% 
  gt_theme_538() %>% 
  cols_width(starts_with("V") ~px(72),new_col ~px(110)) %>% 
  cols_align(align = "center",columns = everything()) %>% 
  cols_label(everything() ~"") %>% 
  tab_style(style = list(
      cell_borders(sides = c("top"),color = "white",weight = px(2)),
      cell_borders(sides = c("bottom"),color = "black",weight = px(3))),
      locations = list(
        cells_body(columns = everything(),rows = c(1)))
      ) %>% 
  tab_style(
    cell_borders(sides = c("bottom"),color = "black",weight = px(1.5)),
    locations = list(cells_body(columns = everything(),rows = c(1)))
    ) %>% 
  tab_style(style = list(
    cell_text(weight = "bold",size=px(28))),
    locations = cells_body(columns = everything(),rows = 1)
    ) %>% 
  tab_style(style = list(
    cell_text(weight = "bold")),
    locations = cells_body(columns = 1,rows = everything())
    ) %>% 
  tab_style(
    cell_borders(sides = c("left"),color = "lightgrey",weight = px(0.75),style="dotted"),
    locations = list(
      cells_body(columns = starts_with("V"),rows = c(2:19)))
    ) %>% 
  tab_style(
    cell_borders(sides = c("bottom"),color = "black",weight = px(1.5),style="solid"),
    locations = list(
      cells_body(columns = c(2:15),rows = c(2:19)))
    ) %>% 
  tab_style(
    cell_borders(sides = c("bottom"),color = "white",weight = px(3),style="solid"),
    locations = list(
      cells_body(columns = 1,rows = c(2:19)))
    ) %>% 
  tab_style(style = list(
    cell_text(size = px(0))),
    locations = cells_body(columns = c(2:15),rows = c(2:19))
    ) %>% 
  tab_style(style = cell_fill(color = "gray85"),locations = cells_body(columns = V1,rows = str_detect(V1,"--"))) %>% 
  tab_style(style = cell_fill(color = "gray85"),locations = cells_body(columns = V2,rows = str_detect(V2,"--"))) %>% 
  tab_style(style = cell_fill(color = "gray85"),locations = cells_body(columns = V3,rows = str_detect(V3,"--"))) %>% 
  tab_style(style = cell_fill(color = "gray85"),locations = cells_body(columns = V4,rows = str_detect(V4,"--"))) %>% 
  tab_style(style = cell_fill(color = "gray85"),locations = cells_body(columns = V5,rows = str_detect(V5,"--"))) %>% 
  tab_style(style = cell_fill(color = "gray85"),locations = cells_body(columns = V6,rows = str_detect(V6,"--"))) %>% 
  tab_style(style = cell_fill(color = "gray85"),locations = cells_body(columns = V7,rows = str_detect(V7,"--"))) %>% 
  tab_style(style = cell_fill(color = "gray85"),locations = cells_body(columns = V8,rows = str_detect(V8,"--"))) %>% 
  tab_style(style = cell_fill(color = "gray85"),locations = cells_body(columns = V9,rows = str_detect(V9,"--"))) %>% 
  tab_style(style = cell_fill(color = "gray85"),locations = cells_body(columns = V10,rows = str_detect(V10,"--"))) %>% 
  tab_style(style = cell_fill(color = "gray85"),locations = cells_body(columns = V11,rows = str_detect(V11,"--"))) %>% 
  tab_style(style = cell_fill(color = "gray85"),locations = cells_body(columns = V12,rows = str_detect(V12,"--"))) %>% 
  tab_style(style = cell_fill(color = "gray85"),locations = cells_body(columns = V13,rows = str_detect(V13,"--"))) %>% 
  tab_style(style = cell_fill(color = "gray85"),locations = cells_body(columns = V14,rows = str_detect(V14,"--"))
            )%>%  
  tab_style(style = cell_fill(color = "#cce7f5"),locations = cells_body(columns = V1,rows = str_detect(V1,"HOME"))) %>%
  tab_style(style = cell_fill(color = "#cce7f5"),locations = cells_body(columns = V2,rows = str_detect(V2,"HOME"))) %>% 
  tab_style(style = cell_fill(color = "#cce7f5"),locations = cells_body(columns = V3,rows = str_detect(V3,"HOME"))) %>% 
  tab_style(style = cell_fill(color = "#cce7f5"),locations = cells_body(columns = V4,rows = str_detect(V4,"HOME"))) %>% 
  tab_style(style = cell_fill(color = "#cce7f5"),locations = cells_body(columns = V5,rows = str_detect(V5,"HOME"))) %>% 
  tab_style(style = cell_fill(color = "#cce7f5"),locations = cells_body(columns = V6,rows = str_detect(V6,"HOME"))) %>% 
  tab_style(style = cell_fill(color = "#cce7f5"),locations = cells_body(columns = V7,rows = str_detect(V7,"HOME"))) %>% 
  tab_style(style = cell_fill(color = "#cce7f5"),locations = cells_body(columns = V8,rows = str_detect(V8,"HOME"))) %>% 
  tab_style(style = cell_fill(color = "#cce7f5"),locations = cells_body(columns = V9,rows = str_detect(V9,"HOME"))) %>% 
  tab_style(style = cell_fill(color = "#cce7f5"),locations = cells_body(columns = V10,rows = str_detect(V10,"HOME"))) %>% 
  tab_style(style = cell_fill(color = "#cce7f5"),locations = cells_body(columns = V11,rows = str_detect(V11,"HOME"))) %>% 
  tab_style(style = cell_fill(color = "#cce7f5"),locations = cells_body(columns = V12,rows = str_detect(V12,"HOME"))) %>% 
  tab_style(style = cell_fill(color = "#cce7f5"),locations = cells_body(columns = V13,rows = str_detect(V13,"HOME"))) %>% 
  tab_style(style = cell_fill(color = "#cce7f5"),locations = cells_body(columns = V14,rows = str_detect(V14,"HOME"))
            ) %>% 
  tab_source_note(md("Viz by @cobrastats | Data via cfbfastR | April 27, 2024")) %>%  
  tab_style(style = cell_text(size = px(24)),locations = cells_source_notes()
            ) %>%
  gtsave("/Users/connorbradley/Desktop/basketball data/CFBschedule.png", expand = c(30,60,30,60),vwidth =1600, zoom=4)


