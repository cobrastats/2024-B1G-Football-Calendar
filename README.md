# Conference Football Calendar<br>
![CFBschedule](https://github.com/cobrastats/2024-B1G-Football-Calendar/assets/109628356/c887582c-c5d5-41d5-ae1b-28e8a3580e11)

### Sometimes I've seen conference put out their conference schedules as a massive grid, week by week. So I wanted to recreate that for the B1G. This template can be copied for other conferences. There are some things you'll have to do manually (noted below), but it can be done! It's not always pretty, but it works. <br>
##### This is a fairly extensive explanation, meant to walk anyone through the process. You can also just grab the code from above.
\
First, you'll need to load in libraries needed throughout this project. Pretty confident they're all in `CRAN` so you may have to install some new packages, but not from GitHub or anything.

``` r
library(cfbfastR)
library(dplyr)
library(gt)
library(gtExtras)
library(stringr)
library(glue)
library(tidyverse) 
```
\
First thing you'll do is scrape the full FBS schedule using `cfbfastR` using the code below (we'll filter it later). We loop `c(1:14)` because weeks 15 + 16 are conference championship games and bowl games. The schedule has nothing there yet for those weeks.

```r
masterschedule = data.frame()
for(i in c(1:14)){
  schedule = espn_cfb_schedule(year=2024,week=i)
  schedule$week = i
  masterschedule = rbind(masterschedule,schedule)
}
```
\
One of the manual parts. The schedule doesn't provide team conferences. Therefore, I made a manual list of teams in the Big Ten
```r
b1g = c("Illinois","Indiana","Iowa","Nebraska","Northwestern","Michigan","Michigan State","Minnesota",
        "Ohio State","Penn State","Rutgers","Wisconsin","Purdue","Maryland","Washington","USC","UCLA","Oregon")
```
\
*Every scraper/source has team names different (such as using "St." vs. "State" for a school). I guessed and got lucky, but if you want a easy, full list of teams you can sift through, you can also add the following code*

```r
teams = masterschedule %>% distinct(home_team_location)
```
\
From there, filter `masterschedule` for only games in which a Big Ten team plays
```r
masterschedule = masterschedule %>% filter(home_team_location %in% b1g | away_team_location %in% b1g)
```
\
We want to show the full schedule for each team, we need to run a loop through for each team. This is because some games contain two Big Ten teams, but only appear once.
```r
newschedule = data.frame()
for(i in b1g){
  teamschedule = masterschedule %>% filter(home_team_location == i | away_team_location == i) %>% 
    mutate(team = i)
  newschedule = rbind(newschedule,teamschedule)
}
```
\
This next part is the bulk of the prep work. Now that we have the full schedule for each team within `newschedule`, we are going to prepare it for the layout we want. Primarily, we need the team's logo and the opponents' logos for each week. Additionally, we'll need to arrange everything in alphabetical order by team, and chronological order by week.
<br>\
*Quick note - this is where some of the ugly shows up. Since we want to create a conditional fill later for the game location, we need a way to do that. After we `pivot_wider` a bit later, it makes that more difficult. That's why we put the location of the game in the `opp_logo`. In the table, we'll shrink that text size down to zero so that it disappars, but use it in a `str_detect` to conditionally fill cells*
```r
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
```
\
More ugly, but a tad by necesscity. When we use `pivot_wider`, we will use the `names_from` the team logos and `values_from` opponent logos. We did this rather than by weeks because this will allow us to reorder the table in chronological order as week number becomes our row value. Then we will transpose to have weeks as column headers. 
<br>\
The problem with doing it the other way is that bye weeks show up as `NA` and reorder our weeks based off of Illinois (first alphabetically). Anyway, it's a bit messy to fix it up - so just a single extra line of code can prevent us from some extra work
```r
newpivot = newschedule %>% pivot_wider(names_from = team_logo,values_from = opp_logo) %>% arrange(week)
newpivot = as.data.frame(t(newpivot))
```
\
As mentioned, bye weeks show up as `NA`, so we just want to convert those to dashes (also shrunk down to 0 later and used to shade bye weeks)
```r
newpivot = newpivot %>% mutate_all(function(x) {ifelse(is.na(x),"--",x)})
```
\
Currently, the row names are the `team_logo` images that we'll want to use for each row identifer. So we'll just pull those row names, make a new column, and add it to the front of our data frame
```r
new_col = rownames(newpivot)
newpivot = cbind(new_col,newpivot)
rownames(newpivot) = 1:nrow(newpivot)
```
\
Also, becuase we are keeping row 1 and column 1 as our row and column names, we are just going to remove "week" from `newpivot[1,1]` because it sort of looks like the header for our team logos. And we don't need that
```r
newpivot$new_col[1]=""
```
\
Alright - time to make the table. First, customize our header, which has 3 main features
<ul>
<li><strong>B1G Logo:</strong> This you'll have to do manually. I just uploaded the logo to imgur and then used the url from there. You can replace it with any other conference doing the same thing</li>
<li><strong>Table Title:</strong> Simply, it's the 2024 Football Calendar. Since it centers off of the other objects in the header, I added some right padding in order to move it closer to center. You can adjust as needed</li>
<li><strong>Legend:</strong> Here is where we have the legend for the shading. Blue is home games, white is away games, gray is buy games. You can adjust colors here (and later in the table code) if you'd like to use a different color. I used something like 70% ligher of B1G Blue.</li>
</ul>

```r
title_header = glue(
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
```
\
Finally - the table. If you work through this, there's a few things you'll have to customize
<ul>
<li>Anywhere that columns and rows are noted (such as border, shading, or other formatting), you'll want to check these for your conference. Columns, likely there will be 15 no matter what - with column 1 being the team's logo and columns 2-15 being weeks 1-14. However, for rows, that is contingent on the number of teams in the conference. For the Big Ten, row 1 is the week number while the next 18 rows (rows 2-19) are each of the 18 teams.</li>
<li>More ugly. I couldn't figure out how to do the string detect across the entire table (that's where the majority of my time was spent. So that is why there are 14 occurances of shading gray for bye weeks and 14 occurances of shading blue of home games. If you change colors or anything, this is where you need to do that.</li>
<li>Finally, don't forget to set your path where you'd like the .png to be saved at the end. Also, edit the source note.</li>
</ul>

```r
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
  tab_style(style = cell_fill(color = "gray85"),locations = cells_body(columns = V1,rows = str_detect(V1,"--")))%>% 
  tab_style(style = cell_fill(color = "gray85"),locations = cells_body(columns = V2,rows = str_detect(V2,"--")))%>% 
  tab_style(style = cell_fill(color = "gray85"),locations = cells_body(columns = V3,rows = str_detect(V3,"--")))%>% 
  tab_style(style = cell_fill(color = "gray85"),locations = cells_body(columns = V4,rows = str_detect(V4,"--")))%>% 
  tab_style(style = cell_fill(color = "gray85"),locations = cells_body(columns = V5,rows = str_detect(V5,"--")))%>% 
  tab_style(style = cell_fill(color = "gray85"),locations = cells_body(columns = V6,rows = str_detect(V6,"--")))%>% 
  tab_style(style = cell_fill(color = "gray85"),locations = cells_body(columns = V7,rows = str_detect(V7,"--")))%>% 
  tab_style(style = cell_fill(color = "gray85"),locations = cells_body(columns = V8,rows = str_detect(V8,"--")))%>% 
  tab_style(style = cell_fill(color = "gray85"),locations = cells_body(columns = V9,rows = str_detect(V9,"--")))%>% 
  tab_style(style = cell_fill(color = "gray85"),locations = cells_body(columns = V10,rows = str_detect(V10,"--")))%>% 
  tab_style(style = cell_fill(color = "gray85"),locations = cells_body(columns = V11,rows = str_detect(V11,"--")))%>% 
  tab_style(style = cell_fill(color = "gray85"),locations = cells_body(columns = V12,rows = str_detect(V12,"--")))%>% 
  tab_style(style = cell_fill(color = "gray85"),locations = cells_body(columns = V13,rows = str_detect(V13,"--")))%>% 
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
  gtsave("/path/schedule.png", expand = c(30,60,30,60),vwidth =1600, zoom=4)
```
\
And that's that. Please reach out or make note if you have any questions or suggestions!
