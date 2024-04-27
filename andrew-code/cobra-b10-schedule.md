cobra \| big ten football schedule
================
Andrew Weatherman
2024-04-27

load libraries

``` r
library(cfbfastR)
library(dplyr)
library(gt)
library(gtExtras)
library(stringr)
library(glue)
library(tidyverse) 
# new packages
library(cbbdata)
library(rlang)
library(nflreadr)
```

## data

get schedule. no looping is needed, actually. you don’t need to specify
a week. you can use the calendar function, instead, to pull week numbers
and join together. you *do* need to specify a limit of 1000, though, or
you won’t get all games. we can use the relatively new “overlap join”
functionality in the `x_join` family.

I’m actually not sure if this is faster, I think it is, but I thought it
would be worth showing off how to use `between` in this context. and
when pulling data from somewhere, it’s usually preferred to find ways to
not constantly hit endpoints if you don’t need to.

``` r
schedule <- espn_cfb_schedule(year = 2024, limit = 1000) %>% 
  filter(is.na(home_record)) %>% # remove games that have occured
  select(home_team = home_team_location, away_team = away_team_location, date = game_date) %>% 
  mutate(date = as.Date(date, format = "%Y-%m-%dT%H:%MZ"))

weeks <- espn_cfb_calendar(year = 2024) %>% 
  select(week, start_date, end_date) %>% 
  mutate(across(-week, ~as.Date(.x, format = "%Y-%m-%dT%H:%MZ")))

schedule <- left_join(
  schedule,
  weeks,
  join_by(between(date, start_date, end_date))
)
```

filter for big ten teams.

``` r
b1g = c("Illinois", "Indiana", "Iowa", "Nebraska", "Northwestern", "Michigan", "Michigan State", "Minnesota",
        "Ohio State", "Penn State", "Rutgers", "Wisconsin", "Purdue", "Maryland", "Washington", "USC", "UCLA", "Oregon")

schedule <- schedule %>% filter(home_team %in% b1g | away_team %in% b1g)
```

there’s a really nifty function from `nflreadr` called `clean_homeaway`
that will handle much of this pivoting for you. the only real catch is
that your columns need to be in `home_` and `away_` format, which is why
we originally labeled our home team as `home_team` and vice-versa.

I think that the need to fill columns based on game location complicates
things a bit. i briefly thought of a few different ways to attack this,
but I just kept coming back to encoding HTML. it seems like the quickest
way to accomplish that and just get to pivoting and plotting without
much hassle. a nifty idea I had was to encode an alt tag in each string,
which will not affect the logo plotting, that refers to the game
location. it’s a harmless way of keeping that data in our pivoted frame
w/o adding more columns.

we can add the logos by hitting `cbbdata` since all of these teams are
also in my data; just be sure to look for `espn_location`. you could
join this data, but that would get messy. instead, let’s create a named
vector and then use `across`. this is a really useful tool for
situations like this. you can see below how clean the code looks.

``` r
logos <- cbd_teams() %>% select(team = espn_location, logo)
logos <- logos %>% pull(logo) %>% rlang::set_names(logos$team)

plot_data <- schedule %>% 
  select(home_team, away_team, week) %>% 
  clean_homeaway() %>% 
  mutate(is_b1g = ifelse(team %in% b1g, 1, 0)) %>% # is team in big ten
  filter(is_b1g == 1) %>% # only keep those teams
  mutate(opponent = glue("<img src='{logos[opponent]}' alt={location} style='height:30px; vertical-align:middle;'>")) %>% 
  pivot_wider(id_cols = team, names_from = week, values_from = opponent) %>% 
  arrange(team) %>% 
  mutate(team = logos[team])
```

## plotting

I’m just showing how to make the body of the table w/ some different
functions. This will not include the header, other things, etc.

### conditional highlighting

okay, so most of this is pretty straightforward. the only *weird* thing
is how I handled conditional formatting based on location. [more on that
here](https://x.com/andreweatherman/status/1784328837347582211). it’s
just an awkward thing to do w/ no great native solution.

you could rewrite this to be cleaner and do things in just two steps,
but I wanted to keep things a bit more verbose and segragated here.

the `arrayInd` thing is a base R way to grab row and column indicies
that much certain conditions.

``` r
home_cells <- arrayInd(
    which(str_detect(as.matrix(plot_data), 'alt=home')), .dim = dim(plot_data)
)

bye_cells <- arrayInd(
    which(is.na(as.matrix(plot_data))), .dim = dim(plot_data)
)

# then generate the css string
home_cells_css <- map2_chr(
  .x = home_cells[,1],
  .y = home_cells[,2],
  .f = ~glue(".gt_table tbody tr:nth-child({.x}) td:nth-child({.y}) {{ background-color: #cce7f5; }}")
)

bye_cells_css <- map2_chr(
  .x = bye_cells[,1],
  .y = bye_cells[,2],
  .f = ~glue(".gt_table tbody tr:nth-child({.x}) td:nth-child({.y}) {{ background-color: #d9d9d9; }}")
)
```

### table

aside from the above, yeah, most things here are pretty basic. you can
use `sub_missing` to impute new values for NA cells (bye games). you can
also use `gt_add_divider` as a wrapper for adding cell borders.

``` r
plot_data %>% 
  gt(id = 'table') %>% 
  gt_theme_538() %>% 
  fmt_image(team) %>%
  fmt_markdown(-team) %>% 
  # use sub_missing to replace na with empty text string
  sub_missing(-team, missing_text = '') %>% 
  cols_align(columns = everything(), 'center') %>% 
  cols_label(team = '') %>% 
  # bold col. headers
  tab_style(locations = cells_column_labels(), style = cell_text(weight = 'bold')) %>% 
  # add dividers
  gt_add_divider(columns = -team, sides = 'bottom', include_labels = FALSE, color = 'black', weight = px(1.5)) %>% 
  # apply above css
  opt_css(c(home_cells_css, bye_cells_css))
```
