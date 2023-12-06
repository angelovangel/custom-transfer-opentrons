
labware <- vroom('data/labware.csv') %>%
  mutate(
    img_icon = sprintf("<img src='%s' width=30px><div class='jhr'>%s</div></img>", img, id)
  )

select_labware <- split.data.frame(
  labware, f = list(labware$type)
  ) %>% 
  lapply( FUN = function(i) c(i[['id']]) )
