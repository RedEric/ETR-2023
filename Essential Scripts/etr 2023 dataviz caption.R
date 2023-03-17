library(glue)

# Dark background social caption ====

dark_bg_social_caption<-function(twitter="@heerymichael",
                         linkedin="michaelheery",
                         icon_color="#f5f5f5",
                         font_color="#f5f5f5",
                         bg_color="#ffffff",
                         font_family="Chivo"){
  
  icons = list(
    twitter = "&#xf081",
    linkedin = "&#xf08c",
  )  
  
  social = list(twitter =twitter, linkedin =linkedin)
  social = social[!is.na(social)]
  
  caption = ""
  for (name in names(social)){
    icon = icons[name]
    info = social[name]
    html = glue("<span style='font-family:\"Font Awesome 6 Brands\";color:{icon_color};'>{icon};</span><span style='color:{bg_color};'>.</span><span style='font-family:{font_family};color:{font_color};'>{info}</span><span style='color:{bg_color};'>..</span>")
    caption = paste0(caption,html)
  }
  
  caption
}
