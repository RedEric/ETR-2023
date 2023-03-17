
# Loading relevant libraries ====
library(sysfonts)
library(fontawesome)
library(showtext)
showtext_auto()
#adjust dpi in showtext -- fix issues with saving (showtext + ggtext problem)
showtext::showtext_opts(dpi = 300)


# Loading Chivo fonts ====
font_add('Chivo', 'fonts/Chivo-Regular.tff')
font_add('Chivo-Bold', 'fonts/Chivo-Bold.tff')
font_add('Chivo-Italic', 'fonts/Chivo-Italic.tff')
font_add('Chivo-BoldItalic', 'fonts/Chivo-BoldItalic.tff')
font_add('Chivo-LightItalic', 'fonts/Chivo-LightItalic.tff')
font_add('Chivo-Light', 'fonts/Chivo-Light.tff')

# Sora for headers ====
font_add('Sora-Bold', 'fonts/Sora-Bold.tff')
font_add('Sora-Regular', 'fonts/Sora-Regular.tff')

# Inter for subtitles ====
font_add('Inter', 'fonts/Inter-Regular.tff')
font_add('Inter-Bold', 'fonts/Inter-Bold.tff')       

# Font awesome ====
font_add('fs', 'fonts/Font Awesome 6 Free-Solid-900.otf')
font_add('fb', 'fonts/Font Awesome 6 Brands-Regular-400.otf')
