library(hexSticker)
library(showtext)

## Loading Google fonts (http://www.google.com/fonts)
# font_add_google("Bowlby One SC", "seb")
font_add_google("Syncopate", "seb")##
# font_add_google("Michroma", "seb")
# font_add_google("Allerta Stencil", "seb")
## Automatically use showtext to render text for future devices
showtext_auto()

sticker(
  "data-raw/income-chart-white.png",
  package = "nfl4th",
  p_family = "seb",
  p_y = 1.4,
  p_size = 40,
  p_color = "white",
  s_x = 1,
  s_y = 0.75,
  s_width = 0.45,
  s_height = 0.5,
  spotlight = TRUE,
  l_y = 1.25,
  l_alpha = 0.3,
  l_width = 5,
  h_fill = "#8e8c84",
  h_color = "#3e3f3a",
  h_size = 0.5,
  # filename = "man/figures/logo.png",
  # filename = "data-raw/logo_red.svg",
  url = "https://guga31bb.github.io/nfl4th",
  u_color = "white",
  u_size = 6,
  u_angle = -30,
  u_x = 0.475,
  u_y = 0.35,
  dpi = 600
)

# <div>Icons made by <a href="https://www.flaticon.com/authors/dinosoftlabs" title="DinosoftLabs">DinosoftLabs</a> from <a href="https://www.flaticon.com/" title="Flaticon">www.flaticon.com</a></div>
