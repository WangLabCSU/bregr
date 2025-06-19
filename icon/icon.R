# <a href="https://www.flaticon.com/free-icons/shooting-star" title="shooting star icons">Shooting star icons created by Freepik - Flaticon</a>
library(hexSticker)

library(showtext)
font_add_google("Bellefair")
showtext_auto()

sticker("icon/shooting-stars.png", package="bregr",
        s_x=1, s_y=1.1, s_width=.5, s_height = 0.4,
        p_y = 0.5, p_x = 0.9, p_size=28,
        h_fill="#fff", h_color="#5B2787", h_size = 1,
        p_color = "#5B2787", p_family = "Bellefair",
        url = "https://github.com/WangLabCSU/bregr", u_size = 4, u_color = "#aaa",
        filename="man/figures/logo.png")

#use_logo("icon/logo.png")
