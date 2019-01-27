library(tidyverse) 
library(magick)
library(scales) 
library(imager) 
library(zoo)
im <- image_read("https://i.redd.it/5hv4l3032bc01.jpg")
im %>% image_resize(500)
im %>% image_resize(500) %>% image_average()
im %>% image_resize(500) %>% image_normalize()
im %>% image_resize(500) %>% image_enhance()
im %>% image_resize(500) %>% image_quantize()
# get color palette
get_colorPal <- function(im, n=8, cs="RGB"){
  #print(cs) 
  tmp <- im %>% image_resize("100") %>% 
    image_quantize(max = n, colorspace=cs) %>% # reduce colorspace
    magick2cimg() %>%  # convert to get a data.frame output
    RGBtoHSV() %>% ## sorting colour by hue rather than RGB
    as.data.frame(wide ="c") %>%  #3 making it wide makes it easier to output hex colour
    mutate(hex = hsv(rescale(c.1, from = c(0,360)),c.2,c.3),
           hue = c.1,
           sat = c.2,
           value = c.3) %>%
    count(hex, hue, sat,value, sort=T) %>% 
    mutate(colorspace = cs)
  
  return(tmp %>% select(colorspace,hex,hue,sat,value,n)) ## I want data frame as a result.
  
}
palette1 <- get_colorPal(im, n = 24)


params <- list(im=list(im), 
               n=12, ## number of colour you want 
               cs=colorspace_types()[-5]) ## gray fails so I've removed it...
# map get_colorPal with different parameters over image
my_colors <- pmap_df(params,get_colorPal)
# plot result to get intuition for the structure of my_colors
my_colors %>%  
  group_by(colorspace) %>%
  mutate(ypos = row_number(value)) %>%  ## I decided to stack colours by value. 
  ggplot(aes(x = fct_infreq(colorspace),y = ypos, fill = hex)) +  
  geom_tile() +
  geom_text(aes(label = hex), color = "#ffffffbe", 
            size = 4, family = "Roboto Condensed") +
  scale_fill_identity() +
  scale_y_continuous(breaks = NULL) +
  theme_void(base_family = "Roboto Condensed") +
  coord_flip(ylim = c(1,12)) +
  theme(axis.text = element_text(color = "black", family="Roboto Condensed", hjust = 1)) +
  labs(caption = "Using different colourspce to reduce the colour used in images")

# spread to put every palette into its own column 
spreadtest <- spread(my_colors, colorspace, hex)
spreadtest <- spreadtest[,5:ncol(spreadtest)]
natest <- spreadtest[,1]

# return a dense data frame
desparse <- function(df) {
  z <- zoo()
  for (i in 1:ncol(df)) {
    temp <- na.omit(df[,i])
    z <- merge(z, temp )
  }
  z <- as.data.frame(z)
  return(z)
}
dense <- desparse(spreadtest)

          