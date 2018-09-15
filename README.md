# s4ggthemes
ggplot2 and highcharter themes for the S4G group

## Usage

```
devtools::install_github('stats4good/s4ggthemes')

library(ggplot2)
library(s4ggthemes)

ggplot(mtcars, aes(x = mpg)) + 
  geom_dotplot() +
  theme_veraz()
  
ggplot(mtcars, aes(wt, mpg)) +
  geom_point(color = factor(cyl)) +
  theme_veraz() +
  scale_color_veraz('main', discrete = T)
  
ggplot(mtcars, aes(wt, mpg)) +
  geom_point(color = factor(cyl)) +
  theme_veraz() +
  scale_color_veraz('secondary', discrete = T)
  
ggplot(mpg, aes(class)) +
  geom_bar(aes(fill = drv), position = 'dodge') +
  theme_veraz() +
  scale_fill_veraz(palette = 'secondary')
```
