# opacShapes
Different shapes with opacity attributes
## point
### original version
- version 1 | 
![png](./orgPoint1.png)
```{r}
png('./vignettes/orgPoint1.png',
    width = 400,
    height = 500,
    res = 100)
plot(
    x = 1:10,
    y = 1:10,
    type='n',
    main = 'original version 1'
)
point(
    x = 8,
    y = 8,
    r = 5,
    scale = FALSE
    )
dev.off()

```
- version 2 | x-limit changed
![png](./orgPoint2.png)
```{r}
png('./vignettes/orgPoint2.png',
    width = 400,
    height = 500,
    res = 100)
plot(
    x = 1:10,
    y = 1:10,
    type = 'n',
    xlim = c(1,30),
    main = 'original version 2'
    )
point(
    x = 8,
    y = 8,
    r = 5,
    scale = FALSE)
dev.off()

```


### rescaled version
![png](./scaledP.png)

```{r}
png('./vignettes/scaledP.png',
    width = 400,
    height = 500,
    res = 100)
plot(
    x = 1:10,
    y = 1:10,
    type = 'n',
    xlim = c(1,30),
    main = 'rescaled'
    )
point(
    x = 8,
    y = 8,
    r = 5)
dev.off()

```

## circle
![png](./circle.png)
```{}
png('./vignettes/circle.png')
plot(
    x = 1:10,
    y = 1:10,
    type = 'n',
    xlim = c(1,30),
    main = 'circle'
    )
circle(
    x = 8,
    y = 8,
    r = 10,
    fill = rgb(0.1,0.1,1, alpha=0.5),
    stroke = rgb(0,0,0, alpha=0.5),
    stroke.size = 4,
    stroke.position  = 'out'
)
text(8+5,8,'opac')
circle(
    x = 5,
    y = 5,
    r = 10,
    fill = rgb(0.1,0.1,1, alpha=0.5),
    stroke = rgb(0,0,0, alpha=0.5),
    stroke.size = 4,
    stroke.position  = 'mid'
)
text(5+4,5,'alpha')
dev.off()
```