# Helper functions to find rectangles or convex hulls of data

Helper functions to find rectangles or convex hulls of data

## Usage

``` r
rect_around(x, y, data = NULL, expand = 0)

hull_around(x, y, data = NULL, expand = 0)
```

## Arguments

- x, y:

  Unquoted names or expressions

- data:

  A data frame

- expand:

  Amount to expand the data around its midpoint. Default is 10 per cent.

## Value

`rect_around()` returns a list with names `xmin`, `xmax`, `ymin`, and
`ymax`. `hull_around()` returns a data frame with columns `x` and `y`.

## Examples

``` r
library(ggplot2)
to <- c(2, 4.5, 6, 8)
setosas <- iris[iris$Species == "setosa", ]
rect_around(Sepal.Width, Sepal.Length, data = setosas)
#> $xmin
#> [1] 2.3
#> 
#> $xmax
#> [1] 4.4
#> 
#> $ymin
#> [1] 4.3
#> 
#> $ymax
#> [1] 5.8
#> 
hull_around(Sepal.Width, Sepal.Length, data = setosas)
#>     x   y
#> 1 3.6 4.6
#> 2 3.0 4.3
#> 3 2.3 4.5
#> 4 3.5 5.5
#> 5 3.8 5.7
#> 6 4.0 5.8
#> 7 4.4 5.7
#> 8 4.1 5.2
```
