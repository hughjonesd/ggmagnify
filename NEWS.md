# ggmagnify (development version)

# ggmagnify 0.4.1

* Bugfix: `inset_theme()` was overridden in explicitly passed-in inset `plot` 
  objects. Thanks @XanderDes.
  

# ggmagnify 0.4.0

* New `proj.fill` argument to fill the area between the projection lines.


# ggmagnify 0.3.0

* New `corners` argument sets corner radius on target and inset. Thanks
  to @thomasp85 for help with this.


# ggmagnify 0.2.0

* `from` and `to` can now be mapped to aesthetics, allowing different targets in
  different facets.
  - `from` can now be a logical vector of points to select.
  - Use `shape = "outline"` to magnify the convex hull of selected points.
* Changed the order of arguments in `from` and `to`. 
  - Before: `xmin, ymin, xmax, ymax`.
  - Now: `xmin, xmax, ymin, ymax`. This is much easier to think about.
  - You can name the arguments to avoid confusion.
* Added a `proj.combine` argument to draw combined or separate projection lines
  for multiple target areas.
* Added some helper functions to find rectangles, convex hulls and grobs.
* Added a `NEWS.md` file to track changes to the package.

