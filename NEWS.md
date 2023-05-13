# ggmagnify 0.2.0

* `from` and `to` can now be mapped to aesthetics, allowing different targets in
  different facets.
  - `from` can now be a logical vector of points to select.
  - Use `shape = "hull"` to magnify the convex hull of selected points.
* Changed the order of arguments in `from` and `to`. 
  - Before: `xmin, ymin, xmax, ymax`.
  - Now: `xmin, xmax, ymin, ymax`. This is much easier to think about.
  - You can name the arguments to avoid confusion.
* Added a `NEWS.md` file to track changes to the package.

