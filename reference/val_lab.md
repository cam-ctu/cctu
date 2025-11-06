# Set or get value labels

These functions set/get/drop value labels. Duplicated values are not
allowed. If argument `x` is data.frame or list then labels applied to
all elements of data.frame/list. To drop value labels, use
`val_lab(var) <- NULL` or `unval(var)`. For variable labels see
[var_lab](https://cam-ctu.github.io/cctu/reference/var_lab.md).

- `val_lab`:

  returns value labels or NULL if labels doesn't exist.

- `val_lab<-`:

  set value labels.

- `unval`:

  drops value labels.

- `has_labels`:

  check if value labels exists.

## Usage

``` r
val_lab(x)

val_lab(x) <- value

has_labels(x)

unval(x)
```

## Arguments

- x:

  Variable(s). Vector/data.frame/list.

- value:

  Named vector. Names of vector are labels for the appropriate values of
  variable x. Names can be duplicated, but not the value.

## Value

`val_lab` return value labels (named vector). If labels doesn't exist it
return NULL . `val_lab<-` return variable (vector x) which contains
value labels.

## Details

Value labels are stored in attribute "labels" (`attr(x,"labels")`).

## References

This is a modified version from \`expss\` package.

## Examples

``` r
# toy example
set.seed(123)
# score - evaluation of tested product

score <- sample(-1:1, 20, replace = TRUE)
var_lab(score) <- "Evaluation of tested brand"
val_lab(score) <- c(
  "Dislike it" = -1,
  "So-so" = 0,
  "Like it" = 1
)
```
