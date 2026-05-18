# Internal accessor for a single cctu option

Resolves `name` against, in order: `base::getOption("cctu_<name>")`,
`cctu_env$options[[name]]`, then `.cctu_default_opts[[name]]`. Used
inside formal-argument defaults so call sites don't have to repeat the
packaged default literal.

## Usage

``` r
cctu_opt(name)
```

## Arguments

- name:

  Option name (without the `"cctu_"` prefix).

## Value

The resolved value, or an error if `name` is not a known option.
