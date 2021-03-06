# OCaml finder #
This tool is small program as finder, such like fzf, peco.

# TODO #
- [ ] Read from input asynchronously

# Install #

Use `opam` to install `oif` . Currently this package do not merge into opam repository, so you should install locally.

```shell
$ git clone https://github.com/derui/oif
$ cd oif
$ opam install .
```

# Usage #
`oif` is made for pipe from some input.

```shell
$ ls | oif

# If you use migemo together
$ ls | oif --migemo_dict_directory=<dict directory>
# or
$ export OIF_DEFAULT_MIGEMO_DICT_DIRECTORY=<dict directory>
$ ls | oif

# Using pipe after oif is allowed
$ ls | oif | sed -e 's/ /-/g'
```

# Key bindings #

- `C-n/C-p`
  - move cursor next/previous
- `Enter`
  - end program and print selected candidate or marked candidates to stdout
- `Tab`
  - mark current selected candidate

# Options #

```
# If you want to read all info, please type `oif --help`

OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of `auto',
           `pager', `groff' or `plain'. With `auto', the format is `pager` or
           `plain' whenever the TERM env var is `dumb' or undefined.

       --migemo_dict_directory=VAL (absent OIF_DEFAULT_MIGEMO_DICT_DIRECTORY env)
           Directory of migemo directory

       --prompt=VAL (absent OIF_DEFAULT_PROMPT env)
           Prompt of input (Default is 'QUERY> ')

       -q VAL, --query=VAL (absent OIF_DEFAULT_QUERY env)
           Initial query

ENVIRONMENT
       These environment variables affect the execution of oif:

       OIF_DEFAULT_MIGEMO_DICT_DIRECTORY
           Directory of migemo dictionary

       OIF_DEFAULT_PROMPT
           Prompt of input (Default is 'QUERY> ')

       OIF_DEFAULT_QUERY
           Initial query
```

# Development #

## Build ##

```shell
$ dune build
```

## Test ##

```shell
$ dune runtest
```

# License #

MIT License
