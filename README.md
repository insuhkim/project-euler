# Project Euler in OCaml

Solutions to [Project Euler](https://projecteuler.net/) problems written in OCaml.

## Structure

- `problems/` contains one source file for each solution.
- Files are named `pNNN.ml`, where `NNN` is the problem number.
- `inputs/` contains input files for problems that require external data.
- `gen/` contains the code generator for the problem dispatcher.
- `main.ml` is the command-line entry point.
- `dune` defines the build and generation rules.
- `devenv.nix` and `devenv.yaml` define the OCaml development environment.

## Requirements

- [OCaml](https://ocaml.org/)
- [Dune](https://dune.build/)

The repository includes a [Devenv](https://devenv.sh/) configuration that can
provide OCaml, Dune, and related development tools.

## Running a solution

Build and run a solution from the project root:

```sh
dune exec ./main.exe -- 1
```

Replace `1` with the problem number.

For example:

```sh
dune exec ./main.exe -- 14
dune exec ./main.exe -- 22
```

Dune automatically generates the problem dispatcher from the files in
`problems/`.

## Input files

Problems that require external input data can have a corresponding file in
`inputs/`.

Input files are named:

```text
pNNN_<filename>
```

For example:

```text
inputs/
├── p022_names.txt
├── p042_words.txt
└── p067_triangle.txt
```

When a problem is run, the program searches `inputs/` for a file beginning with
the corresponding problem number. If no input file exists, an empty string is
passed to the solution.

## Solution structure

Each problem exposes a "run" function with the common interface:

```ocaml
val run : string -> string
```

A problem that requires input can separate parsing and solving:

```ocaml
let parse input =
  ...

let solve data =
  ...

let run input =
  input
  |> parse
  |> solve
```

A problem that does not require external input can ignore the argument:

```ocaml
let solve () =
  ...

let run _ =
  solve ()
```

The `run` function always returns the answer as a `string`.

## Adding a solution

Create a new file in `problems/` using the format:

```text
pNNN.ml
```

For example:

```text
problems/p015.ml
```

No manual changes to the dispatcher are required. Dune detects the new
problem file and regenerates the dispatcher automatically.

Then run it with:

```sh
dune exec ./main.exe -- 15
```

## Development

Build the project with:

```sh
dune build
```
