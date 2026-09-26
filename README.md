# Project Euler in OCaml

Solutions to Project Euler problems written in OCaml.

## Structure

- `problems/` contains one source file for each solution.
- Files are named `pNNN.ml`, where `NNN` is the problem number.
- `devenv.nix` and `devenv.yaml` define the OCaml development environment.

## Requirements

- OCaml

The repository includes a Devenv configuration that can provide OCaml and
related tools.

## Running a solution

From the project root, run a solution directly with:

```sh
ocaml problems/p001.ml
```

To use the configured development environment:

```sh
devenv shell
ocaml problems/p001.ml
```

Replace `p001.ml` with the solution you want to run.

## Adding a solution

Create a new file in `problems/` using the format `pNNN.ml`, then run it with
the OCaml interpreter.
