# Puissance 4

Jeu de Puissance 4, programmé en 2005, et publié sur GitHub en février 2024.

## Compilation

- Installer [OCaml](https://ocaml.org/docs/installing-ocaml).

- Cloner ce repository:

```bash
git clone https://github.com/davdiv/puiss4
cd puiss4
```

- Installer les dépendances:

```bash
opam install . --deps-only
```

- Compiler:

```bash
opam exec -- dune build
```

- Exécuter le programme:

```bash
./_build/default/bin/puiss4.exe
```

Remarque: étant donné que le programme ne calcule pas jusqu'à la fin du jeu (cela supprimerait tout espoir !!), il est possible de le battre. Plus il y a de colonnes remplies, plus il calcule à l'avance ! Méfiez-vous !

Amusez-vous bien !
