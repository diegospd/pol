# Pol

A simple tree-based notetaker using [brick](https://hackage.haskell.org/package/brick) for the graphical interface.



By default it will try to parse a JSON file located in `~/.pol_tree`. If
the file doesn't exist it will start with an empty tree. All changes will
be written to that file for later use. You can change the filename to be used
in the `AppIO` module.

Features:

* Collapsing any node to hide/show its subtree.
* Reshaping the tree in any way by dragging nodes.
* Adding new elements at any given depth of the tree.
* Editing nodes.
* Deleting a node and its subtree.
* Saving the tree to disk.
* Undoing the last transformation to the tree. 
* The list of controls can be displayed by pressing `h`.


## Build and run

Use stack to build and run the project:

```sh
stack build
stack exec pol
```

Or by adding `~/.local/bin` to your `$PATH` and installing it with:

```sh
stack install
pol
```

More info on stack at http://docs.haskellstack.org/en/stable/README/.
