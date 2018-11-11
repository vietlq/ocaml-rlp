* https://jbuilder.readthedocs.io/en/latest/quick-start.html
* https://medium.com/@bobbypriambodo/starting-an-ocaml-app-project-using-dune-d4f74e291de8
* https://medium.com/@bobbypriambodo/getting-your-feet-wet-with-ocaml-ea1045b6efbc
* https://medium.com/@bobbypriambodo/using-ounit-to-test-your-ocaml-program-ce8e640a828c
* http://ounit.forge.ocamlcore.org/api-ounit/index.html
* https://github.com/facebook/hhvm/issues/3836
* https://stackoverflow.com/questions/29916625/c-program-in-os-x-file-was-built-for-archive-which-is-not-the-architecture-b
* https://inbox.ocaml.org/caml-list/CALdWJ+zQnaobJ7iGXo8gu1vo1ugdtaGByju6Kqp0KNzfnXHT9w@mail.gmail.com/

```
# Do not use ranlib by GNU! Otherwise OCaml will fall apart!
export PATH=/Library/Developer/CommandLineTools/usr/bin:$PATH
```

```
# Create local compiler suite, think of Python venv
opam switch create . ocaml-base-compiler.4.06.1
```
