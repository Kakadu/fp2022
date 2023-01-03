### An implementaion of Make

```make
<target> [<target[s]>...]: [<prerequisite[s]>...]
    [<recipe[s]>...]
#[comments...]
<target> [<target[s]>...]: [<prerequisite[s]>...]
    [<recipe[s]>...]
...
```


This is a homework for functional programming course.

License: LGPL for implementation code + WTFPL for test examles in miniLanguage

Author: Ivan Morozko

TODO:
- Add variables
    - Recursively expanded variable (`=`)
    - Simply expanded variables (`:=`)
    - Conditional variable assignment (`?=`)
    - Multi-line variables (`define`)
    - Variables substitution (`$(var)`)
    - `$$`
    - `$(MAKE)` variable
- Add special variables
    - `.DEFAULT_GOAL`
    - `.PHONY`
- Add implicit rules
    - Add pattern rule definition (when the target contains `%`)
    - Add automatic variable `$*`
- Add functions
    - `call`
        - Support recursive invocation
        - Add arguments support (`$(num)`)
    - `eval`
    - `wildcard`
    - `shell`
    - `foreach`
- Add conditionals

Done:
- Explicit rules parsing
    - Support multiline prerequisite definitions
    - Support multiline recipes
    - Support recipe definiton via `;` on the targets line
    - Support recipe echoing aka prefixing recipe with `@`
    - Ignore comment lines
- Explicit rules interpreting
    - Dropping circular dependencies
    - Timestamp and file presence checking
    - Result verdicts, like
        - `No rule to make target 'x', needed by 'y'`
        - `Nothing to be done for 'x'`
        - `No rule to make target 'x'`

## Running tests

go to demos and run `dune exec demoInterpret`
