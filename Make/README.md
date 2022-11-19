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
- Add recipe definiton via `;` on the targets line
- Support multiline recipes
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
- Add recipe echoing aka prefixing recipe with `@`
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
- Explicit rules parsing and iterpreting
    - Support multiline prerequisite definitions
- Ignore comment lines

