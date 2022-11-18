## An implementation of Pascal mini-language

<table>
    <tr style="vertical-align:top">
        <td width="128">
        <img src="logo.png" width="128" >
        </td>
        <td>
        This is a homework for functional programming course.
        <br><br>
        License: LGPL for implementation code + WTFPL for test examples in miniLanguage
        <br><br>
        Author: Anton Kazancev, st076546@student.spbu.ru
        <br><br>
    </tr>
</table>

Features done (append only):
    
- Ast
- Parser
- Semantic checker
- Interpreter

- Types:
    - boolean
    - integer
    - real
    - char
    - string
    - fix size array
    - record
    - function / procedure
    - custom

- Expressions:
    - simple expressions
    - array / record / string iterating
    - custom function / procedure call
    - some std functions, except inputting - outputting functions
    - '@' unary operator for taking function / procedure as value
        this operator takes only declared function, not a variable
        the result of this operator can only be assigned to a variable

- Statements:
    - assign
    - call
    - if
    - while / repeat
    - for loop with counter
    - continue / break / exit
