<!DOCTYPE html>
<html>
<body>

<h2>An implementation of Pascal mini-language</h2>

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
<br>
<ul>
    <li>AST</li>
    <li>Parser</li>
    <li>Semantic checker</li>
    <li>Interpreter</li>
</ul>
<br>
<ul>
    <li>
        Types:
        <ul>
            <li>boolean</li>
            <li>integer</li>
            <li>real</li>
            <li>char</li>
            <li>string</li>
            <li>fix size array</li>
            <li>record</li>
            <li>function / procedure</li>
            <li>custom</li>
        </ul>
    </li>
</ul>
<br>
<ul>
    <li>
        Expressions:
        <ul>
            <li>simple expressions</li>
            <li>array / record / string iterating</li>
            <li>custom function / procedure call</li>
            <li>some std functions, except inputting - outputting functions</li>
            <li>'@' unary operator for taking function / procedure as value
                <br>
                this operator takes only declared function, not a variable
                <br>
                the result of this operator can only be assigned to a variable        
            </li>
        </ul>
    </li>
</ul>
<br>
<ul>
    <li>
        Statements:
        <ul>
            <li>assign</li>
            <li>call</li>
            <li>if</li>
            <li>while / repeat</li>
            <li>for loop with counter</li>
            <li>continue / break / exit</li>
        </ul>
    </li>
</ul>
</td>

</body>
</html>
