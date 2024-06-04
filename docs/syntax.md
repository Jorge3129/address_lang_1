# ADPL Syntax

ADPL is an adaptation of the Address Programming Language that allows to write programs using only ASCII characters.

### Mathematical expressions

In general, most mathematical expressions in ADPL are written in the same way as in other modern programming languages.

Thus, float literals are written using a dot (instead of a comma like in the original syntax).

### Line labels

In the original syntax, line labels can contain digits, characters and event spaces.  
In ADPL, a line label can only contain Latin letters, digits and the _ character, and it must start with a letter.

A line can be marked with one or more labels. In the original syntax an ellipsis must follow after each label:

```
lab1 ... lab2 ...  <statements>
```

In ADPL, an additional requirement is to put a @ character before each label:

```
@lab1 ... @lab2 ...  <statements>
```



### Stroke operation

The stroke operation is written as ':

```
'a = b
```

In the original syntax, the multiple stroke operation is written as **<sup>n</sup>a**.  
In ADPL the equivalent for that is `` `n`a ``.

The negative stroke operation is written as **<sup>-n</sup>a** or **<sub>n</sub>a**.
In ADPL the equivalent for that is `` m`n`a ``, where ``m`` stands for "minus" (negative).

### Empty set character

In the original syntax, the empty set character ∅ is used to represent a null value, or a placeholder for a value in some expressions.
In ADPL this is represented by the keyword ``Nil``.