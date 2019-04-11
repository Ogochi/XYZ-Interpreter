# Language XYZ

Project includes implementation of interpreter for custom language programming **XYZ**.
Interpreter is written purely in **haskell**.
Grammar of the language is presented in file `XYZgrammar.cf` in BNF format.
It allowed me to use [**BNFC**](https://bnfc.digitalgrammars.com/) to generate
easily parser and simple docs.

Everything is done as a part of class "Programming Languages and Paradigms" during summer term 2019
at University of Warsaw.

## Usage

It is as simple as:
```bash
make
./interpreter
```

More information:
```bash
./interpreter --help
```

## Language description

**XYZ** is imperative language similar to **C**/**Java**.

It is has strong and static typing with overriding of identifiers.
Every code interpretation begins with parsing.
After that takes place static check to find possible errors including incorrect types.
At the end happens real interpretation.

### Types
There are following allowed types

| Type | Default variable value | Description |
| :--: | :--------------------: | :---------: |
| `void` | **do not exist** | type for returning no value from function |
| `int` | 0 | - |
| `string` | "" | - |
| `bool` | `false` | - |
| [`type`] | **empty list** | list of `type` |
| Generator<`type`> | **do not exist** | generator instance of `type` |

For `int`, `string`, `bool` exist intuitive literals.
- `int` - plain number ex. 1234
- `string` - text inside quotes ex. "abc"
- `bool` - `true` or `false`

### Operators

Arithmetic operators: `+, -, /, *, %`

Compare operators: `<, >, <=, >=, ==, !=`.

Logic operators: `&&, ||, !`.

### Code structures

#### Comments

Comments are allowed using: `/* */` multi-line and `//` one line

#### Print
Printing expressions to stdout.

```Java
print(EXPRESSION);
```

#### Variables
```Java
int a = 2, b, c = 10;
a = a + 2;
print(a);
```

#### If, While
```Java
if (CONDITION) {
  BODY
}

if (CONDITION) {
  BODY1
} else {
  BODY2
}

while (CONDITION) {
  BODY
}
```

#### Lists
Lists acts like stacks. We can add value to the end, remove value from the end,
access value at particular position (indexing from 0) and get length;
```Java
// Declaration
[int] arr;
// Adding value at the end
arr.add(5);
// Printing specific value
print(arr[0]);
// Removing last elem
arr.drop();
// Printing length
print(arr.length)
```

#### Functions
Functions allows recursion and infinite definition nesting with static identifiers binding.

`ARGS` are function params separated with comma. Each of them is passed by value vy default. Param identifier can be proceeded by `&` passing it by reference.


```Java
// Definition
func RETURN_TYPE NAME(ARGS) {
  BODY
}

// Calling function
NAME(ARGS);
```

#### Generators
Generator are similar to **Python** ones. Inside the body can be included `yeld EXPRESSION`
statement to make generator return specific next value.

```Java
// Definition
func* RETURN_TYPE NAME(ARGS) {
  BODY
}

// Instantiation
Generator<RETURN_TYPE> myGenInstance;

// Printing next value
print(myGenInstance.next());
```

### Features

- Types: int, string, bool, list, generator_instance
- Variables with assignment
- If and While
- Arithmetics with + - * / ( ) and comparisions
- Functions with recursion and params by value/reference of every possible type
- Printing to stdout
- Overriding of identifiers with static binding
- Handling runtime exceptions
- Nesting of function definitions and returning every possible type
- "Lists"
- Generators
- Static typing

**Expected points count:** 30 / 30
