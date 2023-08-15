# Quack-Stack

Quack-Stack is a personal project that stemmed from my interest in different programming languages, especially esoteric languages.
This language is my take on an esoteric programming language what is stack based.

Currently, the interpreter is being written in Haskell.

## Features

### AST Structure

The AST for Quack-Stack consists of stacks, integers, characters, booleans, primitive operations, routines, and quacks. Each of these is called a term.

Out of these terms, only quacks (represented with `!`) are not data, but they represent function evaluation.

We can categorize terms into two distinct catgories: clean and dirty.

Clean terms consist of:
- stacks containing clean terms
- integers
- characters
- booleans
- routines
- primitive operations

Dirty terms are:
- stacks containing dirty terms
- integers
- characters
- booleans
- routines
- primitive operations
- quacks

Here we can now explain what each of the terms is.

- A stack is a collection of terms with a front
- A integer is an unbounded integer
- A character is an ASCII character
- A boolean is either True or False
- A routine is a collection of dirty terms that are saved to evaluate later
- A primitive operation changes the stack that preceeded it in a specified way when evaluated
- A quack evaluates or applies the preceeding routine or primitive operation and the leftmost quack is the first applied

Since routines and primitive operations interact very similarly in most situations, we refer to these as applicables.

Every expression is a dirty stack.

Additionally, there are a couple of different primitive operations which are implemented:
- Add adds the previous two integer values in the stack
- Neg negates the previous integer value in the stack
- Not takes the boolean not of the previous boolean value in the stack
- And takes the boolean and of the previous two boolean value in the stack
- Or takes the boolean or of the previous two boolean value in the stack
- Compose composes the previous two applicables in the stack into a new routine
- Pop removes the top element of the preceeding substack and adds it to the stack
- Push removes the previous element of the stack and adds it to the top of the preceeding substack

### Evaluation Strategy

Since eveery expression is just a dirty stack, all we have to do in order to evaluate the expression is clean the stack.

Developing a stack cleaning strategy efficiently in Haskell relies on using a zipper. We keep removing from the bottom of the dirty stack, building up a clean stack until the dirty stack is empty. In this sense, we store the amount of evaluation that has been done so far, and the evaluation that is yet to be done together.

It might be of interest to note that evaluating a routine will create some dirty terms again.

Here is an example of this evaluation using a zipper.
`()` represents a routine, and `[]` is a stack

```
step evaluation
   [3 4 5 Neg ! Add Add Compose ! !]
=> [3 4 -5 Add Add Compose ! !]
=> [3 4 -5 (Add ! Add !) !]
=> [3 4 -5 Add ! Add !]
=> [3 -1 Add !]
=> [2]

step evaluation (with zipper)
   clean                    dirty
   []                       [3 4 5 Neg ! Add Add Compose ! !]
=> [3]                      [4 5 Neg ! Add Add Compose ! !]
=> [3 4]                    [5 Neg ! Add Add Compose ! !]
=> [3 4 5]                  [Neg ! Add Add Compose ! !]
=> [3 4 5 Neg]              [! Add Add Compose ! !]
=> [3 4 -5]                 [Add Add Compose ! !]
=> [3 4 -5 Add]             [Add Compose ! !]
=> [3 4 -5 Add Add]         [Compose ! !]
=> [3 4 -5 Add Add Compose] [! !]
=> [3 4 -5 (Add ! Add !)]   [!]
=> [3 4 -5]                 [Add ! Add !]
=> [3 4 -5 Add]             [! Add !]
=> [3 -1]                   [Add !]
=> [3 -1 Add]               [!]
=> [2]                      []
```

## Development Progress

### Implemented

- AST evaluation (Interpreter)

### To Implement

- Parser
- Closures / Environments

