# Clojox

Implementation of the Lox programming language in Clojure.

Lox is a dynamically typed, high-level scripting language from the book [Crafting Interpreters](https://craftinginterpreters.com/). The book contains two parts where you implement the Lox language in two different styles. In the first part, you implement a tree-walk interpreter using all the niceties of a high-level language like Java, _jlox_. In the second part, you implement a bytecode interpreter in C, _clox_.

This is a tree-walk interpreter which covers the part 1 of the book. I've not added classes and interfaces as I don't care about it as such. It can easily be added if you want to.

## Implementation details

The implementation differs a lot from the _jlox_ implementation in Java. Mine is a lot simpler codewise and doesn't have the caveats which are mentioned in the book. Like, I don't have a resolver step (Chapter 10) because my environment uses immutable persistent data structure and the resolving isn't mandatory since there's no environment leak.

### Scanner
The Scanner code is written in Java and the implementation is near identical as the book. I initially thought I'd do the book in Java, but after just one chapter I realized life is too short to be willingly writing Java. Also, the visitor pattern scared me.

### Parser
Parser function receives a vector of `Token` Java class, which includes details like lexeme, token type, and the literal value of the token.

The implementation of the recursive descent parser is similar to the book. The only change is I pass around the tokens in a functional manner. I rely heavily on destructuring and using `rest` to pass the remaining set of tokens to the next grammar function.

Example:

``` Lox
(defn- print-statement
  [tokens]
  (let [[[next-token & remaining :as tokens] expr] (expression tokens)]
    (when-not (match? next-token TokenType/SEMICOLON)
      (throw-error tokens "Expect ';' after value."))
    [remaining (->Print expr)]))
```

A grammar function returns a vector of length 2, containting a vector of remaining tokens that are yet to be processed and a `defrecord` describing the AST.

### Interpreter

Clojure has stellar support for expression problem. Instead of visitor pattern, there's multimethods and Protocols. I initially used multimethods for interpreting the ASTs. However, I ended up using Protocols for performance reasons.

To interpret, the function receives a vector of ASTs from the Parser. The ASTs are _records_ which implements the protocol. Each AST record is evaluated in a loop.

Here's an example of interpretation of the `print` statement.

``` Lox
(defrecord Print [expression]
  protocols/Evaluate
  (evaluate [_ env]
    (let [[value env] (evaluate expression env)]
      (println (stringify value))
      [nil env])))
```

The interpreter function returns a vector of length 2, containing the value returned (nil for the print method) and the environment map.

### Environment

To implement lexical scoping, I have used a combination of immutable persistent maps and Clojure atoms for value referencing. My environment structure is flat, unlike the nested one in the book which uses [parent-pointer tree](https://en.wikipedia.org/wiki/Parent_pointer_tree). I pass the environment to each 'visit' function and make it return the new environment if there has been any change.

## Grammar addition

I've added a `declare` keyword to support mutual recursion. Just like how it's done in [Clojure](https://clojuredocs.org/clojure.core/declare).

## Performance

Performance wasn't the main agenda, but I wanted it to be close to _jlox_, since both are running on JVM. It isn't. There's definitely room for improvement. The below fibonacci code is 4.5 times slower than the Java implementation.

```Lox
fun fib(n) {
  if (n < 2) return n;
  return fib(n - 1) + fib(n - 2);
}

var before = clock();
print fib(40);
var after = clock();
print after - before;
```
