--
-- h4.hs
-- @author Sidharth Mishra
-- @description Haskell Parsec -- Parser combinators library
-- @copyright 2017 Sidharth Mishra
-- @created Mon Oct 23 2017 00:04:39 GMT-0700 (PDT)
-- @last-modified Mon Oct 23 2017 00:04:39 GMT-0700 (PDT)
--

-- Source code -> Lexer/Tokenizer --[tokens]--> Parser --[Abstract Syntax Tree (AST)] ----> Compilers/Interpreters

-- Tokenization
-- • Converts characters to the words of the language.
-- • Popular lexers:
--    • Lex/Flex (C/C++)
--    • ANTLR & JavaCC (Java)
--    • Parsec (Haskell)
-- Parsec is the tokenizer or Lexer in Haskell


-- Categories of Tokens:
-- • Reserved words or keywords
--    e.g. if, while
-- • Literals or constants
--    e.g. 123, "hello"
-- • Special symbols
--    e.g. ";", "<=", "+"
-- • Identifiers
--    e.g. balance, tyrionLannister


-- Parsing
-- • Parsers take tokens and combine them into abstract syntax trees (ASTs).
-- • Defined by context free grammars (CFGs).
-- • Parsers can be divided into:
--   • bottom-up/shift-reduce parsers
--   • top-down parsers

-- Context Free Grammars
-- • Grammars specify a language
-- • Backus-Naur form format
--      Expr -> Number
--            | Number + Expr
-- • Terminals cannot be broken down further.
-- • Non-terminals can be broken down into further phrases.

-- Sample Grammar (CFG)
-----------------------------
-- expr -> expr + expr
--      | expr – expr
--      | ( expr )
--      | number
--
-- number -> number digit
--        | digit
--
-- digit -> 0 | 1 | 2 | … | 9


-- Bottom-up Parsers
-- • a.k.a. shift-reduce parsers
--    1. shift tokens onto a stack
--    2. reduce to a non-terminal.
--      • LR: left-to-right, rightmost derivation
--      • Look-Ahead LR parsers (LALR): most popular style of LR parsers –YACC/Bison
-- • Fading from popularity.

-- Top-down parsers
-- • Non-terminals expanded to match tokens.
-- • LL: left-to-right, leftmost derivation
-- • LL(k) parsers look ahead k elements
--    • example LL(k) parser: JavaCC
--    • LL(1) parsers are of special interest