; http://rosettacode.org/wiki/Compiler/lexical_analyzer#Ol
(import (data parse))

(define (comment)
   (either
      (let-parse* (
            (_ (byte #\*))
            (_ (byte #\/)))
         #true)
      (let-parse* (
            (_ (byte))
            (_ (comment)))
         #true)))

(define whitespace
   (any-of
      (byte (lambda (x) (has? '(#\tab #\newline #\space #\return) x))) ; whitespace
      (let-parse* ( ; comment
            (_ (byte #\/))
            (_ (byte #\*))
            (_ (comment)))
         #true)))

(define operator
   (let-parse* (
         (operator (any-of
            (bytes "||" 'Op_or)
            (bytes "&&" 'Op_and)
            (bytes "!=" 'Op_notequal)
            (bytes "==" 'Op_equal)
            (bytes ">=" 'Op_greaterequal)
            (bytes "<=" 'Op_lessequal)

            (bytes "=" 'Op_assign)
            (bytes "!" 'Op_nop)
            (bytes ">" 'Op_greater)
            (bytes "<" 'Op_less)
            (bytes "-" 'Op_subtract)
            (bytes "+" 'Op_add)
            (bytes "%" 'Op_mod)
            (bytes "/" 'Op_divide)
            (bytes "*" 'Op_multiply))))
      (cons 'operator operator)))

(define symbol
   (let-parse* (
         (symbol (any-of
            (bytes "(" 'LeftParen)
            (bytes ")" 'RightParen)
            (bytes "{" 'LeftBrace)
            (bytes "}" 'RightBrace)
            (bytes ";" 'Semicolon)
            (bytes "," 'Comma))))
      (cons 'symbol symbol)))

(define keyword
   (let-parse* (
         (keyword (any-of
            (bytes "if" 'Keyword_if)
            (bytes "else" 'Keyword_else)
            (bytes "while" 'Keyword_while)
            (bytes "print" 'Keyword_print)
            (bytes "putc" 'Keyword_putc))))
      (cons 'keyword keyword)))



(define identifier
   (let-parse* (
         (lead (byte          (lambda (x) (or (<= #\a x #\z) (<= #\A x #\Z) (= x #\_)))))
         (tail (greedy* (byte (lambda (x) (or (<= #\a x #\z) (<= #\A x #\Z) (= x #\_) (<= #\0 x #\9)))))))
      (cons 'identifier (bytes->string (cons lead tail)))))

(define integer
   (let-parse* (
         (main (greedy+ (byte (lambda (x) (<= #\0 x #\9))))) )
      (cons 'integer (string->integer (bytes->string main)))))

(define character
   (let-parse* (
         (_ (byte #\'))
         (char (any-of
            (bytes "\\n" #\newline)
            (bytes "\\\\" #\\)
            (byte (lambda (x) (not (or (eq? x #\') (eq? x #\newline)))))))
         (_ (byte #\')) )
      (cons 'character char)))

(define string
   (let-parse* (
         (_ (byte #\"))
         (data (greedy* (any-of
            (bytes "\\n" #\newline)
            (bytes "\\\\" #\\)
            (byte (lambda (x) (not (or (eq? x #\") (eq? x #\newline))))))))
         (_ (byte #\")) )
      (cons 'string (bytes->string data))))

(define token
   (let-parse* (
         (_ (greedy* whitespace))
         (token (any-of
            symbol
            keyword
            identifier
            operator
            integer
            character
            string
         )) )
      token))

(define token-parser
   (let-parse* (
         (tokens (greedy+ token))
         (_ (greedy* whitespace)))
      tokens))


(define (translate source)
   (let* ((tokens stream (let-parse token-parser (str-iter source))))
      (for-each print tokens)
      (if (null? stream)
         (print 'End_of_input))))


(translate "
/*
  Hello world
 */
print(\"Hello, World!\\\\n\");
")
(print)
(translate "
/*
  Show Ident and Integers
 */
phoenix_number = 142857;
print(phoenix_number, \"\\\\n\");
")
(print)
(translate "
/*
  All lexical tokens - not syntactically correct, but that will
  have to wait until syntax analysis
 */
/* Print   */  print    /* Sub     */  -
/* Putc    */  putc     /* Lss     */  <
/* If      */  if       /* Gtr     */  >
/* Else    */  else     /* Leq     */  <=
/* While   */  while    /* Geq     */  >=
/* Lbrace  */  {        /* Eq      */  ==
/* Rbrace  */  }        /* Neq     */  !=
/* Lparen  */  (        /* And     */  &&
/* Rparen  */  )        /* Or      */  ||
/* Uminus  */  -        /* Semi    */  ;
/* Not     */  !        /* Comma   */  ,
/* Mul     */  *        /* Assign  */  =
/* Div     */  /        /* Integer */  42
/* Mod     */  %        /* String  */  \"String literal\"
/* Add     */  +        /* Ident   */  variable_name
/* character literal */  '\\n'
/* character literal */  '\\\\'
/* character literal */  ' '
")

(print)
(translate "
/*** test printing, embedded \\\\n and comments with lots of '*' ***/
print(42);
print(\"\\\\nHello World\\\\nGood Bye\\\\nok\\\\n\");
print(\"Print a slash n - \\\\\\\\n.\\\\n\");
")
