#lang racket

(struct Pest (vars start) #:transparent)

(struct Eps () #:transparent)
(struct Any () #:transparent)
(struct Sym (ch) #:transparent)
(struct Var (v) #:transparent)

(struct Cat (l r) #:transparent) ;; e1 ~ e2 -- ok
(struct Alt (l r) #:transparent) ;; e1 | e2 -- ok
(struct Kln (e) #:transparent) ;; e* -- ok
(struct Rep (p) #:transparent) ;; e+ -- ok
(struct RepN (e n) #:transparent) ;; e{n} -- ok
(struct Min (e n) #:transparent) ;; e{n,} -- ok
(struct Max (e n) #:transparent) ;; e{, n} -- ok
(struct Rng (e min max) #:transparent) ;; e{m, n} -- ok
(struct Opt (e) #:transparent) ;; e? -- ok
(struct Keep (e) #:transparent) ;; &e = matches e without making progress -- ok
(struct Not (p) #:transparent) ;; !e = matches if e doesn’t match without making progress -- ok
(struct Push (e) #:transparent) ;; matches e and pushes it’s captured string down the stack -- ok
(struct Pop () #:transparent) ;; pops a string from the stack and matches it -- ok
(struct PopAll () #:transparent) ;; pops the entire state of the stack and matches it -- ok
(struct Peek () #:transparent) ;; peeks a string from the stack and matches it -- ok
(struct PeekAll () #:transparent) ;; peeks the entire state of the stack and matches it
(struct Drop () #:transparent)
(struct DropAll () #:transparent)
(struct Tonat () #:transparent)

(provide (all-defined-out))
