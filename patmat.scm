(define-syntax check/unique
  (lambda (stx)
    "add equality checks for repeated identifiers in patterns and remove them from bindings"
    (syntax-case stx (and)
      ((check/unique condition () #f () bindings actions ... alternative)
       #'(if condition
             (let bindings actions ...)
             alternative))

      ((check/unique condition
                     ((variable path) . bindings)
                     #f
                     bindings/checked
                     bindings/final
                     actions ... alternative)
       #'(check/unique condition
                       bindings
                       (variable path)
                       bindings/checked
                       bindings/final
                       actions ... alternative))

      ((check/unique (and conditions ...)
                     ((variable path) . bindings)
                     (variable+ path+)
                     bindings/checked
                     bindings/final
                     actions ... alternative)
       (bound-identifier=? #'variable #'variable+)
       #'(check/unique (and conditions ... (equal? path path+))
                       bindings
                       (variable+ path+)
                       bindings/checked
                       bindings/final
                       actions ... alternative))

      ((check/unique conditions
                     ((variable path) . bindings)
                     (variable+ path+)
                     bindings/checked
                     bindings/final
                     actions ... alternative)
       #'(check/unique conditions
                       bindings
                       (variable+ path+)
                       ((variable path) . bindings/checked)
                       bindings/final
                       actions ... alternative))

      ((check/unique conditions
                     ()
                     (variable path)
                     bindings/checked
                     bindings/final
                     actions ... alternative)
       #'(check/unique conditions
                       bindings/checked
                       #f
                       ()
                       ((variable path) . bindings/final)
                       actions ... alternative))
      )))

(define-syntax match-clause
  (lambda (stx)
    (syntax-case stx (quote quasiquote unquote and _)
      ((match-clause () condition bindings actions ... alternative)
       #'(check/unique condition bindings #f () () actions ... alternative))

      ((match-clause ((`,pattern root) . rest)
                     condition
                     bindings
                     actions ... alternative)
       #'(match-clause ((pattern root) . rest)
                       condition
                       bindings
                       actions ... alternative))

      ((match-clause ((_ root) . rest)
                     condition
                     bindings
                     actions ... alternative)
       #'(match-clause rest
                       condition
                       bindings
                       actions ... alternative))

      ((match-clause ((variable root) . rest)
                     condition
                     bindings
                     actions ... alternative)
       (identifier? #'variable)
       #'(match-clause rest
                       condition
                       ((variable root) . bindings)
                       actions ... alternative))

      ((match-clause (('datum root) . rest)
                     (and conditions ...)
                     bindings
                     actions ... alternative)
       #'(match-clause rest
                       (and conditions ... (equal? root 'datum))
                       bindings
                       actions ... alternative))
      
      ((match-clause ((`(left . right) root) . rest)
                     (and conditions ...)
                     bindings
                     actions ... alternative)
       #'(match-clause ((`left (car root)) (`right (cdr root)) . rest)
                       (and conditions ... (pair? root))
                       bindings
                       actions ... alternative))

      ((match-clause ((literal root) . rest)
                     (and conditions ...)
                     bindings
                     actions ...)
       #'(match-clause rest
                       (and conditions ... (equal? literal root))
                       bindings
                       actions ...))
      )))

(define-syntax match/evaluated
  (syntax-rules ()
    ((match/evaluated value)
     ;; this behavior is unspecified, and an "unspecified"
     ;; value would also be fine here.
     (error 'no-matching-pattern))

    ((match/evaluated value (pattern actions ...) . clauses)
     (match-clause ((pattern value))
                   (and)
                   ()
                   actions ...
                   (match/evaluated value . clauses)))))

(define-syntax-rule (match expression (pattern actions* ... value) ...)
  (let ((evaluated expression))
    (match/evaluated evaluated (pattern actions* ... value) ...)))
