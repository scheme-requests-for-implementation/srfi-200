(define-syntax check/unique
  (syntax-rules (and)
    ((check/unique condition () #f () bindings actions ... alternative)
     (if condition
         (let bindings actions ...)
         alternative))
      
    ((check/unique condition 
                   ((variable path) . bindings)
                   #f
                   bindings/checked
                   bindings/final
                   actions ... alternative)
     (check/unique condition
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
     (same-variable variable variable+
                    (check/unique (and conditions ... (equal? path path+))
                                  bindings
                                  (variable+ path+)
                                  bindings/checked
                                  bindings/final
                                  actions ... alternative)
                    (check/unique (and conditions ...)
                                  bindings
                                  (variable+ path+)
                                  ((variable path) . bindings/checked)
                                  bindings/final
                                  actions ... alternative)))
      ((check/unique conditions
                     ()
                     (variable path)
                     bindings/checked
                     bindings/final
                     actions ... alternative)
       (check/unique conditions
                     bindings/checked
                     #f
                     ()
                     ((variable path) . bindings/final)
                     actions ... alternative))
      ))

(define-syntax same-variable
  (syntax-rules ()
    ((_ x y same differ)
     (let-syntax ((x= (syntax-rules (x)
                        ((_ x identical _) identical)
                        ((_ z _ different) different))))
       (x= y same differ)))))


(define-syntax match-clause
  (syntax-rules (quasiquote unquote and _)

    ((match-clause () condition bindings actions ... alternative)
     (check/unique condition bindings #f () () actions ... alternative))

    ((match-clause ((`,pattern root) . rest)
                   condition
                   bindings
                   actions ... alternative)
     (match-clause ((pattern root) . rest)
                   condition
                   bindings
                   actions ... alternative))

    ((match-clause ((_ root) . rest)
                   condition
                   bindings
                   actions ... alternative)
     (match-clause rest
                   condition
                   bindings
                   actions ... alternative))
        
    ((match-clause ((`(left . right) root) . rest)
                   (and conditions ...)
                   bindings
                   actions ... alternative)
     (match-clause ((`left (car root)) (`right (cdr root)) . rest)
                   (and conditions ... (pair? root))
                   bindings
                   actions ... alternative))

    ((match-clause ((atom root) . rest)
                   (and conditions ...)
                   bindings
                   actions ... alternative)
     (identifier/literal atom
                         (match-clause rest
                                       (and conditions ...)
                                       ((atom root) . bindings)
                                       actions ... alternative)
                         (match-clause rest
                                       (and conditions ... (equal? atom root))
                                       bindings
                                       actions ... alternative)))
    ))

(define-syntax identifier/literal
  (syntax-rules ()
    ((identifier/literal atom identifier literal)
     (let-syntax ((check-identifier (syntax-rules ()
                                      ((_ atom symbol _) symbol)
                                      ((_ datum _ value) value))))
       (check-identifier raw-symbol identifier literal)))))

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

(define-syntax match
  (syntax-rules ()
    ((match expression (pattern actions* ... value) ...)
     (let ((evaluated expression))
       (match/evaluated evaluated (pattern actions* ... value) ...)))))
