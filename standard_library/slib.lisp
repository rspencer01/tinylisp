('Loading standard library)

(define head λ (a . b) . a)
(define tail λ (a . b) . b)

(define apply
  λ (f . a) . ( (f . (head . a)) . (apply f (tail . a)))
)
