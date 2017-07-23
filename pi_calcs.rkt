#lang racket
(define sum
  (λ (f n m)
    (define iter
      (λ (s f n m)
        (cond [(= n m) (+ s (f n))]
              [else (iter (+ s (f n)) f (add1 n) m)])))
    (iter 0 f n m)))

(define dfact
  (λ (n)
    (define iter
      (λ (p n)
        (cond [(<= n 1) p]
              [else (iter (* p n) (- n 2))])))
    (iter 1 n)))

(define dfact-pi
  (λ (k)
    (/ (dfact (* 2 k)) (* (dfact (add1 (* 2 k))) (expt 2 k)))))

(define ldiv
  (λ (a b)
    (let-values ([(q r) (quotient/remainder a b)])
      (list q r))))

(define exact-div
  (λ (num denom digits)
    (define iter
      (λ (idx ans num denom digits)
        (let ([qr (ldiv num denom)])
          (cond [(zero? digits) ans]
                [(zero? idx) (iter (add1 idx) (string-append ans (number->string (car qr)) ".") (* 10 (cadr qr)) denom (sub1 digits))]
                [else (iter (add1 idx) (string-append ans (number->string (car qr))) (* 10 (cadr qr)) denom (sub1 digits))]))))
    (iter 0 "" num denom digits)))

(define num-terms
  (λ (digits)
    (exact-round (+ (/ (log (expt 10 digits)) (log 2)) 1))))

(define rat-pi
  (λ (digits)
    (* 2 (sum dfact-pi 0 (num-terms digits)))))

(define dec-pi
  (λ (digits)
    (let ([pi (rat-pi digits)])
      (exact-div (numerator pi) (denominator pi) digits))))

(define pi-day
  (λ ()
    (let* ([dpi (string-replace (dec-pi 200) "." "")]
           [month-day (regexp-match #px"(0\\d|1[0-2])([0-2]\\d|3[01])" dpi)]
           [month (cadr month-day)]
           [day (caddr month-day)])
      (string-append (car (string-split dpi (car month-day))) "-" month "-" day))))
