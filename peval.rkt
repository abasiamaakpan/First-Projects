#lang racket
(require 2htdp/batch-io)
(require plot)
(require racket/gui)
(require plot/utils)
(read-csv-file "new.csv")
(define projectish(read-csv-file "new.csv"))





;;;;;;;;;;;;;;;;;;;           ;;;;;;;;;;;;;;;;;         ; ;
;                             ;               ;         ; ;             ;       ;;;;;;;;;;;;;;;         ;;;;;;;;;;;;;;;;;;;;;;;;                ;                  ;  ;             ;         ;;;;;;;;;;;;;;;;;;;;;;;;              ;;;;;;;;;;;;;;;;
;                             ;               ;         ;  ;            ;       ;                                   ;                          ; ;                 ;   ;            ;                     ;                         ;
;                             ;               ;         ;   ;           ;       ;                                   ;                         ;   ;                ;    ;           ;                     ;                         ;
;                             ;               ;         ;    ;          ;       ;                                   ;                        ;     ;               ;     ;          ;                     ;                         ;
;                             ;               ;         ;     ;         ;       ;                                   ;                       ;       ;              ;      ;         ;                     ;                         ; 
;                             ;               ;         ;      ;        ;       ;                                   ;                      ;         ;             ;       ;        ;                     ;                         ;
;                             ;               ;         ;       ;       ;       ;                                   ;                     ;           ;            ;        ;       ;                     ;                         ;
;                             ;               ;         ;        ;      ;       ;;;;;;;;;;;;;;;;                    ;                    ;;;;;;;;;;;;;;;           ;         ;      ;                     ;                         ;;;;;;;;;;;;;;;;;                                                     
;                             ;               ;         ;          ;    ;                      ;                    ;                  ;                 ;         ;           ;    ;                     ;                                         ;
;                             ;               ;         ;           ;   ;                      ;                    ;                 ;                   ;        ;            ;   ;                     ;                                         ;
;                             ;               ;         ;            ;  ;                      ;                    ;                ;                     ;       ;             ;  ;                     ;                                         ;
;                             ;               ;         ;             ; ;                      ;                    ;               ;                       ;      ;              ; ;                     ;                                         ;
;;;;;;;;;;;;;;;;;;;;;;        ;;;;;;;;;;;;;;;;;         ;              ;         ;;;;;;;;;;;;;;                     ;              ;                         ;     ;               ;                      ;                         ;;;;;;;;;;;;;;;;;                





(define qi 7862)      
(define qihyp 8194)
(define Di (/ 0.06 100))
(define b 0.405)
(define stax (/ 7 100))
(define price 3.35)
(define NRI/WI 0.875)
(define opex 1200)
(define netprice  (* price NRI/WI (- 1 stax)))
(define qecl (/ (/ opex netprice) 30.4))
(define D&C -1000000)
(define discountrate ( /(/ 12 100)12))


;                                ;
; ;                             ;  ;                    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;                  ; ;                                               ;;                          ;;                   ;                       ;;                         ;                       ;                    ;       ;;;;;;;;;;;;;;       ;           ;;;;;;;;;;;;;;
;   ;                          ;    ;                                 ;                              ;   ;                                             ;  ;                         ; ;                  ;                      ;  ;                        ;                        ;                  ;        ;                    ;           ;
;     ;                       ;      ;                                ;                             ;     ;                                           ;    ;                        ;  ;                 ;                     ;    ;                       ;                         ;                ;         ;                    ;           ;
;       ;                    ;        ;                               ;                            ;       ;                                         ;      ;                       ;   ;                ;                    ;      ;                      ;                          ;              ;          ;                    ;           ;
;        ;                  ;          ;                              ;                           ;         ;                                       ;        ;                      ;    ;               ;                   ;        ;                     ;                           ;            ;           ;                    ;           ;
;         ;                ;            ;                             ;                          ;           ;                                     ;          ;                     ;     ;              ;                  ;          ;                    ;                            ;          ;            ;                    ;           ;
;          ;              ;              ;                            ;                         ;             ;                                   ;            ;                    ;      ;             ;                 ;            ;                   ;                             ;        ;             ;                    ;           ;
;           ;            ;                ;                           ;                        ;               ;                                 ;              ;                   ;       ;            ;                ;              ;                  ;                              ;      ;              ;                    ;           ;
;            ;          ;                  ;                          ;                       ;                 ;                               ;                ;                  ;        ;           ;               ;                ;                 ;                               ;    ;               ;                    ;           ;
;             ;        ;                    ;                         ;                      ;                   ;                             ;                  ;                 ;         ;          ;              ;                  ;                ;                                ; ;                 ;;;;;;;;;;;;;;;      ;           ;;;;;;;;;;;;;;
;            ;        ;;;;;;;;;;;;;;;;;;;;;;;;                        ;                     ;;;;;;;;;;;;;;;;;;;;;;;                           ;;;;;;;;;;;;;;;;;;;;;;                ;          ;         ;             ;;;;;;;;;;;;;;;;;;;;;;               ;                                 ;                                ;      ;                        ;
;           ;        ;                        ;                       ;                    ;                       ;                         ;                      ;               ;           ;        ;            ;                      ;              ;                                 ;                                ;      ;                        ;
;          ;        ;                          ;                      ;                   ;                         ;                       ;                        ;              ;            ;       ;           ;                        ;             ;                                 ;                                ;      ;                        ;
;         ;        ;                            ;                     ;                  ;                           ;                     ;                          ;             ;             ;      ;          ;                          ;            ;                                 ;                                ;      ;                        ;
;        ;        ;                              ;                    ;                 ;                             ;                   ;                            ;            ;              ;     ;         ;                            ;           ;                                 ;                                ;      ;                        ;
;       ;        ;                                ;                   ;                ;                               ;                 ;                              ;           ;               ;    ;        ;                              ;          ;                                 ;                                ;      ;                        ;
;      ;        ;                                  ;                  ;               ;                                 ;               ;                                ;          ;                ;   ;       ;                                ;         ;                                 ;                                ;      ;                        ;
;     ;        ;                                    ;                 ;              ;                                   ;             ;                                  ;         ;                 ;  ;      ;                                  ;        ;                                 ;                                ;      ;                        ;
;    ;        ;                                      ;                ;             ;                                     ;           ;                                    ;        ;                  ; ;     ;                                    ;       ;                                 ;                                ;      ;                        ;
;   ;        ;                                        ;               ;            ;                                       ;         ;                                      ;       ;                   ;;    ;                                      ;      ;                                 ;                                ;      ;                        ;
; ;         ;                                          ;              ;           ;                                         ;       ;                                        ;      ;                    ;   ;                                        ;     ;;;;;;;;;;;;;;;;;;;;;;            ;                   ;;;;;;;;;;;;;;      ;           ;;;;;;;;;;;;;;                   ;


;;gas rates ............
(define (get-gas-list projectish-file )
             (if (null? projectish-file)
              empty
             (cons  (list(string->number(list-ref (first projectish-file) 7)))  (get-gas-list (rest projectish-file)))))



(define store-gas-list (flatten(get-gas-list projectish)))


;;price-list 
(define (get-price-list projectish-file)
        (if (null? projectish-file)
            empty
            (cons (list (string->number (string-trim (list-ref (first projectish-file) 8)"$"))) (get-price-list (rest projectish-file)))))

(define store-price-list (flatten(get-price-list projectish)))



;;time-list
(define (get-time-list projectish-file )
             (if (null? projectish-file)
              empty
             (cons  (list(string->number(list-ref (first projectish-file) 6)))  (get-time-list (rest projectish-file)))))

(define store-time-list (flatten(get-time-list projectish)))
;;store-time-list

;;Gp-list
(define (get-Gp-list projectish-file )
             (if (null? projectish-file)
              empty
             (cons  (list(string->number(list-ref (first projectish-file) 5)))  (get-Gp-list (rest projectish-file)))))

(define store-Gp-list (flatten(get-Gp-list projectish)))
;;store-Gp-list


;;reciprocal rate
(define (get-reciprocal-gas-list projectish-file )
             (if (null? projectish-file)
              empty
             (cons  (list(/ 1 (string->number(list-ref (first projectish-file) 7))))  (get-reciprocal-gas-list (rest projectish-file)))))

(define store-reciprocal-gas-list (flatten (get-reciprocal-gas-list projectish)))



;;exponetial rate
(define (get-exponential-list projectish-file)
              ( if (null? projectish-file)
                 empty
              (cons ( list (* qi (exp(* (* (- 1) Di) (string->number(list-ref (first projectish-file) 6))))))
                    (get-exponential-list (rest projectish-file)))))

(define store-exponential-list (flatten(get-exponential-list projectish)))


;;arps rate or hyperbolic rate calculation
(define (get-hyperbolic-list projectish-file)
     ( if (null? projectish-file)
                 empty
           (cons  (list ( / qi  (expt (+ 1  (* b Di (string->number(list-ref (first projectish-file) 6)))   )  (/ 1 b) )))
     (get-hyperbolic-list (rest projectish-file)))))

(define store-hyperbolic-list (flatten (get-hyperbolic-list projectish)))
;;store-hyperbolic-list

;;difference between calculated hyperbolic rate and given rate 
(define (get-difference-error-rate-time-list gas-list hyp-list)
  (if (null? gas-list)
      empty
      (append (list (abs (/(- (first gas-list) (first hyp-list)) (first gas-list))))
              (get-difference-error-rate-time-list (rest gas-list) (rest hyp-list)))))

(define store-difference-error-rate-time-list(get-difference-error-rate-time-list  store-gas-list store-hyperbolic-list))


;;calculate net revenue per month in a list
(define (get-net-revenue-list gas-list price-list )
      (if (null? gas-list)
          empty
   (cons (- (* (first gas-list) (first price-list) NRI/WI (- 1 stax))opex)
         (get-net-revenue-list (rest gas-list) (rest price-list)))))

(define store-net-revenue-list (get-net-revenue-list store-gas-list store-price-list))
(define new-store-net-revenue-list (cons D&C (rest store-net-revenue-list)))


;;calculate discount factor in a list
(define (get-discount-factor-list t-list)
      (if (null? t-list)
          empty
   (cons  (expt(+ 1  discountrate) (- (first t-list)))
         (get-discount-factor-list (rest t-list)))))

(define store-discount-factor-list (get-discount-factor-list store-time-list))

;;calculate discounted net revenue in a list
(define (get-discount-net-revenue-list net-r-list disc-f-list )
      (if (null? net-r-list)
          empty
   (cons  (* (first net-r-list) (first disc-f-list))
         (get-discount-net-revenue-list (rest net-r-list) (rest disc-f-list)))))

(define store-discount-net-revenue-list
  (get-discount-net-revenue-list store-net-revenue-list store-discount-factor-list))

(define (new-store-discount-net-revenue-list)
  (cons (* (first store-discount-factor-list) D&C) (rest store-discount-net-revenue-list)))

;;hyperbolic rate cumulative calculation
(define (get-hyperbolic-cum-list  Gp-list)

   (if (null? Gp-list)
              empty
          
 (cons (expt (+(/(*  (first  Gp-list) (- b 1) Di) (expt qihyp b))
        (expt qihyp (- 1 b)))
        (/ 1 (- 1 b)))
                    (get-hyperbolic-cum-list (rest Gp-list)))))

(define store-hyperbolic-cum-list (get-hyperbolic-cum-list store-Gp-list))

;;difference between calculated hyperbolic rate cumulative and given rate 
(define (get-difference-error-rate-cum-list gas-list hyp-cum-list)
  (if (null? gas-list)
      empty
      (append (list (abs (/(- (first gas-list) (first hyp-cum-list)) (first gas-list))))
              (get-difference-error-rate-cum-list (rest gas-list) (rest hyp-cum-list)))))

(define store-difference-error-rate-cum-list(get-difference-error-rate-cum-list  store-gas-list store-hyperbolic-cum-list))

;;for reciprocal rate linear flow graph-splits 2 sets of data into separate fields 
(define (make_pair l1 l2)
  (if (null? l1)
      empty
      (append (list(list (first l1) (first l2)))
      (make_pair (rest l1) (rest l2)))))

(define data (make_pair store-Gp-list store-reciprocal-gas-list ))


;;should comment out 
(define harmonic-decline-curve-analysis (make_pair store-Gp-list store-gas-list))


;;calculate future production lifetime of the well based on current data
(define  half-life-time '())
(define half-life-rate '())

;; future production lifetime (rate-time forecast)
(define (get-half-life-list time expon hyp)
  (if (<= hyp (/ (/ opex  netprice) 30.4))
      empty   
(cons   time  (get-half-life-list (* time (expt 1.2 (+ expon 1)))  expon   ( / qi  (expt (+ 1  (* b Di  time  )) (/ 1 b))) )
       )))
;;(/ qi (expt (+ 1  (* b Di  (apply max store-time-list)  )) (/ 1 b))) ;;calculation for hyperbolic rate at maximum time. It replaces 1085
(define store-half-life-list (get-half-life-list (apply max store-time-list) 0 (/ qi (expt (+ 1  (* b Di  (apply max store-time-list)  )) (/ 1 b))) ))





;;future production lifetime (rate-cum forecast)
(define (get-new-hyp-list time expon hyp)
  (if (<= hyp (/ (/ opex  netprice) 30.4)) 
      empty    
(cons   hyp  (get-new-hyp-list (* time (expt 1.2 (+ expon 1)))  expon   ( / qi  (expt (+ 1  (* b Di  time  )) (/ 1 b))) )
       )))

;;(/ qi (expt (+ 1  (* b Di  (apply max store-time-list)  )) (/ 1 b))) ;;calculation for hyperbolic rate at maximum time. It replaces 1085

(define store-new-hyp-list (get-new-hyp-list  (apply max store-time-list)  0 (/ qi (expt (+ 1  (* b Di  (apply max store-time-list)  )) (/ 1 b)))));;fix
store-half-life-list
store-new-hyp-list
;;future production lifetime (rate-time forecast)
(define (get-new-nominal-list store-half-life-list )
  (if (null? store-half-life-list)
      empty    
  (cons ( / Di  (+ 1  (* b Di  (first store-half-life-list)   )))
        (get-new-nominal-list  (   rest store-half-life-list   ))))
       )


(define store-new-nominal-list (get-new-nominal-list store-half-life-list))

;;reserve-value-rate-time  
(define reserve-value-rate-time   (*(/( expt (first store-new-hyp-list)  b)
     (* (first store-new-nominal-list)  (- b 1))) (- (expt qecl (- 1 b)) (expt (first store-new-hyp-list) (- 1 b)))))

(define EUR-value-rate-cum (*(/( expt (first store-hyperbolic-cum-list)  b)
     (* Di  (- b 1))) (- (expt qecl (- 1 b)) (expt (first store-hyperbolic-cum-list) (- 1 b)))))

;;EUR-value-rate-cum

(define max-sorted-Gp-list (list-ref(sort store-Gp-list >)0))
;; max-sorted-Gp-list

(define  reserve-value-rate-cum    (- EUR-value-rate-cum max-sorted-Gp-list))
;;reserve-value-rate-cum




;;;;rate-time-reserve-calculation
(define (get-reserve-value-change-with-time-rate-time n-hyp-list n-nominal-list)
(if (null? n-hyp-list)
      empty
 (cons (*(/( expt (first n-hyp-list)  b)
     (* (first n-nominal-list)  (- b 1)))
         (- (expt qecl (- 1 b)) (expt (first n-hyp-list) (- 1 b))))
       (get-reserve-value-change-with-time-rate-time (rest n-hyp-list) (rest n-nominal-list)
                                                      ))))

;;reserve distribution
(define store-reserve-value-change-with-time-rate-time
  (get-reserve-value-change-with-time-rate-time
   store-new-hyp-list store-new-nominal-list))




;;date extraction
(define (get-calendar-list projectish-file )
             (if (null? projectish-file)
              empty
             (cons  (list (list-ref (first projectish-file) 1))  (get-calendar-list (rest projectish-file)))))

(define store-calendar-list (flatten(get-calendar-list projectish)))

;;net present value of project 
(define (sum-net-revenue-list projectish)
  (if (empty? projectish)
      0
      (+ (first projectish) (sum-net-revenue-list(rest projectish)))))

(define store-net-present-value-list (sum-net-revenue-list new-store-net-revenue-list))


store-new-hyp-list
store-half-life-list
(define future_rate (take store-new-hyp-list 4))

(define future_time (take store-half-life-list 4))

(define future_price '(6.21 4.76 3.48 1.62)) ;;; data extracted from web

(define (future_net_revenue future_rate  future_price )
  (if (empty? future_rate)
      empty
      (cons (-(* (car future_rate)  (car future_price) NRI/WI (- 1 stax)opex))
            (future_net_revenue (cdr future_rate)  (cdr future_price)))))

(define store-future_net_revenue (future_net_revenue future_rate  future_price))

;store-future_net_revenue



;;;;;;;;;;;;;;;;;;;;;;                  ;                                           ;;;;;;;;;                    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;               ;;;;;;;;;;;;;;;;;;;;;;;
;                     ;                 ;                                          ;         ;                                   ;                              ;
;                      ;                ;                                         ;           ;                                  ;                              ;
;                      ;                ;                                        ;             ;                                 ;                              ;
;                      ;                ;                                       ;               ;                                ;                              ;
;                      ;                ;                                      ;                 ;                               ;                              ;
;                      ;                ;                                     ;                   ;                              ;                              ;
;                      ;                ;                                     ;                   ;                              ;                              ;
;                     ;                 ;                                     ;                   ;                              ;                              ;
;;;;;;;;;;;;;;;;;;;;;;                  ;                                     ;                   ;                              ;                              ;
;                                       ;                                     ;                   ;                              ;                              ;;;;;;;;;;;;;;;;;;;;;;;;
;                                       ;                                     ;                   ;                              ;                                                     ;
;                                       ;                                     ;                   ;                              ;                                                     ;
;                                       ;                                     ;                   ;                              ;                                                     ;
;                                       ;                                     ;                   ;                              ;                                                     ;
;                                       ;                                     ;                   ;                              ;                                                     ;
;                                       ;                                     ;                   ;                              ;                                                     ;
;                                       ;                                      ;                 ;                               ;                                                     ;
;                                       ;                                       ;               ;                                ;                                                     ;
;                                       ;                                        ;             ;                                 ;                                                     ;
;                                       ;                                         ;           ;                                  ;                                                     ;
;                                       ;                                          ;         ;                                   ;                                                     ;
;                                       ;                                           ;;;;;;;;;                                    ;                               ;;;;;;;;;;;;;;;;;;;;;;;
;                                       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define decline-curve-analysis (make_pair store-time-list store-gas-list))



 (define func2(plot-frame (points decline-curve-analysis )
                          #:x-label  "time,days"  
                          #:y-label "Rate,MCFd"  
                          #:title  "Decline Curve Analysis"))
                          ;;#:label "Rate-Time Forecast"))
                          ;;#:colors (color-seq* '(red blue white)5)))
                          
                          
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define func(plot-frame (points data)
                       
                        ;;(points (vector 1e7 0.0003))

                          #:x-label  "Cumulative production MCF" 
                          #:y-label "Reciprocal rate 1/MCFd"  
                          #:title  "Reciprocal Rate Linear Flow"
                         ;; #:label "Linear flow interpretation"
                          ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define func4 (plot-frame  (discrete-histogram (make_pair (map round store-half-life-list) store-reserve-value-change-with-time-rate-time)
                              ;;  #:x-axis  "Reserves, MCF" 
                              ;;  #:y-axis "Time, days"
                               #:label "Reserve Distribution"
                                  )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define func5(plot-frame  (discrete-histogram (make_pair   store-time-list   new-store-net-revenue-list)
                              ; #:x-label  "Time, days"
                              ;  #:y-label "Net-Revenue-Per-Month, $"
                               #:label "Net-Revenue-Per-Month"
                                  )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (define func6(plot-frame  (discrete-histogram (make_pair   store-half-life-list   store-new-hyp-list)
                              ; #:x-label  "Time, days"
                              ;  #:y-label "Future Gas production, $"
                               #:label "Future gas production "
                                  )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define func7(plot-frame  (discrete-histogram (make_pair  future_time   store-future_net_revenue)
                              ; #:x-label  "Time, days"
                              ;  #:y-label "Future net revenue production, $"
                               #:label "Future net revenue "
                                  )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Enter start date of Improved Recovery-This is the period where we anticipate the company began implementing the process
(define input_start_date (get-text-from-user "Improved Recovery Economics" "Enter start date for highest gas price search-High gas prices encourage improved recovery implementation. This is the period where the improved recovery process is implemented "))
(message-box "Start Date" (format "You entered: ~a" input_start_date ))
(define input_end_date (get-text-from-user "Improved Recovery Economics" "Enter end date for highest gas price search-This is the period where the implementation ends "))
(message-box "End Date" (format "You entered: ~a" input_end_date ))
(define (list_slice l start end)
  (take (drop l start) (- (+ end 1) start)))
;(define input_start_year (read-line (current-input-port) 'any))
;(define input_final_year (read-line(current-input-port) 'any))

(define highestp(number->string(list-ref (sort (list_slice store-price-list
            (index-of  store-calendar-list input_start_date)  
            (index-of  store-calendar-list input_end_date)
 ) >
   )
     0)))


(define estimatedt(list-ref store-calendar-list (index-of store-price-list          
(list-ref (sort (list_slice store-price-list
            (index-of  store-calendar-list input_start_date)
            (index-of  store-calendar-list input_end_date)
 ) >
   )
     0))))
(define (test-project-economics projectish)  
  (if (> projectish 0)
      "Yes the NPV > 0" 
      "No, the NPV < 0"))


(define display-npv-criteria (test-project-economics store-net-present-value-list))
(define profitability  (number->string store-net-present-value-list))



(define frame (new frame%
                   [label "Economic Evaluation of Oil and Gas Project"]
                   [x 0]
                   [y 0]
                   [width 500]
                   [height 500]))
(define msg (new message% [parent frame]
                          [label "This software investigates the viability and the commodity price implications of an oil and gas project"]))

(new button% [parent frame]
             [label "Production rate vs time"]
             [callback (lambda (button event)
                         (send func2 show #t))])
(define msg1 (new message% [parent frame]
                          [label "Displays current gas production vs time in days."]))
(new button% [parent frame]
             [label "Reciprocal Rate Plot"]
             [callback (lambda (button event)
                         (send func show #t))])
(define msg2 (new message% [parent frame]
                          [label "Flow pattern examination"]))

(new button% [parent frame]
             [label "Reserve Distribution"]
             [callback (lambda (button event)
                         (send func4 show #t))])
(define msg3 (new message% [parent frame]
                          [label "Histogram displays forecasted gas reserves vs time in days."]))
(new button% [parent frame]
            [label "Net Revenue Per Month "]
             [callback (lambda (button event)
                         (send func5 show #t))])
(define msg4 (new message% [parent frame]
                          [label "Amount of money recovered by the producer per month."]))
                           ;;[font menu-control-font]))
(new button% [parent frame]
            [label "Highest gas price within time frame"]
             [callback (lambda (button event)
                         
                         (send msg set-label highestp))])

(define msg5 (new message% [parent frame]
                          [label "Drive investment decisions"]))

(new button% [parent frame]
            [label "Estimated time to perform Enhanced Gas Recovery Technique: "]
             [callback (lambda (button event)
                         (send msg set-label estimatedt))])
(define msg6 (new message% [parent frame]
                          [label "Production enhancement procedure"]))
(new button% [parent frame]
            [label "Was this project profitable or not in the short term? "]
             [callback (lambda (button event)
                         (send msg set-label display-npv-criteria))])
(define msg7 (new message% [parent frame]
                          [label "Profit or Loss "]))
(new button% [parent frame]
            [label "Net Present Value as of 2007"]
             [callback (lambda (button event)
                         (send msg set-label profitability))])
(define msg8 (new message% [parent frame]
                          [label "Negative value-Loss. Positive value-Profit "]))
(new button% [parent frame]
            [label "Future Gas production "]
             [callback (lambda (button event)
                         (send func6 show #t))])
(define msg9 (new message% [parent frame]
                          [label "Forecasted Gas production "]))
(new button% [parent frame]
            [label "Future Net Revenue "]
             [callback (lambda (button event)
                         (send func7 show #t))])
(define msg10 (new message% [parent frame]
                          [label "Forecasted net revenue estimates "]))
(new button% [parent frame]
             [label "Quit"]
             [callback (lambda (button event)
                         (exit))])
(send frame show #t)

