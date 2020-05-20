#lang scheme

; Solver function
(define (TENTS-SOLUTION listt)
  (if (and (eq? 0 (length (caddr listt))) (ALL-ZEROS (car listt)) (ALL-ZEROS (cadr listt)))
      '();no tree in the map and edges are all 0.
      (FUNC (caddr listt) (caddr listt) '() '() (car listt) (cadr listt) (NEIGHBOR-LIST (caaddr listt)) '() )))

(define (FUNC allTrees unprocessedTrees processedTrees tents verticals horizantals currentNeighbours beforeNeighboursList)
  (if (eq? (length allTrees) (length tents))
      (if (and (ALL-ZEROS verticals) (ALL-ZEROS horizantals))
          tents
          #f)
      (if (null? currentNeighbours)
          ;could set up any of the currentNeighbours:
          (if (null? beforeNeighboursList)
              ;have a tree back there try that with it's neighbour
              #f ;but dont have any tree back there. sorry, no solution
              (FUNC allTrees (cons (car processedTrees) unprocessedTrees)
                    (cdr processedTrees) (cdr tents)
                    (PLUS1 verticals (caar tents)) (PLUS1 horizantals (cadar tents))
                    (car beforeNeighboursList)
                    (cdr beforeNeighboursList)
                   ))
                      
          (if (RULES allTrees
                     (cons (car currentNeighbours) tents)
                     (MINUS1 verticals (caar currentNeighbours))
                     (MINUS1 horizantals (cadar currentNeighbours))) 
              ;no rules are violated, move to next tree
              (FUNC allTrees
                    (cdr unprocessedTrees)
                    (cons (car unprocessedTrees) processedTrees)
                    (cons (car currentNeighbours) tents)
                    (MINUS1 verticals (caar currentNeighbours))
                    (MINUS1 horizantals (cadar currentNeighbours))
                    (if (eq? 1 (length unprocessedTrees)) '() (NEIGHBOR-LIST (cadr unprocessedTrees)))
                    (cons (cdr currentNeighbours) beforeNeighboursList))
              ;rules are violated, move to next neighbour
              (FUNC allTrees unprocessedTrees processedTrees tents verticals horizantals
                    (cdr currentNeighbours) (if (null? beforeNeighboursList) '() beforeNeighboursList))))))


  ;if rules are violated return false
  (define (RULES trees tents verticals horizantals)
    (if (and (RULE1 tents) (RULE2 verticals horizantals)
             (RULE3 tents trees) (RULE4 (car tents) trees tents)) #t #f )) ;

  ; Helper functions

  ;if RULE1 is true then it's okey, no worries. No two tents are adjacent.
  (define (RULE1 tents)
    (if (eq? 1 (length tents))
        #t
        (if (ADJACENT-WITH-LIST (car tents) (cdr tents))
            #f
            (RULE1 (cdr tents)))))

  ;if verticals or horizantals have -1 then we placed tent to the wrong place.
  (define (RULE2 verticals horizantals)
    (if (or (equal? #f verticals) (equal? #f horizantals))
        #f
        (if (or (CONTAINS-NEGATIVE verticals) (CONTAINS-NEGATIVE horizantals))
            #f #t)))

  ; true if list contains any negative integer
  (define (CONTAINS-NEGATIVE listt)
    (if (eq? 0 (length listt))
        #f
        (if (< (car listt) 0)
            #t
            (CONTAINS-NEGATIVE (cdr listt))
            )))


  ;one tree for one tent or vice versa, idk
  (define (RULE3 tents trees)
    (if (> (length tents) (length trees))
        #f
        (if (eq? 0 (length tents))
            #t
            (if (not(IS-NEIGHBOUR (car tents) trees))
                #f
                (RULE3 (cdr tents) trees)
                ))))

;is this location empty?
(define (RULE4 pos trees tents)
  (if (or (member pos trees)(if (null? tents) #f (member pos (cdr tents)))) #f #t)) 


  (define (REPLACE-NTH listt index new)
    (if (eq? 0 (length listt))
        listt
        (cons
         (if (eq? 1 index)
             new
             (car listt))
         (REPLACE-NTH (cdr listt) (- index 1) new))))


  ;is an atom member of a list
  ( define ( member atm lis )
     (cond ( ( null? lis ) #f )
           ( (equal? atm (car lis ) ) #t )
           ( else ( member atm (cdr lis ) ) ) ) )

  (define (RETURN-FIRST-NOT-FALSE  func listt)
    (if (and (eq? #f (func (car listt))) (> (length listt) 1) )
        (RETURN-FIRST-NOT-FALSE  func (cdr listt))
        (func (car listt))))

  ;one tent with many trees
  (define (IS-NEIGHBOUR tree tents)
    (if (eq? 0 (length tents))
        #f
        (if (IS-NEIGHBOURATOM tree (car tents))
            #t
            (IS-NEIGHBOUR tree (cdr tents)))))

  ;one tent with one tree
  (define (IS-NEIGHBOURATOM tree tent)
    (if (member tree (NEIGHBOR-LIST tent))
        #t #f))
  
  (define (ADJACENT pos1 pos2)
    (if (< (+ (*  (-(cadr pos1) (cadr pos2)) (-(cadr pos1) (cadr pos2))) (* (-(car pos1) (car pos2)) (-(car pos1) (car pos2)))) 3)
        #t #f))

  (define (NEIGHBOR-LIST pos)
    (list
     (list (- (car pos) 1) (cadr pos))
     (list (car pos) (+ (cadr pos) 1))
     (list (+ (car pos) 1) (cadr pos))
     (list (car pos) (- (cadr pos) 1))))

  (define (ADJACENT-WITH-LIST pos listt)
    (if (eq? 0 (length listt))
        #f
        (if (ADJACENT pos (car listt))
            #t
            (if(> (length (cdr listt)) 0)
               (ADJACENT-WITH-LIST pos (cdr listt))
               #f))))

  ;minus1
  (define (MINUS1 listt index)
    (if (equal? #f (NTH listt index))
        #f
        (REPLACE-NTH listt index (- (NTH listt index) 1))))
  ;plus1
  (define (PLUS1 listt index)
    (if (equal? #f (NTH listt index))
        #f
        (REPLACE-NTH listt index (+ (NTH listt index) 1))))
    
  
  ;return nth of a list
  (define (NTH listt place)
    (if (or (> place (length listt)) (eq? 0 place))
        #f
        (if (= place 1)
            (car listt)
            (NTH (cdr listt) (- place 1)))))

  ;in the end verticals and horizantals must be all 0
  (define (ALL-ZEROS listt)
    (if (eq? 0 (length listt))
        #t
        (if (not(eq? 0 (car listt)))
            #f
            (ALL-ZEROS (cdr listt)))))
