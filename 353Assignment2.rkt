#lang racket
; Define a structure for representing game data
(define-struct game (index rank title platform year genre publisher na eu jp rest global review))
; Function to load game data from a CSV file
(define (load-games-from-csv file-path)
  (call-with-input-file file-path
    (lambda (input-port)
      (let loop ((lines (port->lines input-port))
                 (data '())
                 (first-line #t)) ; Flag to skip the first line
        (if (null? lines)
            (reverse data)
            (let* ((line (car lines))
                   (fields (string-split line ",")))
              (if first-line
                  (loop (cdr lines) data #f) ; Skip the first line
                  (loop (cdr lines)
                        (cons (list->game fields) data) ; Convert fields to game struct
                        #f))))))))
; Function to convert a list of fields to a game struct
(define (list->game fields)
  (make-game (list-ref fields 0)  ; Index
             (list-ref fields 1)  ; Rank
             (list-ref fields 2)  ; Title
             (list-ref fields 3)  ; Platform
             (string->number (list-ref fields 4))  ; Year
             (list-ref fields 5)  ; Genre
             (list-ref fields 6)  ; Publisher
             (string->number (list-ref fields 7))  ; NA Sales
             (string->number (list-ref fields 8))  ; EU Sales
             (string->number (list-ref fields 9))  ; JP Sales
             (string->number (list-ref fields 10)) ; Rest Sales
             (string->number (list-ref fields 11)) ; Global Sales
             (string->number (list-ref fields 12)))) ; Review Score


; Function to filter games by name
(define (filter-by-name games name)
  (filter (lambda (game)
            (string-ci=? name (game-title game)))
          games))
; Function to filter games by year range
(define (filter-by-year games start-year end-year)
  (let ((start (min start-year end-year))
        (end (max start-year end-year)))
    (filter (lambda (game)
              (and (>= (game-year game) start)
                   (<= (game-year game) end)))
            games)))
; Function to filter games by publisher
(define (filter-by-publisher games publisher-part)
  (filter (lambda (game)
            (string-contains? (string-downcase (game-publisher game))
                              (string-downcase publisher-part)))
          games))
; Function to filter games by sales in a particular region
(define (filter-by-sales games region sales)
  (filter (lambda (game)
            (> (case (string->symbol region) ; Convert input to symbol
                 [(na) (game-na game)]
                 [(eu) (game-eu game)]
                 [(jp) (game-jp game)]
                 [(rest) (game-rest game)]
                 [(global) (game-global game)]
                 [else (error "Unknown region")])
               sales))
          games))

; Function to filter games by genre
(define (filter-by-genre games genre)
  (filter (lambda (game)
            (string-ci=? genre (game-genre game)))
          games))
; Main function
(define (main)
  ; Prompt user for input
  (display "Enter the name of the game (or press Enter to skip): ")
  (define name (read-line))
  
  (display "Enter the start year of the range: ")
  (define start-year (string->number (read-line)))
  
  (display "Enter the end year of the range: ")
  (define end-year (string->number (read-line)))
  
  (display "Enter the publisher (or press Enter to skip): ")
  (define publisher (read-line))
  
  (display "Enter the region (na, eu, jp, rest, global) to filter by sales (or press Enter to skip): ")
  (define region (read-line))
  
  (display "Enter the minimum sales threshold: ")
  (define sales-threshold (string->number (read-line)))
  
  (display "Enter the genre (or press Enter to skip): ")
  (define genre (read-line))
  ; Load game data from CSV file
  (define games (load-games-from-csv "Video Games Sales.csv"))
  ; Apply filters based on user input
  (set! games (if (string=? name "") games (filter-by-name games name)))
  (set! games (if (or (= start-year end-year) (> start-year end-year)) games (filter-by-year games start-year end-year)))
  (set! games (if (string=? publisher "") games (filter-by-publisher games publisher)))
  (set! games (if (string=? region "") games (filter-by-sales games region sales-threshold)))
  (set! games (if (string=? genre "") games (filter-by-genre games genre)))
  ; Display filtered games
  (for-each (lambda (game)
            (display "Index: ")
            (display (game-index game))
            (newline)
            (display "Rank: ")
            (display (game-rank game))
            (newline)
            ; Print other fields similarly
            (newline))
          games))
; Call the main function to start the program
(main)