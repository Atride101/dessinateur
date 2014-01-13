;; Fichier : "dessinateur-def.scm"

;;-----------------------------------------------------------------------------

;; Representation des vecteurs et segments.

(define vect cons)
(define vect-x car)
(define vect-y cdr)

(define segm (lambda (depart arrivee) (list depart arrivee)))
(define segm-depart car)
(define segm-arrivee cadr)

;;-----------------------------------------------------------------------------

;; Parcours normalises pour les chiffres 0 a 9.

(define parcours-pour-chiffres
  (list->vector
    (list

      (list (vect 0 7/8)        ; 0
            (vect 2/3 1/2)
            (vect 2/3 -1/2)
            (vect 0 -7/8)
            (vect -2/3 -1/2)
            (vect -2/3 1/2)
            (vect 0 7/8))

      (list (vect -1/4 3/4)     ; 1
            (vect 0 7/8)
            (vect 0 -7/8))

      (list (vect -2/3 1/2)     ; 2
            (vect 0 7/8)
            (vect 2/3 1/2)
            (vect -2/3 -7/8)
            (vect 2/3 -7/8))

      (list (vect -2/3 1/2)     ; 3
            (vect 0 7/8)
            (vect 2/3 1/2)
            (vect 0 0)
            (vect 2/3 -1/2)
            (vect 0 -7/8)
            (vect -2/3 -1/2))

      (list (vect 1/2 0)        ; 4
            (vect -1/2 0)
            (vect 1/2 7/8)
            (vect 1/2 -7/8))

      (list (vect 2/3 7/8)      ; 5
            (vect -2/3 7/8)
            (vect -2/3 1/2)
            (vect 2/3 0)
            (vect 2/3 -1/2)
            (vect 0 -7/8)
            (vect -2/3 -1/2))

      (list (vect 2/3 7/8)      ; 6
            (vect 0 7/8)
            (vect -2/3 1/2)
            (vect -2/3 -1/2)
            (vect 0 -7/8)
            (vect 2/3 -1/2)
            (vect 0 0)
            (vect -2/3 0))

      (list (vect -2/3 7/8)     ; 7
            (vect 2/3 7/8)
            (vect 0 0)
            (vect 0 -7/8))

      (list (vect 0 7/8)        ; 8
            (vect 2/3 1/2)
            (vect -2/3 -1/2)
            (vect 0 -7/8)
            (vect 2/3 -1/2)
            (vect -2/3 1/2)
            (vect 0 7/8))

      (list (vect 2/3 0)        ; 9
            (vect 0 0)
            (vect -2/3 1/2)
            (vect 0 7/8)
            (vect 2/3 1/2)
            (vect 2/3 -1/2)
            (vect 0 -7/8)
            (vect -2/3 -1/2))
    )))

;;-----------------------------------------------------------------------------

;; Fonctions utilitaires.

(define foldr
  (lambda (f base lst)
    (if (null? lst)
        base
        (f (car lst) (foldr f base (cdr lst))))))

(define foldl
  (lambda (f base lst)
    (if (null? lst)
        base
        (foldl f (f base (car lst)) (cdr lst)))))

;;-----------------------------------------------------------------------------

;; Visualisation d'un dessin.

(define dessiner
  (lambda (dessinateur)
    (afficher (dessinateur (lambda (v) v)))))

(define afficher
  (lambda (lst)

    (for-each
     (lambda (s)
       (if (not (and (pair? s) (pair? (cdr s)) (null? (cddr s))
                     (pair? (segm-depart s))
                     (pair? (segm-arrivee s))
                     (number? (vect-x (segm-depart s)))
                     (number? (vect-y (segm-depart s)))
                     (number? (vect-x (segm-arrivee s)))
                     (number? (vect-y (segm-arrivee s)))))
           (error "le parametre de la fonction \"afficher\" n'est pas valide")))
     lst)

    (set! segments lst)))

(define segments '())

;;-----------------------------------------------------------------------------

;; Interface a Tcl/Tk.

(define tk-program "tclsh")
(define canv-width 600)
(define canv-height 600)
(define grid-color "pink")

(define-type tkproc
  port
  sync-port
  handle
  sizes
  segments
)

(define tk-open
  (lambda (handle)
    (let ((p (open-process
              (list (string->keyword "path")
                    tk-program
                    (string->keyword "stderr-redirection")
                    #t))))
      (let ((tkproc (make-tkproc p (open-vector) handle #f #f)))
        (if (##unbound? (##global-var-ref (##make-global-var 'tk-tkproc)))
            (eval '(define tk-tkproc #f))
            (tk-terminate tk-tkproc))
        (set! tk-tkproc tkproc)
        (tk-start-event-loop tkproc)
        (##add-exit-job!
         (lambda ()
           (tk-terminate tkproc)))
        tkproc))))

(define tk-terminate
  (lambda (tkproc)
    (tk-cmd tkproc "exit")
    (tk-close tkproc)))

(define tk-close
  (lambda (tkproc)
    (let ((port (tkproc-port tkproc)))
      (if port
          (begin

            (close-port port)

            (let ((status (process-status port)))
              (if (not (= status 0))
                  (pp (list 'status status))))

            (tkproc-port-set! tkproc #f))))))

(define tk-start-event-loop
  (lambda (tkproc)
    (thread-start!
     (make-thread
      (lambda ()
        (tk-event-loop tkproc))))))

(define tk-event-loop
  (lambda (tkproc)
    (let loop1 ()
      (let ((port (tkproc-port tkproc)))
        (if (and port
                 (with-exception-catcher
                  (lambda (e) #f)
                  (lambda () (char-ready? port))))
            (let ((line (read-line port)))
              ((tkproc-handle tkproc) tkproc line)
              (loop1))
            (let loop2 ()
              (let ((sync-port (tkproc-sync-port tkproc)))
                (input-port-timeout-set! sync-port 0.05 (lambda () #f))
                (let ((mut (read sync-port)))
                  (if (eof-object? mut)
                      (begin
                        ((tkproc-handle tkproc) tkproc #f)
                        (loop1))
                      (let ((result ((mutex-specific mut))))
                        (mutex-specific-set! mut result)
                        (mutex-unlock! mut)
                        (loop2)))))))))))

(define tk-cmd
  (lambda (tkproc command)
    (let ((port (tkproc-port tkproc)))
      (if port
          (with-exception-catcher
           (lambda (e) #f)
           (lambda ()
             (display command port)
             (newline port)
             (force-output port)))))))

(define tk-cmd-with-result
  (lambda (tkproc command)
    (tk-cmd tkproc command)
    (let ((port (tkproc-port tkproc)))
      (if port
          (let loop ()
            (let ((line (read-line port)))
              (if (and (string? line)
                       (>= (string-length line) 1)
                       (char=? (string-ref line 0) #\!))
                  (loop)
                  line)))
          #f))))

(define refresh
  (lambda (tkproc segments)
    (let ((sizes
           (with-input-from-string
            (tk-cmd-with-result
             tkproc
             "puts \"([winfo width .canv] . [winfo height .canv])\"")
            read)))
      (if (and (pair? sizes)
               (or (not (equal? sizes (tkproc-sizes tkproc)))
                   (not (equal? segments (tkproc-segments tkproc)))))
          (begin
            (tkproc-sizes-set! tkproc sizes)
            (tkproc-segments-set! tkproc segments)
            (let* ((width
                    (car sizes))
                   (height
                    (cdr sizes))
                   (ox
                    (quotient width 2))
                   (oy
                    (quotient height 2))
                   (sx
                    (- ox 50))
                   (sy
                    (- oy 50)))
              (redraw tkproc segments sx ox (- sy) oy)))))))

(define redraw
  (lambda (tkproc segments sx ox sy oy)

    (define transform
      (lambda (x s o)
        (let* ((n (inexact->exact (round (* 1000 (+ (* s (real-part x)) o)))))
               (a (abs n))
               (s (string-append
                   (number->string (quotient a 1000))
                   "."
                   (substring (number->string (+ 1000 (modulo a 1000))) 1 4))))
          (if (< n 0) (string-append "-" s) s))))

    (define create-line
      (lambda (x1 y1 x2 y2 color)
        (let ((cmd
               (string-append
                ".canv create line "
                (transform x1 sx ox)
                " "
                (transform y1 sy oy)
                " "
                (transform x2 sx ox)
                " "
                (transform y2 sy oy))))
          (tk-cmd
           tkproc
           (if color
               (string-append cmd " -fill " color)
               (string-append cmd))))))

    (tk-cmd
     tkproc
     ".canv delete all")

    (tk-cmd
     tkproc
     (string-append
      ".canv create text "
      (transform -1 sx ox)
      " "
      (transform -1 sy (+ oy 9))
      " -text \"(-1,-1)\""))

    (tk-cmd
     tkproc
     (string-append
      ".canv create text "
      (transform 1 sx ox)
      " "
      (transform 1 sy (- oy 9))
      " -text \"(1,1)\""))

    (create-line -1 -1 -1 1 grid-color)
    (create-line -1 -1 1 -1 grid-color)
    (create-line -1 1 1 1 grid-color)
    (create-line 1 -1 1 1 grid-color)
    (create-line -1 0 1 0 grid-color)
    (create-line 0 -1 0 1 grid-color)

    (let loop ((lst segments))
      (if (pair? lst)
          (let ((s (car lst)))
            (let ((start (segm-depart s))
                  (end (segm-arrivee s)))
              (let ((start-x (vect-x start))
                    (start-y (vect-y start))
                    (end-x (vect-x end))
                    (end-y (vect-y end)))
                (create-line start-x start-y end-x end-y #f)))
            (loop (cdr lst)))))))

(define gui-setup
  (lambda ()

    ;; La fenetre contient un titre et un canevas :
    ;;
    ;;  +------------------------------------------------------+
    ;;  |                       TP2                            |
    ;;  +------------------------------------------------------+
    ;;  |                                                      |
    ;;  |                                                      |
    ;;  |                                                      |
    ;;  |                                                      |
    ;;  |                                                      |
    ;;  |                      canevas                         |
    ;;  |                                                      |
    ;;  |                                                      |
    ;;  |                                                      |
    ;;  |                                                      |
    ;;  |                                                      |
    ;;  |                                                      |
    ;;  +------------------------------------------------------+

    (define handle
      (lambda (tkproc line)
        (cond ((eof-object? line)
               (exit))

              ((eq? line #f)
               (refresh tkproc segments)))))

    (let ((tkproc (tk-open handle)))

      (tk-cmd tkproc "package require Tk")

      ;; Creer la fenetre.

      (tk-cmd tkproc "wm title . \"TP2\"")
      (tk-cmd tkproc (string-append "wm geometry . " (number->string (+ canv-width 6)) "x" (number->string (+ canv-height 36))))

      (tk-cmd tkproc (string-append "canvas .canv -width " (number->string canv-width) " -height " (number->string canv-height)))

      (tk-cmd tkproc "pack .canv -side top -expand true -fill both"))))

;;-----------------------------------------------------------------------------

;; Demarrer le GUI.

(gui-setup)

;;-----------------------------------------------------------------------------
