;;;; util.lisp

(in-package :stumpwm-dfg)

;;;

(defun check (bool)
  "An adhoc unit tester."
  (unless bool (error "Test failed!")))

;;;

(defun location (x list &key (such-that (lambda (x) t)) (identifier #'equal))
  "Similar to the built-in #'position but different."
  (let* ((list (remove-if-not such-that list))
         (result (loop for k below (length list)
                       if (funcall identifier x (nth k list))
                         return k)))
    (if result result -1)))

(check (= 3 (location 3 '(0 1 2 3 4 5))))
(check (= 1 (location 3 '(0 1 2 3 4 5) :such-that #'oddp)))
(check (= -1 (location 3 '(0 1 2 3 4 5) :such-that #'evenp)))

;;;

(defun sort-logic (list &key (transformer #'identity) (identifier #'equalp))
  "Return a binary comparer based on LIST, which dictates the
order from small to big. If element is absent, make it the
absolutely smallest."
  (lambda (x y) (> (location (ignore-errors (funcall transformer x)) list)
                   (location (ignore-errors (funcall transformer y)) list))))

(check
 (equal '((large l) (medium m) (tiny t) 1 0)
        (stable-sort '((tiny t) 1 (medium m) 0 (large l))
                     (sort-logic '(tiny medium large) :transformer #'car))))

;;;

(defun permute (x y list)
  "Expect LIST to be a list without repeating elements. Return a
new list with X and Y permuted."
  (let* ((l (length list))
         (px (position x list)) (py (position y list))
         (min (min px py)) (max (max px py)))
    (when (and (<= 0 px l) (<= 0 py l) (not (= px py)))
      (append (loop for k below min collect (nth k list))
              (list (nth max list))
              (loop for k from (+ min 1) to (- max 1) collect (nth k list))
              (list (nth min list))
              (loop for k from (+ max 1) to (- l 1) collect (nth k list))))))

(defun permute-with-next (x ring &key (reverse nil))
  "Expect RING to be a list with no element being NIL."
  (let* ((l (length ring))
         (py (+ (if reverse -1 1) (position x ring))))
    (cond ((>= py l) (permute x (car ring) ring))
          ((< py 0) (permute x (car (last ring)) ring))
          (t (permute x (nth py ring) ring)))))

(check (equal '(c b a d e) (permute 'a 'c '(a b c d e))))
(check (equal '(b a c d e) (permute-with-next 'a '(a b c d e))))
(check (equal '(e b c d a) (permute-with-next 'e '(a b c d e))))
(check (equal '(a c b d e) (permute-with-next 'c '(a b c d e) :reverse t)))
(check (equal '(e b c d a) (permute-with-next 'a '(a b c d e) :reverse t)))
