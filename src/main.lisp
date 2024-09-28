(defpackage flx
  (:use :cl)
  (:export score))

(ql:quickload "str")

(in-package :flx)

(defvar word-separators '(#\Space #\- #\_ #\: #\. #\/ #\\)
  "List of characters that act as word separators in flx.")

(defun memq (elt list)
  "Mimic `memq' function."
  (member elt list :test #'eq))

;; Do we need more word separators than ST?
(defun word-p (char)
  "Check if CHAR is a word character."
  (and char
       (not (memq char word-separators))))

(defun capital-p (char)
  "Check if CHAR is an uppercase character."
  (and char
       (word-p char)
       (= char (str:upcase char))))

(defun boundary-p (last-char char)
  "Check if LAST-CHAR is the end of a word and CHAR the start of the next.

This function is camel-case aware."
  (or (null last-char)
      (and (not (capital-p last-char))
           (capital-p char))
      (and (not (word-p last-char))
           (word-p char))))

(defun inc-vec (vec &optional inc beg end)
  "Increment each element in VEC between BEG and END by INC.

INC defaults to 1.  BEG defaults to 0 and is inclusive.
END is not inclusive and defaults to the length of VEC."
  (or inc
      (setq inc 1))
  (or beg
      (setq beg 0))
  (or end
      (setq end (length vec)))
  (loop while (< beg end)
        do (incf (aref vec beg) inc)
           (incf beg))
  vec)

(defun get-hash-for-string (str heatmap-func)
  "Return hash-table for string where keys are characters.

Value is a sorted list of indexes for character occurrences."
  (let* ((res (make-hash-table :test 'eq :size 32))
         (str-len (length str))
         down-char)
    (cl-loop for index from (1- str-len) downto 0
             for char = (aref str index)
             do (progn
                  ;; simulate `case-fold-search'
                  (if (capital-p char)
                      (progn
                        (push index (gethash char res))
                        (setq down-char (downcase char)))
                      (setq down-char char))
                  (push index (gethash down-char res))))
    (puthash 'heatmap (funcall heatmap-func str) res)
    res))

;; So we store one fixnum per character.  Is this too memory inefficient?
(defun get-heatmap-str (str &optional group-separator)
  "Generate the heatmap vector of string.

See documentation for logic."
  (let* ((str-len (length str))
         (str-last-index (1- str-len))
         ;; ++++ base
         (scores (make-vector str-len -35))
         (penalty-lead ?.)
         (groups-alist (list (list -1 0))))
    ;; ++++ final char bonus
    (cl-incf (aref scores str-last-index) 1)
    ;; Establish baseline mapping
    (cl-loop for char across str
             for index from 0
             with last-char = nil
             with group-word-count = 0
             do (progn
                  (let ((effective-last-char
                          ;; before we find any words, all separaters are
                          ;; considered words of length 1.  This is so "foo/__ab"
                          ;; gets penalized compared to "foo/ab".
                          (if (zerop group-word-count) nil last-char)))
                    (when (boundary-p effective-last-char char)
                      (setcdr (cdar groups-alist)
                              (cons index (cl-cddar groups-alist))))
                    (when (and (not (word-p last-char))
                               (word-p char))
                      (cl-incf group-word-count)))
                  ;; ++++ -45 penalize extension
                  (when (eq last-char penalty-lead)
                    (cl-incf (aref scores index) -45))
                  (when (eq group-separator char)
                    (setcar (cdar groups-alist) group-word-count)
                    (setq group-word-count 0)
                    (push (nconc (list index group-word-count)) groups-alist))
                  (if (= index str-last-index)
                      (setcar (cdar groups-alist) group-word-count)
                      (setq last-char char))))
    (let* ((group-count (length groups-alist))
           (separator-count (1- group-count)))
      ;; ++++ slash group-count penalty
      (unless (zerop separator-count)
        (inc-vec scores (* -2 group-count)))
      ;; score each group further
      (cl-loop for group in groups-alist
               for index from separator-count downto 0
               with last-group-limit = nil
               with basepath-found = nil
               do (let ((group-start (car group))
                        (word-count (cadr group))
                        ;; this is the number of effective word groups
                        (words-length (length (cddr group)))
                        basepath-p)
                    (when (and (not (zerop words-length))
                               (not basepath-found))
                      (setq basepath-found t)
                      (setq basepath-p t))
                    (let (num)
                      (setq num
                            (if basepath-p
                                (+ 35
                                   ;; ++++ basepath separator-count boosts
                                   (if (> separator-count 1)
                                       (1- separator-count)
                                       0)
                                   ;; ++++ basepath word count penalty
                                   (- word-count))
                                ;; ++++ non-basepath penalties
                                (if (= index 0)
                                    -3
                                    (+ -5 (1- index)))))
                      (inc-vec scores num (1+ group-start) last-group-limit))
                    (cl-loop for word in (cddr group)
                             for word-index from (1- words-length) downto 0
                             with last-word = (or last-group-limit
                                                  str-len)
                             do (progn
                                  (cl-incf (aref scores word)
                                           ;; ++++  beg word bonus AND
                                           85)
                                  (cl-loop for index from word below last-word
                                           for char-i from 0
                                           do (cl-incf (aref scores index)
                                                       (-
                                                        ;; ++++ word order penalty
                                                        (* -3 word-index)
                                                        ;; ++++ char order penalty
                                                        char-i)))
                                  (setq last-word word)))
                    (setq last-group-limit (1+ group-start)))))
    scores))

(defun get-heatmap-file (filename)
  "Return heatmap vector for filename."
  (get-heatmap-str filename ?/))


(defsubst bigger-sublist (sorted-list val)
  "Return sublist bigger than VAL from sorted SORTED-LIST.

If VAL is nil, return entire list."
  (if val
      (cl-loop for sub on sorted-list
               do (when (> (car sub) val)
                    (cl-return sub)))
      sorted-list))

(defun make-filename-cache ()
  "Return cache hashtable appropriate for storing filenames."
  (make-string-cache 'get-heatmap-file))

(defun make-string-cache (&optional heat-func)
  "Return cache hashtable appropriate for storing strings."
  (let ((hash (make-hash-table :test 'equal
                               :size 4096)))
    (puthash 'heatmap-func (or heat-func 'get-heatmap-str) hash)
    hash))

(defun process-cache (str cache)
  "Get calculated heatmap from cache, add it if necessary."
  (let ((res (when cache
               (gethash str cache))))
    (or res
        (progn
          (setq res (get-hash-for-string
                     str
                     (or (and cache (gethash 'heatmap-func cache))
                         'get-heatmap-str)))
          (when cache
            (puthash str res cache))
          res))))

(defun find-best-match (str-info
                        heatmap
                        greater-than
                        query
                        query-length
                        q-index
                        match-cache)
  "Recursively compute the best match for a string, passed as STR-INFO and
HEATMAP, according to QUERY.

This function uses MATCH-CACHE to memoize its return values.
For other parameters, see `score'"

  ;; Here, we use a simple N'ary hashing scheme
  ;; You could use (/ hash-key query-length) to get greater-than
  ;; Or, (mod hash-key query-length) to get q-index
  ;; We use this instead of a cons key for the sake of efficiency
  (let* ((hash-key (+ q-index
                      (* (or greater-than 0)
                         query-length)))
         (hash-value (gethash hash-key match-cache)))
    (if hash-value
        ;; Here, we use the value 'no-match to distinguish a cache miss
        ;; from a nil (i.e. non-matching) return value
        (if (eq hash-value 'no-match)
            nil
            hash-value)
        (let ((indexes (bigger-sublist
                        (gethash (aref query q-index) str-info)
                        greater-than))
              (match)
              (temp-score)
              (best-score most-negative-fixnum))

          ;; Matches are of the form:
          ;; ((match_indexes) . (score . contiguous-count))
          (if (>= q-index (1- query-length))
              ;; At the tail end of the recursion, simply
              ;; generate all possible matches with their scores
              ;; and return the list to parent.
              (setq match (mapcar (lambda (index)
                                    (cons (list index)
                                          (cons (aref heatmap index) 0)))
                                  indexes))
              (dolist (index indexes)
                (dolist (elem (find-best-match str-info
                                               heatmap
                                               index
                                               query
                                               query-length
                                               (1+ q-index)
                                               match-cache))
                  (setq temp-score
                        (if (= (1- (caar elem)) index)
                            (+ (cadr elem)
                               (aref heatmap index)

                               ;; boost contiguous matches
                               (* (min (cddr elem)
                                       3)
                                  15)
                               60)
                            (+ (cadr elem)
                               (aref heatmap index))))

                  ;; We only care about the optimal match, so only
                  ;; forward the match with the best score to parent
                  (when (> temp-score best-score)
                    (setq best-score temp-score
                          match (list (cons (cons index (car elem))
                                            (cons temp-score
                                                  (if (= (1- (caar elem))
                                                         index)
                                                      (1+ (cddr elem))
                                                      0)))))))))

          ;; Calls are cached to avoid exponential time complexity
          (puthash hash-key
                   (if match match 'no-match)
                   match-cache)
          match))))

(defun score (str query &optional cache)
  "Return best score matching QUERY against STR."
  (unless (or (zerop (length query))
              (zerop (length str)))
    (let*
        ((str-info (process-cache str cache))
         (heatmap (gethash 'heatmap str-info))
         (query-length (length query))
         (full-match-boost (and (< 1 query-length)
                                (< query-length 5)))

         ;; Raise recursion limit
         (max-lisp-eval-depth 5000)

         ;; Dynamic Programming table for memoizing find-best-match
         (match-cache (make-hash-table :test 'eql :size 10))

         (optimal-match (find-best-match str-info
                                         heatmap
                                         nil
                                         query
                                         query-length
                                         0
                                         match-cache)))
      ;; Postprocess candidate
      (and optimal-match
           (cons
            ;; This is the computed score, adjusted to boost the scores
            ;; of exact matches.
            (if (and full-match-boost
                     (=  (length (caar optimal-match))
                         (length str)))
                (+ (cl-cadar optimal-match) 10000)
                (cl-cadar optimal-match))

            ;; This is the list of match positions
            (caar optimal-match))))))
