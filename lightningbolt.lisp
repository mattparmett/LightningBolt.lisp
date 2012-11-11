; Common Lisp interpretation of LightningBolt.rb (github.com/mattparmett/LightningBolt)
; Written by Matt Parmett (mattparmett.com)
  
; Function to read file as a string
(defun read-file (path)
  "Reads plaintext file located at (path) and returns
  contents as a string."
  (with-open-file (s path)
    (let* ((len (file-length s))
      (data (make-string len)))
      (values data(read-sequence data s)))))

; Function to split string, returns list
; delim = #\Space for words
; delim = #\Newline for paragraphs
(defun split (string delim)
  "Splits (string) at (delim), returning a list of substrings."
  (loop for i = 0 then
    (1+ j) as j = (position delim string :start i)
    if (not (equal "" (subseq string i j)))
    collect (subseq string i j) while j))
    
; Macro for splitting string into paras
(defmacro to-paras (string)
  `(split ,string #\Newline))
  
; Macro for splitting string into words
(defmacro to-words (string)
  `(split ,string #\Space))
  
; Macro to remove duplicates in a list
(defmacro rm-dupes (lst)
  `(remove-duplicates ,lst :test #'equal))
  
; Functions for landscaping strings
; thanks http://www.ai.mit.edu/projects/ncwr/references/nlp/parse.lisp
(defun strip-punctuation (s)
  "Strinps all punctuation from string (s) and returns clean string."
  (if (string-equal s "") ""
    (remove-if-not #'(lambda (x) (or (alphanumericp x) (equal x #\Space) (equal x #\') (equal x #\.))) s)))

(defun whitespacep (c)
  (or (eql c #\Space) (eql c #\Tab)))

(defun trim-whitespace-left (s)
  "Trims leading whitespace from string (s) and returns resulting string."
  (cond ((= 0 (length s)) "")
        (t (cond ((or (whitespacep (char s 0)) (equal (char s 0) #\.))
                  (trim-whitespace-left (string-left-trim (string (char s 0)) s)))
                 (t s)))))

(defun trim-whitespace-right (s)
  "Trims trailing whitespace from string (s) and returns resulting string."
  (let ((len (1- (length s))))
    (cond ((= 0 (length s)) "")
	  (t (cond ((or (whitespacep (char s len)) (equal (char s len) #\.))
		    (trim-whitespace-right (string-right-trim (string (char s (1- (length s)))) s)))
		   (t s))))))
  
(defun trim-whitespace (s)
  "Trims leading and trailing whitespace from string (s) and returns resulting string."
  (trim-whitespace-left (trim-whitespace-right s)))
  
; Macro to encompass all landscaping functions
(defmacro landscape (string)
  `(string-downcase (trim-whitespace (strip-punctuation ,string))))
  
; Function to count words in string
(defun count-words (string)
  "Returns the number of words, split by a space, in (string) as an integer."
  (list-length (to-words string)))
      
; Function to count occurrence of word in string
(defun freq (word string)
  "Returns number of occurrences of (word) in (string) as an integer."
  (loop for w in (to-words string)
    if (equal (landscape w) (landscape word))
    collect w into results
    finally (setf *results* results))
  (return-from freq (list-length *results*)))

; Function to calculate idf
(defun idf (word string)
  "Returns the idf score of (word) in (string) as an integer."
  (setq occurrences (freq word string))
  (log (/ (count-words string) occurrences)))

; Function to generate centroid
(defun centroid (threshold-score string)
  "Returns the centroid of (string) given (threshold-score) as an a-list
  of (word word-score).  Centroid is an a-list of the most 'important'
  (frequent) words in a body of text."
  (setq words (rm-dupes (to-words string)))
  (loop for word in words
    do (setq freq (freq word string))
    do (setq idf (idf word string))
    do (setq word-score (* freq idf))
    if (> word-score threshold-score)
    collect (list word word-score)))

; Function to compute paragraph scores
(defun compute-para-scores (threshold-score string)
  "Returns a list containing importance scores, as integers,
  for each paragraph (determined by newline) in (string)."
  (setq centroid (centroid threshold-score string))
  (setq paras (to-paras string))
  (loop for p in paras
    do (setq para-score 0)
    do (setq para-words (to-words p))
    do (loop for word in para-words
      do (loop for pair in centroid
        if (equal word (car pair))
        do (incf para-score (nth 1 pair))))
    collect para-score))

; Function to summarize a plaintext file
(defun summarize (file-path threshold-score)
  "Creates ./summary.txt which contains a centroid-based summary of the
  plaintext file located at (file-path), given filter strength (threshold-score)."
  ; contents = file contents as string
  (setq contents (read-file file-path))
  
  ; create a-list (para, score) of important paras
  (setq paras (to-paras contents))
  (setq para-scores (compute-para-scores threshold-score contents))
  (loop for para in paras
    if (> (nth (position para paras) para-scores) (* 50 threshold-score))
    collect (list para (nth (position para paras) para-scores)) into summary
    finally (setf *summary* summary))
  
  ; write summary to summary.txt
  (with-open-file (str "summary.txt"
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
  (loop for pair in *summary*
    do (format str "~a" (car pair))
    do (format str "~a" ""))))