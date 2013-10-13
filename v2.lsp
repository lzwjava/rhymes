(defparameter words (make-array 25000 :fill-pointer 0 :element-type 'string))
(defparameter ht (make-hash-table :test #'equal
																	:size 10000))

(defun xform (fn seq)
	(map-into seq fn seq))

(defun main ()
	(defvar n)
	(setf (fill-pointer words) 0)
	(clrhash ht)
	(read-word "source.txt")
	;(make-words)
	(write-word "rhymes.txt"))

(defmacro new-word ()
	`(when (/= pos 0)
		 (let ((word (string-downcase (subseq buf 0 pos))))
			 (when (not (gethash word ht))
				 (setf (gethash word ht) t)
				 (vector-push-extend word words 10000))
			 (setf pos 0))))

(defun read-word (from)
	(with-open-file (str from :direction :input)
		(let ((pos 0)
					(buf (make-string 100))
					(n 0))
			(do ((ch (read-char str 'nil :eof)
							 (read-char str 'nil :eof)))
					((eql ch :eof))
				(cond ((or (alpha-char-p ch)
									 (char= ch #\'))
							 (setf (aref buf pos) ch)
							 (incf pos))
							(t
							 (new-word))))
			(new-word)
			n)))

(defun write-word (to)
	(with-open-file (str to :direction :output
											 :if-exists :supersede)
	  (map nil #'(lambda (x)
								 (fresh-line str)
								 (princ x str))
				     (xform #'nreverse
										(sort (xform #'nreverse words)
													#'string<)))))

(time (main))
