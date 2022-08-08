(defun pwd ()
  (uiop:getcwd))			; => PWD

(defun cd (&optional (dir (user-homedir-pathname)))
  (let ((d (if (equal dir "..")
	       (make-pathname :directory (butlast (pathname-directory (pwd))))
	       (merge-pathnames dir
				(pwd)))))
    (uiop:chdir d)))			; => CD

(defun ls (&optional (path (pwd)))
  (uiop:directory-files path))		; => LS

(defun cat (&rest files)
  (dolist (f (apply #'directory files))
    (with-open-file (in f :direction :input)
      (loop :for line = (read-line in nil nil)
	    :while line
	    :do (format t "~a~%" line))))) ; => CAT



(defun touch (file)
  (with-open-file (out file
		       :if-does-not-exist :create))) ; => TOUCH

;;(touch "hoge.txt")				 ; => T

(defun rm (file)
  (delete-file file))			; => RM

;;(rm "~/test/fuga")			; => T

(defun mkdir (dir)
  (let ((d (if (zerop (mismatch "/" dir :from-end t))
	       dir
	       (format nil "~a/" dir))))
    (ensure-directories-exist
     (merge-pathnames (directory-namestring d)
		      (pwd)))))		; => MKDIR


;; (defun rmdir (dir)
;;   (sb-posix:rmdir dir))			; => RMDIR

(defun rmdir (dir)
  (uiop:delete-empty-directory
   (merge-pathnames dir
		    (pwd))))		; => RMDIR



(defun cp (in-file out-file)
  (uiop:copy-file (merge-pathnames in-file
				   (pwd))
		  (merge-pathnames out-file
				   (pwd)))) ; => CP
