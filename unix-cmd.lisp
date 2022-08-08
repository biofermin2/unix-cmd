(defun pwd ()
  (uiop:getcwd))			; => PWD

;; (pwd)					; => #P"/home/hiro/howm/junk/"

(defun cd (&optional (path (user-homedir-pathname)))
  (uiop:chdir path))			; => CD

;;(cd)					; => 0
;;(pwd)					; => #P"/home/hiro/"
;;(cd "~/test")				; => 0
;;(pwd)					; => #P"/home/hiro/test/"

(defun ls (&optional (path (pwd)))
  (uiop:directory-files path))		; => LS

;; (ls "~/test/")				; => (#P"/home/hiro/test/fuga" #P"/home/hiro/test/hoge")
;; (ls)				; => (#P"/home/hiro/test/fuga" #P"/home/hiro/test/hoge")

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

(defun rmdir (dir)
  (sb-posix:rmdir dir))			; => RMDIR

(defun cp (in-file out-file)
  (uiop:copy-file (merge-pathnames in-file
				   (pwd))
		  (merge-pathnames out-file
				   (pwd)))) ; => CP
