(defun pwd ()
  (uiop:getcwd))			; => PWD

;; (pwd)					; => #P"/home/hiro/howm/junk/"

(defun cd (&optional (path (user-homedir-pathname)))
  (uiop:chdir path))			; => CD

;;(cd)					; => 0
;;(pwd)					; => #P"/home/hiro/"
;;(cd "~/test")				; => 0
;;(pwd)					; => #P"/home/hiro/test/"


(defun ls (path)
  (uiop:directory-files path))		; => LS
;; (ls "~/test/")				; => (#P"/home/hiro/test/fuga" #P"/home/hiro/test/hoge")
;; (ls (pwd))				; => (#P"/home/hiro/test/fuga" #P"/home/hiro/test/hoge")

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

