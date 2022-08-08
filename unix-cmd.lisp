(defun pwd ()
  (uiop:getcwd))			; => PWD

;; (pwd)					; => #P"/home/hiro/howm/junk/"

(defun cd (path)
  (uiop:chdir path))			; => CD

;; (cd "~/howm")				; => 0
;; (pwd)					; => #P"/home/hiro/howm/"


(defun ls (path)
  (uiop:directory-files path))		; => LS
;; (ls "~/test/")				; => (#P"/home/hiro/test/fuga" #P"/home/hiro/test/hoge")

(defun cat (&rest files)
  (dolist (f (apply #'directory files))
    (with-open-file (in f :direction :input)
      (loop :for line = (read-line in nil nil)
	    :while line
	    :do (format t "~a~%" line))))) ; => CAT

;;(cat "~/*.txt")		; =>

(defun touch (file)
    (close (open "~/f.txt" :direction :probe
			   :if-does-not-exist :create 
			   :if-exists :append))) ; => TOUCH
;;(touch "hoge.txt")				 ; => T

(defun rm (file)
  (delete-file file))			; => RM

;;(rm "~/test/fuga")			; => T

