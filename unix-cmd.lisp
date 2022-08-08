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

