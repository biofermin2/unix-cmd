(defpackage :unix-cmd
  (:use :cl)
  (:export :directory-stack :split
   :pwd :cd :ls :ll :cat :rm :touch :rmdir :pushd :popd
   :head :date :cp :mkdir :echo :wc :seq)) ; =>#<PACKAGE "UNIX-CMD"> 
(in-package :unix-cmd)                     ; =>#<PACKAGE "UNIX-CMD"> 


(defvar directory-stack ())             ; =>DIRECTORY-STACK 

(defun split (dt str)                  
  (let ((pos (search dt str))
        (size (length dt)))
    (if pos
	(cons (subseq str 0 pos)
	      (split dt (subseq str (+ pos size))))
      (list str))))                     ; =>SPLIT 


(defun current-dir-name (pwd)
  (car (last (pathname-directory pwd)))) ; =>CURRENT-DIR-NAME 

(defun pwd ()
  (let* ((pwd (uiop:getcwd))
	 (current-dir-name (car (last (pathname-directory pwd)))))
    (values pwd current-dir-name)))     ; =>PWD 

(defun cd (&optional (dir (user-homedir-pathname)))
  (let ((d (merge-pathnames dir (pwd))))
    (uiop:chdir d)
    (pwd)))                             ; =>CD 

(defun ls (&optional (path "*"))
  (format t "~{~a~%~}"
          (mapcar #'(lambda (x) (enough-namestring x (pwd)))
                  (cond ((or (equal path ".") (equal path "*"))
                         (directory (merge-pathnames "*.*" (pwd))))
                        ((equal path "*.*")
                         (set-difference
                          (directory (merge-pathnames "*.*" (pwd)))
                          (directory (merge-pathnames "*" (pwd)))))
                        ((equal path "-d")
                         (directory (merge-pathnames "*" (pwd))))
                        ((equal path "..")
                         (directory (merge-pathnames "*.*" (cd path))))
                        (t (directory (merge-pathnames path (pwd)))))))) ; =>LS 


(defun ll (&optional (path "*"))
  (mapcar #'(lambda (x) (enough-namestring x (pwd)))
          (cond ((or (equal path ".") (equal path "*"))
                 (directory (merge-pathnames "*.*" (pwd))))
                ((equal path "*.*")
                 (set-difference
                  (directory (merge-pathnames "*.*" (pwd)))
                  (directory (merge-pathnames "*" (pwd)))))
                ((equal path "-d")
                 (directory (merge-pathnames "*" (pwd))))
                ((equal path "..")
                 (directory (merge-pathnames "*.*" (cd path))))
                (t (directory (merge-pathnames path (pwd))))))) ; =>LL 

(defun cat (files)
  (dolist (f (directory (merge-pathnames files (pwd))))
    (with-open-file (in f :direction :input)
      (loop :for line = (read-line in nil nil)
	    :while line
	    :do (format t "~a~%" line))))) ; =>CAT 

(defun touch (file)
  (let ((f (merge-pathnames file (pwd))))
    (with-open-file (out f :direction :output
                              :if-does-not-exist :create)))) ; =>TOUCH 

(defun rm (file)
  (let ((f (merge-pathnames file (pwd))))
    (delete-file f)))                   ; =>RM 

(defun mkdir (dir)
  (let ((d (if (zerop (mismatch "/" dir :from-end t))
	       dir
	       (format nil "~a/" dir))))
    (ensure-directories-exist
     (merge-pathnames (directory-namestring d)
		      (pwd)))))         ; =>MKDIR 


(defun rmdir (dir)
  (uiop:delete-empty-directory
   (merge-pathnames dir
		    (pwd))))            ; =>RMDIR 

(defun cp (in-file out-file)
  (uiop:copy-file (merge-pathnames in-file
				   (pwd))
		  (merge-pathnames out-file
				   (pwd)))) ; =>CP 

(defun pushd (dir)
  (push (pwd) directory-stack)
  (let ((d (merge-pathnames dir
			    (pwd))))
    (cd d)))                            ; =>PUSHD 

(defun popd ()
  (cd (pop directory-stack)))           ; =>POPD 

(defun date ()
  (multiple-value-bind (sec min hr date mon yr dow)
      (decode-universal-time (get-universal-time))
    (let ((jdow (nth dow '("月" "火" "水" "木" "金" "土" "日"))))
      (format t "~d年 ~d月 ~d日 ~a曜日 ~2,'0d:~2,'0d:~2,'0d " yr mon date jdow hr min sec)))) ; =>DATE 


(defun echo (x)
  (identity x))   ; =>ECHO 
;; (defun echo (x)
;;   x)                                    ; => ECHO

(defun head (file &optional (n 10) &key header)
  (with-open-file (in file :direction :input)
    (let ((filename (file-namestring file)))
      (loop :initially (when header (format t "==> ~a <==~%" filename))
            :repeat n :for line = (read-line in nil nil)
	    :while line
	    :do (format t "~a~%" line))))) ; =>HEAD 

(defun wc (files)
  "line word char (in case unix,number means line word byte)"
  (dolist (f (directory (merge-pathnames files (pwd))))
    (with-open-file (in f :direction :input)
      (loop :for l = (read-line in nil nil)
	    :while l
	    :count l :into line
	    :sum (length (split " " l)) :into word
	    :sum (length l) :into char
	    :finally (format t "~d ~d ~d ~a~%" line word char f))))) ; =>WC 

(defun seq (&rest arg)
  ;; seq [OPTION]... LAST
  ;; seq [OPTION]... FIRST LAST
  ;; seq [OPTION]... FIRST INCREMENT LAST
  (let ((end (car arg))
        (start 1)
        (inc 1)
        (len (length arg)))
    (case len
      (2 (setq start end
               end (second arg)))
      (3 (setq start end
               inc (second arg)
               end (third arg))))
    (loop :for i :from start :to end :by inc
          :do (print i))))              ; =>SEQ 

