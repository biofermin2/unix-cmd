(defvar directory-stack ())		; => DIRECTORY-STACK

(defun pwd ()
  (let* ((pwd (uiop:getcwd))
	 (current-dir-name (car (last (pathname-directory pwd)))))
    (values pwd current-dir-name)))	; => PWD

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

(defun rm (file)
  (let ((f (merge-pathnames file (pwd))))
    (delete-file f)))			; => RM

(defun mkdir (dir)
  (let ((d (if (zerop (mismatch "/" dir :from-end t))
	       dir
	       (format nil "~a/" dir))))
    (ensure-directories-exist
     (merge-pathnames (directory-namestring d)
		      (pwd)))))		; => MKDIR


(defun rmdir (dir)
  (uiop:delete-empty-directory
   (merge-pathnames dir
		    (pwd))))		; => RMDIR



(defun cp (in-file out-file)
  (uiop:copy-file (merge-pathnames in-file
				   (pwd))
		  (merge-pathnames out-file
				   (pwd)))) ; => CP


(defun pushd (dir)
  (push (pwd) directory-stack)
  (let ((d (merge-pathnames dir
			    (pwd))))
    (cd d)))				; => PUSHD

(defun popd ()
  (cd (pop directory-stack)))		; => POPD

(defun date ()
  (multiple-value-bind (sec min hr date mon yr dow daylight-p zone)
      (decode-universal-time (get-universal-time))
    (let ((jdow (nth dow '("月" "火" "水" "木" "金" "土" "日"))))
      (format t "~d年 ~d月 ~d日 ~a曜日 ~2,'0d:~2,'0d:~2,'0d " yr mon date jdow hr min sec)))) ; => DATE

