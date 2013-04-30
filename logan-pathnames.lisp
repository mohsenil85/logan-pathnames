;;;; logan-pathnames.lisp

(in-package #:logan-pathnames)

;;; "logan-pathnames" goes here. Hacks and glory await!

;;;; from  ch15 practical common lisp
(make-package :logan-pathnames)
(defpackage :logan-pathnames
  (:use :common-lisp)
  (:export test
           component-present-p
           directory-pathname-p
           pathname-as-directory
           directory-wildcard
           list-directory
           file-exists-p
           pathname-as-file))


(defun test () (format t "tested!"))
(test)

(defun component-present-p (value)
  (and value (not (eql value :unspecific))))

(defun directory-pathname-p (p)
  (and
    (not (component-present-p (pathname-name p)))
    (not (component-present-p (pathname-type p)))
    p))

(defun pathname-as-directory (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "can't convert wild pathnames bro"))
    (if (not (directory-pathname-p name))
      (make-pathname 
        :directory (append (or (pathname-directory pathname) (list :relative))
                           (list (file-namestring pathname)))
        :name      nil
        :type      nil
        :defaults  pathname) 
      pathname)))

(defun directory-wildcard (dirname)
  (make-pathname
    :name :wild
    :type #-clisp :wild #+clisp nil
    :defaults (pathname-as-directory dirname)))

(defun list-directory (dirname)
  (when (wild-pathname-p dirname)
    (error "can only list concrete directories dawg"))
  (let ((wildcard (directory-wildcard dirname)))

    #+(or sbcl cmu lispworks)
    (directory wildcard)

    #+openmcl
    (directory wildcard :directories t)

    #+allegro
    (directory wildcard :directories-are-files nil)

    #+clisp
    (nconc
      (directory wildcard)
      (directory (clisp-subdirectories-wildcard wildcard)))

    #-(or sbcl cmu lispworks openmcl allegro clisp)
    (error "list-directories not implementd for your bizziarass lisp")))

#+clisp
(defun clisp-subdirectories-wildcard (wildcard)
  (make-pathname
    :directory (append (pathname-directory wildcard) (list :wild))
    :name nil
    :type nil
    :defaults wildcard))

(defun file-exists-p (pathname)
  #+(or sbcl lispworks openmcl)
  (probe-file pathname)

  #+(or allegro cmu)
  (or 
    (probe-file (pathname-as-directory pathname))
    (probe-file pathname))

  #+clisp
  (or (ignore-errors 
        (probe-file (pathname-as-file pathname)))
      (ignore-errors 
        (let ((directory-form (pathname-as-directory pathname)))
          (when (ext:probe-directory directory-form)
            directory-form))))
  #-(or sbcl lispworks openmcl allegro cmu clisp)
  (error "file-exists-p not implement")) 

(defun pathname-as-file (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "cant conv wild pathnames!!!"))
    (if (directory-pathname-p name)
      (let* ((directory (pathname-directory pathname))
             (name-and-type (pathname (first (last directory)))))
        (make-pathname 
          :directory (butlast directory)
          :name (pathname-name name-and-type)
          :type (pathname-name name-and-type)
          :defaults pathname))
      pathname)))

(defun walk-directory (dirname fn &key directories (test (constantly t)))
  (labels
    ((walk (name)
       (cond
         ((directory-pathname-p name)
          (when (and directories (funcall test name))
            (funcall fn name))
          (dolist (x (list-directory name)) (walk x)))
         ((funcall test name) (funcall fn name)))))
    (walk (pathname-as-directory dirname))))

