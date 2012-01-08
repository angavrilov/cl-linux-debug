(in-package :asdf)

(let ((cg (find-system :cffi-grovel)))
  (unless (system-source-file cg)
    (%set-system-source-file
     (pathname (directory-namestring
                (merge-pathnames #P"mk-runtime/"
                                 (system-definition-pathname :cl-linux-debug))))
     cg)))

(in-package :elf)

#| Patch the ELF library. The original implementation of the
   function is massively inefficient. |#

(defun name-symbols (sec)
  "Assign names to the symbols contained in SEC."
  (when (member (type sec) '(:symtab :dynsym))
    (with-slots (elf sh type) sec
      (let* ((name-sec (nth (link sh) (sections elf)))
             (tab (data name-sec)))
        (mapcar (lambda (sym)
                  (setf (sym-name sym)
                        (coerce (loop for idx from (name sym)
                                   for code = (aref tab idx)
                                   until (equal code 0)
                                   collect (code-char code))
                                'string)))
                (data sec))))))
