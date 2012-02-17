;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.code-info)

;; Address range

(def (class* e) address-chunk ()
  ((start-address :accessor t)
   (length :accessor t)))

(def (class* e) data-chunk (address-chunk)
  ((start-offset 0 :reader t)
   (data-bytes :reader t)))

(macrolet ((frob (elt-type)
             `(progn
                (defun ,(symbolicate '#:index-chunks/ elt-type) (chunk)
                  (let* ((cset (stable-sort chunk #'< :key #'start-address-of))
                         (cstarts (mapcar #'start-address-of cset)))
                    (list (make-array (length cset) :element-type ',elt-type :initial-contents cstarts)
                          (coerce (mapcar #'length-of cset) 'vector)
                          (coerce cset 'vector))))
                (declaim (ftype (function (t ,elt-type) (values t fixnum fixnum))
                                ,(symbolicate '#:lookup-indexed-chunk/ elt-type)))
                (defun ,(symbolicate '#:lookup-indexed-chunk/ elt-type) (index address)
                  (declare (type ,elt-type address)
                           (optimize (speed 3)))
                  (let ((robj nil)
                        (roff 0)
                        (rgap 0))
                    (declare (type fixnum roff rgap))
                    (when index
                      (destructuring-bind (addrs lengths objs) index
                        (declare (type (simple-array ,elt-type (*)) addrs)
                                 (type simple-vector lengths objs))
                        (with-binsearch-in-array (lookup addrs ,elt-type #'<= :array-var av)
                          (awhen (lookup address)
                            (let* ((base (aref addrs it))
                                   (size (svref lengths it))
                                   (obj (svref objs it))
                                   (diff (logand (- address base) #xFFFFFFFF)))
                              (declare (type (integer 0 #.most-positive-fixnum) size)
                                       (type ,elt-type base diff))
                              (when (< diff size)
                                (setq robj obj roff (the fixnum diff) rgap (- size diff))))))))
                    (values robj roff rgap))))))
  (frob uint32))

;; On-disk executable

(def (class* e) image-section (data-chunk)
  ((image :reader t)
   (file-offset :reader t)
   (section-name :reader t)
   (loaded? :reader t)
   (writable? :reader t)
   (executable? :reader t)))

(def (class* e) section-set ()
  ((sections nil :reader t)
   (section-map (make-chunk-table) :reader t)))

(defgeneric find-section-by-name (executable name)
  (:method ((exec section-set) name)
    (find name (sections-of exec) :key #'section-name-of :test #'equal)))

(defgeneric find-section-by-address (executable address)
  (:method ((exec section-set) addr)
    (lookup-chunk (section-map-of exec) addr)))

(def (class* e) executable-image (section-set)
  ((path :reader t)
   (md5-hash :reader t)
   (binary-timestamp :reader t :initform nil)
   (entry-address :reader t)
   (shared-lib? :reader t)
   (region-map (make-chunk-table) :reader t)
   (region-name-map (make-hash-table :test #'equal) :reader t)))

(defun set-executable-sections (exec sections)
  (setf (slot-value exec 'sections) sections)
  (dolist (section sections)
    (when (start-address-of section)
      (insert section (section-map-of exec)))))

;; ELF

(def (class* e) elf-image-section (image-section)
  ((elf-section :reader t)
   (elf-type :reader t)
   (linked-elf :reader t)))

(def (class* e) elf-executable-image (executable-image)
  ((elf-data :reader t)))

;; PE

(def (class* e) pe-image-section (image-section)
  ((header :reader t)))

(def (class* e) pe-executable-image (executable-image)
  ((dos-header :reader t)
   (win-header :reader t)
   (aux-header :reader t)))

;; Region data

(def (class* e) executable-region (data-chunk)
  ((section :reader t)
   (symbol-name nil :reader t)))

(defmethod image-of ((obj executable-region))
  (image-of (section-of obj)))

(defmethod (setf symbol-name-of) (value (obj executable-region))
  (with-slots (symbol-name) obj
    (let* ((rtbl (region-name-map-of (image-of obj))))
      (awhen symbol-name
        (deletef (gethash it rtbl) obj))
      (awhen (setf symbol-name value)
        (pushnew obj (gethash it rtbl)))
      value)))

(def (class* e) executable-region-object (executable-region)
  ())

(def (class* e) executable-region-function (executable-region)
  ((unwind-info nil :accessor t)))

(def (class* e) executable-region-plt-entry (executable-region)
  ((got-entry :accessor t)))

(def (class* e) executable-region-got-entry (executable-region)
  ())

;; Loaded executable

(def (class* e) loaded-object ()
  ((origin :reader t)
   (relocation-offset :reader t)))

(defgeneric relocated? (obj)
  (:method ((obj loaded-object))
    (/= (relocation-offset-of obj) 0)))

(defun wrap-loaded-chunk (base offset &key (type 'loaded-object))
  (when base
    (let ((real-offset (if (numberp offset) offset
                           (relocation-offset-of offset))))
      (make-instance type
                     :origin base
                     :relocation-offset real-offset
                     :start-address (+ (start-address-of base) real-offset)
                     :length (length-of base)
                     :start-offset (start-offset-of base)
                     :data-bytes (data-bytes-of base)))))

(def (class* e) loaded-section (data-chunk loaded-object)
  ((loaded-image :reader t)
   (mapping :reader t)))

(defmethod section-name-of ((section loaded-section))
  (section-name-of (origin-of section)))

(def (class* e) loaded-image (section-set loaded-object)
  ((executable :reader t)))

(defmethod md5-hash-of ((image loaded-object))
  (md5-hash-of (origin-of image)))

(defmethod binary-timestamp-of ((image loaded-object))
  (binary-timestamp-of (origin-of image)))

(defmethod entry-address-of ((image loaded-image))
  (+ (entry-address-of (origin-of image))
     (relocation-offset-of image)))

(def (class* e) loaded-executable (section-set)
  ((main-image nil :reader t)
   (all-images nil :accessor t)
   (function-map (make-chunk-table) :reader t)))

(defgeneric detect-image-relocation (image executable mappings))

;;

(def (class* e) loaded-region (data-chunk loaded-object)
  ())

(defmethod symbol-name-of ((lrgn loaded-region))
  (symbol-name-of (origin-of lrgn)))

(defgeneric find-region-by-address (executable addr &key next?)
  (:method ((image null) addr &key next?)
    (declare (ignore next?))
    nil)
  (:method (image (addr null) &key next?)
    (declare (ignore next?))
    nil)
  (:method ((image executable-image) addr &key next?)
    (if next?
        (when (not (trees:emptyp (region-map-of image)))
          (lookup-next-chunk (region-map-of image) addr))
        (lookup-chunk (region-map-of image) addr)))
  (:method ((wrapper loaded-object) addr &key next?)
    (wrap-loaded-chunk (find-region-by-address
                        (origin-of wrapper)
                        (- addr (relocation-offset-of wrapper))
                        :next? next?)
                       wrapper
                       :type 'loaded-region))
  (:method ((exec loaded-executable) addr &key next?)
    (awhen (find-section-by-address exec addr)
      (find-region-by-address (loaded-image-of it) addr :next? next?))))

(defgeneric find-regions-by-name (executable name)
  (:method ((image null) name)
    nil)
  (:method (image (name null))
    nil)
  (:method ((image executable-image) name)
    (gethash name (region-name-map-of image)))
  (:method ((wrapper loaded-object) name)
    (mapcar (lambda (x) (wrap-loaded-chunk x wrapper :type 'loaded-region))
            (find-regions-by-name (origin-of wrapper) name)))
  (:method ((exec loaded-executable) name)
    (mapcan (lambda (x) (find-regions-by-name x name)) (all-images-of exec))))
