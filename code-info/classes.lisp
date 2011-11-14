;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.code-info)

;; Address range

(def (class* e) address-chunk ()
  ((start-address :reader t)
   (length :accessor t)))

(def (class* e) data-chunk (address-chunk)
  ((start-offset 0 :reader t)
   (data-bytes :reader t)))

(defun make-chunk-table ()
  (make-binary-tree :red-black #'<
                    :key #'start-address-of
                    :test #'=))

(defun lookup-chunk (table address)
  (awhen (lower-bound address table)
    (let ((offset (- address (start-address-of it))))
      (values (if (or (null (length-of it))
                      (< offset (length-of it)))
                  it nil)
              offset))))

;; On-disk executable

(def (class* e) image-section (data-chunk)
  ((image :reader t)
   (elf-section :reader t)
   (elf-type :reader t)
   (linked-elf :reader t)
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
   (elf-data :reader t)
   (entry-address :reader t)
   (shared-lib? :reader t)
   (region-map (make-chunk-table) :reader t)))

(defgeneric find-region-by-address (executable addr)
  (:method (image (addr null))
    nil)
  (:method ((image executable-image) addr)
    (lookup-chunk (region-map-of image) addr)))

;; Region data

(def (class* e) executable-region (data-chunk)
  ((section :reader t)
   (symbol-name nil :accessor t)))

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

(def (class* e) loaded-section (data-chunk loaded-object)
  ((loaded-image :reader t)
   (mapping :reader t)))

(defmethod section-name-of ((section loaded-section))
  (section-name-of (image-section-of section)))

(def (class* e) loaded-image (section-set loaded-object)
  ((executable :reader t)))

(defmethod entry-address-of ((image loaded-image))
  (+ (entry-address-of (origin-of image))
     (relocation-offset-of image)))

(def (class* e) loaded-executable (section-set)
  ((main-image nil :reader t)
   (all-images nil :accessor t)
   (function-map (make-chunk-table) :reader t)))
