;;-*- Mode: Lisp; Package: XML -*- 
;*********************************************************************
;*                                                                   *
;*            X M L i s p                                            *
;*                                                                   *
;*********************************************************************
;* Author       : Alexander Repenning, alexander@agentsheets.com     *
;*                http://www.agentsheets.com                         *
;* Copyright    : (c) 1996-2011, AgentSheets Inc.                    *
;* Filename     : XMLisp.lisp                                        *
;* Last Update  : 05/19/11                                           *
;* Version      :                                                    *
;*    1.0       : 09/19/04                                           *
;*    1.1       : 09/30/04 encode/decode strings in XML              *
;*    1.1.1     : 10/01/04 subobjects can be aggregated as arrays    *
;*    1.2       : 10/09/04 abreviated printing to inspector/listener *
;*                         serialization includes arrays             *
;*                         <?xml .. ?> headers                       *
;*                         :type slot interpretation                 *
;*    1.3       : 10/11/04 content only tags, e.g., <a>bla</a>       *
;*    1.4       : 10/12/04 SGML tags: <--, <![CDATA[, <!DOCTYPE      *
;*    1.4.1     : 10/16/04 print-slots                               *
;*    1.4.2     : 10/19/04 finished-reading-attributes               *
;*    1.4.3     : 10/28/04 ignore white space after name             *
;*    1.4.4     : 11/12/04 type character added                      *
;*    1.5       : 12/09/04 element and class name can be different   *
;*                         CODECs for typed-attribute-value          *
;*    1.6       : 12/16/04 read error: open file in Fred at position *
;*    1.6.1     : 12/22/04 print-typed-subelement-value              *
;*    2.0       : 02/10/05 use real MOP, allow compilation           *
;*    2.0.1     : 02/15/05 OpenMCL support, save-object              *
;*    2.0.2     : 03/01/05 print-typed-attribute-value for type t    *
;*    2.1       : 03/10/05 (setf part-of) aggregator method          *
;*    2.1.1     : 03/17/05 no warn print-typed-attribute-value       *
;*    2.1.2     : 03/19/05 READ-XMLISP-ELEMENT :after recursion safe *
;*    2.1.3     : 03/24/05 :clisp philippe.de.ryck@skynet.be         *
;*    2.2       : 04/12/05 package-dependent-element-reader          *
;*    2.2.1     : 05/09/05 CMU compat and &apos mycroft@actrix.co.nz *
;*    2.2.2     : 05/10/05 Allegro compat                            *
;*    2.2.3     : 05/11/05 READ-UNTIL-TOKEN no end of stream error   *
;*    2.2.4     : 05/12/05 check if stream has ccl::fblock slot      *
;*    2.3       : 06/24/05 package prefix, e.g., <xml:bla >          *
;*    2.3.1     : 07/11/05 deal with double AND single quote values  *
;*    2.3.2     : 07/14/05 concatenate all content                   *
;*    2.3.3     : 07/15/05 *Warn-if-undefined-XML-Decoder-Type,      *
;*    2.3.4     : 07/20/05 export xml-tag-name-symbol                *
;*    2.3.5     : 08/01/05 show-error-in-stream-to-user in           *
;*                         set-attribute-value                       *
;*                         encode single quote '                     *
;*    2.3.6     : 08/16/05 slot-definition-type most-specific-class  *
;*    2.3.7     : 08/30/05 export decode-xml-string encode-xml-string*
;*    2.3.8     : 09/14/05 check boundp *Xml-Stream*                 *
;*    2.3.9     : 09/16/05 list type decoder                         *
;*    2.4       : 10/17/05 print-default-value-attributes-p          *
;*    2.4.1     : 11/02/05 read-return-value                         *
;*    2.4.2     : 11/04/05 double-float CODEC                        *
;*    2.4.3     : 11/09/05 short-float CODE don't print "d"          *
;*    2.4.4     : 11/10/05 do not print lisp escape chars in strings *
;*    2.4.5     : 12/02/05 AI: convert relative unix path to lisppath*
;*    2.5       : 01/18/06 print non-t slot types attributes         * 
;*    2.5.1     : 01/23/06 attribute-name->slot-name,                *
;*                         slot-name->attribute-name                 *
;*    2.5.2     : 02/10/06 do not print ..D0 double floats           *
;*    2.5.3     : 02/17/06 file (setf file).  Set by load-object     *
;*    2.5.4     : 02/23/06 print pathname to stream                  * 
;*    2.5.5     : 04/05/06 without-xml-reader macro                  *
;*    2.5.6     : 06/20/06 print-slot-name-value-type-as-attribute   *
;*    2.6       : 08/25/06 path type and CODECs                      *
;*    2.7       : 01/17/07 float array CODEC                         *
;*    3.0       : 02/10/07 Optimized tree shacked: single file       *
;*    3.0.1     : 08/14/07 do not print array content into listener  *
;*    3.0.2     : 09/26/07 AI: fallback-class-name-for-element-name  *
;*    3.0.3     : 10/30/07 parse-file-name fixed Eirik Mikkelsen     *
;*    3.0.4     : 01/29/08 AI: single-float codec                    * 
;*    3.0.5     : 09/23/08 reader skip "<)"                          *
;*    3.1       : 10/09/08 enhanced reader to deal with <!--bla-->   *
;*                         not including space after element name    *
;*    3.2       : 11/14/08 if slot if missing lookup acccessor       *
;*    3.2.1     : 11/28/08 handle type specifier lists, e.g., boolean*
;*    3.5       : 12/03/08 early instantiation model (see below)     *
;*    3.5.1     : 12/10/08 string-upcase symbol codec, float codec   *
;*    3.5.2     : 12/16/08 read-return-value fixed, keyword CODEC    *
;*    3.5.3     : 08/04/09 :is-created-by-xml-reader slot            *
;*    3.6       : 10/30/09 more stringent instantiation policy       *
;*                         <bla ... will result in error if there is *
;*                         no matching class BLA                     *
;*    3.7       : 03/09/10 have file slot and set ASAP               *
;*    3.8       : 05/06/10 SBLC compatibility by John Morrison       *
;*    3.9       : 05/19/11 *XMLisp-Print-Synoptic* option            *
;* Systems      : G4, OS X 10.6.2                                    *
;* Lisps        : MCL 5.0, MCL 5.2, LispWorks 4.3.7, CCL 1.4         *
;*                CLISP 2.33.83, CMUCL, AGL                          *
;* Licence      : LGPL                                               *
;* Based on     : XML by Andri Ioannidou                             *
;* Abstract     : Integrate XML reading/writing with Lisp            *
;*   To use XMLisp mix in xml-serializer class into your class.      *
;*   When lisp reader sees: <bla x="13" y="20"> it will              *
;*    - create an instance of class "BLA"                            *
;*    - set slot "X" to 13 and slot "Y" to 20                        *
;*    - if slot includes :type use CODEC                             *
;*   Objects can have subobjects. Aggregation can be controlled      *
;*   by redefining aggregator functions, e.g., add-subobject         *
;*                                                                   *
;* Initialization:                                                   *
;*   this kind of element:                                           *
;*   <window width="380" height="140">                               *
;*     <button text="OK"/>                                           *
;*     <button text="Cancel"/>                                       *
;*   </window>                                                       *
;* - reading "<window" creates instance of window                    *
;* - initialize-instance reads attributes " width="380" height="140" *
;* - initialize-instance :after reads sub elements (buttons)         *
;*********************************************************************

(in-package :xml)

;; each MOP implementation appears to keep its symbols in a different, application specific package: how clever!!
;; probably not a good idea to just USE that entire package

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+:clozure-common-lisp 
  (import '(ccl:slot-definition-name ccl:slot-definition-type ccl:slot-definition-initform ccl:class-slots))
  #+:mcl
  (import '(ccl:slot-definition-name ccl:slot-definition-type ccl:slot-definition-initform))
  #+:clisp
  (import '(clos:class-slots clos:slot-definition-name
            clos:slot-definition-type clos:slot-definition-initform))
  #+(or CMU allegro)
  (import '(mop:class-slots mop:slot-definition-name mop:slot-definition-type
            mop:slot-definition-initform))
  #+:common-lispworks
  (import '(harlequin-common-lisp:class-slots harlequin-common-lisp:slot-definition-name
            harlequin-common-lisp:slot-definition-type harlequin-common-lisp:slot-definition-initform))
  #+:sbcl 
  (import '(sb-mop:class-slots sb-mop:slot-definition-name
                               sb-mop:slot-definition-type sb-mop:slot-definition-initform)))


(export '(;; CODEC
          decode-xml-string encode-xml-string
          ;; classes
          xml-serializer ![cdata[ !doctype !--
          ;; aggregation methods
          set-attribute-value add-subobject part-of add-object-to-slot cleanup-sub-object-slots
          ;; print control methods
          xml-printable-as-attribute-value-p xml-printable-as-subelement-p map-object print-slot-with-name-p
          print-slot-value-as-attribute print-slot-name-value-type-as-attribute print-slot-value-as-subelement print-slots
          print-typed-attribute-value print-typed-subelement-value
          print-default-value-attributes-p
          slot-name->attribute-name
          synoptic-xml-object-indentity-clues
          ;; reading
          load-object save-object finished-reading finished-reading-attributes read-typed-attribute-value
          read-return-value without-xml-reader
          attribute-name->slot-name
          is-created-by-xml-reader
          ;; instantiation
          duplicate
          ;; do not export "File"
	  file
          ;; variables
          def-element-class-name xml-tag-name-string
          ;; MOP
          class-slots find-slot-definition
          ;; MOP goodies
          most-specific-class
          ;; names
          xml-tag-name-symbol
          ;; variables
          *xmlisp-packages*
          *fallback-class-name-for-element-name-hook*
          *XMLisp-Print-Synoptic*
          ;; types
          path string-or-null integer-or-null
          ))


(defvar *XMLiSP-Element-Class-Names* (make-hash-table :test #'eq) "table mapping element names to class names")

(defvar *XMLisp-Print-Verbose* nil "Variable for printing debugging messages. If true, then the messages are printed. If nil, then the messages are not printed.")

(defvar *XMLisp-Print-Synoptic* nil "If non nil use an abreviated, but non readable, version of XML serialization")

(defparameter *Fallback-Class-Name-For-Element-Name-Hook* nil "Function to call to get element name if there is no class to match")

;*******************************
; Printing Utils               *
;*******************************

(defun FORMAT-IF-VERBOSE (Destination Control-String &rest Arguments)
  (when *XMLisp-Print-Verbose*
    (terpri)
    (apply #'format Destination Control-String Arguments)))

;***********************************
; low level: Character Predicates  *
;    elaborate thanks to unicode   *
;***********************************

(defun WHITE-SPACE-P (Char)
  (declare (optimize (speed 3) (safety 0)))
  (case (char-code Char)
    (#x20 t)
    (#x9 t)
    (#xD t)
    (#xA t)
    (t nil)))


(defun DIGITP (Char)
  (declare (optimize (speed 3) (safety 0)))
  (let ((Code (char-code Char)))
    (or (and (>= Code #x0030) (<= Code #x0039))
        (and (>= Code #x0660) (<= Code #x0669))
        (and (>= Code #x06F0) (<= Code #x06F9))
        (and (>= Code #x0966) (<= Code #x096F))
        (and (>= Code #x09E6) (<= Code #x09EF))
        (and (>= Code #x0A66) (<= Code #x0A6F))
        (and (>= Code #x0AE6) (<= Code #x0AEF))
        (and (>= Code #x0B66) (<= Code #x0B6F))
        (and (>= Code #x0BE7) (<= Code #x0BEF))
        (and (>= Code #x0C66) (<= Code #x0C6F))
        (and (>= Code #x0CE6) (<= Code #x0CEF))
        (and (>= Code #x0D66) (<= Code #x0D6F))
        (and (>= Code #x0E50) (<= Code #x0E59))
        (and (>= Code #x0ED0) (<= Code #x0ED9))
        (and (>= Code #x0F20) (<= Code #x0F29)))))


(defun COMBINING-CHAR-P (Char)
  (declare (optimize (speed 3) (safety 0)))
  (let ((Code (char-code Char)))
    (or (and (>= Code #x0300) (<= Code #x0345))
        (and (>= Code #x0360) (<= Code #x0361))
        (and (>= Code #x0483) (<= Code #x0486))
        (and (>= Code #x0591) (<= Code #x05A1))
        (and (>= Code #x05A3) (<= Code #x05B9))
        (and (>= Code #x05BB) (<= Code #x05BD))
        (= Code #x05BF)
        (and (>= Code #x05C1) (<= Code #x05C2))
        (= Code #x05C4)
        (and (>= Code #x064B) (<= Code #x0652))
        (= Code #x0670)
        (and (>= Code #x06D6) (<= Code #x06DC))
        (and (>= Code #x06DD) (<= Code #x06DF))
        (and (>= Code #x06E0) (<= Code #x06E4))
        (and (>= Code #x06E7) (<= Code #x06E8))
        (and (>= Code #x06EA) (<= Code #x06ED))
        (and (>= Code #x0901) (<= Code #x0903))
        (= Code #x093C)
        (and (>= Code #x093E) (<= Code #x094C))
        (= Code #x094D)
        (and (>= Code #x0951) (<= Code #x0954))
        (and (>= Code #x0962) (<= Code #x0963))
        (and (>= Code #x0981) (<= Code #x0983))
        (= Code #x09BC)
        (= Code #x09BE)
        (= Code #x09BF)
        (and (>= Code #x09C0) (<= Code #x09C4))
        (and (>= Code #x09C7) (<= Code #x09C8))
        (and (>= Code #x09CB) (<= Code #x09CD))
        (= Code #x09D7)
        (and (>= Code #x09E2) (<= Code #x09E3))
        (= Code #x0A02)
        (= Code #x0A3C)
        (= Code #x0A3E)
        (= Code #x0A3F)
        (and (>= Code #x0A40) (<= Code #x0A42))
        (and (>= Code #x0A47) (<= Code #x0A48))
        (and (>= Code #x0A4B) (<= Code #x0A4D))
        (and (>= Code #x0A70) (<= Code #x0A71))
        (and (>= Code #x0A81) (<= Code #x0A83))
        (= Code #x0ABC)
        (and (>= Code #x0ABE) (<= Code #x0AC5))
        (and (>= Code #x0AC7) (<= Code #x0AC9))
        (and (>= Code #x0ACB) (<= Code #x0ACD))
        (and (>= Code #x0B01) (<= Code #x0B03))
        (= Code #x0B3C)
        (and (>= Code #x0B3E) (<= Code #x0B43))
        (and (>= Code #x0B47) (<= Code #x0B48))
        (and (>= Code #x0B4B) (<= Code #x0B4D))
        (and (>= Code #x0B56) (<= Code #x0B57))
        (and (>= Code #x0B82) (<= Code #x0B83))
        (and (>= Code #x0BBE) (<= Code #x0BC2))
        (and (>= Code #x0BC6) (<= Code #x0BC8))
        (and (>= Code #x0BCA) (<= Code #x0BCD))
        (= Code #x0BD7)
        (and (>= Code #x0C01) (<= Code #x0C03))
        (and (>= Code #x0C3E) (<= Code #x0C44))
        (and (>= Code #x0C46) (<= Code #x0C48))
        (and (>= Code #x0C4A) (<= Code #x0C4D))
        (and (>= Code #x0C55) (<= Code #x0C56))
        (and (>= Code #x0C82) (<= Code #x0C83))
        (and (>= Code #x0CBE) (<= Code #x0CC4))
        (and (>= Code #x0CC6) (<= Code #x0CC8))
        (and (>= Code #x0CCA) (<= Code #x0CCD))
        (and (>= Code #x0CD5) (<= Code #x0CD6))
        (and (>= Code #x0D02) (<= Code #x0D03))
        (and (>= Code #x0D3E) (<= Code #x0D43))
        (and (>= Code #x0D46) (<= Code #x0D48))
        (and (>= Code #x0D4A) (<= Code #x0D4D))
        (= Code #x0D57)
        (= Code #x0E31)
        (and (>= Code #x0E34) (<= Code #x0E3A))
        (and (>= Code #x0E47) (<= Code #x0E4E))
        (= Code #x0EB1)
        (and (>= Code #x0EB4) (<= Code #x0EB9))
        (and (>= Code #x0EBB) (<= Code #x0EBC))
        (and (>= Code #x0EC8) (<= Code #x0ECD))
        (and (>= Code #x0F18) (<= Code #x0F19))
        (= Code #x0F35)
        (= Code #x0F37)
        (= Code #x0F39)
        (= Code #x0F3E)
        (= Code #x0F3F)
        (and (>= Code #x0F71) (<= Code #x0F84))
        (and (>= Code #x0F86) (<= Code #x0F8B))
        (and (>= Code #x0F90) (<= Code #x0F95))
        (= Code #x0F97)
        (and (>= Code #x0F99) (<= Code #x0FAD))
        (and (>= Code #x0FB1) (<= Code #x0FB7))
        (= Code #x0FB9)
        (and (>= Code #x20D0) (<= Code #x20DC))
        (= Code #x20E1)
        (and (>= Code #x302A) (<= Code #x302F))
        (= Code #x3099)
        (= Code #x309A))))


(defun BASE-CHAR-P (Char)
  (declare (optimize (speed 3) (safety 0)))
  (let ((Code (char-code Char)))
    (or (and (>= Code #x0041) (<= Code #x005A))
        (and (>= Code #x0061) (<= Code #x007A))
        (and (>= Code #x00C0) (<= Code #x00D6))
        (and (>= Code #x00D8) (<= Code #x00F6))
        (and (>= Code #x00F8) (<= Code #x00FF))
        (and (>= Code #x0100) (<= Code #x0131))
        (and (>= Code #x0134) (<= Code #x013E))
        (and (>= Code #x0141) (<= Code #x0148))
        (and (>= Code #x014A) (<= Code #x017E))
        (and (>= Code #x0180) (<= Code #x01C3))
        (and (>= Code #x01CD) (<= Code #x01F0))
        (and (>= Code #x01F4) (<= Code #x01F5))
        (and (>= Code #x01FA) (<= Code #x0217))
        (and (>= Code #x0250) (<= Code #x02A8))
        (and (>= Code #x02BB) (<= Code #x02C1))
        (= Code #x0386)
        (and (>= Code #x0388) (<= Code #x038A))
        (= Code #x038C)
        (and (>= Code #x038E) (<= Code #x03A1))
        (and (>= Code #x03A3) (<= Code #x03CE))
        (and (>= Code #x03D0) (<= Code #x03D6))
        (= Code #x03DA)
        (= Code #x03DC)
        (= Code #x03DE)
        (= Code #x03E0)
        (and (>= Code #x03E2) (<= Code #x03F3))
        (and (>= Code #x0401) (<= Code #x040C))
        (and (>= Code #x040E) (<= Code #x044F))
        (and (>= Code #x0451) (<= Code #x045C))
        (and (>= Code #x045E) (<= Code #x0481))
        (and (>= Code #x0490) (<= Code #x04C4))
        (and (>= Code #x04C7) (<= Code #x04C8))
        (and (>= Code #x04CB) (<= Code #x04CC))
        (and (>= Code #x04D0) (<= Code #x04EB))
        (and (>= Code #x04EE) (<= Code #x04F5))
        (and (>= Code #x04F8) (<= Code #x04F9))
        (and (>= Code #x0531) (<= Code #x0556))
        (= Code #x0559)
        (and (>= Code #x0561) (<= Code #x0586))
        (and (>= Code #x05D0) (<= Code #x05EA))
        (and (>= Code #x05F0) (<= Code #x05F2))
        (and (>= Code #x0621) (<= Code #x063A))
        (and (>= Code #x0641) (<= Code #x064A))
        (and (>= Code #x0671) (<= Code #x06B7))
        (and (>= Code #x06BA) (<= Code #x06BE))
        (and (>= Code #x06C0) (<= Code #x06CE))
        (and (>= Code #x06D0) (<= Code #x06D3))
        (= Code #x06D5)
        (and (>= Code #x06E5) (<= Code #x06E6))
        (and (>= Code #x0905) (<= Code #x0939))
        (= Code #x093D)
        (and (>= Code #x0958) (<= Code #x0961))
        (and (>= Code #x0985) (<= Code #x098C))
        (and (>= Code #x098F) (<= Code #x0990))
        (and (>= Code #x0993) (<= Code #x09A8))
        (and (>= Code #x09AA) (<= Code #x09B0))
        (= Code #x09B2)
        (and (>= Code #x09B6) (<= Code #x09B9))
        (and (>= Code #x09DC) (<= Code #x09DD))
        (and (>= Code #x09DF) (<= Code #x09E1))
        (and (>= Code #x09F0) (<= Code #x09F1))
        (and (>= Code #x0A05) (<= Code #x0A0A))
        (and (>= Code #x0A0F) (<= Code #x0A10))
        (and (>= Code #x0A13) (<= Code #x0A28))
        (and (>= Code #x0A2A) (<= Code #x0A30))
        (and (>= Code #x0A32) (<= Code #x0A33))
        (and (>= Code #x0A35) (<= Code #x0A36))
        (and (>= Code #x0A38) (<= Code #x0A39))
        (and (>= Code #x0A59) (<= Code #x0A5C))
        (= Code #x0A5E)
        (and (>= Code #x0A72) (<= Code #x0A74))
        (and (>= Code #x0A85) (<= Code #x0A8B))
        (= Code #x0A8D)
        (and (>= Code #x0A8F) (<= Code #x0A91))
        (and (>= Code #x0A93) (<= Code #x0AA8))
        (and (>= Code #x0AAA) (<= Code #x0AB0))
        (and (>= Code #x0AB2) (<= Code #x0AB3))
        (and (>= Code #x0AB5) (<= Code #x0AB9))
        (= Code #x0ABD)
        (= Code #x0AE0)
        (and (>= Code #x0B05) (<= Code #x0B0C))
        (and (>= Code #x0B0F) (<= Code #x0B10))
        (and (>= Code #x0B13) (<= Code #x0B28))
        (and (>= Code #x0B2A) (<= Code #x0B30))
        (and (>= Code #x0B32) (<= Code #x0B33))
        (and (>= Code #x0B36) (<= Code #x0B39))
        (= Code #x0B3D)
        (and (>= Code #x0B5C) (<= Code #x0B5D))
        (and (>= Code #x0B5F) (<= Code #x0B61))
        (and (>= Code #x0B85) (<= Code #x0B8A))
        (and (>= Code #x0B8E) (<= Code #x0B90))
        (and (>= Code #x0B92) (<= Code #x0B95))
        (and (>= Code #x0B99) (<= Code #x0B9A))
        (= Code #x0B9C)
        (and (>= Code #x0B9E) (<= Code #x0B9F))
        (and (>= Code #x0BA3) (<= Code #x0BA4))
        (and (>= Code #x0BA8) (<= Code #x0BAA))
        (and (>= Code #x0BAE) (<= Code #x0BB5))
        (and (>= Code #x0BB7) (<= Code #x0BB9))
        (and (>= Code #x0C05) (<= Code #x0C0C))
        (and (>= Code #x0C0E) (<= Code #x0C10))
        (and (>= Code #x0C12) (<= Code #x0C28))
        (and (>= Code #x0C2A) (<= Code #x0C33))
        (and (>= Code #x0C35) (<= Code #x0C39))
        (and (>= Code #x0C60) (<= Code #x0C61))
        (and (>= Code #x0C85) (<= Code #x0C8C))
        (and (>= Code #x0C8E) (<= Code #x0C90))
        (and (>= Code #x0C92) (<= Code #x0CA8))
        (and (>= Code #x0CAA) (<= Code #x0CB3))
        (and (>= Code #x0CB5) (<= Code #x0CB9))
        (= Code #x0CDE)
        (and (>= Code #x0CE0) (<= Code #x0CE1))
        (and (>= Code #x0D05) (<= Code #x0D0C))
        (and (>= Code #x0D0E) (<= Code #x0D10))
        (and (>= Code #x0D12) (<= Code #x0D28))
        (and (>= Code #x0D2A) (<= Code #x0D39))
        (and (>= Code #x0D60) (<= Code #x0D61))
        (and (>= Code #x0E01) (<= Code #x0E2E))
        (= Code #x0E30)
        (and (>= Code #x0E32) (<= Code #x0E33))
        (and (>= Code #x0E40) (<= Code #x0E45))
        (and (>= Code #x0E81) (<= Code #x0E82))
        (= Code #x0E84)
        (and (>= Code #x0E87) (<= Code #x0E88))
        (= Code #x0E8A)
        (= Code #x0E8D)
        (and (>= Code #x0E94) (<= Code #x0E97))
        (and (>= Code #x0E99) (<= Code #x0E9F))
        (and (>= Code #x0EA1) (<= Code #x0EA3))
        (= Code #x0EA5)
        (= Code #x0EA7)
        (and (>= Code #x0EAA) (<= Code #x0EAB))
        (and (>= Code #x0EAD) (<= Code #x0EAE))
        (= Code #x0EB0)
        (and (>= Code #x0EB2) (<= Code #x0EB3))
        (= Code #x0EBD)
        (and (>= Code #x0EC0) (<= Code #x0EC4))
        (and (>= Code #x0F40) (<= Code #x0F47))
        (and (>= Code #x0F49) (<= Code #x0F69))
        (and (>= Code #x10A0) (<= Code #x10C5))
        (and (>= Code #x10D0) (<= Code #x10F6))
        (= Code #x1100)
        (and (>= Code #x1102) (<= Code #x1103))
        (and (>= Code #x1105) (<= Code #x1107))
        (= Code #x1109)
        (and (>= Code #x110B) (<= Code #x110C))
        (and (>= Code #x110E) (<= Code #x1112))
        (= Code #x113C)
        (= Code #x113E)
        (= Code #x1140)
        (= Code #x114C)
        (= Code #x114E)
        (= Code #x1150)
        (and (>= Code #x1154) (<= Code #x1155))
        (= Code #x1159)
        (and (>= Code #x115F) (<= Code #x1161))
        (= Code #x1163)
        (= Code #x1165)
        (= Code #x1167)
        (= Code #x1169)
        (and (>= Code #x116D) (<= Code #x116E))
        (and (>= Code #x1172) (<= Code #x1173))
        (= Code #x1175)
        (= Code #x119E)
        (= Code #x11A8)
        (= Code #x11AB)
        (and (>= Code #x11AE) (<= Code #x11AF))
        (and (>= Code #x11B7) (<= Code #x11B8))
        (= Code #x11BA)
        (and (>= Code #x11BC) (<= Code #x11C2))
        (= Code #x11EB)
        (= Code #x11F0)
        (= Code #x11F9)
        (and (>= Code #x1E00) (<= Code #x1E9B))
        (and (>= Code #x1EA0) (<= Code #x1EF9))
        (and (>= Code #x1F00) (<= Code #x1F15))
        (and (>= Code #x1F18) (<= Code #x1F1D))
        (and (>= Code #x1F20) (<= Code #x1F45))
        (and (>= Code #x1F48) (<= Code #x1F4D))
        (and (>= Code #x1F50) (<= Code #x1F57))
        (= Code #x1F59)
        (= Code #x1F5B)
        (= Code #x1F5D)
        (and (>= Code #x1F5F) (<= Code #x1F7D))
        (and (>= Code #x1F80) (<= Code #x1FB4))
        (and (>= Code #x1FB6) (<= Code #x1FBC))
        (= Code #x1FBE)
        (and (>= Code #x1FC2) (<= Code #x1FC4))
        (and (>= Code #x1FC6) (<= Code #x1FCC))
        (and (>= Code #x1FD0) (<= Code #x1FD3))
        (and (>= Code #x1FD6) (<= Code #x1FDB))
        (and (>= Code #x1FE0) (<= Code #x1FEC))
        (and (>= Code #x1FF2) (<= Code #x1FF4))
        (and (>= Code #x1FF6) (<= Code #x1FFC))
        (= Code #x2126)
        (and (>= Code #x212A) (<= Code #x212B))
        (= Code #x212E)
        (and (>= Code #x2180) (<= Code #x2182))
        (and (>= Code #x3041) (<= Code #x3094))
        (and (>= Code #x30A1) (<= Code #x30FA))
        (and (>= Code #x3105) (<= Code #x312C))
        (and (>= Code #xAC00) (<= Code #xD7A3)))))


(defun EXTENDERP (Char)
  (declare (optimize (speed 3) (safety 0)))
  (let ((Code (char-code Char)))
    (or (= Code #x00B7)
        (= Code #x02D0)
        (= Code #x02D1)
        (= Code #x0387)
        (= Code #x0640)
        (= Code #x0E46)
        (= Code #x0EC6)
        (= Code #x3005) 
        (and (>= Code #x3031) (<= Code #x3035))
        (and (>= Code #x309D) (<= Code #x309E))
        (and (>= Code #x30FC) (<= Code #x30FE)))))

(defun IDEOGRAPHICP (Char)
  (declare (optimize (speed 3) (safety 0)))
  (let ((Code (char-code Char)))
    (or (= Code #x3007)
        (and (>= Code #x4E00) (<= Code #x9FA5))
        (and (>= Code #x3021) (<= Code #x3029)))))


(defun LETTERP (Char)
  (declare (optimize (speed 3) (safety 0)))
  (or (base-char-p Char)
      (ideographicp Char)))


(defun NAMECHARP (Char)
  (declare (optimize (speed 3) (safety 0)))
  (or (letterp Char)
      (digitp Char)
      (char= Char #\.)
      (char= Char #\-)
      (char= Char #\_)
      (char= Char #\:)
      (combining-char-p Char)
      (extenderp Char)))

;*******************************
; File Methods                 *
;*******************************

(defmethod PATHNAME-FROM-STREAM ((Stream stream))
  ;; in the most general case we just don't know what the file is
  nil)


(defmethod PATHNAME-FROM-STREAM ((Stream file-stream))
  (truename (parse-namestring Stream)))


;*******************************
; XML Serializer class         *
;*******************************

(defclass XML-SERIALIZER ()
  ((content :accessor content :initarg :content :initform nil :documentation "content not wrapped up as tag or attribute, e.g. the link name of <a> tag")
   (is-created-by-xml-reader :accessor is-created-by-xml-reader :initform t :initarg :is-created-by-xml-reader :type boolean :documentation "true if this instance was create by the xml reader: important for initialization")
   (file :accessor file :initform nil :initarg :file :documentation "Pathname of file containing XML expression, or nil if that cannot be determined"))
  (:default-initargs :is-created-by-xml-reader nil)
  (:documentation "Mixin to serialize objects as XML looking things"))


(defgeneric DUPLICATE (Xml-Serializer &optional Package)
  (:documentation "Copy agent through XML serialization/deserialization: only print slots will be copied."))


(defgeneric SAVE-OBJECT (Xml-Serializer Filename &key Verbose If-Exists Xml-Header)
  (:documentation "Save object into <Filename>. By default add a valid XML header"))


(defgeneric SET-ATTRIBUTE-VALUE (Xml-Serializer Attribute-Name Value)
  (:documentation "Set the value of an attribute. Default: find slot matching <attribute-name> and set its value to <value>"))


(defgeneric ADD-SUBOBJECT (Xml-Serializer Subobject)
  (:documentation "Add a subobject. Default: If subobject is of type bla and there is a slot called bla assign it to that slot. If subobject is of type bla and there is a slot called blas then add bla as element of a list to slot blas."))


(defgeneric (SETF PART-OF) (Container Xml-Serializer)
  (:documentation "Called after I got added as subobject to container. Add a \"part-of\" to capture this link if needed"))


(defgeneric PART-OF (Xml-Serializer)
  (:documentation "The object containing me."))


(defgeneric ADD-OBJECT-TO-SLOT (Xml-Serializer Object Slot-Name)
  (:documentation "Add object to slot <slot-name>. Default: nconc object to end of list, not good for large lists but preserves reading order."))


(defgeneric FIND-SLOT-DEFINITION (Xml-Serializer Name)
  (:documentation "Return slot defnition matching <Name>"))


(defgeneric CLEANUP-SUB-OBJECT-SLOTS (Xml-Serializer Slot-Names)
  (:documentation "Called after all the sub objects have been added"))


(defgeneric XML-PRINTABLE-AS-SUBELEMENT-P (Xml-Serializer)
  (:documentation "True if printable as subelement <bla .../>"))

(defgeneric XML-PRINTABLE-AS-ATTRIBUTE-VALUE-P (Xml-Serializer)
  (:documentation "True if printable as attribute value bla=\"???\""))


(defgeneric PRINT-TYPED-ATTRIBUTE-VALUE (Value Type Stream)
  (:documentation "Encode attribute <value> into an external XML compliant represetation and print into <stream>"))


(defgeneric READ-TYPED-ATTRIBUTE-VALUE (Value Type)
  (:documentation "Return decoded XML <value> of <type>. "))


(defgeneric PRINT-TYPED-SUBELEMENT-VALUE (Value Type Stream)
  (:documentation "Encode attribute <value> into an external XML compliant represetation and print into <stream>"))


(defgeneric MAP-OBJECT (Collection Function)
   (:documentation "If <collection> is a structured object such as a string, list or array call <function> with each element"))


(defgeneric PRINT-SLOTS (Xml-Serializer)
  (:documentation "List of slot names to be printed. Return nil to print no slots, :all to print all. Slots will still be excluded when print-slot-with-name-p returns nil"))


(defgeneric PRINT-SLOT-WITH-NAME-P (Xml-Serializer Name)
   (:documentation "Return true if slot with <name> should be printed. Default: t. Typical use: avoid recursion"))


(defgeneric PRINT-SUBELEMENTS-TO-STREAM-P (Xml-Serializer Stream)
  (:documentation "If true then sub elements, if there are any, will be printed into stream"))


(defgeneric PRINT-SLOT-NAME-VALUE-TYPE-AS-ATTRIBUTE (Xml-Serializer Name Value Type Stream)
  (:documentation "Print ''<Name>=<Value>'' into <Stream>. <Type> can be used for encoding"))
 

(defgeneric PRINT-SLOT-VALUE-AS-ATTRIBUTE (Xml-Serializer Slot Value)
  (:documentation "Print <slot> as attribute of <Value>"))


(defgeneric PRINT-DEFAULT-VALUE-ATTRIBUTES-P (Xml-Serializer)
  (:documentation "If true print attributes that have same value as :initform. Good idea for large sets with highly redundant information. Bad idea if value if :initform changes later"))


(defgeneric SYNOPTIC-XML-OBJECT-INDENTITY-CLUES (Xml-Serializer)
  (:documentation "String that could reveal the identity of an XML object"))


(defgeneric FINISHED-READING (Xml-Serializer Stream)
  (:documentation "called when done with reading: all attributes and sub elements have been created"))


(defgeneric FINISHED-READING-ATTRIBUTES (Xml-Serializer Stream)
  (:documentation "called when done with reading attributes: sub elements have NOT been created"))


(defgeneric READ-RETURN-VALUE (Xml-Serializer)
  (:documentation "The value returned from reading an xml element. Usually the element itself. This method is called after reading is completely finished."))


(defgeneric XML-TAG-NAME-STRING (Xml-Serializer)
  (:documentation "return the tag name of element. Default to name only - no package prefix."))

;____________________________
;  Attribute & Slot Names    |
;____________________________

(defgeneric ATTRIBUTE-NAME->SLOT-NAME (Xml-Serializer Attribute-Name)
  (:documentation "an attribute name will be mapped to this slot name. Default to identity"))


(defgeneric SLOT-NAME->ATTRIBUTE-NAME (Xml-Serializer Slot-Name)
  (:documentation "a slot name will be mapped to this attribute name. Default to identity"))

;____________________________
;  Element & Class Names     |
;____________________________

(defmacro DEF-ELEMENT-CLASS-NAME (Element-Name Class-Name)
  `(setf (gethash ',Element-Name *XMLISP-Element-Class-Names*) ',Class-Name))


(defun ELEMENT-CLASS-NAME (Element-Name) "
  in:  Element-Name symbol.
  out: Class-Name symbol.
  Return the class name."
 (gethash Element-Name *XMLISP-Element-Class-Names*))


(defun CLASS-ELEMENT-NAME (Class-Name) "
  in:  Class-Name symbol.
  out: Element-Name symbol.
  Return the element-name matching <class-name>."
 (maphash
  #'(lambda (Key Value)
      (when (eq Class-Name Value) (return-from class-element-name Key)))
  *XMLISP-Element-Class-Names*))


;____________________________
;  default implementations   |
;____________________________

(defmethod DUPLICATE ((Self xml-serializer) &optional (Package *Package*))
  ;; a relatively slow way of doing it
  ;; also this depends on *Package*
  (let ((*Package* Package))
    (read-from-string (write-to-string Self))))


;; names and print names

(defmethod XML-TAG-NAME-SYMBOL ((Self xml-serializer))
  (or (class-element-name (type-of Self))
      (type-of Self)))


(defmethod XML-TAG-NAME-STRING ((Self xml-serializer))
  (string-downcase (symbol-name (xml-tag-name-symbol Self))))


;; map objects into their components

(defmethod MAP-OBJECT ((Self xml-serializer) Function)
   (funcall Function Self))

(defmethod MAP-OBJECT ((Self sequence) Function)
   (map nil Function Self))

(defmethod MAP-OBJECT ((Self hash-table) Function)
  (maphash #'(lambda (Key Value) (declare (ignore Key)) (funcall Function Value)) Self))

(defmethod MAP-OBJECT ((Self number) Function)
   (funcall Function Self))

(defmethod MAP-OBJECT ((Self string) Function)
   (funcall Function Self))

(defmethod MAP-OBJECT ((Self symbol) Function)
   (funcall Function Self))

(defmethod MAP-OBJECT ((Self array) Function)
   (let* ((Size (array-total-size Self))
          (Vector (make-array Size :element-type (array-element-type Self) :displaced-to Self)))
     (dotimes (I Size)
       (let ((Element (aref Vector I)))
         (when (xml-printable-as-subelement-p Element)
           (map-object Element Function))))))


;; print which slots and what kinds of values?

(defmethod PRINT-SLOTS ((Self xml-serializer))
  :all)

(defmethod PRINT-SLOT-WITH-NAME-P ((Self xml-serializer) Name)
   (case Name
     (content nil)
     (t t)))


(defmethod XML-PRINTABLE-AS-SUBELEMENT-P ((Self t))    nil) ;; most general case => NO

(defmethod XML-PRINTABLE-AS-SUBELEMENT-P ((Self null))    nil)

(defmethod XML-PRINTABLE-AS-SUBELEMENT-P ((Self xml-serializer))    t)

(defmethod XML-PRINTABLE-AS-SUBELEMENT-P ((Self string))    nil)

(defmethod XML-PRINTABLE-AS-SUBELEMENT-P ((Self sequence))    t)

(defmethod XML-PRINTABLE-AS-SUBELEMENT-P ((Self array)) 
  ;; number arrays should not be printed as subelements
  (not (and (array-element-type Self) (subtypep (array-element-type Self) 'number))))

(defmethod XML-PRINTABLE-AS-SUBELEMENT-P ((Self hash-table)) t)

(defmethod XML-PRINTABLE-AS-SUBELEMENT-P ((Self list))
   (every #'xml-printable-as-subelement-p Self))


(defmethod XML-PRINTABLE-AS-ATTRIBUTE-VALUE-P ((Self t))    nil) ;; most general case => NO

(defmethod XML-PRINTABLE-AS-ATTRIBUTE-VALUE-P ((Self string))    t)

(defmethod XML-PRINTABLE-AS-ATTRIBUTE-VALUE-P ((Self number))    t)

(defmethod XML-PRINTABLE-AS-ATTRIBUTE-VALUE-P ((Self character))  t)

(defmethod XML-PRINTABLE-AS-ATTRIBUTE-VALUE-P ((Self symbol))    t)

(defmethod XML-PRINTABLE-AS-ATTRIBUTE-VALUE-P ((Self list))    t)

(defmethod XML-PRINTABLE-AS-ATTRIBUTE-VALUE-P ((Self pathname))    t)

(defmethod XML-PRINTABLE-AS-ATTRIBUTE-VALUE-P ((Self array))
  ;; number arrays can be printed 
  (and (array-element-type Self) (subtypep (array-element-type Self) 'number)))

(defmethod PRINT-SUBELEMENTS-TO-STREAM-P ((Self xml-serializer) Stream)
  (declare (ignore Stream))
  t)


(defmethod PRINT-DEFAULT-VALUE-ATTRIBUTES-P ((Self xml-serializer))
  ;; lean towards sparse representations
  nil) 

;; finished reading

(defmethod FINISHED-READING ((Self xml-serializer) Stream)
  ;; do nothing
  (declare (ignore Stream))
  )


(defmethod FINISHED-READING-ATTRIBUTES ((Self xml-serializer) Stream)
  ;; do nothing
  (declare (ignore Stream))
  )


(defmethod READ-RETURN-VALUE ((Self xml-serializer))
  Self)

;______________________________
; compilation and load forms   |
;______________________________

(defmethod MAKE-LOAD-FORM ((Self xml-serializer) &optional Environment)
  ;; if we want to compile files containing XML expression we better make some load forms
  (make-load-form-saving-slots Self :environment Environment))

;*******************************************
;* User level Error handling               *
;*******************************************

#+(or (not :mcl) :openmcl)
(defun SHOW-ERROR-IN-STREAM-TO-USER (Stream)
  ;; No generic Common Lisp solution
  (declare (ignore Stream))
  )

#+(and :mcl (not :openmcl))
(defun SHOW-ERROR-IN-STREAM-TO-USER (Stream)
  ;; YEAH, real luxury: Open up stream if it is a file in Fred editor and move cursor to problem location
  (when (slot-exists-p Stream 'ccl::fblock) 
    (format t ";; attempting to open file containing error. Error Position: ~A..." (ccl::%fpos (slot-value Stream 'ccl::fblock)))
    ;; Open file in FRED and set cursor to location, scroll if necessary
    (let ((Fred (ed (parse-namestring Stream))))
      (ccl:set-mark (ccl:fred-buffer Fred) (ccl::%fpos (slot-value Stream 'ccl::fblock)))
      (ccl:window-show-cursor Fred)
      (ccl:fred-update Fred))))


;********************************************
;*  Typed Attribute Value CODECs            *
;*    print encoded value into XML stream   *
;*    read decoded XML into internal format *
;********************************************

;_______________________________________
; default printer/reader:               |
;_______________________________________

(defun TYPE-SPECIFIER-LIST-P (Type-Specifier)
  "true if Type-Specifier is a type specifier list"
  (and (listp Type-Specifier)
       (symbolp (first Type-Specifier))))


(defun PRINT-SPECIFIER-LIST-TYPED-ATTRIBUTE-VALUE (Value Type-Specifier-List Stream)
  ;; try to handle type specifier lists (CLTL 4.2)
  ;; type specifier lists can be the result of subclasses adding types to existing slot types (and type1 type2 ...)
  ;; or types that are not supported as symbols in all CL implementations, e.g., boolean
  (cond
   ((subtypep Type-Specifier-List '(member t nil))
    (print-typed-attribute-value Value 'boolean Stream))
   (t 
    (error "no print attribute method for type: ~A" Type-Specifier-List))))


(defmethod PRINT-TYPED-ATTRIBUTE-VALUE (Value (Type t) Stream)
  (if (type-specifier-list-p Type)
    (print-specifier-list-typed-attribute-value Value Type Stream)
    (format
     Stream
     "\"~A\""
     (etypecase Value
       (string (encode-xml-string Value))
       (number Value)
       (symbol Value)
       (list Value)
       (pathname (namestring Value))))))


(defvar *Warn-if-undefined-XML-Decoder-Type* nil "set to t to get warnings")


(defun READ-SPECIFIER-LIST-TYPED-ATTRIBUTE-VALUE (Value Type-Specifier-List)
  ;; try to handle type specifier lists (CLTL 4.2)
  ;; type specifier lists can be the result of subclasses adding types to existing slot types (and type1 type2 ...)
  ;; or types that are not supported as symbols in all CL implementations, e.g., boolean
  (cond
   ((subtypep Type-Specifier-List '(member t nil))
    (read-typed-attribute-value Value 'boolean))
   (t 
    (error "no read attribute method for type: ~A" Type-Specifier-List))))
  

(defmethod READ-TYPED-ATTRIBUTE-VALUE ((Value t) (Type t))
  (cond
   ((type-specifier-list-p Type)
    (read-specifier-list-typed-attribute-value Value Type))
   (t
    (when *Warn-If-Undefined-Xml-Decoder-Type*
      (warn "no XML decoder for value \"~A\" of type \"~A\"" Value Type))
    Value)))

;_____________________________________
; types and CODECs                    |
;_____________________________________

;; SYMBOL

(defmethod PRINT-TYPED-ATTRIBUTE-VALUE (Value (Type (eql 'symbol)) Stream)
  (format Stream "\"~A\"" (symbol-name Value)))

(defmethod READ-TYPED-ATTRIBUTE-VALUE ((Value string) (Type (eql 'symbol)))
  (intern (string-upcase Value)))

;; KEYWORD

(defmethod PRINT-TYPED-ATTRIBUTE-VALUE (Value (Type (eql 'keyword)) Stream)
  (format Stream "\"~A\"" (symbol-name Value)))

(defmethod READ-TYPED-ATTRIBUTE-VALUE ((Value string) (Type (eql 'keyword)))
  (intern (string-upcase Value) (find-package :keyword)))


;; STRING

(defmethod PRINT-TYPED-ATTRIBUTE-VALUE (Value (Type (eql 'string)) Stream)
  (format Stream "\"~A\"" (encode-xml-string Value)))

(defmethod READ-TYPED-ATTRIBUTE-VALUE ((Value string) (Type (eql 'string)))
  ;; !!! should probably decode the string????
  Value)


;; STRING-OR-NULL  This is basically the same as not having a type

(deftype STRING-OR-NULL () "string or null" '(or string null))

(defmethod PRINT-TYPED-ATTRIBUTE-VALUE (Value (Type (eql 'string-or-null)) Stream)
  (format Stream "\"~A\"" (encode-xml-string Value)))

(defmethod READ-TYPED-ATTRIBUTE-VALUE ((Value string) (Type (eql 'string-or-null)))
  ;; !!! should probably decode the string????
  Value)


;; CHARACTER

(defmethod PRINT-TYPED-ATTRIBUTE-VALUE (Value (Type (eql 'character)) Stream)
  (prin1 (encode-xml-string (string Value)) Stream))

(defmethod READ-TYPED-ATTRIBUTE-VALUE ((Value string) (Type (eql 'character)))
  (char Value 0))

;; INTEGER

(defmethod PRINT-TYPED-ATTRIBUTE-VALUE (Value (Type (eql 'integer)) Stream)
  (format Stream "\"~A\"" Value))

(defmethod READ-TYPED-ATTRIBUTE-VALUE ((Value string) (Type (eql 'integer)))
  (parse-integer Value))


;; INTEGER-OR-NULL

(deftype INTEGER-OR-NULL () "string or null" '(or integer null))

(defmethod PRINT-TYPED-ATTRIBUTE-VALUE (Value (Type (eql 'integer-or-null)) Stream)
  (format Stream "\"~A\"" Value))

(defmethod READ-TYPED-ATTRIBUTE-VALUE ((Value string) (Type (eql 'integer-or-null)))
  (parse-integer Value))


;; NUMBER

(defmethod PRINT-TYPED-ATTRIBUTE-VALUE (Value (Type (eql 'number)) Stream)
  (format Stream "\"~A\"" Value))

(defmethod READ-TYPED-ATTRIBUTE-VALUE ((Value string) (Type (eql 'number)))
  (read-from-string Value))


;; BOOLEAN

(defmethod PRINT-TYPED-ATTRIBUTE-VALUE (Value (Type (eql 'boolean)) Stream)
  (prin1 (if Value "true" "false") Stream))

(defmethod READ-TYPED-ATTRIBUTE-VALUE ((Value string) (Type (eql 'boolean)))
  (if (string-equal Value "true") t nil))

;; FLOAT

(defmethod PRINT-TYPED-ATTRIBUTE-VALUE (Value (Type (eql 'float)) Stream)
  (format Stream "\"~A\"" (coerce Value 'float)))

(defmethod READ-TYPED-ATTRIBUTE-VALUE ((Value string) (Type (eql 'float)))
  (float (read-from-string Value)))


;; SHORT-FLOAT

(defmethod PRINT-TYPED-ATTRIBUTE-VALUE (Value (Type (eql 'short-float)) Stream)
  (format Stream "\"~A\"" (coerce Value 'short-float)))

(defmethod READ-TYPED-ATTRIBUTE-VALUE ((Value string) (Type (eql 'short-float)))
  (float (read-from-string Value) 0s0))

;; SINGLE-FLOAT

(defmethod PRINT-TYPED-ATTRIBUTE-VALUE (Value (Type (eql 'single-float)) Stream)
  (format Stream "\"~A\"" (coerce Value 'single-float)))

(defmethod READ-TYPED-ATTRIBUTE-VALUE ((Value string) (Type (eql 'single-float)))
  (float (read-from-string Value) 0s0))


;; DOUBLE-FLOAT

(defmethod PRINT-TYPED-ATTRIBUTE-VALUE (Value (Type (eql 'double-float)) Stream)
  (format Stream "\"~F\"" Value))

(defmethod READ-TYPED-ATTRIBUTE-VALUE ((Value string) (Type (eql 'double-float)))
  (float (read-from-string Value) 0d0))


;; PATHNAME

(defmethod PRINT-TYPED-ATTRIBUTE-VALUE (Value (Type (eql 'pathname)) Stream)
  (prin1 (convert-to-unix-pathname Value) Stream))

(defmethod READ-TYPED-ATTRIBUTE-VALUE ((Value string) (Type (eql 'pathname)))
  (convert-to-lisp-pathname Value))

;; PATH

(deftype PATH () "pathname or nil, externalized as unix style path" '(or pathname null))

(defmethod PRINT-TYPED-ATTRIBUTE-VALUE (Value (Type (eql 'path)) Stream)
  (prin1 (convert-to-unix-pathname Value) Stream))

(defmethod READ-TYPED-ATTRIBUTE-VALUE ((Value string) (Type (eql 'path)))
  (convert-to-lisp-pathname Value))


;; LIST

;; print as string is OK but internally keep as regular lisp list

(defmethod READ-TYPED-ATTRIBUTE-VALUE ((Value string) (Type (eql 'list)))
  (read-from-string Value))


;; ARRAY

(defmethod PRINT-TYPED-ATTRIBUTE-VALUE (Value (Type (eql 'array)) Stream)
  (unless (and (array-element-type Value) (subtypep (array-element-type Value) 'number))
    (error "don't know how to print ~A in XML" Value))
  (let ((Vector (make-array (array-total-size Value) :displaced-to Value :fill-pointer 0 :element-type 'short-float)))
    ;; store type and dimension
    (format Stream "\"float array ~A " (array-dimensions Value))
    ;; dump numbers as flat vector
    (dotimes (I (array-total-size Value))
      (princ (aref Vector i) Stream)
      (princ #\space Stream))
    ;; end
    (princ #\" Stream)))


(defmethod READ-TYPED-ATTRIBUTE-VALUE ((Value string) (Type (eql 'array)))
  (with-input-from-string (in Value)
    (let ((Type (read In)))
      (unless (equal Type 'float) (error "cannot special attribute type \"~A\"" Type))
      (read In) ;; ignore array keyword
      (let* ((Dimensions (read In))
             (Array (make-array Dimensions :element-type 'short-float))
             (Vector (make-array (array-total-size Array) :displaced-to Array :fill-pointer 0 :element-type 'short-float)))
        (loop
          (let ((Float (read In nil nil)))
            (unless Float (return))
            (vector-push Float Vector)))
        (unless (= (fill-pointer Vector) (array-total-size Array))
          (error "reading float array: expected to see ~A floats but found ~A" (array-total-size Array) (fill-pointer Vector)))
        Array))))
       

;********************************************
;*  Typed Subelement Value CODECs           *
;*    print encoded value into XML stream   *
;********************************************

;_______________________________________
; default printer: warn                |
;_______________________________________

(defmethod PRINT-TYPED-SUBELEMENT-VALUE ((Value t) (Type t) Stream)
  ;; (warn "no XML encoder for \"~A\" of type \"~A\"" Value Type)
  ;; do the same as with untyped subelements: map them
  (map-object
   Value
   #'(lambda (Object)
       (terpri Stream)
       (print-object Object Stream))))

;*************************************
;*  SGML-TAG    Class                *
;*************************************

(defclass SGML-TAG (xml-serializer)
   ()
   (:documentation "SGML Tag. No sub elements, e.g., <!-- Copyright (C) 2000-2003 - GISUser.com -->"))


(defmethod END-TAG-NAME-STRING ((Self sgml-tag))
   ">")


(defmethod READ-XMLISP-ATTRIBUTES ((Self sgml-tag) Stream)
  ;; No attributes
  Self)

(defmethod READ-XMLISP-ELEMENT ((Self sgml-tag) Stream)
   (setf (content Self) (read-until-token Stream (end-tag-name-string Self)))  ;; no decoding
   Self)


(defmethod PRINT-OBJECT ((Self sgml-tag) Stream)
  (print-xml-indent Stream)
  (format Stream "<~A ~A~A" (string-upcase (xml-tag-name-string Self)) (content Self) (end-tag-name-string Self)))  ;; no encoding


;*************************************
;*  ![CDATA[    Class                *
;*************************************

(defclass ![CDATA[ (sgml-tag)
  ()
  (:documentation "SGML uninterpreted content only class. Does not encode/decode strings"))


(defmethod END-TAG-NAME-STRING ((Self ![cdata[))
   "]]>")


;*************************************
;*  !DOCTYPE    Class                *
;*************************************

(defclass !DOCTYPE (sgml-tag)
  ()
  (:documentation "SGML metadata"))


;*************************************
;*  !--         Class                *
;*************************************

(defclass !-- (sgml-tag)
  ()
  (:documentation "SGML comment"))


(defmethod END-TAG-NAME-STRING ((Self !--))
   "-->")


(defmethod PRINT-OBJECT ((Self !--) Stream)
  (print-xml-indent Stream)
  ;; do NOT print a leading space before the content because this is a comment
  (format Stream "<~A~A~A" (string-upcase (xml-tag-name-string Self)) (content Self) (end-tag-name-string Self)))  ;; no encoding


;*************************************
;*  xml-content Class                *
;*************************************

(defclass XML-CONTENT (xml-serializer)
   ((name :accessor name :initform "untitled" :initarg :name :documentation "element tag name"))
   (:documentation "Content elements have ONLY content: they may not hold sub element or attribute-based content, e.g., <copyright>Copyright 2004, AgentSheets Inc.</copyright>"))


(defmethod XML-TAG-NAME-SYMBOL ((Self xml-content))
   (name Self))


(defmethod PRINT-SLOT-WITH-NAME-P ((Self xml-content) Name)
   (case Name
     (name nil)
     (t (call-next-method))))

;******************************
; MOP hacks                   *
;  for Lisps with missing MOP *
;  methods                    *
;******************************

;; this is the place where to put MOP hacks for different Lisp implementations


#+(and :mcl (not :ccl-5.1) (not :openmcl))  ;; MCL < 5.1 does not have this MOP function!
(defmethod CLASS-SLOTS ((Class standard-class))
  ;; Art of MOP: p. 214
  ;; pretty slow: don't use this if you don't have to
  (coerce (rest (slot-value Class 'ccl::slots)) 'list))


(defmethod FIND-SLOT-DEFINITION ((Self xml-serializer) Name)
  (find Name (class-slots (class-of Self)) :key #'slot-definition-name))


#+(and :mcl (not :openmcl) (not :ccl-5.1))  ;; the generic version would be very slow with MCL < 5.1
(defmethod FIND-SLOT-DEFINITION ((Self xml-serializer) Name)
  (declare (optimize))
  (let ((Slot-Definitions (rest (slot-value (class-of Self) 'ccl::slots))))
    (dotimes (I (length Slot-Definitions))
      (declare (fixnum i))
      (let ((Slot-Definition (svref Slot-Definitions i)))
        (when (eq (first Slot-Definition) Name) (return Slot-Definition))))))


(defun MOST-SPECIFIC-CLASS (Class) "
  in: Class symbol or list: (and <type1> ... <typen>)
  Return most specific class"
 (typecase Class
   (atom Class)
   ;; not clear if this is deterministic and same for all lisps
   (list (first (last Class)))))

;_____________________________
; Symbol functions            |
;_____________________________

(defun XMLISP-SYMBOL-NAME (Symbol)
  (string-downcase (symbol-name Symbol)))


(defun READTABLE-STRING (Name) "
  in:  Name string.
  out: Readtable-string string
  Convert name string into symbol according to *Readtable*. Name cannot contain ':' "
 (ecase (readtable-case *Readtable*)
   (:upcase (string-upcase Name))
   (:downcase (string-downcase Name))
   (:preserve Name)
   (:invert
    (cond
     ((every #'upper-case-p Name) (string-downcase Name))
     ((every #'lower-case-p Name) (string-upcase Name))
     (t Name)))))


(defun MAKE-XMLISP-SYMBOL (Name) "
  in:  Name string.
  out: Symbol symbol.
  Turn <Name> into <Symbol> taking into account the current readtable's case."
  (let ((Colon-Position (position #\: Name)))
    (if Colon-Position
      (intern 
       (readtable-string (subseq Name (1+ Colon-Position)))
       (or
        ;; read-from-string does the readtable stuff
        ;; slow but this is not used all that often
        (find-package (intern (readtable-string (subseq Name 0 Colon-Position))))
        (error "trying to read XML name \"~A\" but contains reference to non existing package." Name)))
      (intern (readtable-string Name)))))

;_____________________________
; Pathname conversion         |
;_____________________________

(defun DISK-NAME ()
  (second (pathname-directory (truename "home:"))))


(defun SPLIT-STRING (String Splitter-Char)
  (let ((Start 0)
        (List nil))
    (dotimes (I (length String) List)
      (cond
       ;; splitter char
       ((char= (char String i) Splitter-Char)
        (setq List (append List (list (subseq String Start I))))
        (setq Start (+ i 1)))
       ;; the end
       ((= i (1- (length String)))
        (setq List (append List (list (subseq String Start (1+ i))))))))))


(defun PARSE-FILE-NAME (Name)
  (let ((Dot-Position (position #\. Name :from-end t)))
    (if Dot-Position
      (values
       (subseq Name 0 Dot-Position)
       (subseq Name (1+ Dot-Position)))
      Name)))


(defun UNIX-PATHNAME-DIRECTORY-P (Unix-Pathname)
  (char= (char Unix-Pathname (1- (length Unix-Pathname))) #\/))


(defun CONVERT-TO-UNIX-PATHNAME (Pathname)
  (with-output-to-string (Unix-Pathname)
    (dolist (Component (rest (rest (pathname-directory Pathname))))
      (format Unix-Pathname "/~A" Component))
    (cond
     ;; directory
     ((or (null (pathname-name Pathname))
          (string-equal (pathname-name Pathname) ""))
      (princ #\/ Unix-Pathname))
     ;; file
     (t
      (format Unix-Pathname "/~A" (pathname-name Pathname))
      (when (pathname-type Pathname)
        (format Unix-Pathname ".~A" (pathname-type Pathname)))))))


(defun CONVERT-TO-LISP-PATHNAME (Unix-Pathname)
  (with-input-from-string (Path Unix-Pathname)
    (unless (char= (read-char Path) #\/) (error "path needs to start with \"/\""))
    (cond
     ((unix-pathname-directory-p Unix-Pathname)
      (make-pathname
       :directory (append (list :absolute (disk-name)) (rest (split-string Unix-Pathname #\/)))))
     (t
      (let ((Path-List (split-string Unix-Pathname #\/)))
        (multiple-value-bind (Name Extension)
                             (parse-file-name (first (last Path-List)))
          (make-pathname
           :directory (append (list :absolute (disk-name)) (rest (butlast Path-List)))
           :name Name
           :type Extension)))))))

#+:CCL
(defun CONVERT-RELATIVE-UNIX-PATH-TO-LISP-PATHNAME (Unix-Pathname)
  (with-input-from-string (Path Unix-Pathname)
    (unless (char= (read-char Path) #\/) (error "path needs to start with \"/\""))
    (cond
     ((unix-pathname-directory-p Unix-Pathname)
      (make-pathname
       :directory (append (pathname-directory (ccl:full-pathname "ccl:")) (rest (split-string Unix-Pathname #\/))))) 
     (t
      (let ((Path-List (split-string Unix-Pathname #\/)))
        (multiple-value-bind (Name Extension)
                             (parse-file-name (first (last Path-List)))
          (make-pathname 
           :directory (append (pathname-directory (ccl:full-pathname "ccl:")) (rest (butlast (split-string Unix-Pathname #\/))))
           :name Name
           :type Extension)))))))

#| Test:

(convert-to-unix-pathname
 (convert-to-lisp-pathname "/Users/alex/Desktop/enemy0.vat"))

(convert-to-unix-pathname
 (convert-to-lisp-pathname "/Users/alex/Desktop/"))


|#
;_____________________________
; low level Read functions    |
;_____________________________


(defvar *XML-Entity-Reference-Table*
  '(("lt;" #\<) ("gt;" #\>) ("amp;" #\&) ("sq;" #\') ("apos;" #\') ("dq;" #\") ("quot;" #\") ("#10;" #\newline) ("#39;" #\'))
  "http://www.w3.org/TR/WD-xml-961114.html#sec4.1")


(defun READ-UNTIL-TOKEN (Stream Token &key Escape-Char Decode-Function) "
  in:  Stream stream; Token string; &key Escape-Char char; Decode-Function stream->char.
  out: String string.
  Read from stream until token. If there is an escape-char use the decode-funtion to parse it."
  (let ((String (make-array 40 :fill-pointer 0 :element-type 'character :adjustable t))
        (Match 0)
        (End (length Token)))
    (loop
      (let ((Char (read-char Stream nil nil)))
        (cond
         ;; end of stream
         ((null Char) (return String))
         ;; Match!
         ((char= Char (char Token Match))
          (incf Match)
          ;; are we done yet?
          (when (= Match End) (return String)))
         ;; NO match
         (t
          ;; resolve partial match
          (dotimes (I Match)
            (vector-push-extend (char Token i) String))
          (setq Match 0)
          (cond
           ;; escape character that needs decoding?
           ((and Escape-Char (char= Char Escape-Char) Decode-Function)
            (vector-push-extend (funcall Decode-Function Stream) String))
           ;; legit part of string
           (t
            (vector-push-extend Char String)))))))))


(defun DECODE-XML-ENTITY-REFERENCE (Stream) "
  If the XML escape character & has been encountered use this function to decode the rest of the entity reference"
  (let* ((Name (read-until-token Stream ";")) ;; does not include "&" or ";"
         (Entity-Reference (find Name *XML-Entity-Reference-Table*
                                 :key #'first
                                 :test #'(lambda (N1 N2) (string= N1 N2 :end2 (min (length N1) (length N2)))))))
    (unless Entity-Reference
      (error "\"&~A;\" is not a valid EntityRef. http://www.w3.org/TR/WD-xml-961114.html#sec4.1" Name))
    (second Entity-Reference)))


(defun SKIP-UNTIL-CHARS (Stream &rest Chars) "
  Find all chars in sequence and keep reading until last char of <Chars> is found."
  (dolist (Char Chars)
    (loop
      (when (char= Char (read-char Stream)) (return)))))


#| NOT WORKING!!

(defun DECODE-XML-STRING (String) "
  in:  String string.
  out: Decoded-String string.
  Decode XML ecoded strings back into litteral strings.
  e.g., \"a &gt; b\" turns into \"a > \"b"
 (with-input-from-string (stream (if (stringp String) String (write-to-string String)))
   (read-until-token Stream nil :escape-char #\& :decode-function #'decode-xml-entity-reference)))
|#

(defun DECODE-XML-STRING (Input-String)
  (when (stringp Input-String)
    (with-input-from-string (string Input-String)
      (let ((Output-String (make-array 40 :fill-pointer 0 :element-type 'character :adjustable t)))
        (loop
          (let ((Char (read-char String nil nil)))
            (cond
             ;; end of stream
             ((null Char) (return Output-String))
             ;; found &
             ((char= Char #\&)
              (vector-push-extend (decode-xml-entity-reference String) Output-String))
             (t
              (vector-push-extend Char Output-String)))))))))


(defun ENCODE-XML-STRING (String) "
  Convert String to an XML-compatible string:
   \"<\" becomes \"&lt;\"
   \">\" becomes \"&gt;\"
   \"&\" becomes \"&amp;\"
   \" becomes \"&quot;\"
  \"'\" becomes \"&#39;\"
   and the newline character becomes \"&#10;\""
  ;; should use *XML-ENTITY-REFERENCE-TABLE*
  (unless (stringp String) (setq String (write-to-string String))) ;; just in case
  (let ((Output (make-array 40 :fill-pointer 0 :element-type 'character :adjustable t)))
    (with-input-from-string (Input String)
      (loop
        (let ((Char (or (read-char Input nil nil) (return Output))))
          (case Char
            (#\<
             (vector-push-extend #\& Output)
             (vector-push-extend #\l Output)
             (vector-push-extend #\t Output)
             (vector-push-extend #\; Output))
            (#\>
             (vector-push-extend #\& Output)
             (vector-push-extend #\g Output)
             (vector-push-extend #\t Output)
             (vector-push-extend #\; Output))
            (#\&
             (vector-push-extend #\& Output)
             (vector-push-extend #\a Output)
             (vector-push-extend #\m Output)
             (vector-push-extend #\p Output)
             (vector-push-extend #\; Output))
            (#\newline
             (vector-push-extend #\& Output)
             (vector-push-extend #\# Output)
             (vector-push-extend #\1 Output)
             (vector-push-extend #\0 Output)
             (vector-push-extend #\; Output))
            (#\"
             (vector-push-extend #\& Output)
             (vector-push-extend #\q Output)
             (vector-push-extend #\u Output)
             (vector-push-extend #\o Output)
             (vector-push-extend #\t Output)
             (vector-push-extend #\; Output))
            (#\'
             (vector-push-extend #\& Output)
             (vector-push-extend #\# Output)
             (vector-push-extend #\3 Output)
             (vector-push-extend #\9 Output)
             (vector-push-extend #\; Output))
           (t
             (vector-push-extend Char Output))))))))

;________________________________________
; Token level Reader functions           |
;________________________________________

(defun SKIP-XML-HEADER (Stream) "
  For now we do not do anything with the header content but just make sure we skip it."
 (let ((Char (read-char Stream)))
   (unless (char= Char #\?) (return-from skip-xml-header (unread-char Char Stream)))
   (skip-until-chars Stream #\? #\> #\<)))


(defun READ-XMLISP-NAME (Stream) "
  Valid names start with a letter, _ or :, have to contain letters or digits or other valid characters (see XML spec).
  Extended with SGML spec. allowing names such as'<![CDATA['"
  (let ((Name (make-array 40 :fill-pointer 0 :element-type 'character :adjustable t))
        (Char (read-char Stream)))
    ;; check first character
    (cond
     ((or (letterp Char) (char= Char #\_) (char= Char #\:)
          (char= #\!))  ;; SGML char, <![CADATA[, <!--
      ;; read and check the rest of the characters: stop if you find white space, = or >
      (vector-push-extend Char Name)
      (loop
        (let ((Char (read-char Stream)))
          (cond
           ;; complete: return as symbol
           ((or (white-space-p Char)
                (char= Char #\=)
                (char= Char #\>)
                (char= Char #\/))
            (unread-char Char Stream)
            (return (values (make-xmlisp-symbol Name)
                            Name)))
           ;; comment: do not wait for delimiter http://www.w3.org/TR/REC-xml/#sec-comments
           ((and (char= Char #\-) (string= Name "!-"))
            (vector-push-extend Char Name)
            (return (values (make-xmlisp-symbol Name)
                            Name)))
           ;; part of name
           ((or (namecharp Char)
                (char= Char #\[))  ;; SGML
            (vector-push-extend Char Name))
           ;; trouble
           (t
            (show-error-in-stream-to-user Stream)
            (error "Character ~C is not a valid character for a name" Char))))))
     (t
      (show-error-in-stream-to-user Stream)
      (error "Not a valid start character for name")))))


(defun READ-XMLISP-VALUE (Stream)
  ;; read single and double quote values
  (case (read-char Stream)
   (#\"
    (read-until-token Stream "\"" :escape-char #\& :decode-function #'decode-xml-entity-reference))
   (#\'
    (read-until-token Stream "\'" :escape-char #\& :decode-function #'decode-xml-entity-reference))
   (t
    (show-error-in-stream-to-user Stream)
    (error "not a valid XML value"))))


(defmethod READ-XMLISP-CHARACTER-CONTENT ((Self xml-serializer) Stream)
  (prog1
    (read-until-token Stream "<" :escape-char #\& :decode-function #'decode-xml-entity-reference)
    (unread-char #\< Stream)))



(defun READ-NAME-AND-MAKE-INSTANCE (Stream) "
  If name corresponds to an existing class create an instance that of that instance.
  Search strategy:
  1) look in element-class-name table
  2) look for class with symbol-name matching original case
  3) look for class with symbol-name matching readtable case converted (probably all uppercase) case
  4) create a much more constrained xml-content instance"
  (read-return-value
   (multiple-value-bind (Symbol String)
                        (read-xmlisp-name Stream)
     (declare (special |$xml-filename$| |$xml-stream$|))
     (assert (eq Stream |$xml-stream$|))
     (let ((Element-Class-Name (element-class-name Symbol))
           (Original-Case-Symbol (find-symbol String))
           (File-Name |$xml-filename$|))
       (cond
        ;; 1) lookup element class name table
        (Element-Class-Name
         ;; if this name is in the table we should interpret lack of class to be an error
         (if (find-class Element-Class-Name nil)
           (make-instance Element-Class-Name :is-created-by-xml-reader t :file File-Name)
           (error "element \"~A\" cannot produce instance of class \"~A\" because that class does not exist" String Element-Class-Name)))
        ;; 2) Original Case matches class name
        ((and Original-Case-Symbol (find-class Original-Case-Symbol nil))
         (make-instance Original-Case-Symbol :is-created-by-xml-reader t :file File-Name))
        ;; 3) readtable translated case matches class name
        ((find-class Symbol nil)
         (make-instance Symbol :is-created-by-xml-reader t :file File-Name))
        ;; 4) xml-content
        (t
         (make-instance 
             (if *Fallback-Class-Name-For-Element-Name-Hook* 
               (or (funcall *fallback-class-name-for-element-name-hook* Symbol)
                   'xml-content)
               (error "cannot find class \"~A\" ~%XMlisp is trying to create an instance of this class while reading \"<~A ...\"" Symbol String))
           :name Symbol
           :is-created-by-xml-reader t
           :file File-Name)))))))


(defun READ-WHITE-SPACE (Stream)
  (let ((Char nil))
    (loop 
      (or (setq Char (read-char Stream nil nil)) (throw :read-element-error nil))
      (unless (white-space-p Char)
        (unread-char Char Stream)
        (return t)))))


(defun READ-EQUAL-SIGN (Stream)
  (read-white-space Stream)
  (let ((Char (or (read-char Stream nil nil) (throw :read-element-error nil))))
    (if (char= Char #\=)
      (read-white-space Stream)
      (format-if-verbose t "Did not find an equal sign"))))


(defmethod READ-XMLISP-ATTRIBUTES ((Self xml-serializer) Stream)
  (read-white-space Stream)
  (loop
    (let ((Char (read-char Stream)))
      (case Char
        ((#\/ #\>) ;; delimiters
         (unread-char Char Stream)
         (finished-reading-attributes Self Stream)
         (return))
        (t
         (unread-char Char Stream)
         (read-white-space Stream)
         (set-attribute-value Self (prog1 (read-xmlisp-name Stream) (read-equal-sign Stream)) (read-xmlisp-value Stream))
         (read-white-space Stream)))))
  (finished-reading-attributes Self Stream)  ;; not that important any more: just specialize initialize-instance
  Self)


(defmethod READ-XMLISP-ELEMENT-CONTENT ((Self xml-serializer) Stream)
  (let ((Char (read-char Stream)))
    (case Char
      ;; found an empty element or the end of this element
      (#\/
       (unread-char Char Stream)
       (return-from read-xmlisp-element-content nil))
      ;; start a new sub element
      (t
       (unread-char Char Stream)
       (let ((Element (read-name-and-make-instance Stream)))
         (add-subobject Self Element)
         (setf (part-of Element) Self))
       Self))))


(defmethod READ-XMLISP-END-TAG ((Self xml-serializer) Stream)
  (let ((End-Tag (read-xmlisp-name Stream)))
    (read-white-space Stream)
    (case (read-char Stream)
      (#\> ;; match tags
       (if (eq (xml-tag-name-symbol Self) End-Tag)
         (return-from read-xmlisp-end-tag t)
         (error "Tag mismatch: start tag=~A  end tag=~A" (xml-tag-name-symbol Self) End-Tag)))
      (t
       (error "Not a well formed end tag. Missing '>'")))))


(defmethod READ-XMLISP-ELEMENT ((Self xml-serializer) Stream)
  ;; assume name & attributes have been read
  ;; we are just about to read the end of the first part of the element ">" or "/>"
  ;; (format t "read-xmlisp-element ~A~%" (type-of self))
  (let (($Sub-Element-Slot-Names$ nil))
    (declare (special $Sub-Element-Slot-Names$))
    (loop
      (read-white-space Stream)
      (let ((Char (read-char Stream)))
        (case Char
          ;; end of element
          (#\/
           (case (read-char Stream)
             (#\> ;; DONE!
              (return-from read-xmlisp-element Self))
             (t
              (error "Not a well formed end tag. Missing '>'"))))
          ;; end tag
          (#\<
           (case (read-char Stream)
             (#\/
              (when (read-xmlisp-end-tag Self Stream)
                (cleanup-sub-object-slots Self $Sub-Element-Slot-Names$)
                (return-from read-xmlisp-element Self)))
             (t
              (error "Not a well formed end tag. Missing '/'"))))
          ;; content
          (#\>
           (loop
             (read-white-space Stream)
             (let ((Next-Char (read-char Stream)))
               (case Next-Char
                 (#\<
                  (unless (read-xmlisp-element-content Self Stream)
                    (case (read-char Stream)
                      (#\/
                       (when (read-xmlisp-end-tag Self Stream)
                         (cleanup-sub-object-slots Self $Sub-Element-Slot-Names$)
                         (return-from read-xmlisp-element Self)))
                      (t (error "Not a well formed end tag. Missing '/'")))))
                 (t
                  (unread-char Next-Char Stream)
                  ;; append to existing content
                  (setf (content Self) 
                        (if (content Self)
                          (concatenate 
                           'string
                           (content Self)       
                           (read-xmlisp-character-content Self Stream))
                          (read-xmlisp-character-content Self Stream)))))))))))))


(defmethod READ-XMLISP-ELEMENT :after ((Self xml-serializer) Stream)
  ;; call finished-reading in a new empty dynamic context
  ;; to make sure it does not mess up current one
  ;; this could be a problem if finished-reading called more xml read functions
  (let (($Sub-Element-Slot-Names$ nil))
    (declare (special $Sub-Element-Slot-Names$))
    (finished-reading Self Stream)))

;_____________________________
; Initialization              |
;_____________________________

(defmethod INITIALIZE-INSTANCE ((Self xml-serializer) &rest Args)
  (declare (ignore Args) (special |$xml-stream$|))
  (call-next-method)
  ;; if this instance has been created through the XML reader then 
  ;; read its attributes and set its slots
  (when (is-created-by-xml-reader Self)
    (read-xmlisp-attributes Self |$xml-stream$|))
  Self)


(defmethod INITIALIZE-INSTANCE :after ((Self xml-serializer) &rest Args)
  (declare (ignore Args) (special |$xml-stream$|))
  ;; if this instance has been created through the XML reader then
  ;; read its content & sub elements if there are any
  (when (is-created-by-xml-reader Self)
    (read-xmlisp-element Self |$xml-stream$|))
  Self)

;_____________________________
; File Input/Output           |
;_____________________________

(defun LOAD-OBJECT (Filename &key Verbose (If-Does-Not-Exist :error) (Package *Package*)) "
  in: Filename pathname;
    &key Verbose boolean;
    if-does-not-exist action default :error;
   package package default *Package*.
 out: Object t.
 Load XML object contained in <Filename> into package and return it."
  (when Verbose (format t ";; loading object in file: ~A~%" Filename))
  (let ((*Package* Package))
    (with-open-file (File Filename :direction :input :if-does-not-exist If-Does-Not-Exist)
      (let ((*Xml-Stream* File))
        (declare (special *Xml-Stream*))
        (read File)))))


(defmethod SAVE-OBJECT ((Self xml-serializer) Filename &key Verbose (If-Exists :error) (Xml-Header "<?xml version=\"1.0\"?>"))
  (when Verbose (format t ";; saving object to file: ~A~%" Filename))
  (let ((*XMLisp-Print-Synoptic* nil)) ;; never save XML object with synoptic printing
    (with-open-file (File Filename :direction :output :if-exists If-Exists)
      (when XML-Header (format File "~A~%" XML-Header))
      (princ Self File))))

;_____________________________
; Set & Aggregation Handlers  |
;_____________________________

(defvar *Plural-Name-Table* (make-hash-table :test #'eq) "cached plural forms of symbols, e.g., bla -> blas")


(defun PLURAL-NAME (Name) "
  in: name symbol.
  out: plural-form-of-name symbol.
  Return plural form of <Name>"
  (or (gethash Name *Plural-Name-Table*)
      (setf (gethash Name *Plural-Name-Table*) (make-xmlisp-symbol (format nil "~As" Name)))))


(defmethod SLOT-NAME->ATTRIBUTE-NAME ((Self xml-serializer) Slot-Name)
  ;; default to identity
  Slot-Name)


(defmethod ATTRIBUTE-NAME->SLOT-NAME ((Self xml-serializer) Attribute-Name)
  ;; default to identity
  Attribute-Name)


(defmethod SET-ATTRIBUTE-VALUE ((Self xml-serializer) Name Value)
  ;; (format t "~%set attribute: ~A to: ~A" Name Value)
  ;; use MOP to find suitable slot with matching symbol-name
  (declare (special *Xml-Stream*))
  (let* ((Slot-Definition (or (find-slot-definition Self (attribute-name->slot-name Self Name))
                              (when (boundp '*Xml-Stream*) (show-error-in-stream-to-user *Xml-Stream*))
                              (error "class: ~A does not have slot matching attribute name: ~A" (type-of Self) Name)))
         (Type (slot-definition-type Slot-Definition)))
    (setf (slot-value Self (slot-definition-name slot-definition))
          (if (eq Type t)
            ;; super generic type: need to explore other aspects of slot-definition
            (cond
             ;; try to infer from type of :initform
             ((numberp (slot-definition-initform Slot-Definition))
              (read-from-string Value))
             ;; no clues: fill in as string
             (t
              Value))
            ;; dispatch based on type
            (read-typed-attribute-value Value Type)))))


(defmethod ADD-OBJECT-TO-SLOT ((Self xml-serializer) Object Slot-Name)
  (declare (special $Sub-Element-Slot-Names$))
  ;; not very clever: needs to be reversed in cleanup
  (when (boundp '$Sub-Element-Slot-Names$)
    (pushnew Slot-Name $Sub-Element-Slot-Names$))  ;; keep track of slots for cleanup
  (push Object (slot-value Self Slot-Name)))


(defmethod CLEANUP-SUB-OBJECT-SLOTS ((Self xml-serializer) Slot-Names)
  ;; reverse lists to preserve same order as in stream
  (dolist (Slot-Name Slot-Names)
    (setf (slot-value Self Slot-Name) (reverse (slot-value Self Slot-Name)))))


(defmethod ADD-SUBOBJECT ((Self xml-serializer) Object)
  (let ((Name (xml-tag-name-symbol Object)))
    (let ((Single-Value-Slot-Definition (find-slot-definition Self Name)))
      (if Single-Value-slot-definition
        (setf (slot-value Self (slot-definition-name Single-Value-slot-definition)) Object)
        (let ((Multy-Value-Slot-Definition (find-slot-definition Self (plural-name Name))))
          (if Multy-Value-slot-definition
            (add-object-to-slot Self Object (slot-definition-name Multy-Value-slot-definition))
            (error "element: ~A of class: ~A does not have slots (\"~A\" or \"~A\") to contain sub element: ~A of class: ~A"
                   (xml-tag-name-symbol Self)
                   (type-of Self)
                   Name
                   (plural-name Name)
                   Name
                   (type-of Object))))))))


(defmethod (SETF PART-OF) (Container (Self xml-serializer))
  (declare (ignore Container))
  ;; do nothing
  )


(defmethod PART-OF ((Self xml-serializer))
  ;; to return object containing me we would need a part-of slot to store it
  nil)

;_____________________________
; File                        |
;_____________________________

(defmethod (SETF FILE) (Container (Self t))
  ;; for instance modal window dialogs return any kind of objects, make sure this is not a problem
  (declare (ignore Container))
  ;; do nothing
  )


;_____________________________
; Print                       |
;_____________________________

(defun NUMBER-OF-PRINTABLE-ELEMENTS (Object) "
  Retun the number of object components that can be printed as XML elements."
   (let ((Number 0))
     (map-object
      Object
      #'(lambda (Element)
           (when (xml-printable-as-subelement-p Element)
             (incf Number))))
     Number))

(defvar *XML-Tab-Level* 0 "level of indentation")


(defun PRINT-XML-INDENT (Stream &optional (Level *XML-Tab-Level*))
  (dotimes (I Level)
    (princ "  " Stream)))


(defmethod PRINT-SLOT-NAME-VALUE-TYPE-AS-ATTRIBUTE ((Self xml-serializer) Name Value Type Stream)
  (format Stream " ~A=" (string-downcase (symbol-name Name)))
  (print-typed-attribute-value Value Type Stream))


(defmethod PRINT-SLOT-VALUE-AS-ATTRIBUTE ((Self xml-serializer) Slot-Definition Stream)
  (print-slot-name-value-type-as-attribute 
   Self 
   (slot-name->attribute-name Self (slot-definition-name slot-definition))
   (slot-value Self (slot-definition-name slot-definition))
   (slot-definition-type slot-definition)
   Stream))


(defmethod PRINT-SLOTS-AS-ATTRIBUTES ((Self xml-serializer) Slot-Definitions Stream)
  (dolist (Slot-Definition slot-definitions)
    (let ((Value (slot-value Self (slot-definition-name slot-definition))))
      ;; make sure there is a meaninful way to print the value
      (when (or (print-default-value-attributes-p Self)
                (not (equal Value (slot-definition-initform slot-definition))))
        (print-slot-value-as-attribute Self slot-definition Stream)))))


(defmethod PRINT-SLOT-VALUE-AS-SUBELEMENT ((Self xml-serializer) Slot-Definition Stream)
  (let ((Type (slot-definition-type slot-definition))
        (Value (slot-value Self (slot-definition-name slot-definition))))
    (if Type
      ;; Typed
      (print-typed-subelement-value Value Type Stream)
      ;; untyped
      (map-object
       Value
       #'(lambda (Object)
           (terpri Stream)
           (print-object Object Stream))))))


(defmethod PRINT-SLOTS-AS-SUBELEMENTS ((Self xml-serializer) Slot-Definitions Stream)
  (dolist (Slot-Definition slot-definitions)
    ;; (format t "~%print slot: ~A" (slot-definition-name slot-definition))
    (print-slot-value-as-subelement Self slot-definition Stream)))


(defmethod SLOT-DEFINITIONS-WITH-SLOT-BOUND ((Self xml-serializer) Slot-Definitions)
  ;; keep only slot definitions of bound slots
  (mapcan
   #'(lambda (Definition)
       (typecase Definition
         (symbol (list Definition))
         (t (if (slot-boundp Self (slot-definition-name Definition))
              (list Definition)
              nil))))
   Slot-Definitions))


(defmethod SLOTS-TO-PRINT-LIST ((Self xml-serializer))
  (let ((Slot-Names (print-slots Self)))
    (slot-definitions-with-slot-bound
     Self
     (if (equal Slot-Names :all)
       (class-slots (class-of Self))
       (mapcar
        #'(lambda (Slot-Name)
            (or (find-slot-definition Self Slot-Name)
                ;; if slot definition is not found return slot-name, caller could try to find accessor function
                Slot-Name))
        Slot-Names)))))


(defmethod PRINT-ACCESSOR-VALUES-AS-ATTRIBUTES ((Self xml-serializer) Accessor-Values Stream)
  (dolist (Accessor-Value Accessor-Values)
    ;; we have little meta information: no type, initform etc.
    (format Stream " ~A=" (string-downcase (first Accessor-Value)))
    (print-typed-attribute-value (rest Accessor-Value) t Stream)))


(defmethod PRINT-ACCESSOR-VALUES-AS-SUBELEMENTS ((Self xml-serializer) Accessor-Values Stream)
  (dolist (Accessor-Value Accessor-Values)
    (map-object
       (rest Accessor-Value)
       #'(lambda (Object)
           (terpri Stream)
           (print-object Object Stream)))))


(defmethod SYNOPTIC-XML-OBJECT-INDENTITY-CLUES ((Self xml-serializer))
  "..." ;; default: no real clue really
  )


(defmethod PRINT-OBJECT-SYNOPTIC ((Self xml-serializer) Stream)
  (format Stream "<~A ~A/>" (xml-tag-name-string Self) (synoptic-xml-object-indentity-clues Self)))


(defmethod PRINT-OBJECT ((Self xml-serializer) Stream)
  ;; perhaps we just print synoptic?
  (when *XMLisp-Print-Synoptic*
    (print-object-synoptic Self Stream)
    (return-from print-object))
  ;; nope: we are going for the full Monty
  ;; start tag
  (print-xml-indent Stream)
  (format Stream "<~A" (xml-tag-name-string Self))
  ;; separate printable subelements from others
  (let ((Attribute-Value-Printable-Slot-Definitions nil)
        (Subelement-Printable-Slot-Definitions nil)
        (Accessor-Based-Attribute-Values nil)
        (Accessor-Based-Subelement-Values nil))
    ;; sort out into attribute/subelements and slot/accessor based values
    (dolist (Slot-Definition (reverse (slots-to-print-list Self)))
      (typecase Slot-Definition
        (symbol
         ;; problem: there is no such slot, slot-definition is name that may have matching accessor function
         ;; this should be rare
         (let ((Name Slot-Definition))
           (when (print-slot-with-name-p Self Name)
             (if (and (fboundp Name)
                      (eq (type-of (symbol-function Name)) 'STANDARD-GENERIC-FUNCTION)
                      (compute-applicable-methods (symbol-function Name) (list Self))) ;; MOP function
               (let ((Value (funcall (symbol-function Name) Self)))
                 ;; we have little meta information: no type, initform etc.
                 ;; NIL is probably not very usefull
                 (when Value
                   (if (xml-printable-as-subelement-p Value)
                     (push (cons Name Value) Accessor-Based-Subelement-Values)
                     (push (cons Name Value) Accessor-Based-Attribute-Values))))
               (error "print error: Class \"~A\" does not have slot \"~A\"" (type-of Self) Name)))))

        ;; must be a valid slot definition
        (t 
         (when (print-slot-with-name-p Self (slot-definition-name slot-definition))
           (let ((Value (slot-value Self (slot-definition-name slot-definition))))
             (cond
              ;; subelement
              ((xml-printable-as-subelement-p Value)
               (push slot-definition Subelement-Printable-slot-definitions))
              ;; non-t slot definition type: assume it's printable or there is CODED
              ((not (eq (slot-definition-type Slot-Definition) t))
               (push slot-definition Attribute-Value-Printable-slot-definitions))
              ;; attribute
              ((xml-printable-as-attribute-value-p Value)
               (push slot-definition Attribute-Value-Printable-slot-definitions))
              (t
               (warn "\"~A\" stored in slot ~A is not XML printable" Value (slot-definition-name slot-definition)))))))))
    ;; print single <.../> or nested one
    (cond
     ;; at least one sub element or some content
     ((and (print-subelements-to-stream-p Self Stream) 
           (or Subelement-Printable-slot-definitions 
               Accessor-Based-Subelement-Values
               (content Self)))
      ;; start tag
      (print-slots-as-attributes Self Attribute-Value-Printable-slot-definitions Stream)
      (print-accessor-values-as-attributes Self Accessor-Based-Attribute-Values Stream)
      (format Stream ">")
      ;; content
      (when (content Self) (princ (encode-xml-string (content Self)) Stream))
      ;; sub elements
      (let ((*Xml-Tab-Level* (1+ *XML-Tab-Level*)))
        (print-slots-as-subelements Self Subelement-Printable-slot-definitions Stream)
        (print-accessor-values-as-subelements Self Accessor-Based-Subelement-Values Stream))
      ;; end tag
      (unless (content Self)
        (terpri Stream)
        (print-xml-indent Stream))
      (format Stream "</~A>" (xml-tag-name-string Self)))
     ;; simple tag: no sub elements, no content
     (t
      (print-slots-as-attributes Self Attribute-Value-Printable-slot-definitions Stream)
      (print-accessor-values-as-attributes Self Accessor-Based-Attribute-Values Stream)
      (format Stream "/>")))))

;_____________________________
; Reader                      |
;_____________________________

(defun ELEMENT-READER (Stream Char)
  (declare (ignore Char))
  ;; may not be an XML element after all, e.g., common-lisp functions <, <=
  (let ((Next-Char (read-char Stream nil nil)))
    ;; danger zone: may not catch all the cases.
    ;; Probably better approach: if next-char is not a valid first character of a XML element name then
    ;; finish reading the symbol and return it
    (case Next-Char
      (#\space 
       (unread-char Next-Char Stream)
       (return-from element-reader (values (intern "<"))))
      (#\= (return-from element-reader (values (intern "<="))))
      (#\) 
       (unread-char Next-Char Stream)
       (return-from element-reader (values (intern "<")))))
    (unread-char Next-Char Stream))
  ;; lets read XML
  (skip-xml-header Stream)   ;; this only needs to be done once
  (let ((|$xml-stream$| Stream)
        (|$xml-filename$| (pathname-from-stream Stream)))
    (declare (special |$xml-stream$| |$xml-filename$|))
    (read-name-and-make-instance Stream)))


(defvar *XMLisp-Packages* :all "define which packages support the XML reader: list of packages, nil or keyword :all. Default is :all")

;; error if XMLisp has not been loaded but there already is a #\< reader
;; there can only be one such reader
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (and (not (boundp '*Non-XMLISP-Readtable*)) (get-macro-character #\<))
    (warn "~%XMLisp: The current *readtable* already contains a #/< reader function: ~A" (get-macro-character #\<))))


(defvar *Non-XMLISP-Readtable* (copy-readtable *Readtable*) "A readtable not including the XMLisp #\< reader")

(defvar *XMLisp-Reader-Enabled* t "if nil then no XML reading will take place")


(defmacro WITHOUT-XML-READER (&body Code)
  (let ((Enabled-Var (gensym)))
    `(let ((,Enabled-Var *Xmlisp-Reader-Enabled*))
       (unwind-protect
         (progn 
           (setq *Xmlisp-Reader-Enabled* nil)
           ,@Code)
         (setq *Xmlisp-Reader-Enabled* ,Enabled-Var)))))


(defun PACKAGE-DEPENDENT-ELEMENT-READER (Stream Char)
  (if (and *XMLisp-Reader-Enabled*
           (or (eq *XMLisp-Packages* :all)
               (and (listp *XMLisp-Packages*)
                    (member *Package* *XMLisp-Packages*))))
    ;; XML
    (funcall #'element-reader Stream Char)
    ;; non XML
    (let ((*Readtable* *Non-XMLISP-Readtable*))
      (unread-char Char Stream)
      (read Stream))))

(set-macro-character #\< #'package-dependent-element-reader t)

;_____________________________
; Platform Specific Printing   |
;_____________________________

#+(and :mcl (not :openmcl))
(defmethod PRINT-SUBELEMENTS-TO-STREAM-P ((Self xml-serializer) (Stream inspector::cache-entry-stream))
  ;; never print sublelements in inspector
  (declare (ignore Stream))
  nil)


#+(and :mcl (not :openmcl))
(defmethod PRINT-SLOT-VALUE-AS-SUBELEMENT ((Self xml-serializer) Slot-Definition (Stream ccl::terminal-io))
   (let ((Value (slot-value Self (slot-definition-name slot-definition))))
     ;; in Listener only print all subelements if there are not that many
     (cond
      ((<= (number-of-printable-elements Value) 100)
       (call-next-method))
      ;; too many!!
      (t
       (terpri Stream)
       (print-xml-indent Stream)
       (format Stream "... with ~A ~A subelements ..." (number-of-printable-elements Value) (slot-definition-name slot-definition))))))


#+(and :mcl (not :openmcl))
(defmethod PRINT-TYPED-ATTRIBUTE-VALUE (Value (Type (eql 'array)) (Stream ccl::terminal-io))
  (unless (and (array-element-type Value) (subtypep (array-element-type Value) 'number))
    (error "don't know how to print ~A in XML" Value))
    ;; store type and dimension
    (format Stream "\"float array ~A, ~A values ...\"" (array-dimensions Value) (array-total-size Value)))


#|

;; Example 1: HTML Link
;;   simple mapping between class/element and slot/attribute name


(defclass A (xml-serializer)
   ((href :accessor href :initform "" :initarg :href))
   (:documentation "HTML link"))


(inspect <a href="http://www.agentsheets.com"/>)
(read-from-string "<a href=\"http://www.agentsheets.com\">AgentSheets</a>")

(href <a href="http://agentsheets.com/lisp/XMLisp/">XMLisp examples</a>)


;; Example 2: RSS

(defclass RSS (xml-serializer)
   ((version :accessor version :initform "")
    (channel :accessor channel :initform nil))
   (:documentation "RSS main element"))

(defclass CHANNEL (xml-serializer)
   ((title :accessor title :initform "")
    (link :accessor link :initform "")
    (description :accessor description :initform "")
    (image :accessor image :initform nil)
    (managingeditor :accessor managingeditor :initform "")
    (ttl :accessor ttl :initform nil :documentation "don't know what this is")
    (language :accessor language :initform "")
    (copyright :accessor copyright :initform "")
    (webmaster :accessor webMaster :initform "")
    (pubdate :accessor pubDate :initform "")
    (lastbuilddate :accessor lastBuildDate :initform "")
    (category :accessor category :initform "")
    (generator :accessor generator :initform "")
    (docs :accessor docs :initform "")
    (items :accessor items :initform nil :documentation "list of RSS item"))
   (:documentation "RSS channel"))


(defclass IMAGE (xml-serializer)
   ((title :accessor title :initform "")
    (url :accessor url :initform "")
    (link :accessor link :initform "")
    (width :accessor width :initform 0))
   (:documentation "RSS Image"))


(defclass ITEM (xml-serializer)
   ((title :accessor title :initform "")
    (link :accessor link :initform "")
    (description :accessor description :initform "")
    (pubdate :accessor pubdate :initform ""))
   (:documentation "RSS news Item"))

;; pick an XML RSS file from the examples/xml folder
;; if you pick other RSS files keep in mind that the above spec is incomplete


(defparameter *RSS-News* (load-object (ccl:choose-file-dialog)))

(save-object *RSS-News* "ccl:delete_me.xml" :if-exists :overwrite)


;; and walk throught the RSS structure

(inspect *RSS-News*)



;; Example 3: Typed Slots
;;   Typed slots use the print-typed-attribute-value, read-typed-attribute-value, print-typed-subelement-value
;;   CODECs

(defclass COIN (xml-serializer)
  ((head-is-up :accessor head-is-up :type boolean)))


(inspect <coin head-is-up="true"/>)



;; Example 4: simple Aggregation: rule based Visual AgenTalk-like language
;;  use MOP name matching to implement aggregation
;;  e.g. slot "RULES" will contain a list of "RULE" elements

(defclass COMMAND (xml-serializer)
  ((name :accessor name :initform "" :initarg :name)
   (comments :accessor comments :initform nil)))


(defclass BEHAVIOR (command)
  ((method-commands :accessor method-commands :initform nil)))


(defclass METHOD-COMMAND (command)
  ((trigger :accessor trigger :initform nil)
   (rules :accessor rules :initform nil)))

(defclass TRIGGER (command)
  ())

(defclass RULE (command)
  ((condition-commands :accessor condition-commands :initform nil :initarg :condition-commands)
   (action-commands :accessor action-commands :initform nil :initarg :action-commands)
   (is-enabled :accessor is-enabled :initform t :initarg :is-enabled :type boolean)
   (probablility :accessor probability :initform 0.9s0 :initarg :probability :type short-float)))


(defclass CONDITION-COMMAND (command)
  ())


(defclass ACTION-COMMAND (command)
  ())



(inspect
 <behavior name="random Move">
   <method-command name="mouse" trigger="on mouse down">
     <rule>
       <condition-command name="see_a"/>
       <condition-command name="key"/>
       <action-command name="play_sound"/>
     </rule>
   </method-command>
 </behavior> )


;; Example 5: User defined Aggregation
;;   This is by no means a complete definition


(defclass HTML-BASE-CLASS (xml-serializer)
  ()
  (:documentation "mother of all HTML element classes"))


(defclass HTML (html-base-class)
  ((items :accessor items :initform nil))
  (:documentation "Contains all the html items of an HTML document"))


(defmethod ADD-SUBOBJECT ((Self html) (Item html-base-class))
  ;; extend this method to add all html-base-class instances to the "items" slot
  (add-object-to-slot Self Item 'items))



(defclass A (html-base-class)
   ((href :accessor href :initform "" :initarg :href))
   (:documentation "HTML link"))


(defclass FONT (html-base-class)
  ((face :accessor face)
   (size :accessor size :type number))
  (:documentation "Font info"))




(inspect

<HTML>

<font face="arial, sans-serif" size="-2">Small Text here</font>
<font face="arial, sans-serif" size="+2">Large Text here</font>
<a href="http://www.cs.colorado.edu">Go CU</a>

</HTML>   )



;;; Example 6: specialized attribute/slot name mapping


(defclass ARGUMENT (xml-serializer)
  ((pros :accessor pros :initform nil)
   (against :accessor against :initform nil)))  ;; cons would be slot name that would conflict with Common Lisp symbol


(defmethod ATTRIBUTE-NAME->SLOT-NAME ((Self argument) Attribute-Name)
  (case Attribute-Name
    (cons 'against)
    (t (call-next-method))))


(defmethod SLOT-NAME->ATTRIBUTE-NAME ((Self argument) Slot-Name)
  (case Slot-Name
    (against 'cons)
    (t (call-next-method))))


<argument pros="macs are cool" cons="everybody has windows boxes"/>

<argument pros="macs are cool" against="everybody has windows boxes"/>    ;; still works: could overwrite that if needed

(against <argument pros="macs are cool" cons="everybody has windows boxes"/>)



;;; Example 7: Name spaces
;;; XML Name spaces map to Lisp Packages
;;; if you need to be able to read, say, <simulation pikim:content_type="agentcubes" xmlns:pikim="http://ctl.sri.com/piki/mime"/>

;; define packages if they do not already exist:

(defpackage PIKIM)

(defpackage XMLNS)

;; then define your element class including slot names and accessors with package prefixes:

(defclass SIMULATION (xml-serializer)
  ((pikim::content_type :accessor pikim::content_type :initform nil)
   (xmlns::pikim :accessor xmlns::pikim :initform nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Example 8: comments, http://www.w3.org/TR/REC-xml/#sec-comments

;; well formed: (content <!-- declarations for <head> & <body> -->)

;; not well formed: <!-- B+, B, or B--->

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example 9: printing attributes based on accessor instead of slots

(defclass class-with-missing-slot (xml-serializer)
  ())

(defmethod print-slots ((Self class-with-missing-slot))
  '(accessor-with-no-matching-slot))  ; note: class does not have this slot

(defmethod accessor-with-no-matching-slot ((Self class-with-missing-slot))
  55)

(setq c (make-instance 'class-with-missing-slot))

(defclass list-of-class-with-missing-slot (xml-serializer)
  ((stuff :accessor stuff :initform 0 :type number :initarg :stuff)))

(defmethod elements ((Self list-of-class-with-missing-slot))
  (let ((List nil))
    (dotimes (i 20 List)
      (push (make-instance 'class-with-missing-slot) List))))

(defmethod print-slots ((Self list-of-class-with-missing-slot))
  '(stuff elements))

(defparameter *lc* (make-instance 'list-of-class-with-missing-slot :stuff 111))

(stuff *lc*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example 10: Read return values

(defclass SUM (xml-serializer)
  ((a :accessor a :type number)
   (b :accessor b :type number)))


(defmethod READ-RETURN-VALUE ((Self sum))
  ;; overwrite: instead of returning self return the actual sum of a and b
  (+ (a Self) (b Self)))

<sum a="2" b="3"/>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example 11: Keyword type

(defclass REFERENCE (xml-serializer)
  ((name :accessor name :type keyword)))


(name <reference name="left"/>)

|#


