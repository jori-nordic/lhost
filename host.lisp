(declaim (optimize (debug 3)))

;;;;;;;;;;;;; general utils

(defconstant +u64-max+ (ldb (byte 64 0) -1))

(defun make-range (max)
  (loop for number from 0 to (- max 1) collect number))

(defun extract-byte (number index)
  (ldb (byte 8 (* index 8)) number))

(defun make-uint (octets number)
  (loop for pos in (make-range octets)
        collect (extract-byte number pos)))

(make-uint 4 8000)
 ; => (64 31 0 0)

(defun u2b (type)
  (case type
     (:u8 1)
     (:u16 2)
     (:u32 4)
     (:u64 8)))

(defun custom-type (type)
  (case type
    (:bt-addr 6)))

(defun type->octets (type)
  (let ((octets))
    (setf octets (u2b type))

    (if (not octets)
        (setf octets (custom-type type)))

    (if (not octets)
        (error "unknown type ~A" type))

    octets))

(type->octets :u32)
 ; => 4 (3 bits, #x4, #o4, #b100)
(type->octets :bt-addr)
 ; => 6 (3 bits, #x6, #o6, #b110)

(defun make-c-int (type value)
  (make-uint (type->octets type) value))

(make-c-int :u8 #xFF)
 ; => (255)
(make-c-int :u32 8000)
 ; => (64 31 0 0)

(defun decode-c-int (bytes)
  (let ((result 0))
    (dolist (byte (reverse bytes) result) ; Reverse the byte order for little endian
      (setf result (logior (ash result 8) byte)))))

(decode-c-int '(1 2))

(defun make-c-struct-member (member)
  ;; name is first el, unused here
  (let ((type (nth 1 member))
        (value (nth 2 member)))
    (make-c-int type value)))

(defun make-c-struct (members)
  ;; Make a c-struct. `members' is a list of (name type value)
  ;; Output is a list of bytes.
  (mapcan #'make-c-struct-member members))

;; Eg. write default data length cmd. Len = 200 bytes, time = 1000 us
(make-c-struct '(("tx-octets" :u16 200) ("tx-time-us" :u16 1000)))
 ; => (200 0 232 3)

;;;;;;;;;;;;; babblesim PHY

(defconstant PB_MSG_WAIT #x01)
(defconstant PB_MSG_WAIT_END #x81)
(defconstant PB_MSG_TERMINATE #xFFFE)
(defconstant PB_MSG_DISCONNECT #xFFFF)

(defun make-wait-cmd (us)
  (append
   (make-uint 4 PB_MSG_WAIT)
   (make-uint 8 us)))

(make-wait-cmd 10)

(defun make-terminate-cmd ()
  (make-uint 4 PB_MSG_TERMINATE))

(defun read-bytes (bytes stream)
  (let ((b
          (if (equal bytes 1)
              (list (read-byte stream))
              (loop for byte from 0 to (- bytes 1) collect
                                                   (read-byte stream)))))
    ;; (format t "read-bytes: ~A~%" b)
    b))

(defun make-simplex-raw-fd-stream (file is-output)
  (sb-sys:make-fd-stream (sb-posix:file-descriptor file)
                         :element-type 'unsigned-byte
                         :input (not is-output)
                         :output is-output
                         :buffering :none))

(defun open-simplex-fd (path is-output)
  (let ((file (if is-output
                 (open path :direction :output :if-exists :overwrite)
                 (open path :direction :input))))
    (make-simplex-raw-fd-stream file is-output)))

(defun sim-wait (ms sim)
  (let ((rx (getf sim :rx))
        (tx (getf sim :tx))
        (time (getf sim :time)))
    (setf time (+ time (* ms 1000)))
    (format t "waiting until ~A us (delta ~A ms).." (getf sim :time) ms)

    (write-sequence (make-wait-cmd time) tx)
    (setf (getf sim :time) time)

    (read-bytes 4 rx)
    (format t "done~%")))

(defun sim-terminate (sim)
  (format t "term~%")
  (write-sequence (make-terminate-cmd) (getf sim :tx)))

(mapcan #'(lambda (x) x) '(36 32 1 (200 0 232 3)))
(append '(36 32) '(1) '(200 0 232 3))
(make-uint 2 #x0022)

;;;;;;;;;;;;; HCI packet-building

(defconstant +h4-types+
  (list :cmd #x1
        :acl #x2
        :evt #x4
        :iso #x5))

(defun plist-key (plist value)
  (loop for (key val) on plist by #'cddr
        when (equal val value)
          return key))

(defun make-h4 (type payload)
  (append
   (list (getf +h4-types+ type))
   payload))

;; Schema is (:name (#xOPCODE PARAM-PLIST RSP-PLIST))
(defparameter *hci-cmds*
  '(:reset (#x0c03 nil (:status :u8))

    :write-default-data-length
    (#x2024
     (:tx-octets :u16
      :tx-time-us :u16)
     nil)

    :set-event-mask
    (#x0c01 (:events :u64) (:status :u8))

    :le-set-event-mask
    (#x2001 (:events :u64) (:status :u8))

    :set-random-address
    (#x2005 (:address :bt-addr) (:status :u8))

    :read-buffer-size
    (#x2002
     nil
     (:status :u8
      :le-len :u16
      :le-num :u8))
    ))

(getf *hci-cmds* :write-default-data-length)
 ; => (8228 (:TX-OCTETS :U16 :TX-TIME-US :U16) NIL)
(getf *hci-cmds* :read-buffer-size)
 ; => (8194 NIL (:STATUS :U8 :LE-LEN :U16 :LE-NUM :U8))

;; Destructively pop `amount` bytes out of `buffer` which is a list of bytes.
(defmacro pull (buffer amount)
  `(let ((bytes (subseq ,buffer 0 ,amount)))
     (setf ,buffer (subseq ,buffer ,amount))
     bytes))

(defmacro pull-int (buffer type)
  `(decode-c-int (pull ,buffer (u2b ,type))))

(defparameter *test* '(#x4 #x1 #x3 #xc #x0))
(pull-int *test* :u16)
 ; => 260 (9 bits, #x104)
(pull-int *test* :u16)
 ; => 3075 (12 bits, #xC03)

(defun opcode->cmd (opcode)
  (loop for (command properties) on *hci-cmds* by #'cddr
        when (equal (nth 0 properties) opcode)
          return command))

(opcode->cmd #x2002)
 ; => :READ-BUFFER-SIZE

(defun command-properties (opcode)
  (loop for (command properties) on *hci-cmds* by #'cddr
        when (equal (nth 0 properties) opcode)
          return (list command properties)))

(command-properties #x2002)
 ; => (:READ-BUFFER-SIZE (8194 NIL (:STATUS :U8 :LE-LEN :U16 :LE-NUM :U8)))

(defun parse-cmd-response (opcode payload)
  (let* ((command (command-properties opcode))
         (properties (nth 1 command))
         (schema (if properties (nth 2 properties))))

    (when t
      ;; loop over param plist, replacing
      (loop for (name type) on schema by #'cddr
            nconc
            (list name (pull-int payload type))))))

(parse-cmd-response #x2002 '(0 #xFB 0 3))
 ; => (:STATUS 0 :LE-LEN 251 :LE-NUM 3)

(defun evt-cmd-complete (payload)
  (let ((ncmd (pull-int payload :u8))
        (opcode (pull-int payload :u16)))
    (list :cmd-complete
          (list
           :ncmd ncmd
           :opcode opcode
           :params (parse-cmd-response opcode payload)))))

(evt-cmd-complete '(#x1 #x3 #xc #x0))
 ; => (:CMD-COMPLETE (:NCMD 1 :OPCODE 3075 :PARAMS (:STATUS 0)))
(evt-cmd-complete '(1 2 #x20 0 #xFB 0 3))
 ; => (:CMD-COMPLETE (:NCMD 1 :OPCODE 8194 :PARAMS (:STATUS 0 :LE-LEN 251 :LE-NUM 3)))

(defun evt-cmd-status (payload)
  (list
   :cmd-status
   (list
    :status (pull-int payload :u8)
    :ncmd (pull-int payload :u8)
    :opcode (pull-int payload :u16))))

(evt-cmd-status '(#x1 #x1 #x3 #xc))
 ; => (:CMD-STATUS (:STATUS 1 :NCMD 1 :OPCODE 3075))

(defparameter *hci-events*
  '(#x0e evt-cmd-complete
    #x0f evt-cmd-status))

(getf *hci-events* #x0e)
 ; => EVT-CMD-COMPLETE

(funcall (getf *hci-events* #x0e) '(#x1 #x3 #xc #x0))
 ; => (:CMD-COMPLETE (:NCMD 1 :OPCODE 3075 :PARAMS (0)))

(defun decode-hci-event (header payload)
  (let* ((opcode (pull-int header :u8))
         (len (pull-int header :u8))
         (handler (getf *hci-events* opcode)))
    (declare (ignore len))

    (if (not handler) (error "No entry for op ~X" opcode))

    (funcall handler payload)))

(format nil "~x" (decode-hci-event '(#x0e #x04) '(#x1 #x3 #xc #x0)))
 ; => "(CMD-COMPLETE (NCMD 1 OPCODE C03 PARAMS (STATUS 0)))"
(format nil "~x" (decode-hci-event '(#xe #x7) '(1 2 #x20 0 #xFB 0 3)))
 ; => "(CMD-COMPLETE (NCMD 1 OPCODE 2002 PARAMS (STATUS 0 LE-LEN FB LE-NUM 3)))"

(defun make-hci-cmd-param (name value spec)
  (let ((type (getf spec name)))

    (unless type
      (error (format nil "Unknown param: ~A" name)))

    (make-c-struct-member (list name type value))))

;; test it
(make-hci-cmd-param
 :tx-time-us 1000
 (nth 1 (getf *hci-cmds* :write-default-data-length)))
 ; => (232 3)

(defun serialize-hci-params (params spec)
  (when params
    ;; loop over param plist
    (loop for (name value) on params by #'cddr
          nconc
          (make-hci-cmd-param name value spec))))

(defun make-hci-cmd (cmd-name &rest params)
  (let* ((spec (getf *hci-cmds* cmd-name))
         (opcode (car spec))
         (param-spec (nth 1 spec))
         (serialized-params
           (serialize-hci-params params param-spec)))

    ;; (format t "OP: ~x param-spec: ~A params ~A~%" opcode param-spec params)
    (append
     (make-c-int :u16 opcode)
     (make-c-int :u8 (length serialized-params))
     serialized-params)))

;; HCI cmds in plist
;; name: (plist of :param-name :type)
(format t "~A~%" (make-hci-cmd :write-default-data-length
                               :tx-octets 200
                               :tx-time-us 1000))
; (36 32 4 200 0 232 3)
;  => NIL

(defun send (type payload hci)
  "Format a payload into H4 and send to hci device"
  (let ((stream (getf hci :h2c))
        (packet (make-h4 type payload)))
    (format t "TX: ~x~%" packet)
    (write-sequence packet stream)))

(defun h4-parse-opcode (packet)
  "Looks up the H4 opcode"
  (plist-key +h4-types+ (car packet)))

(defun hci-header-len (opcode)
  "Returns the length of the HCI packet header field"
  (case opcode
    (:evt 2)
    (:acl 4)
    (:iso 4)
    (t (error "doesn't look like anything to me"))))

(defun hci-header-len-field (opcode)
  "Returns the offset and the size of the length field"
  (case opcode
    (:evt '(1 1))
    (:acl '(2 2))
    (:iso '(2 2))
    (t (error "doesn't look like anything to me"))))

(defun hci-parse-len (opcode packet)
  "Extracts the payload length from the HCI packet header"
  (let* ((header (hci-header-len-field opcode))
         (offset (nth 0 header))
         (size (nth 1 header)))
    (decode-c-int
     (subseq packet
             ;; skip the H4 opcode/header byte
             (+ 1 offset) (+ 1 offset size)))))

(hci-parse-len :acl '(2 1 1 0 0 ))
(hci-parse-len :evt '(#x4 #xe #x4 #x1 #x3 #xc #x0))

(defun rx-h4 (stream)
  (let ((packet '())
        (opcode)
        (header)
        (payload))

    ;; TODO: desync handling
    ;; TODO: don't re-iterate

    ;; read h4 opcode
    ;; (format t "read op~%")
    (setf packet (read-bytes 1 stream))
    ;; (format t "packet: ~A~%" packet)

    ;; parse h4 opcode
    (setf opcode (h4-parse-opcode packet))

    ;; read HCI packet header
    ;; (format t "read header~%")
    (setf header (read-bytes (hci-header-len opcode) stream))
    (setf packet (append packet header))
    ;; (format t "header: ~X~%" header)
    ;; (format t "packet: ~A~%" packet)

    ;; parse hci-packet-length from header & read payload
    ;; (format t "read payload~%")
    (setf payload (read-bytes
                   (hci-parse-len opcode packet)
                   stream))
    (setf packet (append packet payload))
    (format t "RX[H4]: ~X~%" packet)

    ;; return raw packet and parsed packet
    (list
     :opcode opcode
     :header header
     :payload payload
     :raw packet)))

(defun receive (hci)
  "Receive and decode a single HCI packet"
  ;; initial implementation is H4
  (let ((stream (getf hci :c2h)))
    (let* ((packet (rx-h4 stream))
           (opcode (getf packet :opcode))
           (header (getf packet :header))
           (payload (getf packet :payload)))

      (case opcode
        (:evt (decode-hci-event header payload))
        (t (error "doesn't look like anything to me"))))))

;;;;;;;;;;;;; host

(defun make-hci-dev (h2c-stream c2h-stream)
  (list
   :h2c h2c-stream
   :c2h c2h-stream
   :acl-tx-size 0
   :acl-tx-num 0
   :acl-rx-size 0
   :random-address 0))

;;;;;;;;;;;;; script

;; Run the REPL
(defparameter *bs-rx-path* "/tmp/bs_jon/myid/2G4.d0.ptd")
(defparameter *bs-tx-path* "/tmp/bs_jon/myid/2G4.d0.dtp")
(defparameter *h2c-path*   "/tmp/repl/myid/uart.h2c")
(defparameter *c2h-path*   "/tmp/repl/myid/uart.c2h")

(defparameter sizes '(:acl-tx-size 0
                      :acl-rx-size 1))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro with-bsim (instance rx-path tx-path &body body)
  (with-gensyms (rx tx)
    `(with-open-stream (,rx (open-simplex-fd ,rx-path nil))
       (with-open-stream (,tx (open-simplex-fd ,tx-path t))
         (let ((,instance (list :rx ,rx :tx ,tx :time 0)))
           (progn ,@body)
           )))))

(defmacro with-hci (instance h2c-path c2h-path &body body)
  (with-gensyms (h2c c2h)
    `(with-open-stream (,h2c (open-simplex-fd ,h2c-path t))
       (with-open-stream (,c2h (open-simplex-fd ,c2h-path nil))
         (let ((,instance (make-hci-dev ,h2c ,c2h)))
           (progn ,@body)
           )))))

;; (with-bsim sim *bs-rx-path* *bs-tx-path*
;;   (format t "connected to PHY (rx ~A tx ~A)~%"
;;           (sb-posix:file-descriptor (getf sim :rx))
;;           (sb-posix:file-descriptor (getf sim :tx)))
;;   (sim-wait 1000 sim)
;;   (sim-terminate sim))

(defun hci-send-cmd (cmd hci)
  "Send a command and check it's return status. Return params if no error."

  (send :cmd cmd hci)
  (let ((response (receive hci)))
    ;; Here `response` is an HCI event object, e.g.
    ;; (CMD-COMPLETE (NCMD 1 OPCODE C03 PARAMS (STATUS 0)))
    (format t "RX: ~x~%" response)

    (let* ((data (nth 1 response))
           (params (getf data :params))
           (status (getf params :status)))

      (if (not (equal status 0))
          (format t "cmd failed: status 0x~x~%" status))

      (if (equal status 0)
          params
          nil))))

(defun hci-reset (hci)
  "Reset the controller"
  (hci-send-cmd (make-hci-cmd :reset) hci))

(defun hci-read-buffer-size (hci)
  "Read (and set) H->C buffer lengths and amount"
  (let ((params
          (hci-send-cmd (make-hci-cmd :read-buffer-size) hci)))

    (if params
        (let ((le-len (getf params :le-len))
              (le-num (getf params :le-num)))

          (setf (getf hci :acl-tx-size) le-len)
          (setf (getf hci :acl-tx-num) le-num)
          t))))

(defun hci-allow-all-the-events (hci)
  "Allow controller to send us all the possible events"
  (hci-send-cmd (make-hci-cmd :set-event-mask
                              :events +u64-max+) hci)
  (hci-send-cmd (make-hci-cmd :le-set-event-mask
                              :events +u64-max+) hci))

(defun hci-set-random-address (address hci)
  (let ((status (hci-send-cmd (make-hci-cmd :set-random-address
                                            :address address) hci)))
    (if status
        (setf (getf hci :random-address) address))))

(with-hci hci *h2c-path* *c2h-path*
  (format t "================ enter ===============~%")
  (hci-reset hci)
  (hci-read-buffer-size hci)
  (hci-allow-all-the-events hci)
  (hci-set-random-address #xC1234567890A hci)

  (format t "HCI: ~A~%" hci)
  (format t "================ exit ===============~%")
  )
