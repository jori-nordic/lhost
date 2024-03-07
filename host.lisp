;;;;;;;;;;;;; general utils

(defun make-range (max)
  (loop for number from 0 to (- max 1) collect number))

(defun extract-byte (number index)
  (ldb (byte 8 (* index 8)) number))

(defun make-uint (octets number)
  (loop for pos in (make-range octets)
        collect (extract-byte number pos)))

(make-uint 4 8000)

(defun u2b (type)
  (case type
     (:u8 1)
     (:u16 2)
     (:u32 4)
     (t (error "unknown type ~A" type))))

(defun make-c-int (type value)
  (make-uint (u2b type) value))

(make-c-int :u8 #xFF)
(make-c-int :u32 8000)

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

;;;;;;;;;;;;; babblesim PHY

(defconstant PB_MSG_WAIT #x01)
(defconstant PB_MSG_TERMINATE #xFFFE)

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
    (format t "read-bytes: ~A~%" b)
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

(defparameter *sim-time-us* 0)
(defun sim-wait (ms tx rx)
  (setf *sim-time-us* (+ *sim-time-us* (* ms 1000)))
  (format t "waiting until ~A us (delta ~A ms).." *sim-time-us* ms)

  (write-sequence (make-wait-cmd *sim-time-us*) tx)

  (read-bytes 4 rx)
  (format t "done~%"))

(defun sim-terminate (tx)
  (format t "term~%")
  (write-sequence (make-terminate-cmd) tx))

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

(defparameter *hci-cmds*
  '(:reset (#x0c03 nil)

    :write-default-data-length
    (#x2024
     (:tx-octets :u16
      :tx-time-us :u16))

    :read-buffer-size (#x2002 nil)
    ))

(getf *hci-cmds* :write-default-data-length)

(defun make-hci-cmd-param (name value spec)
  (let ((type (getf spec name)))

    (unless type
      (error (format nil "Unknown param: ~A" name)))

    (make-c-struct-member (list name type value))))

;; test it
(make-hci-cmd-param
 :tx-time-us 1000
 (nth 1 (getf *hci-cmds* :write-default-data-length)))

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
(defun hci-cmd-write-default-data-length (tx-octets tx-time-us)
  (make-hci-cmd :write-default-data-length
                :tx-octets tx-octets
                :tx-time-us tx-time-us))

(format t "~A~%" (hci-cmd-write-default-data-length 200 1000))

(defun hci-reset () (make-hci-cmd :reset))

(format nil "~x" (make-h4 :cmd (hci-reset)))

;; typing sucks
(defun send (type payload stream)
  (write-sequence (make-h4 type payload) stream))

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
    (format t "read op~%")
    (setf packet (read-bytes 1 stream))
    (format t "packet: ~A~%" packet)

    ;; parse h4 opcode
    (setf opcode (h4-parse-opcode packet))

    ;; read HCI packet header
    (format t "read header~%")
    (setf header (read-bytes (hci-header-len opcode) stream))
    (setf packet (append packet header))
    (format t "packet: ~A~%" packet)

    ;; parse hci-packet-length from header & read payload
    (format t "read payload~%")
    (setf payload (read-bytes
                   (hci-parse-len opcode packet)
                   stream))
    (setf packet (append packet payload))
    (format t "full packet: ~X~%" packet)

    ;; return raw packet and parsed packet
    (list
     :opcode opcode
     :payload payload
     :raw packet)))

(defun receive (stream)
  "Receive a single HCI packet"
  ;; initial implementation is H4
  (rx-h4 stream))

;;;;;;;;;;;;; host

(defun make-hci-dev ()
  '(:acl-tx-size 0
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

(with-open-stream (rx (open-simplex-fd *bs-rx-path* nil))
  (with-open-stream (tx (open-simplex-fd *bs-tx-path* t))

    (setf *sim-time-us* 0)

    (format t "connected to PHY (rx ~A tx ~A)~%"
            (sb-posix:file-descriptor rx)
            (sb-posix:file-descriptor tx))

    (with-open-stream (h2c (open-simplex-fd *h2c-path* t))
      (with-open-stream (c2h (open-simplex-fd *c2h-path* nil))

        (defparameter *hci-dev* (make-hci-dev))

        ;; Send reset
        (format t "TX: ~x~%" (make-h4 :cmd (hci-reset)))
        (send :cmd (hci-reset) h2c)
        (sim-wait 1000 tx rx)

        ;; Wait for reply
        (format t "RX: ~x~%" (receive c2h))

        ;; Read ACL buffer size
        (format t "TX bufsize~%")
        (send :cmd (make-hci-cmd :read-buffer-size) h2c)
        (sim-wait 1000 tx rx)

        ;; Wait for reply
        (format t "RX: ~x~%" (receive c2h))

        (sim-wait 100 tx rx)

        ))))
