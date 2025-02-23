#lang racket/base

(require ffi/unsafe/port
         racket/match
         racket/port
         "private/backend.rkt"
         "private/callout.rkt"
         "private/debug.rkt"
         "private/serde.rkt"
         "unsafe/callout.rkt")

(provide
 define-callout
 define-rpc
 serve)

(define (serve in-fd out-fd)
  (define cust (make-custodian))
  (define thd
    (parameterize ([current-custodian cust])
      (define server-in
        (make-debug-input-port
         (unsafe-file-descriptor->port in-fd 'in '(read))))
      (define server-out
        (make-debug-output-port
         (unsafe-file-descriptor->port out-fd 'out '(write))))
      (thread/suspend-to-kill
       (lambda ()
         (let loop ()
           (sync
            (handle-evt
             (thread-receive-evt)
             (lambda (_)
               (match (thread-receive)
                 ['(stop)
                  (close-input-port server-in)
                  (close-output-port server-out)]
                 [`(response ,id ,response-type ,response-data)
                  (log-noise-debug "<~a: ~.v" id response-data)
                  (write-uvarint id server-out)
                  (if (exn:fail? response-data)
                      (write-error response-data server-out)
                      (write-data response-type response-data server-out))
                  (flush-output server-out)
                  (loop)]
                 [msg
                  (log-noise-warning "backend/serve invalid message: ~e" msg)])))
            (handle-evt
             server-in
             (lambda (in)
               (with-handlers ([exn:fail?
                                (lambda (e)
                                  ((error-display-handler)
                                   (format "backend/serve: ~a" (exn-message e))
                                   e))])
                 (define req-id (read-uvarint in))
                 (define rpc-id (read-uvarint in))
                 (match-define (rpc-info _id rpc-name rpc-args response-type handler)
                   (hash-ref rpc-infos rpc-id))
                 (define args
                   (for/list ([ra (in-list rpc-args)])
                     (read-field (rpc-arg-type ra) in)))
                 (log-noise-debug ">~a: ~.v" req-id (cons rpc-name args))
                 (define request-cust
                   (make-custodian))
                 (define request-thd
                   (parameterize ([current-custodian request-cust])
                     (thread
                      (lambda ()
                        (define response-data
                          (with-handlers ([exn:fail?
                                           (lambda (e)
                                             (begin0 e
                                               ((error-display-handler)
                                                (format "backend/handler: ~a" (exn-message e))
                                                e)))])
                            (apply handler args)))
                        (thread-resume thd (current-thread))
                        (thread-send thd `(response ,req-id ,response-type ,response-data))))))
                 (thread
                  (lambda ()
                    (thread-wait request-thd)
                    (custodian-shutdown-all request-cust))))
               (loop)))))))))
  (lambda ()
    (thread-resume thd (current-thread))
    (thread-send thd '(stop))
    (thread-wait thd)
    (custodian-shutdown-all cust)))

(define (write-error e out)
  (write-byte 0 out)
  (write-field String (~stacktrace e) out))

;; Use a background thread to write the data in order to try and catch
;; any errors that occur during writing without having to buffer writes
;; into Racket memory before sending them over to the client.
(define (write-data type data [out (current-output-port)])
  (define err-ch (make-channel))
  (define-values (pipe-in pipe-out)
    (make-pipe (* 1 1024 1024)))
  (define write-thd
    (thread
     (lambda ()
       (with-handlers ([exn:fail?
                        (lambda (e)
                          (close-output-port pipe-out)
                          (channel-put err-ch e))])
         (unless (eq? type Void)
           (write-field type data pipe-out))
         (close-output-port pipe-out)))))
  (let write-loop ([status-sent? #f])
    (sync
     (handle-evt
      err-ch
      (lambda (e)
        (when status-sent?
          ;; We've already written some response data, so we MUST crash
          ;; in order to avoid having our response be misinterpreted.
          (exit e))
        (write-error e out)))
     (handle-evt
      write-thd
      (lambda (_)
        (unless status-sent?
          (write-byte 1 out))
        (copy-port pipe-in out)))
     (handle-evt
      pipe-in
      (lambda (_)
        (unless status-sent?
          (write-byte 1 out))
        (copy-port pipe-in out)
        (write-loop #t)))))
  (close-input-port pipe-in))

(define (~stacktrace e)
  (call-with-output-string
   (lambda (out)
     (parameterize ([current-error-port out])
       ((error-display-handler)
        (exn-message e)
        e)))))

(module+ test
  (require rackunit)
  (define (response-bytes type data)
    (with-output-to-bytes
      (lambda ()
        (write-data type data))))
  (check-equal?
   (response-bytes UInt32 1)
   #"\1\0\0\0\1")
  (check-equal?
   (response-bytes String "hello")
   #"\1\nhello")
  (check-true
   (regexp-match?
    #rx#"string->bytes/utf-8: contract violation"
    (response-bytes String #f)))
  (check-equal?
   (response-bytes (Optional String) #f)
   #"\1\0"))


;; callout ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-rpc (install-callback [internal-with-id id : UVarint]
                              [and-addr addr : Varint])
  (define cbox (callout-info-cbox (hash-ref callout-infos id)))
  (callout-box-install! cbox addr))
