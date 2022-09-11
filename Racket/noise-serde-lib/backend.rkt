#lang racket/base

(require ffi/unsafe/port
         racket/match
         "private/backend.rkt"
         "private/debug.rkt"
         "private/serde.rkt")

(provide
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
                  (log-noise-debug "response ~a: ~.v" id response-data)
                  (with-handlers ([exn:fail?
                                   (λ (e)
                                     ((error-display-handler)
                                      (format "backend/serve write failed: ~a" (exn-message e))
                                      e))])
                    (write-uvarint id server-out)
                    (write-field response-type response-data server-out)
                    (flush-output server-out))
                  (loop)]
                 [msg
                  (log-warning "backend/serve invalid message: ~e" msg)])))
            (handle-evt
             server-in
             (lambda (in)
               (with-handlers ([exn:fail? (λ (e)
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
                 (log-noise-debug "request ~a: ~.v" req-id (cons rpc-name args))
                 (define request-cust
                   (make-custodian))
                 (define request-thd
                   (parameterize ([current-custodian request-cust])
                     (thread
                      (lambda ()
                        (define response-data
                          (apply handler args))
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
