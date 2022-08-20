#lang racket/base

(require ffi/unsafe/port
         racket/match
         "private/serde.rkt")

(provide
 serve)

(define (serve in-fd out-fd handler)
  (define cust (make-custodian))
  (define thd
    (parameterize ([current-custodian cust])
      (define server-in (unsafe-file-descriptor->port in-fd 'in '(read)))
      (define server-out (unsafe-file-descriptor->port out-fd 'out '(write)))
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
                 [`(response ,id ,data)
                  (with-handlers ([exn:fail? (λ (e)
                                               ((error-display-handler)
                                                (format "backend/serve write failed: ~a" (exn-message e))
                                                e))])
                    (write-response id data server-out)
                    (flush-output server-out))
                  (loop)]
                 [msg
                  (log-warning "backend/serve invalid message: ~e" msg)])))
            (handle-evt
             server-in
             (lambda (in)
               (define-values (id data)
                 (parameterize-break #f
                   (read-request in)))
               (define request-cust
                 (make-custodian))
               (define request-thd
                 (parameterize ([current-custodian request-cust])
                   (thread
                    (lambda ()
                      (define response-data
                        (handler data))
                      (thread-resume thd (current-thread))
                      (thread-send thd `(response ,id ,response-data))))))
               (thread
                (lambda ()
                  (thread-wait request-thd)
                  (custodian-shutdown-all request-cust)))
               (loop)))))))))
  (lambda ()
    (thread-resume thd (current-thread))
    (thread-send thd '(stop))
    (thread-wait thd)
    (custodian-shutdown-all cust)))

(define (read-request in)
  (values
   (read-uvarint in)
   (read-record in)))

(define (write-response id data out)
  (write-uvarint id out)
  (write-record data out))
