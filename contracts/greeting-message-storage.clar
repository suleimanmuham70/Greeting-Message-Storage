(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-invalid-message (err u101))
(define-constant err-message-too-long (err u102))
(define-constant err-message-not-found (err u103))
(define-constant err-unauthorized (err u104))

(define-data-var total-messages uint u0)
(define-data-var max-message-length uint u280)
(define-data-var is-contract-active bool true)

(define-map user-greetings principal 
  {
    message: (string-utf8 280),
    timestamp: uint,
    message-count: uint
  })

(define-map message-history uint 
  {
    sender: principal,
    message: (string-utf8 280),
    timestamp: uint,
    message-id: uint
  })

(define-map user-message-counts principal uint)

(define-public (set-greeting (message (string-utf8 280)))
  (let 
    (
      (sender tx-sender)
      (current-block stacks-block-height)
      (current-count (default-to u0 (map-get? user-message-counts sender)))
      (total-count (var-get total-messages))
      (message-length (len message))
    )
    (asserts! (var-get is-contract-active) err-unauthorized)
    (asserts! (> message-length u0) err-invalid-message)
    (asserts! (<= message-length (var-get max-message-length)) err-message-too-long)
    
    (map-set user-greetings sender 
      {
        message: message,
        timestamp: current-block,
        message-count: (+ current-count u1)
      })
    
    (map-set message-history (+ total-count u1)
      {
        sender: sender,
        message: message,
        timestamp: current-block,
        message-id: (+ total-count u1)
      })
    
    (map-set user-message-counts sender (+ current-count u1))
    (var-set total-messages (+ total-count u1))
    
    (print {
      event: "greeting-set",
      sender: sender,
      message: message,
      timestamp: current-block,
      message-id: (+ total-count u1)
    })
    
    (ok (+ total-count u1))
  ))

(define-public (update-greeting (new-message (string-utf8 280)))
  (let 
    (
      (sender tx-sender)
      (current-block stacks-block-height)
      (existing-greeting (map-get? user-greetings sender))
      (message-length (len new-message))
    )
    (asserts! (var-get is-contract-active) err-unauthorized)
    (asserts! (> message-length u0) err-invalid-message)
    (asserts! (<= message-length (var-get max-message-length)) err-message-too-long)
    (asserts! (is-some existing-greeting) err-message-not-found)
    
    (let ((current-data (unwrap-panic existing-greeting)))
      (map-set user-greetings sender 
        {
          message: new-message,
          timestamp: current-block,
          message-count: (get message-count current-data)
        })
      
      (print {
        event: "greeting-updated",
        sender: sender,
        old-message: (get message current-data),
        new-message: new-message,
        timestamp: current-block
      })
      
      (ok true)
    )))

(define-public (delete-greeting)
  (let ((sender tx-sender))
    (asserts! (var-get is-contract-active) err-unauthorized)
    (asserts! (is-some (map-get? user-greetings sender)) err-message-not-found)
    
    (map-delete user-greetings sender)
    
    (print {
      event: "greeting-deleted",
      sender: sender,
      timestamp: stacks-block-height
    })
    
    (ok true)
  ))

(define-public (set-max-message-length (new-length uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (and (> new-length u0) (<= new-length u280)) err-invalid-message)
    
    (var-set max-message-length new-length)
    
    (print {
      event: "max-length-updated",
      new-length: new-length,
      updated-by: tx-sender
    })
    
    (ok new-length)
  ))

(define-public (toggle-contract-status)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    (let ((current-status (var-get is-contract-active)))
      (var-set is-contract-active (not current-status))
      
      (print {
        event: "contract-status-changed",
        new-status: (not current-status),
        changed-by: tx-sender
      })
      
      (ok (not current-status))
    )))

(define-read-only (get-greeting (user principal))
  (map-get? user-greetings user))

(define-read-only (get-user-message-count (user principal))
  (default-to u0 (map-get? user-message-counts user)))

(define-read-only (get-message-by-id (message-id uint))
  (map-get? message-history message-id))

(define-read-only (get-total-messages)
  (var-get total-messages))

(define-read-only (get-max-message-length)
  (var-get max-message-length))

(define-read-only (is-active)
  (var-get is-contract-active))

(define-read-only (get-contract-info)
  {
    total-messages: (var-get total-messages),
    max-message-length: (var-get max-message-length),
    is-active: (var-get is-contract-active),
    owner: contract-owner
  })

(define-public (get-multiple-greetings (users (list 20 principal)))
  (ok (map get-greeting users)))

(define-public (bulk-set-greetings (messages (list 10 {user: principal, message: (string-utf8 280)})))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (var-get is-contract-active) err-unauthorized)
    
    (let ((results (map process-bulk-message messages)))
      (print {
        event: "bulk-greetings-set",
        count: (len messages),
        processed-by: tx-sender
      })
      (ok results)
    )))

(define-private (process-bulk-message (data {user: principal, message: (string-utf8 280)}))
  (let 
    (
      (user (get user data))
      (message (get message data))
      (current-block stacks-block-height)
      (current-count (default-to u0 (map-get? user-message-counts user)))
      (total-count (var-get total-messages))
    )
    (map-set user-greetings user 
      {
        message: message,
        timestamp: current-block,
        message-count: (+ current-count u1)
      })
    
    (map-set message-history (+ total-count u1)
      {
        sender: user,
        message: message,
        timestamp: current-block,
        message-id: (+ total-count u1)
      })
    
    (map-set user-message-counts user (+ current-count u1))
    (var-set total-messages (+ total-count u1))
    
    {user: user, success: true}
  ))

(define-public (get-recent-messages (limit uint))
  (let 
    (
      (total-msgs (var-get total-messages))
      (safe-limit (if (> limit u10) u10 limit))
      (start-id (if (> total-msgs safe-limit) (- total-msgs safe-limit) u1))
    )
    (if (is-eq total-msgs u0)
      (ok (list))
      (ok (build-recent-messages-list start-id total-msgs safe-limit))
    )))

(define-private (build-recent-messages-list (start-id uint) (end-id uint) (count uint))
  (let 
    (
      (msg-1 (if (<= start-id end-id) (map-get? message-history start-id) none))
      (msg-2 (if (<= (+ start-id u1) end-id) (map-get? message-history (+ start-id u1)) none))
      (msg-3 (if (<= (+ start-id u2) end-id) (map-get? message-history (+ start-id u2)) none))
      (msg-4 (if (<= (+ start-id u3) end-id) (map-get? message-history (+ start-id u3)) none))
      (msg-5 (if (<= (+ start-id u4) end-id) (map-get? message-history (+ start-id u4)) none))
      (msg-6 (if (<= (+ start-id u5) end-id) (map-get? message-history (+ start-id u5)) none))
      (msg-7 (if (<= (+ start-id u6) end-id) (map-get? message-history (+ start-id u6)) none))
      (msg-8 (if (<= (+ start-id u7) end-id) (map-get? message-history (+ start-id u7)) none))
      (msg-9 (if (<= (+ start-id u8) end-id) (map-get? message-history (+ start-id u8)) none))
      (msg-10 (if (<= (+ start-id u9) end-id) (map-get? message-history (+ start-id u9)) none))
    )
    (unwrap-panic (as-max-len? 
      (concat 
        (if (is-some msg-1) (list (unwrap-panic msg-1)) (list))
        (concat
          (if (is-some msg-2) (list (unwrap-panic msg-2)) (list))
          (concat
            (if (is-some msg-3) (list (unwrap-panic msg-3)) (list))
            (concat
              (if (is-some msg-4) (list (unwrap-panic msg-4)) (list))
              (concat
                (if (is-some msg-5) (list (unwrap-panic msg-5)) (list))
                (concat
                  (if (is-some msg-6) (list (unwrap-panic msg-6)) (list))
                  (concat
                    (if (is-some msg-7) (list (unwrap-panic msg-7)) (list))
                    (concat
                      (if (is-some msg-8) (list (unwrap-panic msg-8)) (list))
                      (concat
                        (if (is-some msg-9) (list (unwrap-panic msg-9)) (list))
                        (if (is-some msg-10) (list (unwrap-panic msg-10)) (list))
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ) u10))
  ))

(define-read-only (search-greeting-by-sender (target-sender principal))
  (map-get? user-greetings target-sender))

(define-read-only (get-message-stats)
  (let 
    (
      (total (var-get total-messages))
      (max-length (var-get max-message-length))
      (active (var-get is-contract-active))
    )
    {
      total-messages: total,
      max-message-length: max-length,
      is-contract-active: active,
      last-message-id: (if (> total u0) total u0)
    }
  ))

(define-public (batch-get-messages (message-ids (list 5 uint)))
  (ok (map get-message-by-id message-ids)))

(define-read-only (get-user-stats (user principal))
  (let 
    (
      (greeting (map-get? user-greetings user))
      (count (default-to u0 (map-get? user-message-counts user)))
    )
    {
      has-greeting: (is-some greeting),
      message-count: count,
      greeting-data: greeting
    }
  ))

(define-public (clear-user-data)
  (let ((sender tx-sender))
    (asserts! (var-get is-contract-active) err-unauthorized)
    
    (map-delete user-greetings sender)
    (map-delete user-message-counts sender)
    
    (print {
      event: "user-data-cleared",
      user: sender,
      timestamp: stacks-block-height
    })
    
    (ok true)
  ))