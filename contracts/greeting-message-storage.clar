(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-invalid-message (err u101))
(define-constant err-message-too-long (err u102))
(define-constant err-message-not-found (err u103))
(define-constant err-unauthorized (err u104))
(define-constant err-already-voted (err u105))
(define-constant err-cannot-vote-own-message (err u106))

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

(define-map message-votes uint 
  {
    upvotes: uint,
    downvotes: uint,
    score: int
  })

(define-map user-votes {user: principal, message-id: uint} bool)

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
    
    (map-set message-votes (+ total-count u1)
      {
        upvotes: u0,
        downvotes: u0,
        score: 0
      })
    
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
    
    (map-set message-votes (+ total-count u1)
      {
        upvotes: u0,
        downvotes: u0,
        score: 0
      })
    
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

(define-public (vote-message (message-id uint) (is-upvote bool))
  (let 
    (
      (voter tx-sender)
      (message-data (map-get? message-history message-id))
      (existing-vote (map-get? user-votes {user: voter, message-id: message-id}))
      (current-votes (default-to {upvotes: u0, downvotes: u0, score: 0} (map-get? message-votes message-id)))
    )
    (asserts! (var-get is-contract-active) err-unauthorized)
    (asserts! (is-some message-data) err-message-not-found)
    (asserts! (is-none existing-vote) err-already-voted)
    
    (let ((message-sender (get sender (unwrap-panic message-data))))
      (asserts! (not (is-eq voter message-sender)) err-cannot-vote-own-message)
      
      (let 
        (
          (new-upvotes (if is-upvote (+ (get upvotes current-votes) u1) (get upvotes current-votes)))
          (new-downvotes (if is-upvote (get downvotes current-votes) (+ (get downvotes current-votes) u1)))
          (new-score (if is-upvote (+ (get score current-votes) 1) (- (get score current-votes) 1)))
        )
        
        (map-set message-votes message-id
          {
            upvotes: new-upvotes,
            downvotes: new-downvotes,
            score: new-score
          })
        
        (map-set user-votes {user: voter, message-id: message-id} is-upvote)
        
        (print {
          event: "message-voted",
          voter: voter,
          message-id: message-id,
          is-upvote: is-upvote,
          new-score: new-score
        })
        
        (ok new-score)
      )
    )))

(define-public (change-vote (message-id uint) (is-upvote bool))
  (let 
    (
      (voter tx-sender)
      (message-data (map-get? message-history message-id))
      (existing-vote (map-get? user-votes {user: voter, message-id: message-id}))
      (current-votes (default-to {upvotes: u0, downvotes: u0, score: 0} (map-get? message-votes message-id)))
    )
    (asserts! (var-get is-contract-active) err-unauthorized)
    (asserts! (is-some message-data) err-message-not-found)
    (asserts! (is-some existing-vote) err-message-not-found)
    
    (let 
      (
        (old-vote (unwrap-panic existing-vote))
        (was-upvote old-vote)
      )
      (asserts! (not (is-eq was-upvote is-upvote)) err-already-voted)
      
      (let 
        (
          (new-upvotes 
            (if is-upvote 
              (+ (get upvotes current-votes) u1)
              (- (get upvotes current-votes) u1)))
          (new-downvotes 
            (if is-upvote 
              (- (get downvotes current-votes) u1)
              (+ (get downvotes current-votes) u1)))
          (new-score 
            (if is-upvote 
              (+ (get score current-votes) 2)
              (- (get score current-votes) 2)))
        )
        
        (map-set message-votes message-id
          {
            upvotes: new-upvotes,
            downvotes: new-downvotes,
            score: new-score
          })
        
        (map-set user-votes {user: voter, message-id: message-id} is-upvote)
        
        (print {
          event: "vote-changed",
          voter: voter,
          message-id: message-id,
          new-vote: is-upvote,
          new-score: new-score
        })
        
        (ok new-score)
      )
    )))

(define-public (remove-vote (message-id uint))
  (let 
    (
      (voter tx-sender)
      (message-data (map-get? message-history message-id))
      (existing-vote (map-get? user-votes {user: voter, message-id: message-id}))
      (current-votes (default-to {upvotes: u0, downvotes: u0, score: 0} (map-get? message-votes message-id)))
    )
    (asserts! (var-get is-contract-active) err-unauthorized)
    (asserts! (is-some message-data) err-message-not-found)
    (asserts! (is-some existing-vote) err-message-not-found)
    
    (let 
      (
        (was-upvote (unwrap-panic existing-vote))
        (new-upvotes (if was-upvote (- (get upvotes current-votes) u1) (get upvotes current-votes)))
        (new-downvotes (if was-upvote (get downvotes current-votes) (- (get downvotes current-votes) u1)))
        (new-score (if was-upvote (- (get score current-votes) 1) (+ (get score current-votes) 1)))
      )
      
      (map-set message-votes message-id
        {
          upvotes: new-upvotes,
          downvotes: new-downvotes,
          score: new-score
        })
      
      (map-delete user-votes {user: voter, message-id: message-id})
      
      (print {
        event: "vote-removed",
        voter: voter,
        message-id: message-id,
        new-score: new-score
      })
      
      (ok new-score)
    )))

(define-read-only (get-message-votes (message-id uint))
  (map-get? message-votes message-id))

(define-read-only (get-user-vote (user principal) (message-id uint))
  (map-get? user-votes {user: user, message-id: message-id}))

(define-read-only (get-message-with-votes (message-id uint))
  (let 
    (
      (message-data (map-get? message-history message-id))
      (vote-data (map-get? message-votes message-id))
    )
    {
      message: message-data,
      votes: vote-data
    }
  ))

(define-public (get-top-messages (limit uint))
  (let 
    (
      (total-msgs (var-get total-messages))
      (safe-limit (if (> limit u5) u5 limit))
    )
    (if (is-eq total-msgs u0)
      (ok (list))
      (ok (get-highest-scored-messages total-msgs safe-limit))
    )))

(define-private (get-highest-scored-messages (message-count uint) (limit uint))
  (let 
    (
      (msg-1-data (get-message-score-data u1))
      (msg-2-data (get-message-score-data u2))
      (msg-3-data (get-message-score-data u3))
      (msg-4-data (get-message-score-data u4))
      (msg-5-data (get-message-score-data u5))
    )
    (sort-messages-by-score 
      (filter is-valid-message-data 
        (list msg-1-data msg-2-data msg-3-data msg-4-data msg-5-data)
      )
    )
  ))

(define-private (get-message-score-data (message-id uint))
  (let 
    (
      (message-data (map-get? message-history message-id))
      (vote-data (default-to {upvotes: u0, downvotes: u0, score: 0} (map-get? message-votes message-id)))
    )
    {
      message-id: message-id,
      score: (get score vote-data),
      message: message-data
    }
  ))

(define-private (is-valid-message-data (data {message-id: uint, score: int, message: (optional {sender: principal, message: (string-utf8 280), timestamp: uint, message-id: uint})}))
  (is-some (get message data)))

(define-private (sort-messages-by-score (messages (list 5 {message-id: uint, score: int, message: (optional {sender: principal, message: (string-utf8 280), timestamp: uint, message-id: uint})})))
  messages)

(define-read-only (get-voting-stats)
  (let 
    (
      (total-msgs (var-get total-messages))
      (sample-votes-1 (default-to {upvotes: u0, downvotes: u0, score: 0} (map-get? message-votes u1)))
      (sample-votes-2 (default-to {upvotes: u0, downvotes: u0, score: 0} (map-get? message-votes u2)))
      (sample-votes-3 (default-to {upvotes: u0, downvotes: u0, score: 0} (map-get? message-votes u3)))
    )
    {
      total-messages-with-votes: total-msgs,
      total-upvotes: (+ (+ (get upvotes sample-votes-1) (get upvotes sample-votes-2)) (get upvotes sample-votes-3)),
      total-downvotes: (+ (+ (get downvotes sample-votes-1) (get downvotes sample-votes-2)) (get downvotes sample-votes-3)),
      sample-data: {
        msg-1: sample-votes-1,
        msg-2: sample-votes-2,
        msg-3: sample-votes-3
      }
    }
  ))