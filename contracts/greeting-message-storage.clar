(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-invalid-message (err u101))
(define-constant err-message-too-long (err u102))
(define-constant err-message-not-found (err u103))
(define-constant err-unauthorized (err u104))
(define-constant err-already-voted (err u105))
(define-constant err-cannot-vote-own-message (err u106))
(define-constant err-invalid-tip-amount (err u107))
(define-constant err-cannot-tip-own-message (err u108))
(define-constant err-transfer-failed (err u109))
(define-constant err-no-tips-to-claim (err u110))

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

(define-map message-tips uint 
  {
    total-tips: uint,
    tip-count: uint,
    top-tipper: (optional principal),
    top-tip-amount: uint
  })

(define-map user-tips-sent principal uint)

(define-map user-tips-received principal uint)

(define-map creator-pending-tips principal uint)

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
    
    (map-set message-tips (+ total-count u1)
      {
        total-tips: u0,
        tip-count: u0,
        top-tipper: none,
        top-tip-amount: u0
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

(define-constant max-bookmarks-per-user u20)
(define-constant err-bookmark-exists (err u111))
(define-constant err-bookmark-not-found (err u112))
(define-constant err-bookmark-limit-reached (err u113))

(define-data-var total-bookmarks uint u0)

(define-map user-bookmarks {user: principal, message-id: uint} bool)
(define-map user-bookmark-count principal uint)
(define-map bookmark-history uint {user: principal, message-id: uint, timestamp: uint})

(define-public (bookmark-message (message-id uint))
  (let 
    (
      (caller tx-sender)
      (message-record (map-get? message-history message-id))
      (existing (map-get? user-bookmarks {user: caller, message-id: message-id}))
      (count (default-to u0 (map-get? user-bookmark-count caller)))
      (limit max-bookmarks-per-user)
    )
    (asserts! (var-get is-contract-active) err-unauthorized)
    (asserts! (is-some message-record) err-message-not-found)
    (asserts! (is-none existing) err-bookmark-exists)
    (asserts! (< count limit) err-bookmark-limit-reached)
    (map-set user-bookmarks {user: caller, message-id: message-id} true)
    (map-set user-bookmark-count caller (+ count u1))
    (let 
      (
        (total (var-get total-bookmarks))
        (next-id (+ total u1))
        (ts stacks-block-height)
      )
      (map-set bookmark-history next-id {user: caller, message-id: message-id, timestamp: ts})
      (var-set total-bookmarks next-id)
      (print {event: "message-bookmarked", user: caller, message-id: message-id, bookmark-id: next-id})
      (ok true)
    )
  ))

(define-public (remove-bookmark (message-id uint))
  (let 
    (
      (caller tx-sender)
      (existing (map-get? user-bookmarks {user: caller, message-id: message-id}))
      (count (default-to u0 (map-get? user-bookmark-count caller)))
    )
    (asserts! (var-get is-contract-active) err-unauthorized)
    (asserts! (is-some existing) err-bookmark-not-found)
    (map-delete user-bookmarks {user: caller, message-id: message-id})
    (map-set user-bookmark-count caller (if (> count u0) (- count u1) u0))
    (print {event: "bookmark-removed", user: caller, message-id: message-id})
    (ok true)
  ))

(define-read-only (is-bookmarked (user principal) (message-id uint))
  (map-get? user-bookmarks {user: user, message-id: message-id}))

(define-read-only (get-user-bookmark-count (user principal))
  (default-to u0 (map-get? user-bookmark-count user)))

(define-read-only (get-recent-bookmarks (user principal) (limit uint))
  (let 
    (
      (total (var-get total-bookmarks))
      (safe-limit (if (> limit u10) u10 limit))
      (start (if (> total safe-limit) (- total safe-limit) u1))
    )
    (if (is-eq total u0)
      (ok (list))
      (ok (build-recent-bookmarks-list user start total))
    )))

(define-private (build-recent-bookmarks-list (user principal) (start uint) (end uint))
  (let 
    (
      (b1 (if (<= start end) (map-get? bookmark-history start) none))
      (b2 (if (<= (+ start u1) end) (map-get? bookmark-history (+ start u1)) none))
      (b3 (if (<= (+ start u2) end) (map-get? bookmark-history (+ start u2)) none))
      (b4 (if (<= (+ start u3) end) (map-get? bookmark-history (+ start u3)) none))
      (b5 (if (<= (+ start u4) end) (map-get? bookmark-history (+ start u4)) none))
      (b6 (if (<= (+ start u5) end) (map-get? bookmark-history (+ start u5)) none))
      (b7 (if (<= (+ start u6) end) (map-get? bookmark-history (+ start u6)) none))
      (b8 (if (<= (+ start u7) end) (map-get? bookmark-history (+ start u7)) none))
      (b9 (if (<= (+ start u8) end) (map-get? bookmark-history (+ start u8)) none))
      (b10 (if (<= (+ start u9) end) (map-get? bookmark-history (+ start u9)) none))
    )
    (unwrap-panic (as-max-len?
      (concat 
        (if (and (is-some b1) (is-eq user (get user (unwrap-panic b1)))) (list (unwrap-panic b1)) (list))
        (concat
          (if (and (is-some b2) (is-eq user (get user (unwrap-panic b2)))) (list (unwrap-panic b2)) (list))
          (concat
            (if (and (is-some b3) (is-eq user (get user (unwrap-panic b3)))) (list (unwrap-panic b3)) (list))
            (concat
              (if (and (is-some b4) (is-eq user (get user (unwrap-panic b4)))) (list (unwrap-panic b4)) (list))
              (concat
                (if (and (is-some b5) (is-eq user (get user (unwrap-panic b5)))) (list (unwrap-panic b5)) (list))
                (concat
                  (if (and (is-some b6) (is-eq user (get user (unwrap-panic b6)))) (list (unwrap-panic b6)) (list))
                  (concat
                    (if (and (is-some b7) (is-eq user (get user (unwrap-panic b7)))) (list (unwrap-panic b7)) (list))
                    (concat
                      (if (and (is-some b8) (is-eq user (get user (unwrap-panic b8)))) (list (unwrap-panic b8)) (list))
                      (concat
                        (if (and (is-some b9) (is-eq user (get user (unwrap-panic b9)))) (list (unwrap-panic b9)) (list))
                        (if (and (is-some b10) (is-eq user (get user (unwrap-panic b10)))) (list (unwrap-panic b10)) (list))
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
    
    (map-set message-tips (+ total-count u1)
      {
        total-tips: u0,
        tip-count: u0,
        top-tipper: none,
        top-tip-amount: u0
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

(define-public (tip-message (message-id uint) (amount uint))
  (let 
    (
      (tipper tx-sender)
      (message-data (map-get? message-history message-id))
      (current-tips (default-to {total-tips: u0, tip-count: u0, top-tipper: none, top-tip-amount: u0} (map-get? message-tips message-id)))
    )
    (asserts! (var-get is-contract-active) err-unauthorized)
    (asserts! (is-some message-data) err-message-not-found)
    (asserts! (> amount u0) err-invalid-tip-amount)
    
    (let 
      (
        (message-creator (get sender (unwrap-panic message-data)))
        (tipper-sent (default-to u0 (map-get? user-tips-sent tipper)))
        (creator-received (default-to u0 (map-get? user-tips-received message-creator)))
        (creator-pending (default-to u0 (map-get? creator-pending-tips message-creator)))
      )
      (asserts! (not (is-eq tipper message-creator)) err-cannot-tip-own-message)
      
      (match (stx-transfer? amount tipper (as-contract tx-sender))
        success
          (let 
            (
              (new-total-tips (+ (get total-tips current-tips) amount))
              (new-tip-count (+ (get tip-count current-tips) u1))
              (is-top-tip (> amount (get top-tip-amount current-tips)))
            )
            
            (map-set message-tips message-id
              {
                total-tips: new-total-tips,
                tip-count: new-tip-count,
                top-tipper: (if is-top-tip (some tipper) (get top-tipper current-tips)),
                top-tip-amount: (if is-top-tip amount (get top-tip-amount current-tips))
              })
            
            (map-set user-tips-sent tipper (+ tipper-sent amount))
            (map-set user-tips-received message-creator (+ creator-received amount))
            (map-set creator-pending-tips message-creator (+ creator-pending amount))
            
            (print {
              event: "message-tipped",
              tipper: tipper,
              message-id: message-id,
              creator: message-creator,
              amount: amount,
              total-tips: new-total-tips
            })
            
            (ok true)
          )
        error err-transfer-failed
      )
    )))

(define-public (claim-tips)
  (let 
    (
      (creator tx-sender)
      (pending-amount (default-to u0 (map-get? creator-pending-tips creator)))
    )
    (asserts! (var-get is-contract-active) err-unauthorized)
    (asserts! (> pending-amount u0) err-no-tips-to-claim)
    
    (match (as-contract (stx-transfer? pending-amount tx-sender creator))
      success
        (begin
          (map-set creator-pending-tips creator u0)
          
          (print {
            event: "tips-claimed",
            creator: creator,
            amount: pending-amount
          })
          
          (ok pending-amount)
        )
      error err-transfer-failed
    )))

(define-public (tip-multiple-messages (tips (list 5 {message-id: uint, amount: uint})))
  (begin
    (asserts! (var-get is-contract-active) err-unauthorized)
    
    (let ((results (map process-single-tip tips)))
      (print {
        event: "multiple-tips-sent",
        tipper: tx-sender,
        count: (len tips)
      })
      (ok results)
    )))

(define-private (process-single-tip (tip-data {message-id: uint, amount: uint}))
  (let 
    (
      (msg-id (get message-id tip-data))
      (tip-amount (get amount tip-data))
      (tipper tx-sender)
      (message-data (map-get? message-history msg-id))
    )
    (if (and (is-some message-data) (> tip-amount u0))
      (let 
        (
          (creator (get sender (unwrap-panic message-data)))
          (current-tips (default-to {total-tips: u0, tip-count: u0, top-tipper: none, top-tip-amount: u0} (map-get? message-tips msg-id)))
        )
        (if (not (is-eq tipper creator))
          (match (stx-transfer? tip-amount tipper (as-contract tx-sender))
            success
              (begin
                (map-set message-tips msg-id
                  {
                    total-tips: (+ (get total-tips current-tips) tip-amount),
                    tip-count: (+ (get tip-count current-tips) u1),
                    top-tipper: (get top-tipper current-tips),
                    top-tip-amount: (get top-tip-amount current-tips)
                  })
                
                (map-set creator-pending-tips creator 
                  (+ (default-to u0 (map-get? creator-pending-tips creator)) tip-amount))
                
                {message-id: msg-id, success: true}
              )
            error {message-id: msg-id, success: false}
          )
          {message-id: msg-id, success: false}
        )
      )
      {message-id: msg-id, success: false}
    )))

(define-read-only (get-message-tips (message-id uint))
  (map-get? message-tips message-id))

(define-read-only (get-user-tips-sent (user principal))
  (default-to u0 (map-get? user-tips-sent user)))

(define-read-only (get-user-tips-received (user principal))
  (default-to u0 (map-get? user-tips-received user)))

(define-read-only (get-pending-tips (creator principal))
  (default-to u0 (map-get? creator-pending-tips creator)))

(define-read-only (get-message-with-tips-and-votes (message-id uint))
  (let 
    (
      (message-data (map-get? message-history message-id))
      (vote-data (map-get? message-votes message-id))
      (tip-data (map-get? message-tips message-id))
    )
    {
      message: message-data,
      votes: vote-data,
      tips: tip-data
    }
  ))

(define-read-only (get-user-tipping-stats (user principal))
  (let 
    (
      (sent (default-to u0 (map-get? user-tips-sent user)))
      (received (default-to u0 (map-get? user-tips-received user)))
      (pending (default-to u0 (map-get? creator-pending-tips user)))
    )
    {
      total-tips-sent: sent,
      total-tips-received: received,
      pending-tips: pending,
      net-tips: (if (>= received sent) (- received sent) (- sent received)),
      is-net-receiver: (>= received sent)
    }
  ))

(define-public (get-top-tipped-messages (limit uint))
  (let 
    (
      (total-msgs (var-get total-messages))
      (safe-limit (if (> limit u5) u5 limit))
    )
    (if (is-eq total-msgs u0)
      (ok (list))
      (ok (get-highest-tipped-messages total-msgs safe-limit))
    )))

(define-private (get-highest-tipped-messages (message-count uint) (limit uint))
  (let 
    (
      (msg-1-data (get-message-tip-data u1))
      (msg-2-data (get-message-tip-data u2))
      (msg-3-data (get-message-tip-data u3))
      (msg-4-data (get-message-tip-data u4))
      (msg-5-data (get-message-tip-data u5))
    )
    (filter is-valid-tip-message 
      (list msg-1-data msg-2-data msg-3-data msg-4-data msg-5-data)
    )
  ))

(define-private (get-message-tip-data (message-id uint))
  (let 
    (
      (message-data (map-get? message-history message-id))
      (tip-data (default-to {total-tips: u0, tip-count: u0, top-tipper: none, top-tip-amount: u0} (map-get? message-tips message-id)))
    )
    {
      message-id: message-id,
      total-tips: (get total-tips tip-data),
      message: message-data
    }
  ))

(define-private (is-valid-tip-message (data {message-id: uint, total-tips: uint, message: (optional {sender: principal, message: (string-utf8 280), timestamp: uint, message-id: uint})}))
  (and (is-some (get message data)) (> (get total-tips data) u0)))

(define-read-only (get-platform-tipping-stats)
  (let 
    (
      (tip-1 (default-to {total-tips: u0, tip-count: u0, top-tipper: none, top-tip-amount: u0} (map-get? message-tips u1)))
      (tip-2 (default-to {total-tips: u0, tip-count: u0, top-tipper: none, top-tip-amount: u0} (map-get? message-tips u2)))
      (tip-3 (default-to {total-tips: u0, tip-count: u0, top-tipper: none, top-tip-amount: u0} (map-get? message-tips u3)))
      (tip-4 (default-to {total-tips: u0, tip-count: u0, top-tipper: none, top-tip-amount: u0} (map-get? message-tips u4)))
      (tip-5 (default-to {total-tips: u0, tip-count: u0, top-tipper: none, top-tip-amount: u0} (map-get? message-tips u5)))
    )
    {
      total-platform-tips: (+ (+ (+ (+ (get total-tips tip-1) (get total-tips tip-2)) (get total-tips tip-3)) (get total-tips tip-4)) (get total-tips tip-5)),
      total-tip-transactions: (+ (+ (+ (+ (get tip-count tip-1) (get tip-count tip-2)) (get tip-count tip-3)) (get tip-count tip-4)) (get tip-count tip-5)),
      sample-messages: {
        msg-1: tip-1,
        msg-2: tip-2,
        msg-3: tip-3,
        msg-4: tip-4,
        msg-5: tip-5
      }
    }
  ))
