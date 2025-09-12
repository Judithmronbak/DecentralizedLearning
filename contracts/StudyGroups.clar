;; Study Groups Formation System
;; Enables students to form study groups, share resources, coordinate meetings, and track progress
;; Integrates with the main learning platform for enhanced collaborative learning

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u300))
(define-constant err-not-found (err u301))
(define-constant err-unauthorized (err u302))
(define-constant err-group-full (err u303))
(define-constant err-already-member (err u304))
(define-constant err-not-member (err u305))
(define-constant err-invalid-group (err u306))
(define-constant err-meeting-conflict (err u307))

;; Group size limits
(define-constant max-group-size u8)
(define-constant min-group-size u2)

;; Study group definitions
(define-map study-groups
  uint ;; group-id
  {
    name: (string-ascii 50),
    course-id: uint,
    creator: principal,
    description: (string-ascii 200),
    max-members: uint,
    current-members: uint,
    privacy: (string-ascii 20), ;; "public", "private", "invite-only"
    status: (string-ascii 20), ;; "active", "completed", "inactive"
    created-at: uint,
    study-schedule: (string-ascii 100)
  }
)

;; Group membership tracking
(define-map group-members
  {group-id: uint, member: principal}
  {
    joined-at: uint,
    role: (string-ascii 20), ;; "creator", "moderator", "member"
    contribution-score: uint,
    last-active: uint,
    study-hours: uint
  }
)

;; Study sessions and meetings
(define-map study-sessions
  {group-id: uint, session-id: uint}
  {
    organizer: principal,
    topic: (string-ascii 100),
    scheduled-time: uint,
    duration: uint,
    location: (string-ascii 100), ;; virtual/physical location
    attendees: (list 8 principal),
    session-notes: (string-ascii 300),
    completed: bool
  }
)

;; Shared resources within groups
(define-map group-resources
  {group-id: uint, resource-id: uint}
  {
    contributor: principal,
    title: (string-ascii 50),
    resource-type: (string-ascii 30), ;; "notes", "document", "video", "link"
    content-hash: (string-ascii 64),
    description: (string-ascii 150),
    upload-date: uint,
    access-count: uint
  }
)

;; Group progress tracking
(define-map group-progress
  uint ;; group-id
  {
    collective-study-hours: uint,
    completed-topics: uint,
    group-average-score: uint,
    milestones-achieved: uint,
    collaboration-rating: uint,
    last-updated: uint
  }
)

;; Member achievements within groups
(define-map member-achievements
  {member: principal, group-id: uint}
  {
    contribution-points: uint,
    helpfulness-rating: uint,
    sessions-attended: uint,
    resources-shared: uint,
    leadership-score: uint
  }
)

;; Group discussion threads
(define-map group-discussions
  {group-id: uint, thread-id: uint}
  {
    author: principal,
    subject: (string-ascii 80),
    content: (string-ascii 250),
    posted-at: uint,
    replies-count: uint,
    last-reply: uint
  }
)

;; Counters
(define-data-var next-group-id uint u1)
(define-data-var next-session-id uint u1)
(define-data-var next-resource-id uint u1)
(define-data-var next-thread-id uint u1)

;; Create new study group
(define-public (create-study-group 
  (name (string-ascii 50))
  (course-id uint)
  (description (string-ascii 200))
  (max-members uint)
  (privacy (string-ascii 20))
  (study-schedule (string-ascii 100))
)
  (let ((group-id (var-get next-group-id)))
    (asserts! (and (>= max-members min-group-size) (<= max-members max-group-size)) err-invalid-group)
    
    ;; Create the group
    (map-set study-groups
      group-id
      {
        name: name,
        course-id: course-id,
        creator: tx-sender,
        description: description,
        max-members: max-members,
        current-members: u1,
        privacy: privacy,
        status: "active",
        created-at: stacks-block-height,
        study-schedule: study-schedule
      }
    )
    
    ;; Add creator as first member
    (map-set group-members
      {group-id: group-id, member: tx-sender}
      {
        joined-at: stacks-block-height,
        role: "creator",
        contribution-score: u0,
        last-active: stacks-block-height,
        study-hours: u0
      }
    )
    
    ;; Initialize group progress
    (map-set group-progress
      group-id
      {
        collective-study-hours: u0,
        completed-topics: u0,
        group-average-score: u0,
        milestones-achieved: u0,
        collaboration-rating: u80,
        last-updated: stacks-block-height
      }
    )
    
    ;; Initialize creator's achievements
    (map-set member-achievements
      {member: tx-sender, group-id: group-id}
      {
        contribution-points: u10, ;; Bonus for creating group
        helpfulness-rating: u0,
        sessions-attended: u0,
        resources-shared: u0,
        leadership-score: u10
      }
    )
    
    (var-set next-group-id (+ group-id u1))
    (ok group-id)
  )
)

;; Join existing study group
(define-public (join-study-group (group-id uint))
  (let (
    (group (unwrap! (map-get? study-groups group-id) err-not-found))
    (existing-member (map-get? group-members {group-id: group-id, member: tx-sender}))
  )
    (asserts! (is-none existing-member) err-already-member)
    (asserts! (< (get current-members group) (get max-members group)) err-group-full)
    (asserts! (is-eq (get status group) "active") err-invalid-group)
    
    ;; Add member to group
    (map-set group-members
      {group-id: group-id, member: tx-sender}
      {
        joined-at: stacks-block-height,
        role: "member",
        contribution-score: u0,
        last-active: stacks-block-height,
        study-hours: u0
      }
    )
    
    ;; Update group member count
    (map-set study-groups
      group-id
      (merge group {current-members: (+ (get current-members group) u1)})
    )
    
    ;; Initialize member achievements
    (map-set member-achievements
      {member: tx-sender, group-id: group-id}
      {
        contribution-points: u0,
        helpfulness-rating: u0,
        sessions-attended: u0,
        resources-shared: u0,
        leadership-score: u0
      }
    )
    
    (ok true)
  )
)

;; Schedule study session
(define-public (schedule-study-session 
  (group-id uint)
  (topic (string-ascii 100))
  (scheduled-time uint)
  (duration uint)
  (location (string-ascii 100))
)
  (let (
    (session-id (var-get next-session-id))
    (member (unwrap! (map-get? group-members {group-id: group-id, member: tx-sender}) err-not-member))
  )
    (asserts! (> scheduled-time stacks-block-height) err-meeting-conflict)
    (asserts! (> duration u0) err-invalid-group)
    
    (map-set study-sessions
      {group-id: group-id, session-id: session-id}
      {
        organizer: tx-sender,
        topic: topic,
        scheduled-time: scheduled-time,
        duration: duration,
        location: location,
        attendees: (list tx-sender),
        session-notes: "",
        completed: false
      }
    )
    
    ;; Update member's leadership score
    (let ((achievements (unwrap! (map-get? member-achievements {member: tx-sender, group-id: group-id}) err-not-member)))
      (map-set member-achievements
        {member: tx-sender, group-id: group-id}
        (merge achievements {leadership-score: (+ (get leadership-score achievements) u5)})
      )
    )
    
    (var-set next-session-id (+ session-id u1))
    (ok session-id)
  )
)

;; Attend study session
(define-public (attend-study-session (group-id uint) (session-id uint))
  (let (
    (session (unwrap! (map-get? study-sessions {group-id: group-id, session-id: session-id}) err-not-found))
    (member (unwrap! (map-get? group-members {group-id: group-id, member: tx-sender}) err-not-member))
    (current-attendees (get attendees session))
  )
    (asserts! (not (get completed session)) err-invalid-group)
    (asserts! (is-none (index-of current-attendees tx-sender)) err-already-member)
    
    ;; Add attendee to session
    (map-set study-sessions
      {group-id: group-id, session-id: session-id}
      (merge session {attendees: (unwrap! (as-max-len? (append current-attendees tx-sender) u8) err-group-full)})
    )
    
    ;; Update member's session attendance
    (let ((achievements (unwrap! (map-get? member-achievements {member: tx-sender, group-id: group-id}) err-not-member)))
      (map-set member-achievements
        {member: tx-sender, group-id: group-id}
        (merge achievements {sessions-attended: (+ (get sessions-attended achievements) u1)})
      )
    )
    
    (ok true)
  )
)

;; Share resource with group
(define-public (share-resource 
  (group-id uint)
  (title (string-ascii 50))
  (resource-type (string-ascii 30))
  (content-hash (string-ascii 64))
  (description (string-ascii 150))
)
  (let (
    (resource-id (var-get next-resource-id))
    (member (unwrap! (map-get? group-members {group-id: group-id, member: tx-sender}) err-not-member))
  )
    (map-set group-resources
      {group-id: group-id, resource-id: resource-id}
      {
        contributor: tx-sender,
        title: title,
        resource-type: resource-type,
        content-hash: content-hash,
        description: description,
        upload-date: stacks-block-height,
        access-count: u0
      }
    )
    
    ;; Update member's resource sharing score
    (let ((achievements (unwrap! (map-get? member-achievements {member: tx-sender, group-id: group-id}) err-not-member)))
      (map-set member-achievements
        {member: tx-sender, group-id: group-id}
        (merge achievements {
          resources-shared: (+ (get resources-shared achievements) u1),
          contribution-points: (+ (get contribution-points achievements) u5)
        })
      )
    )
    
    (var-set next-resource-id (+ resource-id u1))
    (ok resource-id)
  )
)

;; Complete study session with notes
(define-public (complete-study-session 
  (group-id uint) 
  (session-id uint) 
  (session-notes (string-ascii 300))
  (study-hours uint)
)
  (let (
    (session (unwrap! (map-get? study-sessions {group-id: group-id, session-id: session-id}) err-not-found))
    (member (unwrap! (map-get? group-members {group-id: group-id, member: tx-sender}) err-not-member))
    (progress (unwrap! (map-get? group-progress group-id) err-not-found))
  )
    (asserts! (is-eq (get organizer session) tx-sender) err-unauthorized)
    (asserts! (not (get completed session)) err-invalid-group)
    
    ;; Mark session as completed
    (map-set study-sessions
      {group-id: group-id, session-id: session-id}
      (merge session {session-notes: session-notes, completed: true})
    )
    
    ;; Update group progress
    (map-set group-progress
      group-id
      (merge progress {
        collective-study-hours: (+ (get collective-study-hours progress) study-hours),
        completed-topics: (+ (get completed-topics progress) u1),
        last-updated: stacks-block-height
      })
    )
    
    ;; Update organizer's member data
    (map-set group-members
      {group-id: group-id, member: tx-sender}
      (merge member {study-hours: (+ (get study-hours member) study-hours)})
    )
    
    (ok true)
  )
)

;; Start group discussion thread
(define-public (start-discussion 
  (group-id uint)
  (subject (string-ascii 80))
  (content (string-ascii 250))
)
  (let (
    (thread-id (var-get next-thread-id))
    (member (unwrap! (map-get? group-members {group-id: group-id, member: tx-sender}) err-not-member))
  )
    (map-set group-discussions
      {group-id: group-id, thread-id: thread-id}
      {
        author: tx-sender,
        subject: subject,
        content: content,
        posted-at: stacks-block-height,
        replies-count: u0,
        last-reply: stacks-block-height
      }
    )
    
    ;; Update member's contribution score
    (let ((achievements (unwrap! (map-get? member-achievements {member: tx-sender, group-id: group-id}) err-not-member)))
      (map-set member-achievements
        {member: tx-sender, group-id: group-id}
        (merge achievements {contribution-points: (+ (get contribution-points achievements) u2)})
      )
    )
    
    (var-set next-thread-id (+ thread-id u1))
    (ok thread-id)
  )
)

;; Leave study group
(define-public (leave-study-group (group-id uint))
  (let (
    (group (unwrap! (map-get? study-groups group-id) err-not-found))
    (member (unwrap! (map-get? group-members {group-id: group-id, member: tx-sender}) err-not-member))
  )
    (asserts! (not (is-eq (get role member) "creator")) err-unauthorized) ;; Creator cannot leave
    
    ;; Remove member from group
    (map-delete group-members {group-id: group-id, member: tx-sender})
    
    ;; Update group member count
    (map-set study-groups
      group-id
      (merge group {current-members: (- (get current-members group) u1)})
    )
    
    (ok true)
  )
)

;; Read-only functions
(define-read-only (get-study-group (group-id uint))
  (map-get? study-groups group-id)
)

(define-read-only (get-group-member (group-id uint) (member principal))
  (map-get? group-members {group-id: group-id, member: member})
)

(define-read-only (get-study-session (group-id uint) (session-id uint))
  (map-get? study-sessions {group-id: group-id, session-id: session-id})
)

(define-read-only (get-group-resource (group-id uint) (resource-id uint))
  (map-get? group-resources {group-id: group-id, resource-id: resource-id})
)

(define-read-only (get-group-progress (group-id uint))
  (map-get? group-progress group-id)
)

(define-read-only (get-member-achievements (member principal) (group-id uint))
  (map-get? member-achievements {member: member, group-id: group-id})
)

(define-read-only (get-group-discussion (group-id uint) (thread-id uint))
  (map-get? group-discussions {group-id: group-id, thread-id: thread-id})
)

(define-read-only (is-group-member (group-id uint) (member principal))
  (is-some (map-get? group-members {group-id: group-id, member: member}))
)


