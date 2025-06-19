;; DecentralizedLearning - Educational Content Marketplace
;; Core features: Course management, enrollment, achievements, and credentials

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-not-enrolled (err u103))
(define-constant err-already-completed (err u104))


(define-constant platform-fee-percentage u10)
(define-constant err-insufficient-payment (err u106))
(define-constant err-insufficient-balance (err u107))
(define-constant err-payment-failed (err u108))

(define-map InstructorBalances
    { instructor: principal }
    { balance: uint }
)

(define-map PlatformRevenue
    { platform: bool }
    { total-revenue: uint }
)

(define-map CoursePayments
    { student: principal, course-id: uint }
    {
        amount-paid: uint,
        payment-date: uint,
        instructor-share: uint,
        platform-share: uint
    }
)

(define-data-var platform-balance uint u0)

;; Data Maps
(define-map Courses 
    { course-id: uint }
    {
        title: (string-ascii 50),
        instructor: principal,
        price: uint,
        max-students: uint,
        current-students: uint,
        active: bool
    }
)

(define-map StudentEnrollments
    { student: principal, course-id: uint }
    {
        enrolled-at: uint,
        completed: bool,
        progress: uint
    }
)

(define-map Credentials
    { student: principal, course-id: uint }
    {
        issued-at: uint,
        credential-hash: (string-ascii 64),
        verified: bool
    }
)

(define-map InstructorStats
    { instructor: principal }
    {
        total-courses: uint,
        total-students: uint,
        rating: uint
    }
)

;; Data Variables
(define-data-var next-course-id uint u1)

;; Public Functions

;; Create a new course
(define-public (create-course (title (string-ascii 50)) (price uint) (max-students uint))
    (let
        ((course-id (var-get next-course-id)))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-insert Courses
            { course-id: course-id }
            {
                title: title,
                instructor: tx-sender,
                price: price,
                max-students: max-students,
                current-students: u0,
                active: true
            }
        )
        (var-set next-course-id (+ course-id u1))
        (ok course-id)
    )
)

;; Enroll in a course
(define-public (enroll-in-course (course-id uint))
    (let
        ((course (unwrap! (get-course-by-id course-id) err-not-found))
         (current-stacks-block-height stacks-block-height))
        
        (asserts! (get active course) err-not-found)
        (asserts! (< (get current-students course) (get max-students course)) err-already-exists)
        
        (map-insert StudentEnrollments
            { student: tx-sender, course-id: course-id }
            {
                enrolled-at: current-stacks-block-height,
                completed: false,
                progress: u0
            }
        )
        
        (map-set Courses
            { course-id: course-id }
            (merge course { current-students: (+ (get current-students course) u1) })
        )
        
        (ok true)
    )
)

;; Update course progress
(define-public (update-progress (course-id uint) (new-progress uint))
    (let
        ((enrollment (unwrap! (get-enrollment tx-sender course-id) err-not-enrolled)))
        
        (asserts! (not (get completed enrollment)) err-already-completed)
        (asserts! (<= new-progress u100) err-already-exists)
        
        (map-set StudentEnrollments
            { student: tx-sender, course-id: course-id }
            (merge enrollment { progress: new-progress })
        )
        
        (ok true)
    )
)

;; Complete course and issue credential
(define-public (complete-course (course-id uint) (credential-hash (string-ascii 64)))
    (let
        ((enrollment (unwrap! (get-enrollment tx-sender course-id) err-not-enrolled)))
        
        (asserts! (not (get completed enrollment)) err-already-completed)
        (asserts! (>= (get progress enrollment) u100) err-not-found)
        
        (map-set StudentEnrollments
            { student: tx-sender, course-id: course-id }
            (merge enrollment { completed: true })
        )
        
        (map-insert Credentials
            { student: tx-sender, course-id: course-id }
            {
                issued-at: stacks-block-height,
                credential-hash: credential-hash,
                verified: true
            }
        )
        
        (ok true)
    )
)

;; Read-only Functions

;; Get course details
(define-read-only (get-course-by-id (course-id uint))
    (map-get? Courses { course-id: course-id })
)

;; Get student enrollment details
(define-read-only (get-enrollment (student principal) (course-id uint))
    (map-get? StudentEnrollments { student: student, course-id: course-id })
)

;; Get credential details
(define-read-only (get-credential (student principal) (course-id uint))
    (map-get? Credentials { student: student, course-id: course-id })
)

;; Private Functions

;; Verify if student is enrolled
(define-private (is-enrolled (student principal) (course-id uint))
    (is-some (get-enrollment student course-id))
)

;; Check if course exists and is active
(define-private (is-active-course (course-id uint))
    (match (get-course-by-id course-id)
        course (get active course)
        false
    )
)



(define-map CourseReviews 
    { course-id: uint, reviewer: principal }
    {
        rating: uint,
        review-text: (string-ascii 280),
        review-date: uint
    }
)

(define-public (add-course-review (course-id uint) (rating uint) (review-text (string-ascii 280)))
    (let (
        (enrollment (unwrap! (get-enrollment tx-sender course-id) err-not-enrolled))
        (current-time stacks-block-height)
    )
        (asserts! (>= rating u1) err-not-found)
        (asserts! (<= rating u5) err-not-found)
        (asserts! (get completed enrollment) err-not-enrolled)
        
        (map-insert CourseReviews
            { course-id: course-id, reviewer: tx-sender }
            {
                rating: rating,
                review-text: review-text,
                review-date: current-time
            }
        )
        (ok true)
    )
)



(define-map CourseCategories
    { category-id: uint }
    { 
        name: (string-ascii 50),
        description: (string-ascii 200)
    }
)

(define-map CourseCategoryMapping
    { course-id: uint }
    { category-id: uint }
)

(define-data-var next-category-id uint u1)

(define-public (create-category (name (string-ascii 50)) (description (string-ascii 200)))
    (let ((category-id (var-get next-category-id)))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-insert CourseCategories
            { category-id: category-id }
            {
                name: name,
                description: description
            }
        )
        (var-set next-category-id (+ category-id u1))
        (ok category-id)
    )
)


(define-map CoursePrerequisites
    { course-id: uint }
    { required-courses: (list 10 uint) }
)

(define-public (set-prerequisites (course-id uint) (prerequisites (list 10 uint)))
    (let ((course (unwrap! (get-course-by-id course-id) err-not-found)))
        (asserts! (is-eq tx-sender (get instructor course)) err-owner-only)
        (map-set CoursePrerequisites
            { course-id: course-id }
            { required-courses: prerequisites }
        )
        (ok true)
    )
)

(define-private (check-prerequisites (student principal) (prerequisites (list 10 uint)))
    (fold check-completion prerequisites true)
)

(define-private (check-completion (course-id uint) (prev-result bool))
    (if prev-result
        (match (get-enrollment tx-sender course-id)
            enrollment (get completed enrollment)
            false
        )
        false
    )
)


(define-map CourseModules 
    { course-id: uint, module-id: uint }
    {
        title: (string-ascii 50),
        content-hash: (string-ascii 64),
        duration: uint,
        order: uint
    }
)

(define-map ModuleProgress
    { student: principal, course-id: uint, module-id: uint }
    {
        completed: bool,
        completion-date: uint
    }
)

(define-public (add-module 
    (course-id uint) 
    (module-id uint) 
    (title (string-ascii 50)) 
    (content-hash (string-ascii 64))
    (duration uint)
    (order uint)
)
    (let ((course (unwrap! (get-course-by-id course-id) err-not-found)))
        (asserts! (is-eq tx-sender (get instructor course)) err-owner-only)
        (map-insert CourseModules
            { course-id: course-id, module-id: module-id }
            {
                title: title,
                content-hash: content-hash,
                duration: duration,
                order: order
            }
        )
        (ok true)
    )
)



(define-map Badges
    { badge-id: uint }
    {
        name: (string-ascii 50),
        description: (string-ascii 200),
        image-url: (string-ascii 200)
    }
)

(define-map StudentBadges
    { student: principal, badge-id: uint }
    {
        earned-at: uint,
        course-id: uint
    }
)

(define-data-var next-badge-id uint u1)

(define-public (create-badge (name (string-ascii 50)) (description (string-ascii 200)) (image-url (string-ascii 200)))
    (let ((badge-id (var-get next-badge-id)))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-insert Badges
            { badge-id: badge-id }
            {
                name: name,
                description: description,
                image-url: image-url
            }
        )
        (var-set next-badge-id (+ badge-id u1))
        (ok badge-id)
    )
)


(define-map ForumPosts
    { post-id: uint, course-id: uint }
    {
        author: principal,
        content: (string-ascii 500),
        posted-at: uint,
        parent-post-id: (optional uint)
    }
)

(define-data-var next-post-id uint u1)

(define-public (create-forum-post (course-id uint) (content (string-ascii 500)) (parent-post-id (optional uint)))
    (let (
        (post-id (var-get next-post-id))
        (enrollment (unwrap! (get-enrollment tx-sender course-id) err-not-enrolled))
    )
        (map-insert ForumPosts
            { post-id: post-id, course-id: course-id }
            {
                author: tx-sender,
                content: content,
                posted-at: stacks-block-height,
                parent-post-id: parent-post-id
            }
        )
        (var-set next-post-id (+ post-id u1))
        (ok post-id)
    )
)

(define-constant err-refund-period-expired (err u105))
(define-constant refund-period-blocks u144) ;; ~24 hours in blocks

(define-map RefundRequests
    { student: principal, course-id: uint }
    {
        request-time: uint,
        status: (string-ascii 20)
    }
)

(define-public (request-refund (course-id uint))
    (let
        ((enrollment (unwrap! (get-enrollment tx-sender course-id) err-not-enrolled))
         (course (unwrap! (get-course-by-id course-id) err-not-found))
         (current-block stacks-block-height))
        
        (asserts! (<= (- current-block (get enrolled-at enrollment)) refund-period-blocks) err-refund-period-expired)
        (asserts! (not (get completed enrollment)) err-already-completed)
        
        (map-insert RefundRequests
            { student: tx-sender, course-id: course-id }
            {
                request-time: current-block,
                status: "pending"
            }
        )
        
        (map-set Courses
            { course-id: course-id }
            (merge course { current-students: (- (get current-students course) u1) })
        )
        
        (ok true)
    )
)


(define-non-fungible-token course-certificate uint)

(define-map CertificateMetadata
    { certificate-id: uint }
    {
        course-id: uint,
        student: principal,
        issue-date: uint,
        metadata-url: (string-ascii 256)
    }
)

(define-data-var next-certificate-id uint u1)

(define-public (mint-course-certificate (course-id uint) (metadata-url (string-ascii 256)))
    (let
        ((enrollment (unwrap! (get-enrollment tx-sender course-id) err-not-enrolled))
         (certificate-id (var-get next-certificate-id)))
        
        (asserts! (get completed enrollment) err-not-enrolled)
        
        (try! (nft-mint? course-certificate certificate-id tx-sender))
        
        (map-insert CertificateMetadata
            { certificate-id: certificate-id }
            {
                course-id: course-id,
                student: tx-sender,
                issue-date: stacks-block-height,
                metadata-url: metadata-url
            }
        )
        
        (var-set next-certificate-id (+ certificate-id u1))
        (ok certificate-id)
    )
)


(define-public (enroll-with-payment (course-id uint))
    (let
        ((course (unwrap! (get-course-by-id course-id) err-not-found))
         (course-price (get price course))
         (instructor (get instructor course))
         (platform-fee (/ (* course-price platform-fee-percentage) u100))
         (instructor-share (- course-price platform-fee))
         (current-block stacks-block-height))
        
        (asserts! (get active course) err-not-found)
        (asserts! (< (get current-students course) (get max-students course)) err-already-exists)
        (asserts! (> course-price u0) err-insufficient-payment)
        
        (try! (stx-transfer? course-price tx-sender (as-contract tx-sender)))
        
        (map-insert StudentEnrollments
            { student: tx-sender, course-id: course-id }
            {
                enrolled-at: current-block,
                completed: false,
                progress: u0
            }
        )
        
        (map-set Courses
            { course-id: course-id }
            (merge course { current-students: (+ (get current-students course) u1) })
        )
        
        (map-insert CoursePayments
            { student: tx-sender, course-id: course-id }
            {
                amount-paid: course-price,
                payment-date: current-block,
                instructor-share: instructor-share,
                platform-share: platform-fee
            }
        )
        
        (let ((current-instructor-balance (default-to u0 (get balance (map-get? InstructorBalances { instructor: instructor })))))
            (map-set InstructorBalances
                { instructor: instructor }
                { balance: (+ current-instructor-balance instructor-share) }
            )
        )
        
        (var-set platform-balance (+ (var-get platform-balance) platform-fee))
        
        (ok true)
    )
)

(define-public (withdraw-instructor-earnings)
    (let
        ((instructor-balance-data (unwrap! (map-get? InstructorBalances { instructor: tx-sender }) err-insufficient-balance))
         (withdrawal-amount (get balance instructor-balance-data)))
        
        (asserts! (> withdrawal-amount u0) err-insufficient-balance)
        
        (try! (as-contract (stx-transfer? withdrawal-amount tx-sender tx-sender)))
        
        (map-set InstructorBalances
            { instructor: tx-sender }
            { balance: u0 }
        )
        
        (ok withdrawal-amount)
    )
)

(define-public (withdraw-platform-fees)
    (let ((withdrawal-amount (var-get platform-balance)))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (> withdrawal-amount u0) err-insufficient-balance)
        
        (try! (as-contract (stx-transfer? withdrawal-amount tx-sender contract-owner)))
        
        (var-set platform-balance u0)
        
        (ok withdrawal-amount)
    )
)

(define-read-only (get-instructor-balance (instructor principal))
    (default-to u0 (get balance (map-get? InstructorBalances { instructor: instructor })))
)

(define-read-only (get-platform-balance)
    (var-get platform-balance)
)

(define-read-only (get-course-payment (student principal) (course-id uint))
    (map-get? CoursePayments { student: student, course-id: course-id })
)

(define-read-only (calculate-course-fees (course-price uint))
    (let ((platform-fee (/ (* course-price platform-fee-percentage) u100)))
        {
            total-price: course-price,
            platform-fee: platform-fee,
            instructor-share: (- course-price platform-fee)
        }
    )
)