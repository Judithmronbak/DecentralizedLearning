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

(define-constant err-invalid-subscription-tier (err u109))
(define-constant err-subscription-expired (err u110))
(define-constant err-subscription-not-found (err u111))
(define-constant subscription-duration-blocks u1440)

(define-map SubscriptionTiers
    { tier-id: uint }
    {
        name: (string-ascii 50),
        price: uint,
        course-limit: uint,
        duration-blocks: uint,
        benefits: (string-ascii 200)
    }
)

(define-map UserSubscriptions
    { user: principal }
    {
        tier-id: uint,
        start-block: uint,
        end-block: uint,
        courses-used: uint,
        active: bool
    }
)

(define-map SubscriptionCourseAccess
    { user: principal, course-id: uint }
    {
        granted-at: uint,
        subscription-tier: uint
    }
)

(define-data-var next-tier-id uint u1)

(define-public (create-subscription-tier 
    (name (string-ascii 50)) 
    (price uint) 
    (course-limit uint) 
    (duration-blocks uint) 
    (benefits (string-ascii 200))
)
    (let ((tier-id (var-get next-tier-id)))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-insert SubscriptionTiers
            { tier-id: tier-id }
            {
                name: name,
                price: price,
                course-limit: course-limit,
                duration-blocks: duration-blocks,
                benefits: benefits
            }
        )
        (var-set next-tier-id (+ tier-id u1))
        (ok tier-id)
    )
)

(define-public (subscribe-to-tier (tier-id uint))
    (let
        ((tier (unwrap! (map-get? SubscriptionTiers { tier-id: tier-id }) err-invalid-subscription-tier))
         (subscription-price (get price tier))
         (duration (get duration-blocks tier))
         (current-block stacks-block-height)
         (end-block (+ current-block duration)))
        
        (asserts! (> subscription-price u0) err-insufficient-payment)
        
        (try! (stx-transfer? subscription-price tx-sender (as-contract tx-sender)))
        
        (map-set UserSubscriptions
            { user: tx-sender }
            {
                tier-id: tier-id,
                start-block: current-block,
                end-block: end-block,
                courses-used: u0,
                active: true
            }
        )
        
        (var-set platform-balance (+ (var-get platform-balance) subscription-price))
        
        (ok true)
    )
)

(define-public (access-course-with-subscription (course-id uint))
    (let
        ((subscription (unwrap! (map-get? UserSubscriptions { user: tx-sender }) err-subscription-not-found))
         (tier (unwrap! (map-get? SubscriptionTiers { tier-id: (get tier-id subscription) }) err-invalid-subscription-tier))
         (course (unwrap! (get-course-by-id course-id) err-not-found))
         (current-block stacks-block-height))
        
        (asserts! (get active subscription) err-subscription-expired)
        (asserts! (< current-block (get end-block subscription)) err-subscription-expired)
        (asserts! (< (get courses-used subscription) (get course-limit tier)) err-already-exists)
        (asserts! (get active course) err-not-found)
        
        (map-insert StudentEnrollments
            { student: tx-sender, course-id: course-id }
            {
                enrolled-at: current-block,
                completed: false,
                progress: u0
            }
        )
        
        (map-insert SubscriptionCourseAccess
            { user: tx-sender, course-id: course-id }
            {
                granted-at: current-block,
                subscription-tier: (get tier-id subscription)
            }
        )
        
        (map-set Courses
            { course-id: course-id }
            (merge course { current-students: (+ (get current-students course) u1) })
        )
        
        (map-set UserSubscriptions
            { user: tx-sender }
            (merge subscription { courses-used: (+ (get courses-used subscription) u1) })
        )
        
        (ok true)
    )
)

(define-public (renew-subscription (tier-id uint))
    (let
        ((existing-subscription (unwrap! (map-get? UserSubscriptions { user: tx-sender }) err-subscription-not-found))
         (tier (unwrap! (map-get? SubscriptionTiers { tier-id: tier-id }) err-invalid-subscription-tier))
         (renewal-price (get price tier))
         (duration (get duration-blocks tier))
         (current-block stacks-block-height)
         (new-end-block (+ current-block duration)))
        
        (try! (stx-transfer? renewal-price tx-sender (as-contract tx-sender)))
        
        (map-set UserSubscriptions
            { user: tx-sender }
            {
                tier-id: tier-id,
                start-block: current-block,
                end-block: new-end-block,
                courses-used: u0,
                active: true
            }
        )
        
        (var-set platform-balance (+ (var-get platform-balance) renewal-price))
        
        (ok true)
    )
)

(define-read-only (get-subscription-tier (tier-id uint))
    (map-get? SubscriptionTiers { tier-id: tier-id })
)

(define-read-only (get-user-subscription (user principal))
    (map-get? UserSubscriptions { user: user })
)

(define-read-only (is-subscription-active (user principal))
    (match (map-get? UserSubscriptions { user: user })
        subscription 
        (and 
            (get active subscription)
            (< stacks-block-height (get end-block subscription))
        )
        false
    )
)

(define-read-only (get-remaining-course-slots (user principal))
    (match (map-get? UserSubscriptions { user: user })
        subscription
        (match (map-get? SubscriptionTiers { tier-id: (get tier-id subscription) })
            tier (- (get course-limit tier) (get courses-used subscription))
            u0
        )
        u0
    )
)

;; Peer-to-Peer Tutoring Marketplace
;; Enables students to become tutors and offer personalized learning sessions

(define-constant err-invalid-tutor-profile (err u112))
(define-constant err-session-not-found (err u113))
(define-constant err-session-already-booked (err u114))
(define-constant err-session-not-booked (err u115))
(define-constant err-insufficient-rating (err u116))
(define-constant err-session-already-completed (err u117))
(define-constant err-unauthorized-tutor (err u118))
(define-constant err-invalid-session-time (err u119))

;; Tutor profiles with skills and verification status
(define-map TutorProfiles
    { tutor: principal }
    {
        bio: (string-ascii 200),
        hourly-rate: uint,
        expertise-areas: (string-ascii 150),
        total-sessions: uint,
        average-rating: uint,
        verified: bool,
        active: bool
    }
)

;; Tutor skill verification based on completed courses
(define-map TutorSkillVerification
    { tutor: principal, course-id: uint }
    {
        verified-at: uint,
        credential-score: uint,
        endorsements: uint
    }
)

;; Tutoring session definitions and scheduling
(define-map TutoringSessions
    { session-id: uint }
    {
        tutor: principal,
        student: (optional principal),
        course-topic: uint,
        session-date: uint,
        duration-minutes: uint,
        rate: uint,
        status: (string-ascii 20),
        meeting-info: (string-ascii 100)
    }
)

;; Session payments and escrow management
(define-map SessionPayments
    { session-id: uint }
    {
        student: principal,
        amount-escrowed: uint,
        payment-date: uint,
        released: bool,
        tutor-share: uint,
        platform-fee: uint
    }
)

;; Session ratings and feedback
(define-map SessionRatings
    { session-id: uint }
    {
        student-rating: uint,
        tutor-rating: uint,
        student-feedback: (string-ascii 200),
        tutor-feedback: (string-ascii 200),
        completed-at: uint
    }
)

;; Session availability slots
(define-map TutorAvailability
    { tutor: principal, time-slot: uint }
    {
        available: bool,
        duration-minutes: uint,
        rate-override: (optional uint)
    }
)

(define-data-var next-session-id uint u1)
(define-constant tutoring-platform-fee-percentage u15)
(define-constant min-tutor-rating u350) ;; 3.5 out of 5 stars (multiplied by 100)

;; Create tutor profile after course completion verification
(define-public (create-tutor-profile 
    (bio (string-ascii 200)) 
    (hourly-rate uint) 
    (expertise-areas (string-ascii 150))
)
    (let ((tutor-qualifications (get-tutor-course-completions tx-sender)))
        ;; Verify tutor has completed at least one course
        (asserts! (> tutor-qualifications u0) err-insufficient-rating)
        (asserts! (> hourly-rate u0) err-insufficient-payment)
        
        (map-set TutorProfiles
            { tutor: tx-sender }
            {
                bio: bio,
                hourly-rate: hourly-rate,
                expertise-areas: expertise-areas,
                total-sessions: u0,
                average-rating: u0,
                verified: true,
                active: true
            }
        )
        (ok true)
    )
)

;; Verify tutor skills based on course completion
(define-public (verify-tutor-skill (tutor principal) (course-id uint))
    (let 
        ((credential (unwrap! (get-credential tutor course-id) err-not-found))
         (course (unwrap! (get-course-by-id course-id) err-not-found)))
        
        (asserts! (get verified credential) err-not-found)
        
        (map-set TutorSkillVerification
            { tutor: tutor, course-id: course-id }
            {
                verified-at: stacks-block-height,
                credential-score: u100,
                endorsements: u0
            }
        )
        (ok true)
    )
)

;; Create available tutoring session slot
(define-public (create-tutoring-session 
    (course-topic uint) 
    (session-date uint) 
    (duration-minutes uint)
    (meeting-info (string-ascii 100))
)
    (let 
        ((session-id (var-get next-session-id))
         (tutor-profile (unwrap! (map-get? TutorProfiles { tutor: tx-sender }) err-invalid-tutor-profile)))
        
        (asserts! (get active tutor-profile) err-invalid-tutor-profile)
        (asserts! (get verified tutor-profile) err-invalid-tutor-profile)
        (asserts! (> session-date stacks-block-height) err-invalid-session-time)
        (asserts! (>= duration-minutes u30) err-invalid-session-time)
        (asserts! (<= duration-minutes u180) err-invalid-session-time)
        
        (map-insert TutoringSessions
            { session-id: session-id }
            {
                tutor: tx-sender,
                student: none,
                course-topic: course-topic,
                session-date: session-date,
                duration-minutes: duration-minutes,
                rate: (get hourly-rate tutor-profile),
                status: "available",
                meeting-info: meeting-info
            }
        )
        
        (var-set next-session-id (+ session-id u1))
        (ok session-id)
    )
)

;; Book tutoring session with payment escrow
(define-public (book-tutoring-session (session-id uint))
    (let 
        ((session (unwrap! (map-get? TutoringSessions { session-id: session-id }) err-session-not-found))
         (session-cost (calculate-session-cost (get rate session) (get duration-minutes session)))
         (platform-fee (/ (* session-cost tutoring-platform-fee-percentage) u100))
         (tutor-share (- session-cost platform-fee)))
        
        (asserts! (is-none (get student session)) err-session-already-booked)
        (asserts! (is-eq (get status session) "available") err-session-already-booked)
        (asserts! (> (get session-date session) stacks-block-height) err-invalid-session-time)
        
        ;; Transfer payment to contract escrow
        (try! (stx-transfer? session-cost tx-sender (as-contract tx-sender)))
        
        ;; Update session with student booking
        (map-set TutoringSessions
            { session-id: session-id }
            (merge session { 
                student: (some tx-sender), 
                status: "booked" 
            })
        )
        
        ;; Record payment in escrow
        (map-insert SessionPayments
            { session-id: session-id }
            {
                student: tx-sender,
                amount-escrowed: session-cost,
                payment-date: stacks-block-height,
                released: false,
                tutor-share: tutor-share,
                platform-fee: platform-fee
            }
        )
        
        (ok true)
    )
)

;; Complete tutoring session and release payment
(define-public (complete-tutoring-session 
    (session-id uint) 
    (student-rating uint) 
    (student-feedback (string-ascii 200))
)
    (let 
        ((session (unwrap! (map-get? TutoringSessions { session-id: session-id }) err-session-not-found))
         (payment (unwrap! (map-get? SessionPayments { session-id: session-id }) err-session-not-found))
         (tutor (get tutor session)))
        
        (asserts! (is-eq (some tx-sender) (get student session)) err-unauthorized-tutor)
        (asserts! (is-eq (get status session) "booked") err-session-already-completed)
        (asserts! (not (get released payment)) err-session-already-completed)
        (asserts! (>= student-rating u1) err-not-found)
        (asserts! (<= student-rating u5) err-not-found)
        
        ;; Release payment to tutor
        (try! (as-contract (stx-transfer? (get tutor-share payment) tx-sender tutor)))
        
        ;; Add platform fee to balance
        (var-set platform-balance (+ (var-get platform-balance) (get platform-fee payment)))
        
        ;; Update session status
        (map-set TutoringSessions
            { session-id: session-id }
            (merge session { status: "completed" })
        )
        
        ;; Mark payment as released
        (map-set SessionPayments
            { session-id: session-id }
            (merge payment { released: true })
        )
        
        ;; Record student rating
        (map-set SessionRatings
            { session-id: session-id }
            {
                student-rating: student-rating,
                tutor-rating: u0,
                student-feedback: student-feedback,
                tutor-feedback: "",
                completed-at: stacks-block-height
            }
        )
        
        ;; Update tutor profile statistics
        (update-tutor-stats tutor student-rating)
        
        (ok true)
    )
)

;; Add tutor feedback after session completion
(define-public (add-tutor-feedback 
    (session-id uint) 
    (tutor-rating uint) 
    (tutor-feedback (string-ascii 200))
)
    (let 
        ((session (unwrap! (map-get? TutoringSessions { session-id: session-id }) err-session-not-found))
         (existing-rating (unwrap! (map-get? SessionRatings { session-id: session-id }) err-session-not-found)))
        
        (asserts! (is-eq tx-sender (get tutor session)) err-unauthorized-tutor)
        (asserts! (is-eq (get status session) "completed") err-session-not-found)
        (asserts! (>= tutor-rating u1) err-not-found)
        (asserts! (<= tutor-rating u5) err-not-found)
        
        (map-set SessionRatings
            { session-id: session-id }
            (merge existing-rating { 
                tutor-rating: tutor-rating,
                tutor-feedback: tutor-feedback 
            })
        )
        
        (ok true)
    )
)

;; Private helper functions
(define-private (calculate-session-cost (hourly-rate uint) (duration-minutes uint))
    (/ (* hourly-rate duration-minutes) u60)
)

(define-private (get-tutor-course-completions (tutor principal))
    ;; Simplified: check if tutor has any completed courses
    ;; In practice, would iterate through all courses to count completions
    (if (is-some (get-credential tutor u1)) u1 u0)
)

(define-private (update-tutor-stats (tutor principal) (new-rating uint))
    (match (map-get? TutorProfiles { tutor: tutor })
        profile 
        (let 
            ((total-sessions (get total-sessions profile))
             (current-avg (get average-rating profile))
             (new-total (+ total-sessions u1))
             (new-average (/ (+ (* current-avg total-sessions) (* new-rating u100)) new-total)))
            
            (map-set TutorProfiles
                { tutor: tutor }
                (merge profile { 
                    total-sessions: new-total,
                    average-rating: new-average 
                })
            )
            true
        )
        false
    )
)

;; Read-only functions for tutoring marketplace
(define-read-only (get-tutor-profile (tutor principal))
    (map-get? TutorProfiles { tutor: tutor })
)

(define-read-only (get-tutoring-session (session-id uint))
    (map-get? TutoringSessions { session-id: session-id })
)

(define-read-only (get-session-payment (session-id uint))
    (map-get? SessionPayments { session-id: session-id })
)

(define-read-only (get-session-rating (session-id uint))
    (map-get? SessionRatings { session-id: session-id })
)

(define-read-only (is-qualified-tutor (tutor principal))
    (match (map-get? TutorProfiles { tutor: tutor })
        profile (and 
            (get verified profile)
            (get active profile)
            (>= (get average-rating profile) min-tutor-rating)
        )
        false
    )
)


