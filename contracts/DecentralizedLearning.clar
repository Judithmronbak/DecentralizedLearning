;; DecentralizedLearning - Educational Content Marketplace
;; Core features: Course management, enrollment, achievements, and credentials

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-not-enrolled (err u103))
(define-constant err-already-completed (err u104))

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
