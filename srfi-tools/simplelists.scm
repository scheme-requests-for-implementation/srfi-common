;;;; Use Simplelists.com API to create and query mailing lists.

(define simplelists-account-id "4137")

(define (simplelists-api-token)
  "Get the Simplelists API token from the SIMPLELISTS_TOKEN environment
variable."
  (or (get-environment-variable "SIMPLELISTS_TOKEN")
      (user-error "SIMPLELISTS_TOKEN environment variable not set.")))

(define (simplelists-api-base-url)
  "Return the Simplelists API v2 base URL."
  "https://www.simplelists.com/api/2")

(define (alist->json-string alist)
  "Convert an alist to a JSON string."
  (let ((port (open-output-string)))
    (json-write alist port)
    (get-output-string port)))

(define (safe-delete-file filename)
  "Delete a file if it exists, ignoring errors."
  (when (file-exists? filename)
    (delete-file filename)))

(define (http-request method url auth body-type body)
  "Make an HTTP request using curl.  Returns (status-code . body-text).

 method: HTTP method string (GET, POST, PUT, DELETE, etc.)
 url: Full URL string
 auth: Authentication string for --user, or #false for none
 body-type: 'json, 'form, or #false for no body
 body: For 'json: alist to convert to JSON
       For 'form: list of (key . value) pairs
       For #false: ignored"
  (let ((temp-body-file (make-temp-file-name))
        (temp-headers-file (make-temp-file-name)))
    (let ((curl-args
           (append
            (list "curl"
                  "--dump-header" temp-headers-file
                  "--output" temp-body-file
                  "--request" method
                  "--silent")
            (if auth (list "--user" auth) '())
            (cond
             ((eq? body-type 'json)
              (list "--data" (alist->json-string body)
                    "--header" "Content-Type: application/json"))
             ((eq? body-type 'form)
              (apply append
                     (map (lambda (pair)
                            (list "--data"
                                  (string-append (car pair) "=" (cdr pair))))
                          body)))
             (else '()))
            (list "--" url))))
      (run-program curl-args))
    (let* ((body-text (if (file-exists? temp-body-file)
                          (read-text-file temp-body-file)
                          ""))
           (headers (if (file-exists? temp-headers-file)
                        (read-text-file temp-headers-file)
                        ""))
           (header-lines (if (string=? headers "")
                             '()
                             (string-split #\newline headers)))
           (status-line (if (null? header-lines) "" (car header-lines)))
           (status-parts (string-split #\space status-line))
           (http-code (if (and (>= (length status-parts) 2)
                               (not (null? (cdr status-parts))))
                          (cadr status-parts)
                          "000")))
      (safe-delete-file temp-body-file)
      (safe-delete-file temp-headers-file)
      (cons http-code body-text))))

(define (simplelists-api-request method path body)
  "Make an HTTP request to Simplelists API v2.  Return parsed JSON response."
  (let* ((token (simplelists-api-token))
         (separator (if (string-index path #\?) "&" "?"))
         (url (string-append (simplelists-api-base-url)
			     path
			     separator
			     "account_id="
			     simplelists-account-id))
         (response (http-request method
                                 url
                                 (string-append token ":")
                                 (if body 'json #false)
                                 body))
         (http-code (car response))
         (body-text (cdr response)))
    (cond ((string=? body-text "") (user-error "Empty API response."))
	  ((string-prefix? "<" body-text)
           (user-error (string-append "Invalid API response (HTTP "
				      http-code
				      "): "
				      body-text)))
	  (else (json-read (open-input-string body-text))))))

(define (simplelists-get-list list-name)
  "Get the configuration for a mailing list.  Return parsed JSON as alist."
  (simplelists-api-request "GET" (string-append "/lists/" list-name "/") #false))

(define (simplelists-get-contact contact-id)
  "Get contact information by ID.  Return parsed JSON as alist."
  (simplelists-api-request "GET"
			   (string-append "/contacts/"
					  (number->string contact-id)
					  "/")
			   #false))

(define (parse-email-address address)
  "Parse 'Name <email@example.com>' or 'email@example.com' into (name . email)."
  (let ((open-bracket (string-index address #\<))
        (close-bracket (string-index address #\>)))
    (if (and open-bracket close-bracket (< open-bracket close-bracket))
        (let ((name (string-trim-both (substring address 0 open-bracket)))
              (email (substring address (+ open-bracket 1) close-bracket)))
          (cons name email))
        (cons #false (string-trim-both address)))))

(define (url-encode-address address)
  "URL-encode an address address, replacing '@' with '%40'."
  (let ((chars (string->list address)))
    (list->string
     (apply append
            (map (lambda (c)
                   (if (char=? c #\@)
                       (string->list "%40")
                       (list c)))
                 chars)))))

(define (simplelists-find-contact-by-address email)
  "Search for a contact by email address.  Return contact object or #false."
  (let* ((encoded-email (url-encode-address email))
         (path (string-append "/contacts/?email=" encoded-email))
         (response (simplelists-api-request "GET" path #false))
         (data (assoc 'data response)))
    (if (and data
	     (vector? (cdr data))
	     (> (vector-length (cdr data)) 0))
        (vector-ref (cdr data) 0)
        #false)))

(define (simplelists-create-contact address)
  "Create a new contact using form data.  Return contact object."
  (let* ((parsed (parse-email-address address))
         (name (car parsed))
         (email (cdr parsed))
         (token (simplelists-api-token))
         (url (string-append (simplelists-api-base-url)
			     "/contacts/?account_id="
			     simplelists-account-id))
         (form-data (if name
                        (list (cons "emails" email)
                              (cons "firstname" name))
                        (list (cons "emails" email))))
         (response (http-request "POST"
                                 url
                                 (string-append token ":")
                                 'form
                                 form-data))
         (body-text (cdr response)))
    (if (string=? body-text "")
        (user-error "Empty API response.")
        (json-read (open-input-string body-text)))))

(define (simplelists-add-membership contact-id list-name)
  "Add a contact to a list as a member using form data."
  (let* ((token (simplelists-api-token))
         (url (string-append (simplelists-api-base-url)
			     "/membership/?account_id="
			     simplelists-account-id))
         (form-data (list (cons "contact" (number->string contact-id))
                          (cons "list" list-name)))
         (http-response (http-request "POST"
                                      url
                                      (string-append token ":")
                                      'form
                                      form-data))
         (body-text (cdr http-response))
         (response (if (string=? body-text "")
                       (user-error "Empty API response.")
                       (json-read (open-input-string body-text)))))
    (disp "  Membership API response: ")
    (pretty-print response)
    response))

(define (simplelists-find-or-create-contact address)
  "Find existing contact by email address, or create new one.  Return contact
ID."
  (let* ((parsed (parse-email-address address))
         (email (cdr parsed))
         (existing (simplelists-find-contact-by-address email)))
    (cond (existing
           (disp "  Found existing contact for " email)
           (let ((id-pair (assoc 'id existing)))
             (if id-pair
                 (cdr id-pair)
                 (user-error
		  "Contact found but has no 'id' field. Available fields: "
                  (map car existing)))))
          (else
           (disp "  Creating new contact for " email)
           (let* ((new-contact (simplelists-create-contact address))
                  (id-pair (assoc 'id new-contact)))
             (if id-pair
                 (cdr id-pair)
                 (user-error
		  "Created contact but has no 'id' field. Available fields: "
                  (map car new-contact))))))))

(define (contact-display-name contact)
  "Extract a display name from a contact object."
  (let ((firstname (assoc 'firstname contact))
        (surname (assoc 'surname contact))
        (email (assoc 'email contact)))
    (cond ((and firstname surname (cdr firstname) (cdr surname))
	   (string-append (cdr firstname) " " (cdr surname)))
	  (firstname (cdr firstname))
	  (surname (cdr surname))
	  (email (cdr email))
	  (else "Unknown"))))

(define (simplelists-update-list-form list-name field-name values-list)
  "Update a mailing list field using form data.  For arrays, pass list of
values."
  (let* ((token (simplelists-api-token))
         (url (string-append (simplelists-api-base-url)
			     "/lists/"
			     list-name
			     "/?account_id="
			     simplelists-account-id))
         (form-data (map (lambda (val) (cons field-name val)) values-list))
         (response (http-request "PUT"
                                 url
                                 (string-append token ":")
                                 'form
                                 form-data))
         (body-text (cdr response)))
    (if (string=? body-text "")
        (user-error "Empty API response.")
        (json-read (open-input-string body-text)))))

(define (simplelists-create-list list-name params-alist)
  "Create a new mailing list via API using form data."
  (let* ((token (simplelists-api-token))
         (url (string-append (simplelists-api-base-url)
                             "/lists/?account_id="
                             simplelists-account-id))
         (form-data (apply append
                           (map (lambda (pair)
                                  (let ((key (symbol->string (car pair)))
                                        (val (cdr pair)))
                                    (cond
                                     ((vector? val)
                                      (map (lambda (v) (cons key v))
                                           (vector->list val)))
                                     ((boolean? val)
                                      (list (cons key (if val "1" "0"))))
                                     ((number? val)
                                      (list (cons key (number->string val))))
                                     (else
                                      (list (cons key val))))))
                                params-alist)))
         (response (http-request "POST"
                                 url
                                 (string-append token ":")
                                 'form
                                 form-data))
         (body-text (cdr response)))
    (if (string=? body-text "")
        (user-error "Empty API response.")
        (json-read (open-input-string body-text)))))

(define (json-extract-restrict-post-lists json-obj)
  "Extract the restrict_post_lists array from parsed JSON.  Return list of
strings."
  (let ((val (assoc 'restrict_post_lists json-obj)))
    (if val
        (let ((lists (cdr val)))
          (if (vector? lists)
              (vector->list lists)
              '()))
        '())))

(define (simplelists-standard-target-lists)
  "Return the standard lists that SRFI lists should be allowed to post to."
  (list "srfi-auto-subscribe"
        "srfi-discuss"
        "srfi-editors"
        "stickers"))

(define (simplelists-add-to-target-list list-name target-list)
  "Add list-name to the restrict_post_lists of target-list."
  (let* ((current-json (simplelists-get-list target-list))
         (current-lists (json-extract-restrict-post-lists current-json)))
    (if (member list-name current-lists)
        (disp "  '"
	      list-name
	      "' already allowed to post to '"
	      target-list
	      "'.")
        (let ((new-lists (append current-lists (list list-name))))
          (disp "  Adding '"
		list-name
		"' to restrict_post_lists of '"
		target-list
		"'.")
          (simplelists-update-list-form target-list
					"restrict_post_lists"
					new-lists)))))

(define (simplelists-add-standard-lists list-name)
  "Add list-name to the restrict_post_lists of standard target lists."
  (disp "Allowing '" list-name "' to post to standard lists:")
  (let ((targets (simplelists-standard-target-lists)))
    (for-each (lambda (target)
                (simplelists-add-to-target-list list-name target))
              targets)
    (disp "Finished adding '" list-name "' to standard lists.")))

(define (simplelists-create-srfi-list num author-email)
  "Create a new mailing list for SRFI <num> with standard configuration and add
author."
  (let* ((list-name (srfi-num-stem num))
         (params `((name . ,list-name)
                   (archive_enabled . #true)
                   (archive_protected . #false)
                   (archive_spammode . #true)
                   (message_footer . "")
                   (moderate . 6)
                   (restrict_post_lists . ,(list->vector
					    (list list-name
						  "srfi-auto-subscribe")))
                   (subs_memberview . ""))))
    (disp "Creating mailing list for '" list-name "'.")
    (let* ((response (simplelists-create-list list-name params))
	   (is-error (assoc 'is_error response)))
      (when (and is-error (cdr is-error))
        (user-error "Failed to create list: "
                    (cdr (assoc 'message response))))
      (disp "Successfully created list '" list-name "'."))
    (disp "Adding author to list:")
    (let ((contact-id (simplelists-find-or-create-contact author-email)))
      (disp "  Adding contact "
	    (number->string contact-id)
	    " to '"
	    list-name
	    "'.")
      (simplelists-add-membership contact-id list-name)
      (disp "  Author added successfully."))
    (disp "Adding srfi-auto-subscribe to list:")
    (let ((contact-id
	   (simplelists-find-or-create-contact
	    "srfi-auto-subscribe@srfi.schemers.org")))
      (disp "  Adding contact "
	    (number->string contact-id)
	    " to '"
	    list-name
	    "'.")
      (simplelists-add-membership contact-id list-name)
      (disp "  srfi-auto-subscribe added successfully."))
    (simplelists-add-standard-lists list-name)))

;; Commands

(define-command (simplelists-create num author-email)
  "Create mailing list for SRFI <num> with author as member."
  (simplelists-create-srfi-list (parse-srfi-number num) author-email))

(define (sort-alist alist)
  "Sort an alist by symbolic keys."
  (list-sort (lambda (a b)
               (string<? (symbol->string (car a))
                         (symbol->string (car b))))
             alist))

(define (contact-email contact)
  "Extract the primary email address from a contact object."
  (let ((addresses (assoc 'addresses contact)))
    (if (and addresses
	     (vector? (cdr addresses))
	     (> (vector-length (cdr addresses)) 0))
        (let* ((first-email-obj (vector-ref (cdr addresses) 0))
               (email-pair (assoc 'email first-email-obj)))
          (if email-pair
              (cdr email-pair)
              "no-email-address"))
        "no-email-address")))

(define (resolve-contacts-in-list list-config)
  "Add contact-names-x and contact-addresses-x fields alongside existing
contacts field."
  (let ((contacts-pair (assoc 'contacts list-config)))
    (if (and contacts-pair (vector? (cdr contacts-pair)))
        (let* ((contact-ids (vector->list (cdr contacts-pair)))
               (contacts-data (map simplelists-get-contact contact-ids))
               (contact-names
		(list->vector (map contact-display-name contacts-data)))
               (contact-addresses
		(list->vector (map contact-email contacts-data))))
          (cons `(contact-names-x . ,contact-names)
                (cons `(contact-addresses-x . ,contact-addresses)
                      list-config)))
        list-config)))

(define-command (simplelists-get num)
  "Get mailing list configuration for SRFI with given number."
  (let* ((srfi-num (parse-srfi-number num))
         (list-name (srfi-num-stem srfi-num))
         (json (simplelists-get-list list-name))
         (with-names (resolve-contacts-in-list json))
         (sorted (sort-alist with-names)))
    (pretty-print sorted)))