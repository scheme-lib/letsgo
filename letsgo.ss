;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;作者:evilbinary on 2018-04-12 23:54:31.
;邮箱:rootdebug@163.com
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;letsgo auto buy

(import (net curl-ffi) (cffi cffi) (json json) )

(def-function-callback
  make-write-callback
  (void* int int void*) int)


(define-syntax try
  (syntax-rules (catch)
    ((_ body (catch catcher))
     (call-with-current-continuation
      (lambda (exit)
	(with-exception-handler
	 (lambda (condition)
	   (catcher condition)
	   (exit condition))
	 (lambda () body)))))))

(define (create-request)
  (curl-easy-init))

(define (destroy-request req)
  (curl-easy-cleanup req))

(define (post req url data)
  ;;(printf "POST ~a ~a\n" url data)
  (let ((buffer "")
	(tmp-ptr 0)
	(res -1))
    (curl-easy-setopt req CURLOPT_URL url)
    (curl-easy-setopt req CURLOPT_POST 1)
    (curl-easy-setopt req CURLOPT_POSTFIELDS data)
    ;;(curl-easy-setopt req CURLOPT_POSTFIELDSIZE  -1)
    (curl-easy-setopt req 20011
		      (make-write-callback
		       (lambda (ptr size nmemb stream)
			 ;;(printf "~a\n" (cffi-string ptr))
			 ;;(printf  "callback ~a ~a ~a ~a\n" ptr size nmemb stream)
			 (set! buffer (string-append buffer (cffi-string ptr)))
			 (* size nmemb))))
    (set! res (curl-easy-perform req))
    (cffi-free tmp-ptr)
    (if (= 0 res)
	(begin
	  ;;(printf "   ~a\n" buffer)
	  buffer)
	(begin
	  (printf  "[~a] curl-easy-perform failed ~s\n" (date-and-time)  (curl-easy-strerror res))
	  '()))))

(define (set-headers req headers)
  (curl-easy-setopt curl CURLOPT_HTTPHEADER headers) )


(define (read-file file-name)
  (with-input-from-file file-name
    (lambda ()
      (let loop((ls1 '()) (c (read-char)))
	(if (eof-object? c)
	    (list->string (reverse ls1))
	    (loop (cons c ls1) (read-char)))))))


(define (get-cookie file)
  (read-file file))

;;(cffi-log #t)

(define content-type "Content-Type:application/json")
(define user-agent "User-Agent:Mozilla/5.0 (Linux; Android 8.0; SM-G900P Build/LRX22T) AppleWebKit/333.36 (KHTML, like Gecko) Chrome/333.0.343.11 Mobile Safari/111.36")

(define (get-normal-headers)
  (curl-slist-append (curl-slist-append 0 content-type ) user-agent))

(define (load-account files)
  (let loop ((file files) (all '()))
    (if (pair? file)
	(begin
	  (if (null? all)
	      (set! all
		    (list
		     (list (curl-slist-append
			    (get-normal-headers)
			    (get-cookie (car file) ))
			   (car file))))
	      (set! all
		    (append  all
			     (list (list (curl-slist-append
					  (get-normal-headers)
					  (get-cookie (car file) )) (car file)) ))))
	  ;;(printf "~a\n" all)
	  (loop (cdr file) all ))
	all)))

;;
(define account-headers
  (load-account (list
		 "my-account.txt"
		 )))


;;(printf "load account-headers=>~a\n" account-headers)
;;(set! account-headers (append (cdr account-headers) (list (car account-headers))))
;;(printf "load account-headers=>~a\n" account-headers)


(define (change-account req)
  (set! reg-capcha '())
  (set! last-capcha-time 0)
  (curl-easy-setopt req CURLOPT_HTTPHEADER 0)
  (set! account-headers (append (cdr account-headers) (list (car account-headers))))
  (set-headers req (caar account-headers))
  (printf "[~a] change-account ~a\n" (date-and-time) (cadar account-headers) ) )

(define (get-request-time)
  (let ((cur-time (current-time)))
    (+  (/ (time-nanosecond cur-time) 1000)
	(* 1000 (time-second cur-time) ) )))

(define (get-pets-sale-query-arg page filter  sort-type)
  (let* ((request-time (get-request-time)))
    (scm->json-string  `((appId . 1 )
			 (filterCondition  ,@filter)
			 (lastAmount . "")
			 (lastRareDegree . "")
			 (nounce . ())
			 (pageNo  ,@page)
			 (pageSize . 10)
			 (petIds . (0) )
			 (querySortType ,@sort-type) ;;CREATETIME_DESC AMOUNT_ASC
			 (requestId ,@request-time  )
			 (timeStamp . ())
			 (token . ())
			 (tpl . "")
			 (type . ())))))


(define (query-pets-on-sale req page filter sort-type)
  (try
   (json-string->scm (post req "https://pet-chain.baidu.com/data/market/queryPetsOnSale" (get-pets-sale-query-arg page filter sort-type)))
   (catch (lambda (x)
	    (display-condition x)(newline) ))))

(define (get-captcha req)
  (let ((arg (scm->json-string `((requestId ,@(get-request-time))
				 (appId . 1)
				 (timeStamp . ())
				 (token . ())
				 (nounce . ())
				 (tpl . ""))))
	(url  "https://pet-chain.baidu.com/data/captcha/gen"))

    (let loop ((try 3)
	       (ret  (json-string->scm (post req url arg) )))
      (if (string=? "00" (hashtable-ref ret "errorNo" ""))
	  (hashtable-ref ret "data" '())
	  (begin
	    (printf "[~a] get-captcha ~a erromsg:~a\n"
		    (date-and-time)
		    (hashtable-ref ret "errorNo" "")
		    (hashtable-ref ret "errorMsg" "") )

	    (if (string=? "05" (hashtable-ref ret "errorNo" "") )
		(change-account req))
	    (if (> try 0)
		(loop  (- try 1 ) (json-string->scm (post req url arg) ))
		'()))))))

(define (recognize-captcha req img)
  (let ((ret (json-string->scm (post req "http://127.0.0.1:5000/capcha" (string-append "img=" (curl-easy-escape req img (string-length img) ))))))
    ;;(printf "ret=>~a\n" ret)
    (if (= 0 (hashtable-ref  ret "code" -1))
	(hashtable-ref  ret "result" ""))))

(define (purchase req petid amount code seed captcha )
  (let ((arg (scm->json-string `((appId . 1 )
				  (petId  ,@petid)
				  (requestId ,@(get-request-time))
				  (tpl . "")
				  (amount ,@amount)
				  (seed ,@seed)
				  (captcha ,@captcha)
				  (validCode  ,@code))))
	(url  "https://pet-chain.baidu.com/data/txn/create"))
    (let loop ((retry 3)
	       (ret (post req url arg)))
      (if (and (< retry 0)(null? ret))
	  (loop (- retry 1) (post req url arg))
	  (json-string->scm ret ) ) )))

(define (auto-capcha req )
  (let ((cap  (get-captcha req)))
    (if (hashtable? cap)
	(let ((seed (hashtable-ref cap "seed" "" ))
	      (reg (recognize-captcha capcha-req (hashtable-ref cap "img" "") )))
	  ;;(printf "seed=~a\n" seed )
	  ;;(printf "regnize-captch=>~a\n"  reg)
	  (list seed reg )
	  ))
    ))

(define (get-pet-detail req petid)
  (let* ((arg (scm->json-string
	       `((requestId ,@(get-request-time))
		 (appId . 1)
		 (petId ,@petid)
		 (timeStamp . ())
		 (token . ())
		 (nounce . ())
		 (tpl . ""))))
	 (ret  (json-string->scm (post req "https://pet-chain.baidu.com/data/pet/queryPetById" arg) )))
    (if (string=? "00" (hashtable-ref ret "errorNo" ""))
	(begin
	  (hashtable-ref ret "data" '()) )
	(printf "[~a] get-pet-deail ~a ~a\n" (date-and-time) (hashtable-ref ret "errorNo" "")  (hashtable-ref ret "errorMsg" "") ))))

;;预先打码列表
(define reg-capcha '())

(define (get-reg-capcha req)
  (if (> (length reg-capcha) 0)
      (let ((ca (car reg-capcha)))
	(set! reg-capcha (cdr reg-capcha))
	ca)
      (auto-capcha req)))


(define last-capcha-time 0 )

(define (gen-reg-capcha req)
  (printf "[~a] gen-reg capcha current ~a ~a\n" (date-and-time) (length reg-capcha) (- (time-second  (current-time)) last-capcha-time ))
  (if (or (<= (length reg-capcha) 0) (>= (- (time-second  (current-time)) last-capcha-time ) 120))
      (begin
	(set! reg-capcha '())
	(let loop ((i 5))
	  (if (> i 0)
	      (begin
		(set! reg-capcha (append reg-capcha (list (auto-capcha req))))

		(loop (- i 1))
		)))
	(set! last-capcha-time (time-second  (current-time)))
	(printf "[~a] gen-reg capcha ~a\n" (date-and-time) (length reg-capcha))
	)))

(define (get-rare-degree degree)
  (case degree
    ((0) "普通")
    ((1) "稀有")
    ((2) "卓越")
    ((3) "史诗")
    ((4) "神话")
    ((5) "传说")))

(define (count-degree attributes)
  (let loop
      ((attr attributes)
       (count 0)
       (i 0))
    (if (pair? attr)
	(let ((d (hashtable-ref (list-ref attributes i) "rareDegree" '() )))
	  (if (and (not (null? d)) (string=? "稀有" d))
	      (set! count (+ count 1)))
	  (loop (cdr attr) count (+ i 1))) count) ))

(define (can-buy rare-degree amount body eye mouth count)
  ;;(printf "can-buy ~a ~a ~a\n" rare-degree amount count)
  (case  rare-degree
    ;;普通
    ((0) (<= amount 180) )
    ;;稀有
    ((1) (or (and (string=? body "天使") (<= amount 190))
	     (and (string=? eye "白眉斗眼") (<= amount 190))
	     (<= amount 300) ))
    ;;卓越
    ((2) (or (and (string=? body "天使") (<= amount 600))
	     (and (string=? eye "白眉斗眼") (<= amount 600))
	     (<= amount 400)
	     ))
     ;;史诗
    ((3) (or (and (= count 4) (string=? body "天使") (<= amount 8500))
	     (and (= count 4) (string=? body "天使") (string=? eye "白眉斗眼") (<= amount 55000))
	     (and (= count 4) (string=? eye "白眉斗眼") (<= amount 8000))
	     (and (= count 4) (<= amount 3000))

	     ;;count==5
	     (and (= count 5) (string=? body "天使") (<= amount 45000 ))
	     (and (= count 5) (string=? eye "白眉斗眼") (<= amount 55000))
	     (and (= count 5) (<= amount 15000))
	      ))
     ;;神话
     ((4) (<= amount 100000))
     ;;传说
     ((5) (<= amount 100000) )
     (else #f)
     ) )

(define curl (cffi-alloc 1024))


(define (buy req petid amount validcode)
  (let* ((capcha (get-reg-capcha req) )
	 (result (purchase req petid amount validcode (car capcha) (cadr capcha))))
    (list  (hashtable-ref result "errorNo" "")  (hashtable-ref result "errorMsg" "") ) ))

(define (process-data req data)
  (let loop ((item data))
    (if (pair? item)
	(begin
	  ;;(printf "~a==>~a\n" (length data) (hashtable-values (car item)  ))
	  (try
	   (let ((petid (hashtable-ref (car item) "petId" ""))
		 (id (hashtable-ref (car item) "id" ""))
		 (amount (string->number (hashtable-ref (car item) "amount" "0")))
		 (validcode (hashtable-ref (car item) "validCode" ""))
		 (cooling-interval (hashtable-ref (car item) "coolingInterval" ""))
		 (generation (hashtable-ref (car item) "generation" -1 ))
		 (rare-degree (hashtable-ref (car item) "rareDegree" -1)))
	     (if (and (= 0 generation)  (string=? cooling-interval "0分钟"))
		 (begin
		   ;;(printf "[~a] ~a ~a ~10a ~a ~a ~a代 ~a" (date-and-time)
		;;	   petid id amount (get-rare-degree rare-degree) cooling-interval generation  validcode)
		   (let* ((detail (get-pet-detail req petid))
			  (attributes (hashtable-ref detail "attributes" '() ) ))
		     (if (and  (list? attributes) (> (length attributes) 0))
			 (let ((body (hashtable-ref (list-ref attributes 0) "value" ""))
			       (eye (hashtable-ref (list-ref attributes 2) "value" ""))
			       (mouth (hashtable-ref (list-ref attributes 4) "value" ""))
			       (count (count-degree attributes)) )

			   ;;(printf " body=~4a eye=~4a mouth=~4a count=~a \n" body eye  mouth count )
			   (if (can-buy rare-degree amount body eye mouth count)
			       (let ((result (buy req petid amount validcode)))
				 (printf "[~a] ~a ~a ~10a ~a ~a ~a ~a ~a ~a  ~a ~a代 ~a\n" (date-and-time)
					 petid id amount (get-rare-degree rare-degree) amount body eye mouth count   cooling-interval generation  validcode)

				 (printf "[~a] buy ~a ~a ~a\n" (date-and-time)  (cadar account-headers) (car result) (list-ref result 1) )
				 ;;10003 上笔交易正在进行中,请稍后重;10018 您今日交易次数已超限，明天再试试吧;10001 很抱歉,您的余额不足
				 (if (or (string=? (car result) "10003")
					 (string=? (car result) "10001")
					 (string=? (car result) "10018") )
				     (begin
				       (change-account req)
				       (set! result (buy req petid amount validcode))
				       ;;100 验证码错误重试
				       (if (string=? (car result) "100")
					   (let loop ((result  (buy req petid amount validcode) ))
					     (printf "[~a] buy2 ~a ~a ~a\n" (date-and-time)  (cadar account-headers)  (car result) (list-ref result 1))
					     (if (string=? (car result) "00")
						 (change-account req))
					     (if (string=? "100" (car result))
						 (loop (buy req petid amount validcode)))))
				       ))
				 ;;change account
				 (if (string=? (car result) "00")
				     (change-account req))
				 ;;100 验证码错误重试
				 (if (string=? (car result) "100")
				     (let loop ((result  (buy req petid amount validcode) ))
				       (printf "[~a] buy2 ~a ~a ~a\n" (date-and-time)  (cadar account-headers)  (car result) (list-ref result 1))
				       (if (string=? "100" (car result))
					   (loop (buy req petid amount validcode)))))
				 ;;08 交易火爆，区块链处理繁忙，请稍后再
				 (if (string=? (car result) "08")
				     (let loop ((result (buy req petid amount validcode))
						(retry 10))
				       (printf "[~a] buy retry ~a ~a ~a\n" (date-and-time)  (cadar account-headers) (car result) (list-ref result 1) )
				       (if (and (> retry 0) (string=? "08" (car result)))
					   (loop (buy req petid amount validcode) (- retry 1 )))))

				 )))  )))))
	  (catch (lambda (x)
		   (display-condition x)(newline) )))

	  (loop (cdr item)) ) )))


(define capcha-req 0)
(define capcha-header 0)

(define (run-with-cond page filter sort-type)
  (let ((ret (query-pets-on-sale curl page filter sort-type )))
    (if (string=? "00" (hashtable-ref ret "errorNo" ""))
	(begin
	  (process-data curl (hashtable-ref (hashtable-ref ret "data" '() ) "petsOnSale" '() ) )
	  ;;(printf "\n")
	  )
	(printf "[~a] query-pets ~a ~a\n" (date-and-time)  (hashtable-ref ret "errorNo" "")  (hashtable-ref ret "errorMsg" "") )) ))

(define (run )
  (let loop ()
    (try
     (begin
       ;;(change-account curl)
        ;;时间排序
       (run-with-cond 1  "{}" "CREATETIME_DESC")

       (gen-reg-capcha curl)
     )
     (catch (lambda (x)
	      (display-condition x)(newline) )))
    (collect)
    (sleep (make-time 'time-duration 0 1))
    (loop)))

(define (main)
  (curl-global-init CURL_GLOBAL_ALL)
  (set! curl (create-request))
  (curl-easy-setopt curl CURLOPT_TIMEOUT 100)
  ;;(curl-easy-setopt curl  CURLOPT_COOKIEJAR "cookie.txt")
  ;;(curl-easy-setopt curl CURLOPT_COOKIEFILE "cookie.txt")
  ;;(printf "account=>~a ~a\n" (caar account-headers) (cadar account-headers))

  (curl-easy-setopt curl CURLOPT_HTTPHEADER (caar account-headers))
  ;;(curl-easy-setopt curl CURLOPT_SSL_VERIFYPEER 0)

  (set! capcha-req (create-request))
  (set! capcha-header (curl-slist-append capcha-header "appkey:evilbinary"))
  (set! capcha-header (curl-slist-append capcha-header "appsecret:bc5b3a30-9e47-11ec-ac67-3c15cddee2d8"))
  (set! capcha-header (curl-slist-append capcha-header "Content-Type:application/x-www-form-urlencoded"))
  ;;(curl-easy-setopt capcha-req CURLOPT_VERBOSE 1)
  (curl-easy-setopt capcha-req CURLOPT_HTTPHEADER capcha-header)

  ;;(curl-easy-setopt capcha-req CURLOPT_SSL_VERIFYPEER 0)

  ;;(curl-easy-setopt curl CURLINFO_COOKIELIST cookies )
  ;;(curl-easy-setopt curl CURLOPT_VERBOSE 1)

  (try
   (run)
   (catch (lambda (x)
	    (display-condition x)(newline) )))

  (destroy-request curl)
  (curl-global-cleanup))


;;main entry
(main)
