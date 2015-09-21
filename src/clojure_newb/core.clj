 (ns clojure-newb.core
  (:gen-class))

(use 'clojure-newb.core :reload) ; reload the repl

(def bbd 90)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "I'm a little teapot!"))

(def mydocs
  {:nrepl (def nrepl
            {:refresh "(use 'clojure-newb.core :reload)"
             :emacs-kbd-macro "C-( to open, then C-) to close"
             :start-nrepl "M-x cider-jack-in"
             :quit-nrepl "nrepl-close"
             })

   :vect1 (def vect1 (vector "hi" "there" "you"))

   :datatypes (def datatypes {
                              :strings "Double Quotes"
                              :maps {:a "this " :b "is " :c "a map."}
                              :vectors [3 1 2 "this is a vector" "Vectors are 0-indexed collections"]
                              :vector-notes ["elements get added to the elnd of a vector, using conj"]
                              :lists '("similar to vectors" "can't use 'get' to retrieve") 
                              :list-notes ["elements are added to beginning of a list"]
                              :sets #{"sets are collections of UNIQUE values" 2 3 4 [23 34] "hi ;)"}
                              :sets-notes #{ "the 'set' function will create sets from existing VECTORS and LISTS" ["check out: " "hash sets " "sorted sets"]}
                              :set-use #{"see if an element exists in a collection"}
                              :remember "don't use parentheses alone to make a list like in other languages"
                              })

   :symbols-and-naming (def symbols-and-naming
                         {:ch2.9 "2.9"
                          :symbols "used to refer to something. Like with def or defn"
                          :as-data "symbols can be manipulated as data. I think this is done by adding an apostrphe in front of them, like 'symbols-and-naming"
                          })
   :quoting (def quoting
              {:ch2.10 "2.10"
               :unevaluated "quoted symbols will be unevaluated."
               :collections "collections can also be quoted, and all symbols within them will also be unevaluated."})
   :methods (def meths
              {:general "general collection of methods, or pre-made funcitons"
               :inc "increments a number by one"
               :map "creates a new list by applying a function to each member fo a collection"
               :doc "used in the REPL to see documentation. Functions can be added to the docs by including a docstring."})

   :functions  (def functions
                 {:ch3.1 "functions"
                  :recursive-eval "Function args are evaluated recursively before passing to the function"
                  :function-calls "Expressions which have a function expression as the operator"})

   :special-forms (def special-forms
                    {:ch3.2 "3.2"
                     :special-forms "don't always evaluate their operators"
                     :if "if is an example of a special form"
                     :non-args "can't be used as arguments in functions"
                     :core-functionality "implement core cunfionality that can't be achieved otherwise"})

   :defining-functions (def defining-functions
                         {:ch3.3 3.3
                          :ingredients ["defn"
                                        "a name"
                                        "(optional) a docstring"
                                        "parameters"
                                        "the function body"]
                          :ex (defn too-enthusiastic
                                "Return a cheer that might be a bit too enthusiastic"
                                [name]
                                (str "OH. MY. GOD! " name " YOU ARE MOST DEFINITELY LIKE THE BEST "
                                     "MAN SLASH WOMAN EVER I LOVE YOU AND WE SHOULD RUN AWAY TO SOMEWHERE"))
                          :docstring ["Return a cheer..." "Used for documenting, maybe other things?"]
                          :parameters {:a (defn no-params [] "I take no parameters!")
                                       :b (defn one-params [x] (str "I take " x " as a param."))
                                       :c (defn two-params [x y] (str x " and " y " are my params."))}
                          :multi-arity (defn multi-arity
                                         "Example showing a function that runs differently depending on the number of arguments supplied."
                                         ;; 3-arity arguments and body
                                         ([first-arg second-arg third-arg]
                                          (str first-arg " " second-arg " " third-arg " three args!"))
                                         ;; 2-arity arguments and body
                                         ([first-arg second-arg]
                                          (str "just two args -> " first-arg second-arg))
                                         ;; 1-arity argument and body
                                         ([first-arg]
                                          (str "Whoa, " first-arg " is the arg!")))
                          :multi-arity-ex (defn cat-by-night "Multi-arity example. What's going to happen to the cat(s) when they go out at night? Could be called like this: (cat-by-night args)"
                                            ([cat1 mood1 cat2 mood2]
                                             (str cat1 " goes to the roof because he's feeling " mood1 ", while " cat2 " strolls under the moon, feeling " mood2 "."))
                                            ([cat1 mood1]
                                             (str "Feeling " mood1 ", " cat1 "decides to seek out company."))
                                            ([cat1]
                                             (str "A wonderful night it is, when " cat1 " can wander in the fresh breeze under the ripe full moon. To " cat1 ", this moment is one for savoring." )))
                          :rest-params [(defn codger-communication "Rest-Params must come last. Rest as in all-the-rest. Unlimited parameters. "
                                          [whippersnapper]
                                          (str "Get off my lawn, " whippersnapper "!!!"))
                                        (defn codger
                                          [& whippersnappers] ;; amersand --> rest-param, for unlimited params
                                          (map codger-communication whippersnappers))]
                          :rest-params-mix (defn favorite-things
                                             "Can mix params with rest-params, but rest-params must come last."
                                             [name & things] ;; & things is the rest-param.
                                             (str "Hola, " name  ", these are my faves: "
                                                  (clojure.string/join ", " things)))
                          })         ; end defining functions

   :destructuring {:ch3.3.3 "Destructuring"
                   :def "Bind symbols to values within a collection."
                   :ex1-vectors  (defn my-first ;returns first element of a collection
                                   "for vectors, provide a vector in the parameters"
                                   [[first-thing]] ; first-thing is in a vector
                                   first-thing) 
                   ;; call with (my-first ["oven" "bike" "lover"]) => "oven"
                   :ex3-maps (defn treasure-location [{ lat :lat lng :lng}]
                               (println (str "Treasure lat: " lat ))
                               (println (str "Treasure lng: " lng)))
                   ;; call-> (announce-treasure-location {:lat 28.22 :lng 81.33})
                   ;; interesting that the destructuring for the map seems to show 
                   ;; associate lat with value linked to key :lat...
                   :ex4-maps-quickbreakout (defn wheres-treasure
                                             "Breaks keywords out of a map"
                                             [{:keys [lat lng]}]
                                             (println (str "Treasure lat: " lat))
                                             (println (str "Treasure lng: " lng)))
                   :ex-5-maps-chang-key  (defn sailto-treasure ; not working. don't grok.
                                           "Breaks keywords out of a map"
                                           [{:keys [lat lng] :as treasure-location}]
                                           (println (str "Treasure lat: " lat))
                                           (println (str "Treasure lng: " lng)))
                   :function-body (defn illustrative-function
                                    "Clojure automatically returns last form evaluated. Here, 'jojo'"
                                    []
                                    (+ 32 43)
                                    20
                                    "jojo")}
   
   :anonymous-functions {:basic (fn [param-list] "function body")
                         :ex1 (map (fn [name] (str "Hi, " name)) ["Aphrodite" "Dionysos"])
                         ;; => "Hi, Aphrodite" "Hi, Dionysos"
                         :ex2 ((fn [x] [* x 3]) 8) ; => 24
                         :ex3-assoc-w-name (def multi-mania (fn [x] (* x 3))) ; (multi-mania 12) => 36
                         :compact #(* % 3) ; => % => argument to be passed
                         :ex-call-anon-compact (#(* % 3) 8) ; => 24
                         :nameit (def compacted #(* % 4))
                         :not-cmpct (defn not-compact [%] (* % 4)) ; params need vector []
                         :multi-args (#(str %1 " and " %2) "milk" "honey") ; multipls args %1 %2 => "milk and honey"
                         :rest-param-cmpct (#(identity %&) "Hi" 43 :uo) }
   :returning-functions [(defn inc-maker
                           ; "create a custom incrementor"
                           [inc-by]
                           #(+ % inc-by))
                         (def inc3 (inc-maker 3))
                         ; (inc3 7) => 10
                         ]
   :let "(let [x 3] x)" ; => 3
   :let2 [(def cat-list
            ["Sticky" "Gray" "Spotty" "Moonlight"])
          (defn let-cats [] (let [cats (take 2 cat-list)] cats))
          ;;let's try without let
          (defn nolet-cats [] (take 2 cat-list)) ; it works!
          ]
   
   } ; end mydocs{map}
  ) ; end mydocs



(def looky {:a "whoa" :b "Sticky is love" :c "Gray is lovely"})

;; Hobbit - The Shire's Next Top Model

(def asym-hobbit-body-parts [{:name "head" :size 3}
                             {:name "left-eye" :size 1}
                             {:name "left-ear" :size 1}
                             {:name "mouth" :size 1}
                             {:name "nose" :size 1}
                             {:name "neck" :size 2}
                             {:name "left-shoulder" :size 2}
                             {:name "left-upper-shoulder" :size 3}
                             {:name "chest" :size 10}
                             {:name "back" :size 10}
                             {:name "left-forearm" :size 3}
                             {:name "abdomen" :size 6}
                             {:name "left-kidney" :size 1}
                             {:name "left-hand" :size 2}
                             {:name "left-knee" :size 2}
                             {:name "left-thigh" :size 4}
                             {:name "left-lower-leg" :size 3}
                             {:name "achilles" :size 1}
                             {:name "left-foot" :size 2}])

;;  Given a sequence (in this case, a vector of body parts and their sizes), continuously split the sequence into a "head" and a "tail".

(defn needs-matching-part?
  [part]
  (re-find #"^left-" (:name part)))

(defn make-matching-part
  [part]
  {:name (clojure.string/replace (:name part) #"^left-" "right-")
   :size (:size part)
  }
)

(defn symmetrize-body-parts
  "Expects a seq of maps which have a :name and :size"
  [asym-body-parts]
  (loop [remaining-asym-parts asym-body-parts ; remaining-asym = asym-body
         final-body-parts [] ]    ; final-body-parts = [] empty vector
    (if (empty? remaining-asym-parts) ; if remaining-body-part is empty...
      final-body-parts                ; return final-body-parts 
            ;; "part" = 1st of "remaining-asym-parts", remaining" = the rest
       ; split remaining-asym-parts into head and body: part and remaining respectively
      (let [ [part & remaining] remaining-asym-parts ; head/body parts split
            ; put part (1st element of remaining-asym-parts) onto the end of final-bosy-parts
            final-body-parts (conj final-body-parts part)] ; assoc "final-body-parts w/ result of (conj final-body-parts part)
        (if (needs-matching-part? part) ; call needs-matching; if true, recur
          (recur remaining (conj final-body-parts (make-matching-part part)))
          (recur remaining final-body-parts))))))
      ; because of if statement, both recurs are in tail position

;; (symmetrize-body-parts asym-hobbit-body-parts)
;; end hobbit symmetrizer



;; Second Hobbit Symmetrizer



(defn needs-right?
"use regex to match with 'left'"
  [part]
  (re-find #"left" (:name part)))


(defn build-right-side
  "generate right side of body part"
  [part]
  {:name (clojure.string/replace (:name part) #"left" "right")
   :size (:size part)})


(defn add-right-side
  [left-siders]
  (loop [unfinished left-siders
         final-group [] ]
    (if (empty? unfinished)
      final-group  ; if empty, return final-group
      (let [ [first & rest] unfinished
             final-group (conj final-group first)]
        (if (needs-right? first) ; calls needs-right? on first
          (recur rest (conj final-group (build-right-side first)))
          (recur rest final-group))))))

;; (add-right-side asym-hobbit-body-parts)

;; symmetrizer with reduce

(defn symmetrize-better
  "expects seq of maps with :name and :size"
  [asym-parts]
  (reduce (fn [final-parts part] ; two params?
            (let [final-parts (conj final-parts part)]
              (if (needs-right? part)
                (conj final-parts (build-right-side part))
                final-parts)))
          []
          asym-parts))



;; Hobbit Violence

(defn hit
  [asym-body-parts]
  (let [sym-parts (symmetrize-better asym-body-parts) ; sym-parts result of call
        body-part-size-sum (reduce + 0 (map :size sym-parts))  ; body-part-size
        target (inc (rand body-part-size-sum))]
    (loop [[part & rest] sym-parts
           accumulated-size (:size part)]
      (if (> accumulated-size target)
        part
        (recur rest (+ accumulated-size (:size part)))))))





(defn headtail
    [collection]
    (map println collection))
;;; 
;; (defn printparts [asym-parts]
;;   (map println asym-parts))

(defn loopme [] (loop [i 0]
                 (println (str "Iteration " i))
                 (if (> i 3)
                   (println "Goodbye ^^")
                   (recur (inc i)))))
 
(defn loopz [] 
  (loop [x 10]
    (when (> x 1)
      (println x)
      (recur (- x 2)))))

;; (defn foursome [] (for [3] (println ("hi"))))

(defn fact-loop [n]
  (loop [current n fact 1]
    (if (= current 1)
      fact
      (recur (dec current) (* fact current)))))

(defn loopsum [c]
  (loop [sum 0 cnt c]
    (if (= cnt 0)
      sum
      (do
        (println sum)
        (recur (+ cnt sum) (dec cnt))))))


(defn loop5 [n]
  (loop [i 1]
    (when (> i n)
      (println i)
      (recur (inc i)))))

(defn loopw [n m] 
  (loop [i n j m] ; two args, i is n, j is m -- within loop's scope they can be mutable
    (println i j)
    (if (< i 1)
      (println "Time to go shopping")
      (recur (dec i) (inc j))))) ; recur can do more than one thing at a time!

(defn do-me [n]
  (let [x n]
    (if (< x 5)
      (println "bye, baby")
      (do
        (println "hi,")
        (println "baby")
        (dec x)
        (println x)
        ;; (dec x)
        (println "ohhuh")
        ))
    )
  )

(defn huh? [num]
  (let [neu num]
    (println (dec neu))
    (println neu) 
    ))

(defn loopy []
  (loop [i 0]
    (if (> i 5)
      (println "That's all for now.")
      (do
        (println i)
        (recur (inc i))))))

;; what do we need to do?

(defn add-right-parts
  [part]
  (println part)
)


(defn factorial [n] (apply * (range 1 (+ 1 n))))

(defn feedthem
  [beast feast]
  (str feast " for " beast))

(defn feedthem-mapped
  [beast feast]
  (clojure.string/capitalize (str feast " for " (clojure.string/capitalize beast) ".")))


;; (loop [i 0]  
;;   (when (< i 5)    
;;     (println i)    
;;     (recur (inc i)); loop i will take this value
;;     ))

(defn lrecur [n]
  (loop [result []
         i n]  
  (if (< i 5) 
    (do
      (inc i)
      (recur (conj result i) (inc i)))
    result)))



(def gre 24)

(def life-means? 42)

(def meaning-of-life 42)

(defn label-key-val
  [[key val]]
  (str "key: " key ", val: " val ))


;; (def mapmap
;;   [f]
;;   (map f {:name "Talbert de Sang"
;;           :occupation "perennial high-schooler"}))


;; (true? ( #( ) :a {:a nil :b 3 :c "f"} ))

;; (false? ( :b {:a nil :b 3 :c "f"} ))

;; (false? ( :c {:a nil :b 3 :c "f"} ))


;; (#(and (%1 %2)))

(defn factorial  [x]
  (if (zero? x)
    1
    (* x (factorial (dec x)))))

(defn factorial2 [x]
  (reduce * (range 1 (inc x))))



;;(fn [coll] (let count coll) )

(def bzz [2 1 4 3])
(def vvv (count bzz) )

;; get the last element of a sequence
(defn getlast [coll]
  (let [len (count coll)]
    (let [final (- len 1)]
      (nth coll final))))

;; Can also be written like this

(= "a" ( #(get % (- (count %) 1)) [1 2 3 4 "a"]) )

;; Or with the additional . in front of .get

(= "a" ( #(.get % (- (count %) 1)) [1 2 3 4 "a"]) )


(= ( #(get % (- (count %) 2))
     (list 1 2 3 4 5)) 4)

(defn penul [coll]
  (let [len (count coll)]
    (let [sectolast (- len 2)]
      (nth coll sectolast))))


#( let [c (count %)] (nth %(- c 2)))


#(second (reverse %))
;; (= ( #((let [len count %] let [secondlast (- len 2)] (nthsecondlast))

;;     (List 1 2 3 4 5)) 4))

;; def
;; (defn penul [coll]
;;   (let [len (count coll)]
;;     (let [sectolast (- len 2)]
;;       (nth coll sectolast)
;;       )
;;     )
;;   )


(defn secondlast [coll]
  (let [c (count coll)]
    (nth coll (- c 2))))

(#( let [c (count %)]
    (nth % (- c 2))
    ) [ 2 3 4] )

(defn map2arr [a b]
  ( assoc {} ( b a))
  )


(def aa 1)
(def bb 2)
(def cc 3)



;; (__ 0 [:a :b :c])

(defn inter-arr [a b]
  (assoc {} )
  )

(defn in2arr [a b]
  (into {} (map b (repeat a ()))) 
  )

;;  still don't understand this well enough
(defn zipit [a b]
  zipmap b (repeat a))

;; #21 nth element. Restriction: nth

;; (=   ( '(4 5 6 7) 2) 6)

;; (defn nth-el [coll guide]
;;   (get coll guide)
;;  )


(apply vector '(4 5 6 7) )
;
;; (fn findnth
;;   (apply vector coll)
;;   )


;; for guide +1 conj 
(defn finth [coll guide]
  (let [vcol (apply vector coll)]
    (let [box []]
    (for [guide vcol]
      (conj box vcol))
    box
    )))

;; (defn conjure [coll guide]
;;   (let [box []]
;;     (let [package (for conj box coll)]
;;       package)))

(defn nthesis [coll ind]
  (let [vcol (apply vector coll)]
    (for [x vcol]
      (inc vcol))))


; (= (__ [:a :b :c] 0) :a)






(defn nthloop [coll guide]
  "to find the nth number, loop through to number of guide. If count = guide, return number."


  )

;; try to work a loop that can work without the specifics here, but can recur over a collection, maybe by separating first and rest...

;; (defn loopster [coll guidenum]
;;   (loop [[prem & rest] coll
;;          cnt 0
;;          newvec []]
;;     (if (= (cnt guidenum))
;;       guidenum
;;       (recur (inc cnt) (conj newvec prem ))
;;       )))

(def lalal {:lala "hello there"})
(def jalal :okok )
(def hmmm "hmmmus")

(defn loopsum [x]
  (loop [sum 0 cnt x]
    (if (= cnt 0)
      sum
      ;;else use recur to feed back to loop
      (recur (+ cnt sum) (dec cnt)))))

(defn loopmen [ coll x ]
  (loop [cnt 0 clect coll]
    (if (= cnt x)
      (first clect)
      (do
        (println "clect is: " clect)
        (recur (inc cnt) (rest clect))))))


;; 4Clojure question # 21
;; Write a function which returns the Nth element from a sequence.

;; This was hard for me because I wasn't clear about recursion.
;; I had a basic idea from reading The Little Schemer on the Seoul Metropolitan Subway,  but the specifics are tricky even after you grasp the idea.
;; The hardest part in fact was the syntax.
;; I kept on getting error messages about Longs not fitting into Seqs, etc etc, for ex "java.lang.ClassCastException: clojure.lang.PersistentList cannot be cast to clojure.lang.IFn"

;; also I didn't understand the interplay of arguments in the function definition and the loop definition (since the loop is a kind of function in itself, with, crucially, its own scope).

(defn loopnth [coll i]
  (loop [cnt 0 clect coll]
    (if (= cnt i)
      (first clect)
      ;; else
      (recur (inc cnt) (rest clect)))))


;; 4clojure #156: Write a function which takes a default value and a sequence of keys and constructs a map.


;; (= (__ 0 [:a :b :c]) {:a 0 :b 0 :c 0})

;; (= (__ "x" [1 2 3]) {1 "x" 2 "x" 3 "x"})

;; (= (__ [:a :b] [:foo :bar]) {:foo [:a :b] :bar [:a :b]})

;; idea: two args [fyrst sekond]
;; recursively: split sekond into head & rest, conj head and fyrst into new vector
;; when no more rest,
;; turn the vector into a map and return it.
;;
;; Is vector necessary?

;; (defn vkmap
;;   [value keys]
;;   (loop [remainkeys keys  
;;          newvec []]
;;     (if empty? remainkeys)
;;     newvec ;not done...
;;     (let [[head & body] remainkeys
;;           newvec (conj newvec head)]
;;       recur )))

(defn first-rest [[frst & rst]]
  (let [nvec []
        rvec []]
    (println (str "This is nvec: ") (conj nvec frst))
    (println (str "The rest is: "  (apply conj rvec rst)))))



;; (defn loop-rest [coll]
;;   (loop [nvec []] 
;;     (if empty? rest)
;;     nvec
;;     (recur (conj (first coll) nvec))))

;; Basic loop
(defn testmeonce [coll]
  (loop [nvec [] clect coll]
    (if (empty? clect)
      nvec
      (do
        (println "clect is: " (into [] clect) ". nvec is: " nvec)
        (recur (conj nvec (first clect)) (rest clect))))))

 
;; 4clojure #156
;; Write a function which takes a default value and a sequence of keys and constructs a map.
;; (= (__ 0 [:a :b :c]) {:a 0 :b 0 :c 0})

;; (= (__ "x" [1 2 3]) {1 "x" 2 "x" 3 "x"})

;; (= (__ [:a :b] [:foo :bar]) {:foo [:a :b] :bar [:a :b]})

;; I'm *not* trying to do this the easy way, or using a built-in function.
;; Instead, let's get loop / recur down straight. That seems much more important.

(defn fc156 [vlyu coll]
  (loop [v vlyu , clect coll , nmap {}]
    (if (empty? clect)
        (do
          (println v)
          (println clect))
        (do
          (println clect)
          (recur (v) (assoc nmap (first clect) v) (nmap) )))))

;; this works
(defn fc156t [vlyu coll]
  (assoc {} (first coll) vlyu))
;; (fc156t 0 [ 1 2 3])
;; => {1 0}

(defn fc156t2 [vlyu coll]
  (let [nmap {}]
    (assoc nmap (first coll) vlyu)))
;; (fc156t2 0 [ :a 2 3])
;; => {:a 0}

;; not tested or ready
;; (defn fc156t3 [vlyu coll]
;;   (loop [nmap {} v vlyu clect coll]
;;     (assoc nmap (first clect) v) (rest clect) ))

;; now let's test the recursion part
(defn fc156t4 [vlyu coll]
  (loop [nvec [] clect coll]           ; clect starts as coll, our arg
    (if (empty? clect)
      (println "This is " nvec)
      (recur (conj nvec (first clect))
             (rest clect)))))


;; and now we can combine both the map part and the recursion part
;; there are problably

(defn fc156t5 [vlyu coll]
  (loop [nmap {} clect coll]           ; clect starts as coll, our arg
    (if (empty? clect)
      nmap ; final return 
      (recur (assoc nmap (first clect) vlyu)
             (rest clect))))) ;  both args in the recur

;; The concise way to do this is to use zipmap and repeat.
(defn zipmymap [kys vls] ; kys -> keys, vls -> values
  (zipmap kys vls))

;; Anonymous version
#(zipmap % (repeat %2))
;; You'd call it like this:
(#(zipmap % (repeat %2)) [:a :b :c] [1 2 3])
;; => {:c [1 2 3], :b [1 2 3], :a [1 2 3]}

;; for the 4clojure question you have to reverse the args, but that's about it.




(defn fc156fast [value coll]
  (zipmap coll (repeat value)))


(def stickymap {:sticks "awesome"})


;; ---------------------
;; From clojuredocs.org:
;; if-let 
(defn sum-even [nums]
  (if-let [nums (seq (filter even? nums))]
    (reduce + nums)
    "No evens found."))

;; Let's try to change the arg name in the let
;;Both work! if-let can use the original arg name for its new arg.



(defn sum-even-nuhms [nums]
  (if-let [nuhms (seq (filter even? nums))]
    (reduce + nuhms)
    "No evens found."))

;; What if we take away the (seq )... is it really necessary?
(defn sum-even-2 [nums]
  (if-let [nums (filter even? nums)]
    (reduce + nums)
    "No evens found."))
;; (sum-even-2 [1 2 3 4])
;; => 6
;; (sum-even-2 [ 1 3 5 7])
;; => 0
;; Why exactly?


;; -----------------------
;; 4CLOJURE #22  ;
;; (yes, it comes after #152)

;; Write a function which returns the total number of elements in a sequence.
;; (= (__ '(1 2 3 3 1)) 5)
;; (= (__ "Hello World") 11)
;; (= (__ [[1 2] [3 4] [5 6]]) 3)
;; (= (__ '(13)) 1)
;; restriction: count

;; First thoughts.
;; Use loop / recurs again? If so, could "drop" or "pop," couting loops until empty. That's a simple way. Let's do it, and het more loop / recur practice.
;; I'm wondering, however, what exactly is an "element"?


(defn fc22 [sequins]
  (loop [cnt 0, sqns sequins]
    (if (empty? sqns)
      cnt
      ;else
      (recur (inc cnt) (pop (vec sqns))))))

;; This works fine for numbers and so on, but not for strings!
;; pop takes the removes the first element of a sequence.

(defn fc22a [sea-quince]
  (loop [cnt 0, sequins sea-quince]
    (if (empty? sequins)
      cnt
      (recur (inc cnt) (pop (vec sequins))))))

;; There we go, that passes.
;; again, there are interesting short ways of doing this.


;; 4clojure 24
;; Write a function which returns the sum of a sequence of numbers.
;; (= (__ [1 2 3]) 6)

;; (= (__ #{4 2 1}) 7)

;; Seems pretty straight forward.

(defn sumseq [sq]
  (reduce + sq))

;; anonymous version
#( reduce + %)

;; another was simply
apply +

;; I still don't fully underestand apply. I've need to.


;; ----------------------------------
;; 4Clojure #23 Reverse a Sequence --
;;-----------------------------------
;; Write a function which reverses a sequence.
;; can't use "reverse" or "rseq".
 
;; (= (__ [1 2 3 4 5]) [5 4 3 2 1])
;; (= (__ (sorted-set 5 7 2 7)) '(7 5 2))
;; (= (__ [[1 2][3 4][5 6]]) [[5 6][3 4][1 2]])

;; For this one, empty might work.
;; Oddly, though, the second example they use gives us a sorted-set, but requires a list '().
;; (empty) returns a set #{}, so maybe this won't work here?

(defn esrever [sq]
  (loop [nsq (empty sq), sqn (vec sq)]
    (if (empty? sqn)
      nsq
      (recur (conj nsq (peek sqn)) (vec (drop-last sqn)) ))))

;; As predicted, this works for all but the "sorted-set" question.

;; This case highlights the importance of knowing one's data structures.
;; I heard osmeone say that if you know the data types well, the right algorithm comes naturally. I didn't really understand wht they meant, though it sounded nice, but here is an example, I think. Though "algorithm" is a bit much here...

;; Simply:
(defn esrever-a [coll]
  into () coll)
;; and anonymously
#(into () %)

;; Wow! This is because conj(oining) to a list happens at the beginning.
;; I'd read this, but it hadn't sunk in well enough. In fact, I only saw it as a problem, because up to now vectors have been my datatype of choice.
;; The other thing to realize: *Sequence* datatypes (vectors, lists) are compared for equality by elements. So
 (= '(1 2 3) [1 2 3])
;; => true

;; __________________________________
;;   4CLOJURE #27 check palindrome
;; __________________________________
;; (false? (__ '(1 2 3 4 5)))
;; (true? (__ "racecar"))
;; (true? (__ [:foo :bar :foo]))
;; (true? (__ '(1 1 3 3 1 1)))
;; (true? (__ '(1 1 3 3 1 1)))
;; __________________________________

;; a few ideas - check the first half against the second,
;; might be messy, like requires a 
;; let's try a loop, to make it easy for now

(defn elle [sq]
  (let [sqr sq]
    (if (string? sqr)
      (= (apply str (into () sqr)) sq)
      (= (reverse sqr) sq))))

;; OK, so I took the cheap route, doing a type check. But hey, I've never done one before...

;; The missing ingredient was "seq".
;; The problem with reverse is that it does this:
;; (reverse "nice")
;; => (\e \c \i \n)
;; So we just need to check against the first-order string in a way that will escape the characters as well. And this means turning it into a seq.

;; (seq "nice")
;; => (\n \i \c \e)

;; Then we check those against each other.
(defn palin [sq]
  (= (reverse sq) (seq sq)))

;; that's is. faster:
#(= (reverse %) (seq %))

;; pretty amazing brevity. Another lesson in 


;; _______________________
;; 4Clojure 26  FIBONACCI
;; _______________________

;; Write a function which returns the first x fibonacci numbers

;; (= (__ 8) '(1 1 2 3 5 8 13 21))

;; There's probably an extraordinarily terse way to do this.
;; But let's try a loop / recur?

(def n1 '(1) )

;; (defn fib1 [x]
;;   (let [f []]
;;     (loop [cnt x, n 1]  ; cnt is x because we'll decrement to 0
;;       (if (= cnt 0)
;;         f
;;         (recur (dec cnt)
;;                (conj f (+ n (- n 1)))))))) 

;; (defn ftest [x]
;;   (let [n x, m (- n 1), f []]
;;     (if (< n 2)
;;       n
;;       )))

(defn halver [x]
  (let [ r []]
    (loop [n x]
      (if (< n 2)
        r
        (do
          (conj r n)
          (recur (/ n 2) ))))))

(defn ttt [x]
  (loop [n x, v [1], cnt x]
     (when (> cnt 1)
       (println "hi")
       (println v)
       (recur (+ n (- n 1)) (conj v n) (dec cnt)))))


(defn ccc [x]
  (let [v []]
    (conj [] (/ x 2))))

;; not like a fib, but some mechanics.
(defn fibbit [x]
  (loop [fsq '(), n x]
    (when (> n -1)
      (println "fsq is: " fsq ", and n is " n)
      (recur (conj fsq (+ n (- n 1)))
             (- n 1)))))

;; I had a terrible time of things, because I didn't understand that loop args and recur args have to be inthe same order. That's to say that you state you arguments in the same order that you recur on them. Or else.


(defn poornocci [x]
  (loop [cnt 1,
         res [1],
         a 1
         b 0]
    (if (= cnt x)
      res
      (recur (inc cnt)
             (conj res (+ a b))
             (+ a b)
             (nth res (- cnt 1))))))

;; This is probably a terrible way to do a fib, although it works and takes adv of tail call optimization.
;; Already I can get rid of "b".

; (require '[clojure.string :as str])

(def fishy "one fish two      fish red   fish blue fish 발간 생선")

;; get this: {"one" 1, "fish" 4, "two" 1, "red" 1, "blue" 1}

;
;; (defn count-words [st]
;;   (let [res (str/split st #" ")]
;;     (frequencies res)))

;;#([let [res (str/split % #"/s+")]])


;; ____________________________
;; 4Clojure 38 Maximum Value
;; ----------------------------
;; Write a function which takes a variable number of parameters and returns the maximum value.
;; RESTRICTIONS: max, max-key
;; (= (__ 1 8 3 4) 8)
;; (= (__ 45 67 11) 67)

;; (defn maxval [values]
;;   (loop [head (first values) body (rest values)]
;;     (println "Head: " head ", Body: " body)
;;     (recur (first body) (rest body))))

;; (defn more-testing [coll]
;;   (let [head (first coll), body (rest coll)]
;;     (println head " + " body)
;;     (cons head body)))


;; (defn maxi-v [coll]
;;   (loop [head (first coll), body (rest coll)]
;;     (println head " & " body)))  


(defn maxmyvals [x & more]
  "Accepts variable number of numerical params and returns their maximum value."
  ;(println "x: " x ", & more: " more)
  (if (>= x (first more))
    x
    (first more)))


(defn maxem [n & nums]
  "integers as parameters"
  (loop [head n, body nums]
    (if (true? (> head (first body)))
      head
      (first body))))

#(true? (<= % %2) 321 123)



(defn maxout [x & more]
  (reduce  #(if (>= %1 %2 ) %1 %2) x more ))

;; (defn recurif? [vctr]
;;   (loop [v vctr]
;;     (if (= (first v) 6)
;;       v
;;; 
;; recur (rest v))))


;; or in a control statement
(defn ifor [vctr]
  (if (or (> (first vctr) 4) (= (first vctr) 234))
    (do (println "yup") 
        (first vctr))
    (do (println "tiny beginning")
        "hiya")))


;; multiple ifs?)

;; (defn ifif [a-string]
;;   (cond
;;     (= a-string "yes") "Cool, Baby"
;;     (= a-string "no") "Oh well, catch you next time."
;;     (= a-string "maybe") "Hmm, well I'll be checking my messages."
;;     :else "Not sure what you mean by that..."))



;; ---------------------------
;; PARTIAL, MAP, REDUCE
;; ---------------------------

;; ((partial * 3) 5)
;; => 15

;; (map (partial * 3) [3 4 5])
;; (9 12 15)

;; (reduce (partial * 3) [3 4 5])
;; => 540 ... 540??


;; (reduce (partial + 2) [ 2 3 4 ])

;; (+ 4 (+ 3 (+ 2 2)))

;; These two are the same:

;; (#(if (> %1 %2) %1 %2) 4 5 )

;; ((fn [x y] (if (> x y) x y )) 4 5)

(defn maxme [n & more]
  "Takes any amount of numerical args, returns the greatest."
  (reduce #(if (> %1 %2) %1 %2) n more))



;; there are other very good solutions, like:
;; #(last (sort %&))
;;

(defn returnem [x & y]
  (apply #(sort %&) x y))


;; OK so you can make a variadic arg like this.
(defn maxmax [ & y]
  (apply max y))

(defn addem [& x]
  (reduce + x))


(defn askit []
  (let [inputme (read-line)]
    inputme))

(defn print-message [pid pw] 
(println "PID : " pid)
 (println "PW : " pw))

(defn inp[]
  (println "Enter your PID ") 
  (let[pid (read-line)]
    (println "Enter your password")
    (let [pw (read-line)]
      (print-message pid pw))))


(defn allcaps [s]
  (apply str (re-seq #"[A-Z]" s)))

 #(apply str (re-seq #"[A-Z]" %&))


;; _________________________
;; 4CLOJURE 32
;; Write a function which duplicates each element of a sequence.
;; _________________________
;; (= (__ [1 2 3]) '(1 1 2 2 3 3))
;; (= (__ [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))


 (#(interleave % %) [1 2 3])
;; => (1 1 2 2 3 3)
;; pretty nice. Seems like if you get good with these built-in functions, this language must be amazing.


  (filter #(= 0 (mod % 10)) (range 100))
;; (0 10 20 30 40 50 60 70 80 90)

 (filter #(= 3 (mod % 10)) (range 100))
;; (3 13 23 33 43 53 63 73 83 93)

  (filter #(= 3 (mod % 11)) (range 100))
;; (3 14 25 36 47 58 69 80 91)
  (filter #(= 0 (mod % 11)) (range 100))
;; (0 11 22 33 44 55 66 77 88 99)

 (map #(mod % 2) (range 100))
;; (0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1)

 ;; ______________________________
 ;; 4CLOJURE 34
 ;; Write a function which creates a list of all integers in a given range.
 ;; RESTRICTED: range
 ;; ______________________________


;;; 
;;; 
(#(take (- %2 %1) (iterate inc %1)) 3 7)

 (defn implement-range [lo hi] (take (- hi lo) (iterate inc lo)))



