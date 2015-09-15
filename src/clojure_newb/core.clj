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
()

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


(defn testmeonce [coll]
  (loop [nvec [] clect coll]
    (if (empty? clect)
      nvec
      (do
        (println "clect is: " (into [] clect) ". nvec is: " nvec)
        (recur (conj nvec (first clect)) (rest clect))))))

;; 
