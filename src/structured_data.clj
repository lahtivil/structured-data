(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x) ]
  (Math/pow xx xx)))

(defn spiff [v]
  (let [x (get v 0)
        y (get v 2)]
    (+ x y)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    do (Math/abs (- x2 x1))))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    do (Math/abs (- y2 y1))))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    do (== (- (- x1 x2) (- y1 y2)) 0)))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    do (* (- x2 x1) (- y2 y1))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [px py] point]
    do (and (and (>= px x1)(>= py y1))(and (<= px x2)(<= py y2)))))

(defn contains-rectangle? [outer inner]
  (let [[[ix1 iy1] [ix2 iy2]] inner]
    do (contains-point? outer (point ix2 iy2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (count (:authors book))))

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (let [dead (:death-year author)]
    (not (number? dead))))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [sq-of-2nd-elems (fn [x] (get x 1))]
    (map sq-of-2nd-elems collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= (seq a-seq))(apply >= (seq a-seq))))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
      (disj a-set elem)
      (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [a-set (set a-seq)]
   (< (count a-set) (count a-seq))))


(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union(map :authors books)))

(defn all-author-names [books]
  (set(map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        byear (str(:birth-year author))
        dyear (:death-year author)]
    (if (clojure.string/blank? byear) name (str name " (" byear " - " dyear ")"))))

(defn authors->string [authors]
  (if (empty? authors) "" (apply str (interpose ", " (map author->string authors)))))

(defn book->string [book]
  (let [title (:title book)]
    (str title ", written by " (authors->string (:authors book)))))

(defn books->string [books]
  (let [numof-books (count books)]
    (cond
      (zero? numof-books) "No books."
      (== numof-books 1) (str numof-books " book. " (apply str (interpose " " (map book->string books))) ".")
      :else (str numof-books " books. " (apply str (interpose ". " (map book->string books))) "."))))

(defn books-by-author [author books]
  (let [x (:book books)]
   (filter (fn[x] (has-author? x author)) books)))

(defn author-by-name [name authors]
  (let [x (:author authors)
        author (filter (fn [x] (= name (:name x))) authors)]
    (if(empty? author) nil (nth author 0))))

(defn living-authors [authors]
  (let [x (:author authors)
        alive-authors (filter (fn [x] (alive? x)) authors)]
    alive-authors))


(defn has-a-living-author? [book]
  (let [authors (:authors book)]
    (not (empty? (living-authors authors)))))

(defn books-by-living-authors [books]
  (let [x (:book books)
    books-by-authors (filter (fn [x] (has-a-living-author? x)) books)]
    books-by-authors))

; %________%
