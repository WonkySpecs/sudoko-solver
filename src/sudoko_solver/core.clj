(ns sudoko-solver.core
	(:gen-class))

(def num-boxes-x 
	3)

(def num-boxes-y 
	3)

(def test-grid-9
	[[1 2 0] [4 0 6] [0 8 0]])

(def test-grid-81
	[[0 0 5 0 6 8 7 0 9] [6 0 7 1 0 0 0 0 8] [0 0 3 0 7 2 0 0 0] [0 0 0 0 4 0 0 7 0] [0 0 2 0 1 0 8 0 0][0 1 0 0 2 0 0 0 0] [0 0 0 7 3 0 4 0 0] [2 0 0 0 0 9 1 0 7] [8 0 4 2 5 0 3 0 0]])

(defn select-range
	"Inclusive of end, exclusive of start"
	[coll start end]
	(->>
		coll
		(take end)
		(drop start)))

(defn no-duplicates?
	"Considers 0s as blank"
	[coll]
	(let [coll-without-zeros (filter #(> % 0) coll)]
		(= (count coll-without-zeros) (count (distinct coll-without-zeros)))))

(defn box-including
	[[x y] grid]
	(let [box-width (quot (count (nth grid 0)) num-boxes-x) box-height (quot (count grid) num-boxes-y)
		  box-x (quot x box-width) box-y (quot y box-height)
		  start-col (* box-x box-width) start-row (* box-y box-height)]
	  	(reduce (fn [selected row] (into selected (select-range row start-col (+ start-col box-width)))) [] (select-range grid start-row (+ start-row box-height)))))

(defn grid-square-valid?
	[grid [x y]]
	(and (no-duplicates? (nth grid y))
		 (no-duplicates? (map (fn [row] (nth row x)) grid))
		 (no-duplicates? (box-including [x y] grid))))

(defn grid-valid?
	"Note - this only checks the diagonal, which we can get away with as grid-square-valid? checks the whole row and column of the 'square'
	 This means this method checks each row once, each column once, and each box n times where the boxes are n x n"
	[grid]
	(every? (fn [coord] (grid-square-valid? grid coord))
		(map vector (range (count grid)) (range (count (nth grid 0))))))

(defn grid-with-value
	[grid value [x y]]
	(assoc-in grid [y x] value))

(defn find-valid-number
	[grid coord [next-blank & remaining :as all-remaining]]
	(loop [value 1]
		(let [try-grid (grid-with-value grid value coord)]
			(if (grid-square-valid? try-grid coord)
				(if next-blank
					(let [next-try (find-valid-number try-grid next-blank remaining)]
						(if next-try
							next-try
							(if (= 9 value)
								false
								(recur (inc value)))))
					(str "SOLVED: " try-grid))
				(if (= 9 value)
					false
					(recur (inc value)))))))

(defn solve
	[clue-grid]
	(let [all-coords (for [x (range (count (nth clue-grid 0))) y (range (count clue-grid))] [x y])]
		(let [[first-blank & remaining-blanks] (filter (fn [[x y]] (= (nth (nth clue-grid y) x) 0)) all-coords)]
			(find-valid-number clue-grid first-blank remaining-blanks))))


(defn -main
	"Sudoko solver. Expects input sudoko to be a sequence of sequences"
	[]
	(println "GO")
	(let [grid test-grid-81]
		(if (grid-valid? grid)
			(println (solve grid))
			(println "Invalid input grid"))))
