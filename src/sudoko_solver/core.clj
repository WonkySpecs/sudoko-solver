(ns sudoko-solver.core
	(:gen-class))

(def num-boxes-x 
	3)

(def num-boxes-y 
	3)

(def max-val
	9)

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

(defmacro next-val-or-backtrack
	[v]
	`(if (= max-val ~v)
		false
		(recur (inc ~v))))

(defn find-valid-number
	"Implements recursive backtracking - try a value in a blank cell. If the grid is valid, recurse to the next blank.
	 Otherwise, try the next possible value. If there are no more values to try for the cell, go back to the previoius cell."
	[grid coord [next-blank & remaining :as all-remaining]]
	(loop [value 1]
		(let [try-grid (grid-with-value grid value coord)]
			(if (grid-square-valid? try-grid coord)
				(if next-blank
					(let [next-try (find-valid-number try-grid next-blank remaining)]
						(if next-try
							next-try
							(next-val-or-backtrack value)))
					try-grid)
				(next-val-or-backtrack value)))))

(defn solve
	[clue-grid]
	(let [all-coords (for [x (range (count (nth clue-grid 0))) y (range (count clue-grid))] [x y])]
		(let [[first-blank & remaining-blanks] (filter (fn [[x y]] (= (nth (nth clue-grid y) x) 0)) all-coords)]
			(find-valid-number clue-grid first-blank remaining-blanks))))

(defn -main
	"Sudoko solver. Expects input to be string literal of a sequence of sequences of numbers. 0 represents a blank to be solved"
	[in]
	(let [grid (read-string in)]
		(if (grid-valid? grid)
			(let [solved (solve grid)]
				(println "Solved: " solved)
				solved)
			(println "Invalid input grid"))))
