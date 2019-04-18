(ns sudoko-solver.core-test
  (:require [clojure.test :refer :all]
            [sudoko-solver.core :refer :all]))

(def test-grid-9
	"[[1 2 0] [3 0 2] [0 0 0]]")

(def test-grid-81
	"[[0 0 5 0 6 8 7 0 9] [6 0 7 1 0 0 0 0 8] [0 0 3 0 7 2 0 0 0] [0 0 0 0 4 0 0 7 0] [0 0 2 0 1 0 8 0 0] [0 1 0 0 2 0 0 0 0] [0 0 0 7 3 0 4 0 0] [2 0 0 0 0 9 1 0 7] [8 0 4 2 5 0 3 0 0]]")

(deftest input-grids-valid
	(testing "Input grids used for testing are valid"
		(is (and (grid-valid? (read-string test-grid-9)) (grid-valid? (read-string test-grid-81))))))

(deftest invalid-grids-are-invalid
	(testing "grid-valid? working for some invalid grids"
		(is (and (not (grid-valid? (read-string "[[1 1 0] [0 0 0] [0 0 0]]"))) (not (grid-valid? (read-string "[[1 0 0] [0 0 0] [1 0 0]]")))))))

(deftest test-grid-solutions-valid?
  (testing "Runs main for test grids"
    (is (and (grid-valid? (-main test-grid-9)) (grid-valid? (-main test-grid-81))))))
