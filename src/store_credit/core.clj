(ns store-credit.core (:gen-class))

(defn read-input
	[filename]
	(let [content (.split (slurp filename) "\n")
		casecount (first content)
		cases (partition 3 (rest content))]
		(for [case cases]
			{:credit (first case)
			:itemcount (second case)
			:items (seq (.split (nth case 2) "[ ]"))})))

(defn solve-case
	[{:keys [credit itemcount items]}]	
		(take 1 
			(filter identity
			(for [x (range (read-string itemcount))
				y (range (read-string itemcount))]
				(if (and (< x y)(= (read-string credit)
					(+  (read-string (nth items x))
						(read-string (nth items y)))))
				[(inc x) (inc y)])))))

(defn solve
  [inputfile tofile]
  (let [cases (read-input inputfile)
  		solutions (map solve-case cases)
  		result (apply str 
  			(map #(str "Case #" (inc %2) ": " (first (first %1)) " " (second (first %1)) "\n") 
  				solutions (range)))]
  		(spit tofile result)))

(defn -main 
	[& args] 
	(solve (first args) (second args)))
