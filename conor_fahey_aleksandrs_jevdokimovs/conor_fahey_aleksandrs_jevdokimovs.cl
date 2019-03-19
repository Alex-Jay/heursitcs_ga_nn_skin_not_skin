(defparameter *trainingData* '())
(defparameter *testDataFilePath* "")
(defparameter *trainingDataFilePath* "")
(defparameter *testData* '())
(defparameter *tempDataLoad* '())

(defvar *outputLength* 1) ;either skin or not skin
(defvar *inputLength* 3) ; R - G - B
(defvar *hlNeruons* 4)
(defvar *dummyValue* 1)

(defvar *defaultGenerations* 10)
(defvar *populationSize* 5)
(defvar *fittestForReproductionAmount* 4)
(defvar *eliteMemSize* 2)

(defvar *mutationChance* 15)
(defvar *mutationRateHigh* 0.2)
(defvar *mutationRateLow* 0.05)
(defvar *amountOfWeightsMutatedPerChromosome* 7)
(defvar *learningRate* 0.1)
(defvar *totalCorrect* 0)
(defvar *totalError* 0)
(defvar *printErrorRate* 7000)

(defparameter *randomHigh* 1.0)
(defparameter *randomLow* -1.0)

(setf *random-state* (make-random-state t))

(defun reset-all-data ()
    "Reset the variables"
	(setf *trainingData* '())
	(setf *tempDataLoad* '())
	(setf *testData* '())
	(setf *trainingData* '())
	(setf *chromosomes* '())
	(setf *fitnessValues* '())
	(setf *population* '())
)

(defun NN-def-Weights ()
  	"Set up hidden layers: Weight B & Weight A. Define chromosome length (weightB + weightA) [Element-wise]"
  ; Weight B Size
	(setf *inputWithDummyLength*
		(+ *dummyValue* *inputLength*)
	)

  ; Weight A size
	(setf *hlNeruonsWithDummyLength*
		(+ *dummyValue* *hlNeruons*)
	)
	
  ; Weight B & A in one chromosome
	(setf *chromosomeLength*
		(+
     ; 4 * 4 = 16
     ; 5 * 1 = 5
     ; 16 + 5 = 21
			(* *inputWithDummyLength* *hlNeruons*)
			(* *hlNeruonsWithDummyLength* *outputLength*)
		)
	)
	
  ; -------------------------------------- WEIGHT B --------------------------------------
	;no. inputs x no. outputs -> 4 x 4 = 16
  ; :initial-element -> Initial elements value. It will make an array with all the elements initialized to a particular value.
  ; (make-array (list *inputWithDummyLength* *hlNeruons*) :initial-element 1) -> Creates 4x4 matrix
  ; (make-array (4 4) :initial-element 1) -> #2A((1 1 1 1) (1 1 1 1) (1 1 1 1) (1 1 1 1))
	(setf *weightB* (make-array (list *inputWithDummyLength* *hlNeruons*) :initial-element 1)) 
	
  ; -------------------------------------- WEIGHT A --------------------------------------
	;no. inputs x no. outputs -> 5 x 1 = 5
  ; (make-array (list 5 1) :initial-element 1) -> Creates 5x1 matrix
  ; Output: #2A((1) (1) (1) (1) (1))
	(setf *weightA* (make-array (list *hlNeruonsWithDummyLength* *outputLength*) :initial-element 1)) 
	
	T
)

(defun NN-check-gens (generations)
	"Check how many generations passed in, if none set to defaultGenerations [10]"
	(setf generations (- generations 1))
	(if (< generations 0)
		(setf *generations* *defaultGenerations*)
		(setf *generations* generations)
	)
)

 ;(cons 1 2) =>  (1 . 2)
 ;(cons 1 nil) =>  (1)
 ;(cons nil 2) =>  (NIL . 2)
 ;(cons nil nil) =>  (NIL)
 ;(cons 1 (cons 2 (cons 3 (cons 4 nil)))) =>  (1 2 3 4)
 ;(cons 'a 'b) =>  (A . B)
 ;(cons 'a (cons 'b (cons 'c '()))) =>  (A B C)
 ;(cons 'a '(b c d)) =>  (A B C D)

(defun GA-create-initial-chromosome()
  	"Needed for creating random chromosomes for initial population"
  	; Create a chromosome of size 21
    (let* (
	          (chromosome '())
	      	)
		 (dotimes (i *chromosomeLength*)
	         (setf chromosome (cons (float (GA-get-random-weight)) chromosome))
	     )
		 
		 chromosome
	)
)

(defun GA-get-random-weight ()
	(get-random-range *randomLow* *randomHigh*)
)

(defun GA-init-population ()
  "Create the initial population of parent chromosomes with initial fitness of 0"
	(setf *population* (make-list *populationSize* :initial-element 1))
	
	;Create initial population
  ; Population[i] = (CHROMOSOME . FITNESS) <- CONS!!!
	(dotimes (i *populationSize*)
        (setf (nth i *population*) (cons (GA-create-initial-chromosome) 0))
	)
)

(defun NN-convert-chromosome-to-weights (chromosomeToConvert)
    "Convert the chromosome value to set of weights, for the layer 1 (inputToHidden) 
	& layer 2 (hiddenToOutput)"
  	; chromosome-index -> Index of the chromosome with both weights
    (let* (
	          (chromosome-index 0)
	      	)
          
		;WeightB
		(dotimes (x *inputWithDummyLength*) ; Loop 4 times
			(dotimes (y *hlNeruons*)					; Loop 4 times per above loop
        ;weightB at position x,y -> chromosome val
				(setf (aref *weightB* x y) (nth chromosome-index chromosomeToConvert))
				(setf chromosome-index (+ chromosome-index 1))
			)
		)
		
		;WeightA
		(dotimes (x *hlNeruonsWithDummyLength*) 	; Loop 5 times
			(dotimes (y *outputLength*)						; Loop 1 time
        ;weightA at position x,y -> chromosome val
				(setf (aref *weightA* x y) (nth chromosome-index chromosomeToConvert))
				(setf chromosome-index (+ chromosome-index 1))
			)
		)
	)
)

(defun NN-read-training-file ()
  "1: Load data from file [INPUTS, OUTPUT] | 
  	2: Set retrieve data to *trainingData* | 
  	3: Clear cached list (incase we want to load something again)"
	(NN-read-file *trainingDataFilePath*)
	(setf *trainingData* *tempDataLoad*)
	(setf *tempDataLoad* '())
)

(defun NN-read-test-file ()
  "Load & Read test data"
	(NN-read-file *testDataFilePath*)
	(setf *testData* *tempDataLoad*)
	(setf *tempDataLoad* '())
)


(defun NN-train-initial-population ()
    "Decode population to weights
		run them through our fitness function to determine
	fitness values. Store chromosome & fitness as dotted pair"
	(let*
		(
     ; *populationSize* -> Amount of parent chromosomes
     ; *populationSize* -> (NIL NIL NIL NIL NIL)
			(populationFitnesses (make-list *populationSize*))
			(currChromosomePair '()) ; Current parent chromosome
			(dataSetError 0)
		)
		;currChromosomePair
		;Train initial network
   	; Iterate 5 times
		(dotimes (i *populationSize*)
			(setf currChromosomePair (nth i *population*))
      ; *population* -> List of all parent chromosomes
			
      ; car -> chromosome list
      ; cdr -> fitness
			(NN-convert-chromosome-to-weights (car currChromosomePair))
			(format t "~%Start with chromosome # ~4d" i)
			(setf dataSetError (train))
			
			;Insert fitness value for the matching chromosome
			(setf (nth i *population*) 
				(cons (car currChromosomePair) (/ 1 dataSetError))
			)
		)
		
		(dotimes (j *populationSize*)
			(setf (nth j populationFitnesses) (cdr (nth j *population*)))
		)
		
		(print-new-line)
		(format t "~%All Fitness values of current generation ~4d" populationFitnesses)
		(print-new-line)
	)
)


(defun NN-get-fitness-test-set ()
    "Chromosomes are trained and we get their total error and fitness"
	(setf *totalCorrect* 0)
	(NN-read-test-file)
	
	(sort *population* #'> :key #'cdr)
	(NN-convert-chromosome-to-weights (car (nth 0 *population*)))
	
	(let*
		(	
			(dataSetError 0)
		)
		
		(dotimes (i (length *testData*))
			(setf currLine (nth i *testData*))
			(setf dataSetError 
				(+ 
					dataSetError 
					(determine-fitness (take-line currLine) (parse-desired-outputs currLine))
				)
			)
		)
		
		;Prints the ending errors and number of successes
		(format t "~% Ending dataset error: ~9,6F" dataSetError)
		(format t "~% Ending fitness: ~9,10F" (/ 1 dataSetError))
		(format t "~% Test Set: ~9,10F" (length *testData*))
		(format t "~% Ending formatted error: ~9,10F" (/ dataSetError 2))
		(format t "~% Success rate: ~9,10F" (* (/ *totalCorrect* (length *testData*)) 100))
	)
)

(defun NN-start-training (generations)
	"Main function that is called to start training off. From here Neural Network, initial population &
	fitness values are defined. After initial population is trained, the Genetic Algorithm is performed to
	find new children for the next generation but also keeping elite members"

    ;Initialise/reset variables	
	(NN-check-gens generations) 
  (reset-all-data)
  (NN-read-training-file)
	
	;Define Neural Network & train initial population
	(NN-def-Weights)
	(GA-init-population)
	(NN-train-initial-population)
	
	;After the initial training is done, new generation is made. This new generation population 
	;consists of elite members & offspring from Roulette Wheel Selection, Single Point Crossover &
	;chance of Mutation (multiple times if the hidden nodes is high). This is done for no. generations defined
	(let*
		(
			(eliteMembers (make-list *eliteMemSize*))
		)
		
		(dotimes (i generations)
			
			(sort *population* #'> :key #'cdr) ; Sort population by key (fitness) i.e. highest is first

			(dotimes (j *eliteMemSize*) ; create a list for the specified elite member amount
				(setf (nth j eliteMembers) (nth j *population*))	; Iterate from fittest and note elite members
			)
			
			(print-new-line)
			(format t "~%Gen: # ~4d" (+ i 1))
			(print-new-line)
			
			(let* 
				(
					(totalFitnessOfReproductionParents 0) ;used to calculate the cumulative and normalized value
					(requiredOffspring 0)
					(randomNumberOne 0)
					(parentOne '()) ; chromosome
					(randomNumberTwo 0)
					(parentTwo '())
					(childAverageCrossover'())
					(offspring '())
					(currentOffspring '())
					(currentOffspringError 0)
					(fittestForReproduction (make-list *fittestForReproductionAmount* :initial-element 1))
					(cumulative (make-list *fittestForReproductionAmount* :initial-element 1))
					(normalized (make-list *fittestForReproductionAmount* :initial-element 1))
					(newPopulationFitnesses '())
				)
				
				; we loop 4 times and get the fittest parents in the population
				(dotimes (j *fittestForReproductionAmount*) 
					(setf (nth j fittestForReproduction) (nth j *population*)) 
					(setf totalFitnessOfReproductionParents (+ totalFitnessOfReproductionParents (cdr (nth j fittestForReproduction))))
				)

				; now we just need to apply the needed calculations of the fittest chromosomes - cumulative and normalized value
				(dotimes (j *fittestForReproductionAmount*) 
					(if (= j 0) ; calculate accumulative
						(setf (nth 0 cumulative) (cdr (nth 0 fittestForReproduction))) ; the fittest one will always have the same fitness - cumulative pair
						(setf (nth j cumulative) (+ (nth (- j 1) cumulative) (cdr (nth j fittestForReproduction)))) ; Get current fitness, add it with previous cumulative, set current cumulative
					)
					(setf (nth j normalized) (/ (nth j cumulative) totalFitnessOfReproductionParents)) ; calculate normalized from the cumulative we just calculated
				)
				
				; Find how many offspring needed, minus elite members
				(setf requiredOffspring (- *populationSize* *eliteMemSize*))
				(setf offspring (make-list requiredOffspring))
				
			  ; Create offspring
       	; Iterate 3 times
				(dotimes (j requiredOffspring)

					(setf randomNumberOne (get-random-range 0.0 1.0)) ; random number is used against the cumulative, 'roulette'
					(setf randomNumberTwo (get-random-range 0.0 1.0)) 
					
					; get the first random parent from the roulette wheel
          ; Iterates 4 times
          ; first parent has 30% chance on roulette wheel
          ; 0 - 0.3
          ; random number = 0.2
          ; if random < 0.3
					(dotimes (k *fittestForReproductionAmount*)
						(if (< randomNumberOne (nth k normalized))
							(progn
                ; car -> Chromosome
                ; cdr -> Fitness
								(setf parentOne (car (nth k fittestForReproduction))) 		
								(return) ; Break out of the loop
							)
						)
					)
					
					; do the same for the second parent
					(dotimes (k *fittestForReproductionAmount*) 
						(if (< randomNumberTwo (nth k normalized))
							(progn
								(setf parentTwo (car (nth k fittestForReproduction))) 				
								(return)
							)
						)
					)

					 (setf childAverageCrossover (GA-crossover parentOne parentTwo))
          
          ; now to add the two together and mutate to whole chromosome		
          ; (append '(a b c) '(d e f) '() '(g)) =>  (A B C D E F G)
          ;	Index		=  0  1  2  3  4
          ;	ParentA = {5, 6, 7, 8, 9}
          ;	ParentB = {11, 12, 13, 14, 15}
          ;	Child[0] =	(ParentA[0] + ParentB[0]) / 2
					(setf currentOffspring (GA-mutate childAverageCrossover))
					
					(NN-convert-chromosome-to-weights currentOffspring) ; convert the new offspring to neural network weights so that we can check its fitness 
					
					(print-new-line)
					(format t "~%Working out the fitness for child # ~4d" j)
					(print-new-line)
					
					;Determine the fitness of this new offspring
					(setf currentOffspringError (train)) 
					(setf (nth j offspring) (cons currentOffspring (/ 1 currentOffspringError)))
				)
				
				;Set the new population & fitness values after Genetic Algorithm is finished
				(setf *population* (append eliteMembers offspring))
				
				(setf newPopulationFitnesses (make-list *populationSize*))
				(dotimes (j *populationSize*)
					(setf (nth j newPopulationFitnesses) (cdr (nth j *population*)))
				)
				
				(print-new-line)
				(format t "~%All Fitness values of generation# ~4d " (+ i 1))
				(princ newPopulationFitnesses)
				(print-new-line)
			)
		)
	)
)

(defun GA-split-chromosome (chromosome where)
	"Takes a whole chromosome and splits it into two defined by the where location"
	(let*
		(
			(chromosomeB '())
			(chromosomeA '())
			(pair '())
		)
		
		(setf chromosomeB (subseq chromosome 0 where))
		(setf chromosomeA (subseq chromosome where))
			
		(setf pair (cons chromosomeB chromosomeA))
	)
)

(defun GA-mutate (chromToMutate)
	"Mutate randomly by adding or subtracting a small value"
	(setf mutateRandomNumber (random 101))
	
	(if (> *mutationChance* mutateRandomNumber) 
		(progn
			(print-new-line)
			(princ "Mutating...")
			(print-new-line)
      ; Iterate based on how many weight we want to mutate per chromosome
			(dotimes (i *amountOfWeightsMutatedPerChromosome*)
				(if (oddp (random 2))	; Random [0,1]
				    (setf chromToMutate (GA-doMutate 0 chromToMutate))	; If odd (Add)
					(setf chromToMutate (GA-doMutate 1 chromToMutate))		; If even (Subtract)
				)
			)
			chromToMutate
		)
		chromToMutate
	)
)

(defun GA-doMutate (x chromToMutate)
    "If x = 0, then add to the random place in the chromosome else subtract"
    (let* (
			(rand (random *chromosomeLength*))
	      	)
		(if (= x 0)
			(setf (nth rand chromToMutate) (+ (nth rand chromToMutate) (get-random-range *mutationRateLow* *mutationRateHigh*)))
			(setf (nth rand chromToMutate) (- (nth rand chromToMutate) (get-random-range *mutationRateLow* *mutationRateHigh*)))
		)
		chromToMutate
	)
)


;https://ai.stackexchange.com/questions/3428/mutation-and-crossover-in-a-genetic-algorithm-with-real-numbers
;genome1 = { GeneA: 1, GeneB: 2.5, GeneC: 3.4 }
;genome2 = { GeneA: 0.4, GeneB: 3.5, GeneC: 3.2 }
;A few examples of crossover could be:
;Taking the average: { GeneA: 0.7, GeneB: 3.0, GeneC: 3.3 }
;Uniform (50% chance): { GeneA: 0.4, GeneB: 2.5, GeneC: 3.2 }
(defun GA-crossover (parent1 parent2)
    "Returns a new path resulting from genetic crossover of individual1 and individual2."

  	(let*	(
          	(childChromosome (make-list *chromosomeLength* :initial-element 1))
        	)
			(dotimes (i *chromosomeLength*)
				(setf (nth i childChromosome) (/ (+ (nth i parent1) (nth i parent2) ) 2))
      )
      childChromosome
    )
  
    ;(let* (
    ;      	(where (random (length parent1))) 	; Random number between 0-20
		;  		)
    ;	(append (left-part parent1 where) (right-part parent2 where))
    ;)
)

(defun left-part (path k)
    "Returns the prefix of PATH having length K."
	(print-new-line)
	(princ "K = ")
	(princ k)
    (subseq path 0 k) 
)

(defun right-part (path k)
    "Returns the suffix of PATH starting at position K."
    (subseq path k) 
)

;------------------------ End (C) Steven L. Tanimoto -------------------------------

; Train iterates 200k+ times
(defun train ()
    "Function trains over whatever chromosome was selected from current population. It calculates the fitness for each
	through our fitness function. This displays the errors for each set of weights as it is training."
	(let*
		(	
			(dataSetError 0)
			(currLine '())
		)
		
		; Calculate the fitness for these weights ( 1/total dataset error)
    ; Our population size [Skin-Not-Skin]: trainingData -> 245,057 (Full Dataset)
   	; Test dataset => 10% of overall -> 24,505
		(dotimes (i (length *trainingData*))
      ; currLine -> ['(INPUTS) . DESIRED_OUTCOME]
			(setf currLine (nth i *trainingData*))
			(setf dataSetError 
				(+ 
					dataSetError 
					(determine-fitness (take-line currLine) (parse-desired-outputs currLine))
				)
			)
			
      ; Print dataset error ever 7,000 inputs
			(if (zerop (mod i *printErrorRate*))
				(progn
					(format t "~%At end of i # ~4d,  error: ~9,6F." i dataSetError)
				)
			)
		)
		
		; Display final results from the current set of weights
   	(princ "=================== Current Weight Results ===================")
		(format t "~% Total Error [Current Weights]: ~9,6F" dataSetError)
		(format t "~% Total Fitness: ~9,10F" (/ 1 dataSetError))
		(if (= *totalCorrect* 0)
			(progn
				(format t "~% Chromosome Accuracy: ~9,10F" 0)
			)
			(format t "~% Chromosome Accuracy: ~9,10F" (* (/ *totalCorrect* (length *trainingData*)) 100)) ; Print accuracy: 0%-100%
		)
		(setf *totalCorrect* 0) ; Reset total correct
		
   ; Return current dataset error
		dataSetError
	)
)

(defun determine-fitness (inputs desiredOutput)
    "Our main fitness function. This performs a feed-forward through the network, calculating the outputs, rounding 
	the outputs (max to 1 and rest to 0), subtract desired from output to get errors, square + add errors to get total error
	This is usually run through total dataset (batch calculation), Sum of total errors = Our Fitness value assigned"
    (let* (
			 (xB (NN-matrix-multiplication inputs *weightB*))
			 (hA)
			 (h (make-array (list 1 (nth 0 (array-dimensions *weightA*))) :initial-element 1))
			 (output (make-array (list 1 *outputLength*) :initial-element 1))
			 (errors (make-array (list 1 *outputLength*) :initial-element 1))

			 (totalError 0)
			 (otpt '(1))
	      )

		  (setf (aref h 0 0) *dummyValue*)
		  (dotimes (i (- (nth 1 (array-dimensions h)) 1))   ;Width of matrix (4) - 1 (as dummy input)
			  (setf (aref h 0 (+ i 1)) (sigmoid(aref xB 0 i)))
		  )
		  
		  (setf hA (NN-matrix-multiplication h *weightA*))
		  (dotimes (i *outputLength*)
			  (setf (aref output 0 i) (sigmoid(aref hA 0 i)))
			   (setf (nth i otpt) (aref output 0 i)) ; round the values
		  ) 
		  
		  ; round up the output
		  (setf biggestIdx (position (maximum otpt) otpt)) ;Index of highest 1
		  (dotimes (i *outputLength*)
		      (if (= i biggestIdx)
		  	      (setf (aref output 0 i) 1)
		  		  (setf (aref output 0 i) 0)
		  	  )
		  )
		  
		; checks if the actual is the same as desired
		(if (equalp output desiredOutput)
			(setf *totalCorrect* (+ *totalCorrect* 1))
		)
		  
		  ;Get errors from each desiredOutput and output
		  (dotimes (i *outputLength*)
		      (setf (aref errors 0 i) (- (aref desiredOutput 0 i) (aref output 0 i)))
			  (setf totalError (+ (square (aref errors 0 i)) totalError))
		  )

		  totalError
	  )
)

(defun get-weights-from-chromosome (chromosome)
	"Chromosome transformed back into two sets of weights"
    (let* 
		(
			(weightsB (make-array (list *inputWithDummyLength* *hlNeruons*) :initial-element 1))
			(weightsA (make-array (list *hlNeruonsWithDummyLength* *outputLength*) :initial-element 1))
			(weightPair '())
			(currentNode 0)
		)

		(dotimes (i *inputWithDummyLength*)
			(dotimes (j *hlNeruons*)
				(setf (aref weightsB i j) (nth currentNode chromosome))
				(setf currentNode (+ currentNode 1))
			)
		)

		; currentNode will know from where to pick off  for the hiddentoOutput NN
		(dotimes (i *hlNeruonsWithDummyLength*)
			(dotimes (j *outputLength*)
				(setf (aref weightsA i j) (nth currentNode chromosome))
				(setf currentNode (+ currentNode 1))
			)
		)
		
		(setf weightPair (cons weightsB weightsA))
		
		weightPair
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Some helper functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-double-list (lineStr)
    "Take string input and parse double values (return as a list)"
    (with-input-from-string (in lineStr)        ;(with-input-from-string (in "3.14 5.646 4 9.6")
        (loop for x = (read in nil nil) while x collect x)
		)
)
;https://archive.ics.uci.edu/ml/machine-learning-databases/00229/Skin_NonSkin.txt
;(with-input-from-string (is " ") (read is nil 'the-end)) =>  THE-END
;74	85	123	1
;73	84	122	1
;70	81	119	1
;70	81	119	1
;69	80	118	1
;70	81	119	1
;1 74 84 123

(defun NN-read-file (path)
    "Read outputs and inputs and store them in a list of lists called *trainingData*
	In train function these are later separated line by line into output matrix and input matrix"
	(let ((stream (open path :if-does-not-exist nil)))
		(when stream
			(loop for line = (read-line stream nil)
				while line 
					do 
					(
					  progn
           	; progn => Do things in order, just like let* creates variables in order           
           
           	; PSEUDO-CODE <- Desired outcome is last digit in a line
           	; (setf inputPart (parse-double-list (string (subseq line 0 (- (length line) 1) )))
           	;	(setf outputPart (subseq line 0 1))
           	;	(setf *tempDataLoad* (cons (cons outputPart inputPart) *tempDataLoad*))
           	;	(setf *tempDataLoad* (cons (cons inputPart outputPart) *tempDataLoad*))
           	; Output => '((INPUTS) . DESIRED_OUTPUT)
           	(setf inputPart (parse-double-list (string (subseq line 0 (- (length line) 1) )))) ; Parse integers 
           	(setf outputPart (subseq line (- (length line) 1) (length line)))
           	(setf *tempDataLoad* (cons (cons inputPart outputPart) *tempDataLoad*))
           
						;(setf inputPart (parse-double-list (string (subseq line *outputLength*)))) ;Parse doubles from the line
						;(setf outputPart (subseq line 0 *outputLength*)) ;Parse the output as a string
						;(setf *tempDataLoad* (cons (cons outputPart inputPart) *tempDataLoad*))
					)
			)
		)
		(close stream)
	)
)


(defun sigmoid (x)
    "Sigmoid Function ( 1/1+e^-x )"
    (/ 1 (+ 1 (exp (- x))))
)

(defun square (x)
    "Square a number"
    (* x x)
)

(defun maximum (list)
    "Get max value from a list"
    (reduce #'max list)
)

; Code written by Shane Dowdall in lecture
(defun NN-matrix-multiplication (a b)
    "Multiply two matrices"
    (let* 
		(
	    (m (nth 0 (array-dimensions a)))
			(s (nth 1 (array-dimensions a)))
			(n (nth 1 (array-dimensions b)))
			(result (make-array (list m n) :initial-element 1))
		)
		(dotimes (i m)
		  (dotimes (j n)
			  (setf (aref result i j) 0.0)
			  (dotimes (k s)
				  (incf (aref result i j) (* (aref a i k) (aref b k j)))
			  )
		  )
		)
		result
	)
)


(defun take-line (line)
    "Take a line from the data list and convert to matrix 1*4"
    (let* (
	          (result (make-array (list 1 *inputWithDummyLength*) :initial-element 1))
	      )
		 (setf (aref result 0 0) *dummyValue*) ;Set dummy value 1
		 (dotimes (i (- (length line) 1))
		     (setf (aref result 0 (+ i 1)) ( nth (+ i 1) line ))
		 )
		 result
	)
)


(defun parse-desired-outputs (line)
    "Take a line of output and inputs and parse it"
    (let* (
	          (result (make-array (list 1 *outputLength*) :initial-element 1))
			  (outputStr (nth 0 line))
	      )
		  (dotimes (i (length outputStr))
		      (setf (aref result 0 i) (parse-integer (subseq outputStr i (+ i 1))))
		  )
		  result
	)
)

(defun print-new-line()
    "Print a new line"
	(format t "~%")
)

(defun set-test-data-file-path (filePath)
    "Set test data file path"
	(setf *testDataFilePath* filePath)
)

(defun set-training-data-file-path (filePath)
    "Set training data file path"
	(setf *trainingDataFilePath* filePath)
)

(defun get-random-range(min max)
    "Get random range of integer/float values"
	(+ min (random max *random-state*))
)



;https://archive.ics.uci.edu/ml/machine-learning-databases/00229/Skin_NonSkin.txt
;(load [filepath])

;//Start the training passing number of generations (e.g. 100)
;(NN-start-training [numberOfGenerations])

;//Test the Neural Network
;(NN-get-fitness-test-set)

; Extra test commands
; 1: (load "C:\\path\\to\\cl")
; 2: (set-training-data-file-path "C:\\path\\to\\training_data")
; 3: (set-test-data-file-path "C:\\path\\to\\test_data")
; 4i: (NN-read-training-file)
; 5i: (print *trainingData*)
; 4ii: (NN-read-test-file)
; 5ii: (print *testData*)



