;;;file: ethnocentrism8-1.cl
;;;purpose: Hammond & Axelrod 2006 agent-based evolutionary model
;;;programmer: Shuaibo Huang
;;;started: 31 aug 07
;;;current: 17 nov 10
;;;fixed possible divide-by-0 errors in proportion-of4-strategies & proportions-behaviors

;;;globals

"---------------add global var idtrack------------------------"
(defvar *idtrack* 0
  "Track id for agents to ensure the uniquness")

(defvar *path* nil
  "Path for saving files.")

(defvar *torus* nil
  "Toroidal shaped environment created by folding 2d lattice.")

(defvar *lattice-size* 50
  "Size of torus. 0 to *lattice-size* minus 1.")

(defvar *ntags* 4
  "Number of group tags.")

(defvar *nstrategies* 2
  "Number of strategies.")

(defvar *cost* 0.01
  "Cost of giving help. Subtract from donor's PTR.")

(defvar *benefit* 0.03
  "Benefit of receiving help. Add to recipient's PTR.")

(defvar *base-ptr*  0.12
  "Base potential to reproduce.")

(defvar *mutation-rate* 0.005
  "Rate of mutation.")

(defvar *death-rate* 0.10
  "Rate of death.")

(defvar *immigration-rate* 1
  "Rate of immigration per time step.")

(defvar *ncooperate* 0
  "Count of cooperative behaviors per cycle. Reset to 0 at start of each cycle in run1.")

(defvar *ndefect* 0
  "Count of defection behaviors per cycle. Reset to 0 at start of each cycle in run1.")

(defvar *strategies* nil
  "List of strategy counts for each run.")

(defvar *strategy-proportions* nil
  "List of strategy proportions for each run.")

(defvar *behaviors* nil
  "List of behavior counts for each run.")

(defvar *behavior-proportions* nil
  "List of behavior proportions for each run.")

"-----------------------------------add Parent id and parent as new traits---------------------------- "
(defstruct agent 
  "tag is group id, igs is ingroup-strategy, ogs is outgroup-strategy, ptr is potential to reproduce.
 Strategies are 0 = defect or 1 = cooperate, id denotes the unique identity of a agent, and parent is its parent."
  id
  parent
  tag
  igs
  ogs
  ptr)

;;;set up torus

(defun initialize-torus (size)
  "(size)
Initialize torus of size with nil elements."
  (setf *torus* (make-array (list size size) :initial-element nil)
    *lattice-size* size))

;;;random numbers

(defun call-random (m n)
  "(m n)
Call (random n) m times. Used by seed-random."
  (do ((i 0 (1+ i)))
      ((= i m) nil)
    (if (zerop n) 
        (random 1)
      (random n))))

(defun seed-random ()
  "Seed random from the last 4 digits of time. 
Useful for generating unique random sequences."
  (let* ((time (get-internal-real-time))
         (n (multiple-value-bind (x y) (floor time 100) (cadr (list x y))))
         (hundreds (multiple-value-bind (x y) (floor time 100) (car (list x y))))
         (m (multiple-value-bind (x y) (floor hundreds 100) (cadr (list x y)))))
    (call-random m n)))

(defun integers (x y)
  "(x y)
Return integers from x to y inclusive."
  (do ((i x (1+ i))
       (result nil (cons i result)))
      ((= i (1+ y)) (reverse result))))

(defun random-range (n x y)
  "(n x y)
Return n random integers between x and y inclusive without replacement."
  (do ((i (1+ (- y x)) (1- i))
       (result nil)
       (pending (integers x y)))
      ((= (length result) n) result)
    (let* ((vec (make-array (length pending) :initial-contents pending))
           (index (random i))
           (selection (aref vec index)))
      (setq pending (remove selection pending))
      (setq result (cons selection result)))))

;;;immigration

(defun empty-location ()
  "()
Find random empty location in torus."
  (let ((row (random *lattice-size*))
        (column (random *lattice-size*)))
    (if (null (aref *torus* row column))
        (list row column)
      (empty-location))))

(defun immigrant ()
  "()
Make 1 immigrant with random characteristics."
  (let* ((id *idtrack*)
         (tag (random *ntags*))
         (igs (random 2))
         (ogs (random 2))
         (parent nil)
         (location (empty-location))
         (row (first location))
         (column (second location)))
    (setf (aref *torus* row column) 
      (make-agent :tag tag
                  :igs igs
                  :ogs ogs
                  :ptr *base-ptr*
                  :parent parent
                  :id id)))
  (setf *idtrack* (1+ *idtrack*)))

(defun immigrate ()
  "()
Allow *immigration-rate* immigrants per cycle."
  (do ((i 0 (1+ i)))
      ((= i *immigration-rate*))
    (immigrant)))

;;;interaction

(defun reset1-to-base-ptr (r c)
  "(r c)
Reset 1 agent at r rows & c columns to base ptr."
  (let ((agent (aref *torus* r c)))
    (if (null agent)
        nil
      (setf 
       (agent-ptr agent) *base-ptr*
       (aref *torus* r c) agent))))
    
(defun reset-to-base-ptr ()
  "()
Reset all ptrs to base-ptr."
  (do ((i 0 (1+ i)))
      ((= i *lattice-size*))
    (do ((j 0 (1+ j)))
        ((= j *lattice-size*))
      (reset1-to-base-ptr i j))))

(defun northern-neighbor (r c)
  "Northern neighbor of agent at r, c."
  (let ((row (if (= r 0)
                 (- *lattice-size* 1)
               (- r 1))))
    (list row c)))

(defun eastern-neighbor (r c)
  "Eastern neighbor of agent at r, c."
  (let ((col (if (= c (- *lattice-size* 1))
                 0
               (+ c 1))))
    (list r col)))

(defun southern-neighbor (r c)
  "Southern neighbor of agent at r, c."
  (let ((row (if (= r (- *lattice-size* 1))
                 0
               (+ r 1))))
    (list row c)))

(defun western-neighbor (r c)
  "Western neighbor of agent at r, c."
  (let ((col (if (= c 0)
                 (- *lattice-size* 1)
               (- c 1))))
    (list r col)))

(defun neighbors (r c)
  "(r c)
Find neighbors of agent at location r, c."
  (let ((agent (aref *torus* r c)))
    (if (null agent)
        nil
      (list (northern-neighbor r c) 
            (eastern-neighbor r c) 
            (southern-neighbor r c) 
            (western-neighbor r c)))))

(defun agent-group (r c)
  "(r c)
Return group tag of agent at r, c."
  (let ((agent (aref *torus* r c)))
    (unless (null agent)
      (agent-tag agent))))
(defun agent-parent (r c)
  "(r c)
Return parent of agent at r, c")

(defun same-tag? (agent1-r agent1-c agent2-r agent2-c)
  "(agent1-r agent1-c agent2-r agent2-c)
Do 2 agents, defined by their r & c, have the same tag?"
  (= (agent-group agent1-r agent1-c)
     (agent-group agent2-r agent2-c)))

(defun donate? (igs ogs same-group?)
  "(igs ogs same-group?)
Does potential donor donate?
If donor & recipient are in same group, then if donor cooperates in group, donor cooperates.
If donor & recipient are in different groups, then if donor cooperates out of group, donor cooperates."
  (if same-group?
      (if (= igs 1)
          t)
    (if (= ogs 1)
        t)))

;;;(donate? 0 0 t) -> nil
;;;(donate? 0 1 t) -> nil
;;;(donate? 1 0 t) -> t
;;;(donate? 1 1 t) -> t
;;;(donate? 0 0 nil) -> nil
;;;(donate? 0 1 nil) -> t
;;;(donate? 1 0 nil) -> nil
;;;(donate? 1 1 nil) -> t

(defun update-ptr (r c cost-benefit)
  "(r c cost-benefit)
Update ptr of agent at r, c by cost-benefit.
If cost-benefit is cost, then decrement ptr by *cost*.
Else increment ptr by *benefit*."
  (let ((agent (aref *torus* r c)))
    (if (eq cost-benefit 'cost)
        (setf (agent-ptr agent) 
          (- (agent-ptr agent) 
             *cost*))
      (setf (agent-ptr agent) 
        (+ (agent-ptr agent) 
           *benefit*)))
    (setf (aref *torus* r c) agent)))

(defun interact (doner-r doner-c recipient-r recipient-c)
  "(doner-r doner-c recipient-r recipient-c)
Play out interaction between potential doner & potential recipient.
Same group tag? If donate then update ptrs."
  (let* ((same-tag (same-tag? doner-r doner-c recipient-r recipient-c))
         (agent (aref *torus* doner-r doner-c))
         (igs (agent-igs agent))
         (ogs (agent-ogs agent))
         (donate (donate? igs ogs same-tag)))
    (if donate
        (progn
          (update-ptr doner-r doner-c 'cost)
          (update-ptr recipient-r recipient-c 'benefit)
          (setf *ncooperate* (1+ *ncooperate*)))
      (setf *ndefect* (1+ *ndefect*)))))

(defun interactions (r c)
  "(r c)
Interactions of agent at r, c with its 4 neighbors."
  (do ((neighbors (neighbors r c) (cdr neighbors)))
      ((null neighbors))
    (let* ((neighbor (car neighbors))
           (neighbor-r (first neighbor))
           (neighbor-c (second neighbor))
           (agent (aref *torus* neighbor-r neighbor-c)))
      (if (null agent)
          nil
        (interact r c neighbor-r neighbor-c)))))

(defun all-interactions ()
  "()
Each agent interacts with its neighbors."
  (reset-to-base-ptr)
  (do ((i 0 (1+ i)))
      ((= i *lattice-size*))
    (do ((j 0 (1+ j)))
        ((= j *lattice-size*))
      (interactions i j))))

;;;reproduction

(defun 0-to-n (n)
  "(n)
Return list of n integers from 0 to n-1, in steps of 1."
  (do ((x 0 (1+ x))
       (values nil (cons x values)))
      ((= x n) (reverse values))))

;;;(0-to-n 2) -> (0 1)
;;;(0-to-n 4) -> (0 1 2 3)

(defun values-of-trait (trait)
  "(trait)
Return values of trait."
  (case trait
    (tag (0-to-n *ntags*))
    (igs (0-to-n *nstrategies*))
    (ogs (0-to-n *nstrategies*))))

(defun random-mutation (current possible)
  "(current possible)
Mutate current value to randomly selected 1 of remaining possible integer values."
  (nth (random (1- (length possible))) 
       (remove current possible)))

;;;(random-mutation 1 '(0 1)) -> 0
;;;(random-mutation 0 '(0 1)) -> 1
;;;(random-mutation 0 '(0 1 2 3)) -> 1, 2, or 3
;;;(random-mutation 1 '(0 1 2 3)) -> 0, 2, or 3
;;;(random-mutation 2 '(0 1 2 3)) -> 0, 1, or 3
;;;(random-mutation 3 '(0 1 2 3)) -> 0, 1, or 2

(defun mutate (trait current)
  "(trait current)
Mutate trait from current value if random probability < *mutation-rate*."
  (if (< (random 1.0) *mutation-rate*)
      (random-mutation current (values-of-trait trait))
    current))

(defun empty-neighbors (r c)
  "(r c)
Find empty neighbors of agent at location r, c."
  (let ((neighbors (list (northern-neighbor r c) 
                         (eastern-neighbor r c) 
                         (southern-neighbor r c) 
                         (western-neighbor r c))))
    (do ((ns neighbors (cdr ns))
         (empty nil (if (null (aref *torus* (first (car ns)) (second (car ns))))
                        (cons (car ns) empty)
                      empty)))
        ((null ns) (reverse empty)))))

(defun clone (r c)
  "(r c)
Clone agent at r, c to an empty neighboring location, if any, after possible mutations."
  (let* ((agent (aref *torus* r c))
         (tag (agent-tag agent))
         (igs (agent-igs agent))
         (ogs (agent-ogs agent))
         (possible-locations (empty-neighbors r c))
         (clone-location (if possible-locations
                             (nth 
                              (random (length possible-locations)) 
                              possible-locations)
                           nil))
         (tag (mutate 'tag tag))
         (igs (mutate 'igs igs))
         (ogs (mutate 'ogs ogs)))
    (if clone-location
        (setf 
         (aref *torus* (first clone-location) (second clone-location))
         (make-agent :tag tag
                     :igs igs
                     :ogs ogs
                     :ptr *base-ptr*)))))

(defun reproduce (r c)
  "(r c)
Agent at r, c reproduces if random probability < ptr."
  (let* ((agent (aref *torus* r c))
         (ptr (agent-ptr agent)))
  (if (< (random 1.0) ptr)
      (clone r c))))

(defun random-sort (lst)
  "(lst)
Random sort of items in lst."
  (let* ((indices (0-to-n (length lst)))
         (n (length indices))
         (random-indices (random-range n 0 (- n 1))))
    (do ((indices random-indices (cdr indices))
         (random-list nil (cons (nth (car indices) lst)
                                random-list)))
        ((null indices) random-list))))

(defun agent-locations ()
  "()
List of locations of current agents."
  (do ((i 0 (1+ i))
       (locations nil))
      ((= i *lattice-size*) (reverse locations))
    (do ((j 0 (1+ j)))
        ((= j *lattice-size*))
      (if (aref *torus* i j)
          (setf locations (cons (list i j)
                                locations))))))

(defun reproduction ()
  "()
Allow agents to reproduce in random order according to their ptr value 
& if there is an empty neighboring location."
  (do ((agents (random-sort (agent-locations)) (cdr agents)))
      ((null agents))
    (reproduce (first (car agents)) (second (car agents)))))

;;;death

(defun die (r c)
  "(r c)
Kill agent at location r c."
  (setf (aref *torus* r c) nil))

(defun death ()
  "()
Allow agents to die if random p < *death-rate*."
  (do ((agents (agent-locations) (cdr agents)))
      ((null agents))
    (if (< (random 1.0) *death-rate*)
        (die (first (car agents))
             (second (car agents))))))

;;;strategy counting

(defun count-strategies ()
  "()
Count strategies across agents. 0 is defect. 1 is cooperate. Ingroup first. Outgroup second.
00 is selfish. 01 is traitor. 10 is ethnocentric. 11 is humanitarian."
  (do ((agents (agent-locations) (cdr agents))
       (strategy00 0)
       (strategy01 0)
       (strategy10 0)
       (strategy11 0))
      ((null agents) (list strategy00 strategy01 strategy10 strategy11))
    (let* ((location (car agents))
           (row (first location))
           (col (second location))
           (agent (aref *torus* row col))
           (igs (agent-igs agent))
           (ogs (agent-ogs agent)))
      (cond ((and (= igs 0)
                  (= ogs 0))
             (setf strategy00 (1+ strategy00)))
            ((and (= igs 0)
                  (= ogs 1))
             (setf strategy01 (1+ strategy01)))
            ((and (= igs 1)
                  (= ogs 0))
             (setf strategy10 (1+ strategy10)))
            ((and (= igs 1)
                  (= ogs 1))
             (setf strategy11 (1+ strategy11)))
            (t nil)))))

(defun add (lst)
  "(lst)
Sum integers in lst but return as floating-point number."
  (do ((x lst (cdr x))
       (total 0.0 (+ total (car x))))
      ((null x) total)))

;;;(add '(1 2 3 4)) -> 10.0

(defun proportion-of4-strategies (strategy-counts)
  "(strategy-counts)
Compute proportions of each of 4 strategies at each cycle."
  (do ((counts strategy-counts (cdr counts))
       (proportions nil))
      ((null counts) (reverse proportions))
    (let* ((cycle (car counts))
           (total (add cycle)))
      (if (= total 0)
          (setf proportions (cons cycle proportions))
        (setf proportions (cons
                           (list (/ (first cycle) total)
                                 (/ (second cycle) total)
                                 (/ (third cycle) total)
                                 (/ (fourth cycle) total))
                           proportions))))))

(defun proportions-behaviors (behavior-counts)
  "(behavior-counts)
Compute proportions of cooperative & defection behaviors at a given cycle."
  (do ((counts behavior-counts (cdr counts))
       (proportions nil))
      ((null counts) (reverse proportions))
    (let* ((cycle (car counts))
           (total (add cycle)))
      (if (= total 0)
          (setf proportions (cons cycle proportions))
        (setf proportions (cons
                           (list (/ (first cycle) total)
                                 (/ (second cycle) total))
                           proportions))))))
  
;;;saving lists in file

(defun lists->file (lst file &optional (separator " "))
  "(lst file &optional (separator " "))
Save reverse lst in file. Items in flat lst are printed 1 per line.
Separator is used to separate items on a line for embedded lst."
  (with-open-file
      (output-stream file :direction :output)
    (do ((items (reverse lst) (cdr items)))
        ((null items) 'done)
      (let ((sub-item (car items)))
        (if (listp sub-item)
            (print-line sub-item separator output-stream)
          (format output-stream "~a ~%"
            sub-item))))))

(defun print-line (lst &optional (separator " ") output-stream)
  "(lst &optional (separator " ") output-stream)
Print each item in list on a line separated by separator. 
Then go to new line."
  (do ((lst lst (cdr lst)))
      ((null lst) (terpri output-stream))
    (princ (car lst) output-stream)
    (princ separator output-stream)))

;;;data analysis

(defun nextn (n lst)
  "(n lst)
Return list of next n items in lst."
  (do ((x lst (cdr x))
       (i 0 (1+ i))
       (result nil (cons (car x) result)))
      ((= i n) (reverse result))))

;;;(nextn 2 '(a (b c) d (e f) g)) -> (A (B C))
;;;(nextn 4 '(a (b c) d (e f) g)) -> (A (B C) D (E F))

(defun lastn (n lst)
  "(n lst)
Return list of last n items in lst."
  (reverse (nextn n (reverse lst))))

(defun mean (lst)
  "(lst)
Mean of lst."
  (do ((x lst (cdr x))
       (sum 0 (+ (car x) sum)))
      ((null x) (float (/ sum (length lst))))))

;;;(mean '(4 5 6)) -> 5.0
;;;(mean '(3 4 5 6)) -> 4.5

(defun 2d-array->lists (array)
  "(array)
Convert 2d array to list of lists."
  (let* ((dimensions (array-dimensions array))
         (rows (car dimensions))
         (cols (cadr dimensions)))
    (do ((result nil)
         (i 0 (1+ i)))
        ((= i rows) (reverse result))
      (do ((sublist nil)
           (j 0 (1+ j)))
          ((= j cols) (setf result (cons (reverse sublist) result)))
        (setf sublist (cons (aref array i j) sublist))))))

(defun transpose (lists)
  "(lists)
Transpose rows & columns of nested lists."
  (let* ((rows (length lists))
         (cols (length (car lists)))
         (start-array (make-array  (list rows cols)
                                  :initial-contents
                                  lists))
         (result-array (make-array (list cols rows))))
    (do ((i 0 (1+ i)))
        ((= i rows) (2d-array->lists result-array))
      (do ((j 0 (1+ j)))
          ((= j cols))
        (setf (aref result-array j i) (aref start-array i j))))))

;;;(transpose '((1 2 3)
;;;             (4 5 6)
;;;             (7 8 9)
;;;             (10 11 12)))

(defun means-lists (lists)
  "(lists)
Means of each sublist in lists."
  (mapcar #'mean lists))

;;;(means-lists '((1 2 3)
;;;               (4 5 6)
;;;               (7 8 9)
;;;               (10 11 12)))

;;;plotting  

(defun plot-strategies (n) 
  "(n)
Plot agent strategies for run n." 
  (with-open-file (*standard-output* (concatenate 'string *path* "plot-strategy" (princ-to-string n) ".html") 
                                     :direction :output
                                     :if-exists :supersede)
    (with-html-output (*standard-output*)
      (:table :border 0 :cellpadding 4
              (loop for i from 0 below *lattice-size* by 1
                  do (htm
                      (:tr :align "right"
                           (loop for j from 0 below *lattice-size* by 1                 
                               do               
                                 (let ((agent (aref *torus* i j)))
                                   (if (aref *torus* i j)
                                       (htm
                                        (:td :bgcolor ( if (= (agent-tag agent) 0)
                                                          "Darkkhaki")
                                             :bgcolor ( if (= (agent-tag agent) 1)
                                                          "Cornflowerblue")
                                             :bgcolor ( if (= (agent-tag agent) 2)
                                                          "Mediumseagreen")
                                             :bgcolor ( if (= (agent-tag agent) 3)
                                                          "Hotpink")
                                             (cond ((and (= (agent-igs agent) 0)
                                                         (= (agent-ogs agent) 0)) 
                                                    (format t "~a" "s"))
                                                   ((and (= (agent-igs agent) 0)
                                                         (= (agent-ogs agent) 1)) 
                                                    (format t "~a" "t"))
                                                   ((and (= (agent-igs agent) 1)
                                                         (= (agent-ogs agent) 0)) 
                                                    (format t "~a" "e"))
                                                   ((and (= (agent-igs agent) 1)
                                                         (= (agent-ogs agent) 1)) 
                                                    (format t "~a" "h")))))
                                     (htm (:td (print '-)))))))))))))

;;;dominant strategies

(defun 1d-chi-square (lst)
  "(lst)
1d chi-square value from distribution in lst."
  (do ((x lst (cdr x))
       (expected (float (/ (add lst) (length lst))))
       (result 0))
      ((null x) result)
    (setf result (+ result
                    (if (zerop expected)
                        0
                      (/ (expt (- (car x) expected) 2)
                         expected))))))

;;;(1d-chi-square '(13 25 42)) -> 15.925

(defun 1d-chi-square? (lst critical-value)
  "(lst critical-value)
Is 1d chi-square value from distribution in lst > critical value?"
  (do ((x lst (cdr x))
       (expected (float (/ (add lst) (length lst))))
       (result 0))
      ((null x) (if (> result critical-value)
                    t
                  nil))
    (setf result (+ result
                    (if (zerop expected)
                        0
                      (/ (expt (- (car x) expected) 2)
                         expected))))))

;;;p<.01 1-tail
;;;(1d-chi-square? '(47 36 17) 9.21) 2df
;;;(1d-chi-square? '(47 36) 6.635) 1df
;;;(1d-chi-square? '(67 36) 6.635) 1df

(defun dominant-strategy (lst overall top2)
  "(lst overall top2)
Return position of dominant strategy in lst, starting from 1, or if none return 0.
Do frequencies differ & is largest frequency greater than next largest frequency?
overall is critical value for overall x^2. 
top2 is critical value for most frequent vs next most frequent x^2.
Note that sort is destructive, so a copy is sorted."
  (let* ((sorted (sort (copy-list lst) #'>))
         (first-item (first sorted))
         (second-item (second sorted))
         (first-two (list first-item second-item)))
    (if (and (1d-chi-square? lst overall)
             (1d-chi-square? first-two top2))
        (1+ (position first-item lst))
      0)))

;;;(dominant-strategy '(17 47 36) 9.21 6.635)
;;;(dominant-strategy '(17 67 36) 9.21 6.635)

(defun dominant-strategies (lists overall top2)
  "(lists overall top2)
Return positions of dominant strategy in lists, starting from 1, or if none return 0.
Do frequencies differ & is largest frequency greater than next largest frequency?
overall is critical value for overall x^2. 
top2 is critical value for most frequent vs next most frequent x^2."
  (do ((x lists (cdr x))
       (dominants nil (cons (dominant-strategy (car x) overall top2)
                            dominants)))
      ((null x) (reverse dominants))))

;;;(dominant-strategies 
;;; '((0 0 0 0)
;;;   (5 8 5 7)
;;;   (23 25 68 12)
;;;   (13 48 11 10))
;;; 9.21 6.635)
           
;;;run

(defun set-parameters (ntags nstrategies cost benefit base-ptr mutation-rate death-rate immigration-rate)
  "(ntags nstrategies cost benefit base-ptr mutation-rate death-rate immigration-rate)
Set key parameter values."
  (setf *ntags* ntags
    *nstrategies* nstrategies
    *cost* cost
    *benefit* benefit
    *base-ptr*  base-ptr
    *mutation-rate* mutation-rate
    *death-rate* death-rate
    *immigration-rate* immigration-rate))

(defun list-parameters ()
  "()
List parameter values for the record."
  (format t "~%lattice-size=~a ntags=~a nstrategies=~a cost=~,4F benefit=~,4F" 
    *lattice-size* *ntags* *nstrategies* *cost* *benefit*)
  (format t "~%base-ptr=~,4F mutation-rate=~,4F death-rate=~,4F immigration-rate=~a"
    *base-ptr* *mutation-rate* *death-rate* *immigration-rate*))

(defun run1 (ncycles lastn n size)
  "(ncycles lastn n size)
Run ncycles & analyze last lastn cycles for run n for torus of size."
  (initialize-torus size)
  (do ((i 0 (1+ i))
       (strategies nil)
       (behaviors nil))
      ((= i ncycles) (let ((lastn-cycles-strategies (nextn lastn strategies))
                           (lastn-cycles-behaviors (nextn lastn behaviors)))
                       (setf *strategies* (cons (means-lists (transpose lastn-cycles-strategies))
                                                *strategies*)
                         *strategy-proportions* 
                         (cons (means-lists (transpose (proportion-of4-strategies lastn-cycles-strategies)))
                               *strategy-proportions*)
                         *behaviors* (cons (means-lists (transpose lastn-cycles-behaviors))
                                           *behaviors*)
                         *behavior-proportions* 
                         (cons (means-lists (transpose (proportions-behaviors lastn-cycles-behaviors)))
                               *behavior-proportions*))
                       (plot-strategies n)))
    (setf *ncooperate* 0
      *ndefect* 0)
    (immigrate)
    (all-interactions)
    (reproduction)
    (death)
    (setf strategies (cons (count-strategies)
                           strategies))
    (setf behaviors (cons (list *ncooperate* *ndefect*)
                          behaviors))))

(defun run (ncycles lastn nruns &optional (size 50) (ntags 4) (nstrategies 2) (cost 0.01) (benefit 0.03) 
                    (base-ptr 0.12) (mutation-rate 0.005) (death-rate 0.10) (immigration-rate 1))
  "(ncycles lastn nruns &optional (size 50) (ntags 4) (nstrategies 2) (cost 0.01) (benefit 0.03) 
                    (base-ptr 0.12) (mutation-rate 0.005) (death-rate 0.10) (immigration-rate 1))
Run ncycles & analyze last lastn cycles for nruns & other optional parameters."
  (setq *path* "~/desktop/Ethocentrism/results")
  (seed-random)
  (set-parameters ntags nstrategies cost benefit base-ptr mutation-rate death-rate immigration-rate)
  (setf *strategies* nil
    *strategy-proportions* nil
    *behaviors* nil
    *behavior-proportions* nil)
  (list-parameters)
  (do ((i 0 (1+ i)))
      ((= i nruns) (progn
                     (lists->file *strategies* (concatenate 'string *path* "strategies"))
                     (lists->file *strategy-proportions* (concatenate 'string *path* "strategy-proportions"))
                     (lists->file *behaviors* (concatenate 'string *path* "behaviors"))
                     (lists->file *behavior-proportions* (concatenate 'string *path* "behavior-proportions"))
                     'done))
    (run1 ncycles lastn i size)
    (print i)))

;;;(run ncycles lastn nruns)

;;;run all strategies

(defun run1-all-strategies (ncycles n size overall top2)
  "(ncycles n size overall top2)
Run ncycles & record strategies for every cycle.
Test significance of dominant strategy. 
overall is critical value for overall x^2. 
top2 is critical value for most frequent vs next most frequent x^2."
  (initialize-torus size)
  (do ((i 0 (1+ i))
       (strategies nil))
      ((= i ncycles) (lists->file 
                      strategies 
                      (concatenate 'string *path* "strategies" (princ-to-string n))))                       
    (immigrate)
    (all-interactions)
    (reproduction)
    (death)
    (let* ((strategy-count (count-strategies))
           (dominant-strategy (list (dominant-strategy strategy-count overall top2))))
      (setf strategies (cons (append strategy-count dominant-strategy)
                             strategies)))))

(defun run-all-strategies (ncycles nruns overall top2 &optional (size 50) (ntags 4) (nstrategies 2) 
                                   (cost 0.01) (benefit 0.03) (base-ptr 0.12) (mutation-rate 0.005) 
                                   (death-rate 0.10) (immigration-rate 1))
  "(ncycles nruns overall top2 &optional (size 50) (ntags 4) (nstrategies 2) 
                                   (cost 0.01) (benefit 0.03) (base-ptr 0.12) (mutation-rate 0.005) 
                                   (death-rate 0.10) (immigration-rate 1))
Run ncycles & record strategies for every cycle for nruns.
Test significance of dominant strategy. 
overall is critical value for overall x^2. 
top2 is critical value for most frequent vs next most frequent x^2."
  (setq *path* "f:\\documents and settings\\tom\\my documents\\courses\\315\\09\\ethnocentrism\\results\\")
  (seed-random)
  (set-parameters ntags nstrategies cost benefit base-ptr mutation-rate death-rate immigration-rate)
  (list-parameters)
  (do ((i 0 (1+ i)))
      ((= i nruns) 'done)
    (run1-all-strategies ncycles i size overall top2)
    (print i)))

;;;(run-all-strategies ncycles nruns overall top2)
;;;(run-all-strategies 1000 20 11.35 6.64) p<.01

(defun run1plots (ncycles nth &optional (size 50) (ntags 4) (nstrategies 2) (cost 0.01) 
                          (benefit 0.03) (base-ptr 0.12) (mutation-rate 0.005) 
                          (death-rate 0.10) (immigration-rate 1))
  "(ncycles nth &optional (size 50) (ntags 4) (nstrategies 2) (cost 0.01) 
                          (benefit 0.03) (base-ptr 0.12) (mutation-rate 0.005) 
                          (death-rate 0.10) (immigration-rate 1))
Run ncycles & plot every nth cycle for torus of size."
  (setq *path* "f:\\documents and settings\\tom\\my documents\\courses\\315\\09\\ethnocentrism\\results\\")
  (seed-random)
  (set-parameters ntags nstrategies cost benefit base-ptr mutation-rate death-rate immigration-rate)
  (initialize-torus size)
  (do ((i 1 (1+ i))
       (strategies nil))
      ((> i ncycles) (progn
                       (lists->file strategies
                                    (concatenate 'string *path* "strategies"))
                       (lists->file (proportion-of4-strategies strategies)
                                    (concatenate 'string *path* "proportions"))))
    (immigrate)
    (all-interactions)
    (reproduction)
    (death)
    (if (= (mod i nth) 0)
        (progn 
          (setf strategies (cons (count-strategies)
                                 strategies))
          (plot-strategies i)))))

;;;(run1plots ncycles nth size)
