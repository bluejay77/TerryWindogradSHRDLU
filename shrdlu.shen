\\ The Shen SHRDLU
\\
\\ Dr A. J. Y. 2015-10-02
\\
\\ Call (shrdlu) after starting Shen
\\ and loading this file
\\
\\ End the session by saying something ungrammatical
\\



(defcc <command>
    initialize the blocks world := (set-blocks);
    initialize                  := (set-blocks);
    put B1 on B2                := (do (print (put-on B1 B2)) (nl 2));
    put the block B1 on the block B2
                                := (do (print (put-on B1 B2)) (nl 2));
    show the blocks             := (do (print (value *blocks*)) (nl 2));
    show the block B            := (do (inspect-block B) (nl 2));
    describe the blocks         := (do (print (value *blocks*)) (nl 2));
    describe the block B        := (do (inspect-block B) (nl 2));
    identify the blocks         := (do (print (value *blocks*)) (nl 2));
    identify the block B        := (do (inspect-block B) (nl 2));
    count the blocks            := (do (print (length (value *blocks*)))
                                       (nl 2));
    how many blocks are there   := (do (print (length (value *blocks*)))
                                       (nl 2));
    give the positions of the blocks;
    give the locations of the blocks;
    show the hand;
    grasp B                     := (do (grasp B) (print "OK") (nl 2));
    grasp the block B           := (do (grasp B) (print "OK") (nl 2));
    show the table              := (do (inspect-block table) (nl 2));
    quit                        := (cl.exit);
  )


(define shrdlu
    -> (do
           (set-blocks)
	   (shrdlu1)
	   []))

(define shrdlu1
    -> (do
         (compile (function <command>) (lineread))
	 (shrdlu1)))


\\
\\ putprop and getprop
\\
\\ AJY 2015-10-02
\\
\\ Put and get the property of a symbol, like Common LISP
\\
\\ Use the system property vector
\\


(define putprop
    Symbol Property Value ->
        (put Symbol Property Value))

(define getprop
    Symbol Property ->
        (get Symbol Property))


\\ SHRDLU
\\
\\ Terry Winograd's PhD Blocks World in Shen
\\
\\ https://en.wikipedia.org/wiki/SHRDLU
\\
\\ For a Shen starred exercise by AJY 2015-09-27,
\\ The Book of Shen 3rd ed, by Dr Mark Tarver
\\
\\ The Blocks World
\\ From PH Winston, BKP Horn: LISP, 2nd ed
\\
\\ Do as follows
\\
\\ Start Shen
\\ Load this file
\\
\\ initialize
\\
\\ then eg.
\\
\\ identify the blocks
\\
\\ put b1 on b4
\\ put b2 on b4
\\ put l8 on b4
\\ put b4 on table
\\
\\ etc etc.
\\
\\ After the work of Winston-Horn this still is a little bit
\\ bug-ridden -- but as usual: I did not have time for meticulous
\\ scrutiny.... Dr AJY 2015-09-28
\\
\\ Corrected some errors by Winston-Horn AJY 2015-09-28
\\
\\ Try the following:
\\
\\ put b1 on b4
\\ put b2 on b1
\\ put b3 on b2
\\ put l8 on b3
\\
\\ put b4 on b2
\\
\\ Still bugs. Dr AJY 2015-09-28
\\
\\ Fixed 'em.  Dr AJY 2015-09-28
\\
\\ Finally finished final flaw fixes.  Dr AJY 2015-10-01
\\
\\ Converted to Shen, AJY 2015-10-02
\\


(set *plan* []) \\ The plan to be constructed
(set *blocks* []) \\ The list of all objects in the Blocks World


\\ Put object on support

(define put-on
  Object Support ->
      (do
          (set *plan* [])
          (clear-top
              (getprop Object directly-supports)
              Object) \\ To be able to grasp the object, clear its top
          (put-at Object (get-space Object Support)) \\ Put object in place
          (reverse (value *plan*)))) \\ Return up-to-date plan


(define get-space
    Object Support ->
        (let Finds (find-space Object Support) \\ Space exists?
	    (if (not (null? Finds))
	        Finds
		(let Maks
                    (make-space Object Support) \\ Make some space
		    (if (not (null? Maks))
		        Maks
			[])))))


(define put-at
    Object Place ->
      (do
        (grasp Object)
	(move-object Object Place)
	(ungrasp Object)))
	

(define list1
    X -> (cons X []))

(define list2
    X Y -> (append (cons X []) (cons Y [])))

(define list3
    X Y Z -> (append (list1 X) (list1 Y) (list1 Z)))


(define grasp
    Object ->
       (let Kruft (getprop hand grasping)
	    Clutter (getprop Object directly-supports)
	  (do
              (cond ((not (null? Kruft))
	             (get-rid-of Kruft))) \\ Holding something?
              (cond ((not (null? clutter))
	             (clear-top Clutter Object))) \\ Top not clear?
              (move-hand (top-center Object)) \\ Position hand
              (putprop hand grasping Object) \\ Grab it
              (set *plan* (cons (list2 grasp Object)
	           (value *plan*)))))) \\ Extend plan


(define move-object
    Object Newplace ->
      (do
        (remove-support Object) \\ Bookkeeping
        (move-hand (new-top-center Object Newplace))
        (putprop Object position Newplace)
        (add-support Object Newplace)
        (set *plan* (cons (list3 move-object Object Newplace)
	     (value *plan*)))))

    
(define move-hand
    Position ->
       (putprop hand position Position))


(define null?
    L -> (= L []))


(define ungrasp
    Object ->
       (cond ((null? (getprop Object supported-by)) [])
	     (true
	       (do
	         (putprop hand grasping [])
	         (set *plan* (cons (list2 ungrasp Object)
		      (value *plan*)))))))


(define get-rid-of
    Object ->
        (let Above (getprop
	              Object
	              directly-supports) \\ Anything above it?
          (cond ((null? Above) \\ Nothing there
	         (put-at Object (find-space Object table)))
	        (true \\ Objects are there above Object
	          (do
	              (get-rid-of-aux Above) \\ First clear above ones
	              (put-at Object (find-space Object table)))))))


(define get-rid-of-aux
    L ->
        (cond ((null? L) [])
	      (true
	        (do
	           (get-rid-of (hd L))
		   (get-rid-of-aux (tl L))))))


(define clear-top
    Clutter Object ->
        (clear-top-aux Clutter))


(define clear-top-aux
    [] -> []
    L ->
        (do
	    (get-rid-of (hd L))
	    (clear-top-aux (tl L))))
	    

(define remove-support \\ Bookkeeping by P H Winston
    Object ->
       (let Support (getprop Object supported-by)
         (do
           (putprop Support directly-supports
	            (remove Object
		            (getprop Support directly-supports)))
           (putprop Object supported-by []))))


(define add-support
    Object Place ->
       (let Support (get-object-under Place)
           (cond ((or (= Support table)
	              (= (getprop Support is-a) box)
	              (= (getprop Support is-a) brick))
		    (do
	              (putprop Support directly-supports
		               (cons Object
		                     (getprop Support
			                      directly-supports)))
	              (putprop Object supported-by Support)))
		  (true []))))


\\ MAKE-SPACE for OBJECT on SUPPORT
\\
\\ This was wrong in Winston-Horn
\\ Corrected version as follows:


(define make-space
    Object Support ->
      (do
        (get-rid-of-clutter-aux (getprop Support directly-supports))
        (find-space object support))) \\ Return space for OBJECT on SUPPORT


(define get-rid-of-clutter-aux
    [] -> []
    L -> (do
           (get-rid-of (hd L)) \\ Get rid of each CLUTTER object
	   (get-rid-of-clutter-aux (tl L))))


(define find-space
    Object Support ->
     (let Kruft (getprop Support directly-supports)
       (cond ((or (= Support table)
	          (null? Kruft))
	      (append [space above]
	              (list1 Support)
		      [for]
		      (list1 Object)))
	  (true
	    (append [space above]
	            (list1 Support)
		    [for]
		    (list1 Object)))))) \\ Always this way


(define get-object-under
    Place ->
       (hd (tl (tl Place))))

(define top-center
    Object ->
        (list2 top-center Object))

(define new-top-center
    Object Place ->
        (list3 new-top-center Object Place))


\\ ------------------------------------------------------------


(define make-block
    Block Name Width Height Position Directly-Supports Supported-By ->
      (do
        (putprop Name is-a Block)
        (putprop Name width Width)
        (putprop Name height Height)
        (putprop Name position Position)
        (putprop Name directly-supports Directly-Supports)
        (putprop Name supported-by Supported-By) \\ Also, sorta its position...
        (set *blocks* (append (value *blocks*) (list1 Name)))))


(define set-blocks
    ->
      (do
        (putprop hand grasping [])
        (set *blocks* [])
        (make-block table table 20 0 [0 0] [] [])
        (make-block brick b1 2 2 [0 0] [] table)
        (make-block brick b2 2 2 [2 0] [] table)
        (make-block brick b3 4 4 [4 0] [] table)
        (make-block brick b4 2 2 [8 0] [] table)
        (make-block wedge w5 2 4 [10 0] [] table)
        (make-block brick b6 4 2 [12 0] [] table)
        (make-block wedge w7 2 2 [16 0] [] table)
        (make-block ball l8 2 2 [18 0] [] table)))


\\ ------------------------------------------------------------


(define inspect-block
    Name ->
      (do
        (print [the block Name]) (nl)
        (print [the block is a (getprop Name is-a)]) (nl)
        (print [its width is (getprop Name width)]) (nl)
        (print [its height is (getprop Name height)]) (nl)
        (print [its position is (getprop Name position)]) (nl)
        (print [it supports (getprop Name directly-supports)]) (nl)
        (print [it is supported by (getprop Name supported-by)]) (nl)))



