;;; Guile-Utils - Miscellaneous utility functions for GNU Guile.
;;; Copyright (C) 2021 Tim Van den Langenbergh (tmt_vdl@gmx.com)

;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but without ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
(library (utils (1 1 0))
  (export sorted-insert
		  prime?
		  next-prime
		  pick
		  shuffle)
  (import (rnrs (6))
		  (srfi srfi-27))

  (define (sorted-insert cmp a d)
	"Insert a value into a list via comparison with provided function."
	(cond ((or (not (pair? d))
			   (cmp a (car d)))
		   (cons a d))
		  ((cmp (car d) a)
		   (cons (car d)
				 (sorted-insert cmp
								a
								(cdr d))))
		  (else d)))

  (define (prime? x)
	"Rabin-Miller primality check."
	(define (square n)
	  (* n n))

	(define (triviality-check em)
	  (let ((w (mod (square em) x)))
		(if (and (= w 1)
				 (not (or (= em 1)
						  (= em (- x 1)))))
			0
			w)))

	(define (expmod base exp)
	  (cond ((= exp 0) 1)
			((even? exp)
			 (triviality-check (expmod base (/ exp 2))))
			(else
			 (mod
			  (* base (expmod base (- exp 1)))
			  x))))

	(define (test a)
	  (= (expmod a x) a))

	(define (random-factor)
	  (+ (random-integer (- x 3)) 2))

	(define (run-tests num-tests)
	  (cond ((= num-tests 0) #t)
			((not (test (random-factor)))
			 #f)
			(else (run-tests (- num-tests 1)))))

	(cond ((< x 2) #f)
		  ((< x 4) #t)
		  ((even? x) #f)
		  (else (run-tests 100))))

  (define (next-prime n)
	"Find the first prime number after n."
	(if (< n 2)
		2
		(let np ((x (+ n 1)))
		  (if (prime? x)
			  x
			  (np (+ x 1))))))

  (define (pick lst n)
	"Pick N random elements from list LST.

If N is less than the total amount of elements in LST, each element will only
be picked once."

	(define (do-pick pick-from lst-rem result elt to-pick)
	  (cond ((zero? to-pick) result)
			((null? pick-from)
			 (do-pick (if (null? lst-rem)
						  lst
						  lst-rem)
					  '()
					  result
					  (if (zero? elt)
						  (random-integer (length lst))
						  elt)
					  to-pick))
			((zero? elt)
			 (do-pick (cdr pick-from)
					  lst-rem
					  (cons (car pick-from) result)
					  (random-integer (if (and (null? (cdr pick-from))
											   (null? lst-rem))
										  (length lst)
										  (+ (length (cdr pick-from))
											 (length lst-rem))))
					  (- to-pick 1)))
			(else
			 (do-pick (cdr pick-from)
					  (cons (car pick-from) lst-rem)
					  result
					  (- elt 1)
					  to-pick))))

	(cond ((negative? n)
		   (error 'pick
				  "Cannot create a list of negative length."
				  n))
		  ((or (null? lst)
			   (zero? n))
		   '())
		  (else
		   (do-pick lst
					'()
					'()
					(random-integer (length lst))
					n))))

  (define (shuffle lst)
	"Shuffle the provided list LST."

	(pick lst (length lst))))
