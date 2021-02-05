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
(library (utils (1 0 0))
  (export prime?
		  next-prime)
  (import (rnrs (6))
		  (srfi srfi-27))

  (define (prime? x)
	"Rabin-Miller primality check."
	(define (square n)
	  (* n n))

	(define (triviality-check em)
	  (let ((w (remainder (square em) x)))
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
			 (remainder
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
			  (np (+ x 1)))))))
