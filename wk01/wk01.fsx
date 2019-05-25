// ====================
// Week 01
// http://www2.compute.dtu.dk/courses/02157/ExercisesWeek01.pdf
// ====================

// ====================
// Wk 01, Ex 01
// ====================
let rec f = function
    | 0 -> 0
    | n -> n + f(n - 1);;
let test1 = f(5) = 15;;
let test2 = f(10) = 55;;

let rec sum = function
    | m, 0 -> m
    | m, n -> m + n + sum(m, n - 1);;
let test3 = sum(5, 3) = 26;;

let rec bin = function
    | n, k when n = k -> 1
    | _, 0            -> 1
    | n, k            -> bin(n-1, k-1) + bin((n-1),k)
let test4 = bin(2,1) = 2;;
let test5 = bin(4,2) = 6;;
let test6 = bin(4,4) = 1;;

let rec multiplicity = function
    | _, [] -> 0
    | x, y::ys when x = y -> 1 + multiplicity(x, ys)
    | x, _::ys -> multiplicity(x, ys)

let test7 = multiplicity(2, [2; 4; 2; 10; 1; 2]) = 3;;

let rec mulC = function
    | _, [] -> []
    | x, y::ys -> x * y :: mulC((x, ys))

let test8 = mulC(2, [4; 10; 1]) = [8; 20; 2]

let rec addE = function
    | xs, [] -> xs
    | [], ys -> ys
    | x::xs, y::ys -> x + y :: addE((xs, ys))

let test9 = addE([1; 2; 3], [4; 5; 6]) = [5; 7; 9]
let test10 = addE([1; 2], [3; 4; 5; 6]) = [4; 6; 5; 6]
let test11 = addE([1; 2; 3; 4], [5; 6]) = [6; 8; 3; 4]

let mulX xs = 0 :: xs;

let test12 = mulX([1; 2; 3]) = [0; 1; 2; 3]

let rec mul = function
    | [], _ -> []
    | x::xs, ys -> addE(mulC(x, ys), mulX(mul((xs, ys))))

let test13 = mul([2; 3; 0; 1], [1; 2; 3]) = [2; 7; 12; 10; 2; 3]

open System;;
let rec text = function
    | [], _ -> ""
    | [x], n -> String.Format("{0}x^{1}", x, n)
    | x::xs, n when n = 0 -> String.Format("{0} + {1}", x, text(xs, n + 1))
    | x::xs, n when n = 1 -> String.Format("{0}x + {1}", x, text(xs, n + 1))
    | x::xs, n            -> String.Format("{0}x^{1} + {2}", x, n, text(xs, n + 1))

let test14 = text([1; 2; 3; 4], 0) = "1 + 2x + 3x^2 + 4x^3"