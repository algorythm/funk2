// side 39, ex2.1
let f = function
    | x when x % 5 = 0 -> false
    | x when x % 2 = 0 || x % 3 = 0 -> true
    | _ -> false
let test1 = f(30) = false
let test2 = f(4) = true
let test3 = f(12) = true

// side 39, ex 2.2
let rec pow = function
    | _, 0 -> ""
    | s, n -> s + pow(s, n - 1)
let test4 = pow("hej", 4) = "hejhejhejhej"

// side 89, ex 4.3
let rec evenN = function
    | 0 -> []
    | n when n % 2 = 0 -> n::evenN(n - 1)
    | n                -> evenN(n - 1)

let test5 = evenN(10) = [10; 8; 6; 4; 2]

// side 89, ex 4.8
let rec split = function
    | [] | [_] -> [], []
    | x1::x2::xs ->
        let (xlist1, xlist2) = split xs
        (x1::xlist1, x2::xlist2)

let test6 = split([1..10]) = ([1; 3; 5; 7; 9], [2; 4; 6; 8; 10])