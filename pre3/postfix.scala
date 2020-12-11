// Shunting Yard Algorithm
// by Edsger Dijkstra
// ========================

object CW8a {

// type of tokens
type Toks = List[String]

// the operations in the basic version of the algorithm
val ops = List("+", "-", "*", "/")

// the precedences of the operators
val precs = Map("+" -> 1,
		"-" -> 1,
		"*" -> 2,
		"/" -> 2)

// helper function for splitting strings into tokens
def split(s: String) : Toks = s.split(" ").toList


// (1) Implement below the shunting yard algorithm. The most
// convenient way to this in Scala is to implement a recursive 
// function and to heavily use pattern matching. The function syard 
// takes some input tokens as first argument. The second and third 
// arguments represent the stack and the output of the shunting yard 
// algorithm.
//
// In the marking, you can assume the function is called only with 
// an empty stack and an empty output list. You can also assume the
// input os  only properly formatted (infix) arithmetic expressions
// (all parentheses will be well-nested, the input only contains 
// operators and numbers).

// You can implement any additional helper function you need. I found 
// it helpful to implement two auxiliary functions for the pattern matching:  
// 

def is_op(op: String) : Boolean = "/+*-".contains(op)


def syard(toks: Toks, st: Toks = Nil, out: Toks = Nil) : Toks = {
/*****************************************************************************************************************
 *
 * Adapted from: Fundamental Algorithms in Scala
 * Author : James Cutajar
 * Date : 11 December 2020
 * URL : https://www.packtpub.com/product/fundamental-algorithms-in-scala-integrated-course/9781788474887
 *
 *****************************************************************************************************************
*/
	
	val (rpn, st1) = toks.foldLeft((out,st)) { (z,tok) =>
		val (outp, stk) = z
		tok match {
			
			case "+" =>
				val (higher,lower) = stk.span(op => "/*+".contains(op))
				(outp ::: higher, "+" +: lower)
		
			case "-" =>
				val (higher,lower) = stk.span(op => "/*+-".contains(op))
				(outp ::: higher, "-" +: lower)
			
			case "*" =>
				val (higher,lower) = stk.span(op => "/*".contains(op))
				(outp ::: higher, "*" +: lower)
		
			case "/" =>
				val (higher,lower) = stk.span(op => "/".contains(op))
				(outp ::: higher, "/" +: lower)
		
			case "(" => (outp, "(" +: stk)
		
			case ")" =>
				val (before,rest) = stk.span (op => op != "(" )
				(outp ::: before, rest.tail)
			
			case _ => (outp :+ tok, stk)

		}

	}
	rpn ::: st1

}


// test cases
//syard(split("3 + 4 * ( 2 - 1 )"))  // 3 4 2 1 - * +
//syard(split("10 + 12 * 33"))       // 10 12 33 * +
//syard(split("( 5 + 7 ) * 2"))      // 5 7 + 2 *
//syard(split("5 + 7 / 2"))          // 5 7 2 / +
//syard(split("5 * 7 / 2"))          // 5 7 * 2 /
//syard(split("9 + 24 / ( 7 - 3 )")) // 9 24 7 3 - / +

//syard(split("3 + 4 + 5"))           // 3 4 + 5 +
//syard(split("( ( 3 + 4 ) + 5 )"))    // 3 4 + 5 +
//syard(split("( 3 + ( 4 + 5 ) )"))    // 3 4 5 + +
//syard(split("( ( ( 3 ) ) + ( ( 4 + ( 5 ) ) ) )")) // 3 4 5 + +

 
// (2) Implement a compute function that evaluates an input list
// in postfix notation. This function takes a list of tokens
// and a stack as argumenta. The function should produce the 
// result as an integer using the stack. You can assume 
// this function will be only called with proper postfix 
// expressions.    

def compute(toks: Toks, st: List[Int] = Nil) : Int = {
	val answer =toks.foldLeft(st){ (stk,tok)=>
		(stk,tok) match{
			case (x :: y :: tail, "+") => (y+x) +: tail
			case (x :: y :: tail, "-") => (y-x) +: tail
			case (x :: y :: tail, "*") => (y*x) +: tail
			case (x :: y :: tail, "/") => (y/x) +: tail
			case (_, x) => x.toInt +: stk

		}

	}
	answer.head
}


// test cases
// compute(syard(split("3 + 4 * ( 2 - 1 )")))  // 7
// compute(syard(split("10 + 12 * 33")))       // 406
// compute(syard(split("( 5 + 7 ) * 2")))      // 24
// compute(syard(split("5 + 7 / 2")))          // 8
// compute(syard(split("5 * 7 / 2")))          // 17
// compute(syard(split("9 + 24 / ( 7 - 3 )"))) // 15

}


