// Finding a single tour on a "mega" board
//=========================================

object CW9c {

// !!! Copy any function you need from file knight1.scala !!!
// !!! or knight2.scala                                   !!! 
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.


type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

//(9) Implement a function that searches for a 
//    you have to be careful to write a tail-recursive version as this 
//    function will be called with dimensions of up to 70 * 70
//    and starting field (0, 0). It has to produce a solution within
//    30 seconds.

import scala.annotation.tailrec

def is_legal(dim: Int, path: Path, x: Pos) : Boolean = {
  if (x._1 < dim && x._2 < dim && x._1 >= 0 && x._2 >=0)
    !path.contains(x) 
  else false
}

def legal_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
  // hard code all the 8 moves a knight has, using the previous function to 
  // check that the move can be done.
  val a = List(2, 1, -1, -2, -2, -1, 1, 2)
  val b = List(1, 2, 2, 1, -1, -2, -2, -1)
  val all_moves = for(n <- 0 to 7) yield ( x._1+b(n), x._2+a(n) )
  val filtered = all_moves.filter(is_legal(dim,path,_))
  filtered.toList 
}

def first(xs: List[Pos], f: Pos => Option[Path]) : Option[Path] = {
  if(xs == Nil) None
  else{
    val head = f(xs.head)
    if (head.isDefined) head
    else first(xs.tail,f)
  }

}  

def ordered_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
    val legal_moves_ordered = legal_moves(dim, path, x).sortBy(legal_moves(dim,path,_).size)
    legal_moves_ordered
}

@tailrec
def tour_on_mega_board(dim: Int, path: Path) : Option[Path] = List(path) match {
    case Nil => None
    
    case x::xs => {
        if (x.size == dim * dim) Some(x)
        else {
            val ordered = ordered_moves (dim,x, x.head)
            tour_on_mega_board(dim, (for (s <- ordered ) yield s::x).flatten )
        }
    }
}

}
