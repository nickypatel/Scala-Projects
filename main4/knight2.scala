// Core Part about finding a single tour for a board using the
// Warnsdorf Rule
//==============================================================

object CW9b {


// !!! Copy any function you need from file knight1.scala !!!
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions


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
//(6) Complete the function that calculates a list of onward
//    moves like in (2) but orders them according to Warnsdorf’s 
//    rule. That means moves with the fewest legal onward moves 
//    should come first.


def ordered_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
    val legal_moves_ordered = legal_moves(dim, path, x).sortBy(legal_moves(dim,path,_).size)
    legal_moves_ordered
}

//(7) Complete the function that searches for a single *closed* 
//    tour using the ordered_moves function from (6). This
//    function will be tested on a 6 x 6 board. 


def first_closed_tour_heuristics(dim: Int, path: Path) : Option[Path] = {
    if(path.length == dim*dim && legal_moves(dim, List(path.head), path.head).contains(path.last)) Some(path)
    else{
        first(ordered_moves(dim,path,path.head), (f: (Int,Int)) => first_closed_tour_heuristics(dim, f::path))
    }
}

//(8) Same as (7) but searches for *non-closed* tours. This 
//    version of the function will be called with dimensions of 
//    up to 30 * 30.

def first_tour_heuristics(dim: Int, path: Path) : Option[Path] = {
    if (dim * dim == path.size) Some(path)
    else{
        first(ordered_moves(dim,path,path.head), (f: (Int,Int)) => first_tour_heuristics(dim, f:: path) )
    }
}

}
