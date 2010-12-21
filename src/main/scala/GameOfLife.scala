import scalaz._

/**
 * an implementation of the game of life in scala
 * dimensions are optional (ie the board can be infinite) but
 * some boards will eventually use up all your memory (eg a
 * glider gun)
 *
 * a nice feature of this implementation is that it allows a board
 * to be infinite which is I believe how Conway originally specified
 * it (ie a glider can keep going forever although a glider gun will
 * eventually use up all memory)
 *
 * an optional board dimension limit can be specified
 */
case class GameOfLife(val dimensions: Option[Int] = None) {
  import Scalaz._

  type Coord = (Int,Int)
  type Board = Set[Coord]

  case class Life(board: Board) {

    // get the neighbours for a given coordinate
    def neighbours(coord: Coord): Board = 
      (
	((coord._1 -1) until (coord._1 + 2)).toList <|*|>
	((coord._2 -1) until (coord._2 + 2)).toList filter (_ != coord)
      ).toSet.intersect(board)
    
    def kill(coord: Coord) = {
      val n = neighbours(coord).size
      n < 2 || n> 3
    }  
    
    def kills: Board = board filter ( kill(_) )

    def birth(coord: Coord) = neighbours(coord).size == 3
    
    /**
     * generate candidates for births based on the where cells are in the board 
     * and then check if any of those should live
     *
     * If the board has dimensions then we limit the candidates to the board
     * dimensions
     */
    def births: Board = {
      def sequenceRanges(xs: Iterable[Int]) = 
	(xs map (x => (x - 1) until (x + 2)) map (_.toList) ∑) filter 
	  (i => if (dimensions.isDefined) { i < dimensions.get && i > -(dimensions.get) } else true)
      val xs = board map (_._1)
      val ys = board map (_._2)      
      (sequenceRanges(xs) <|*|> sequenceRanges(ys)).toSet -- board filter( birth(_) )
    }    

    def inDimensions(x: Int, y: Int) =
      (board map (_._1)).max < x && (board map (_._2)).max < y

      /*
      (
	(xs.min until (xs.max + 1)).toList <|*|> (ys.min until (ys.max+1)).toList
      ).toSet -- board filter( birth(_) )      */

    def iterate: Life = Life(board -- kills ++ births)

    def ++(life: Life) = Life(board ++ life.board)

    def transposeX(i: Int) = Life( board map  (x => (x._1 + i, x._2) ) )

    def transposeY(i: Int) = Life( board map  (x => (x._1, x._2 + i) ) )

  }
}

object GameOfLife {
  val glider = Set((0,1),(1,2),(2,0),(2,1),(2,2))
  val blinker = Set((1,0),(1,1),(1,2))
}
