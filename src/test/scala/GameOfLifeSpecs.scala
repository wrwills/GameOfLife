import org.specs._

object GameOfLifeSpecs extends Specification {

  import GameOfLife._
  val game = GameOfLife()
  import game._

  "get neighbours should work" in {
    Life(Set((0,0))).neighbours((1,1)).size must_== 1
    Life(Set((0,0))).neighbours((1,1)) mustContain(0,0)
  }

  "should be able to query dimensions" in {
    Life(blinker).inDimensions(30,30) must beTrue
    Life(blinker).transposeX(40).inDimensions(30,30) must beFalse
    
  }

  "two blocks should die" in {
    Life(Set((2,1), (1,1))).iterate.board must_== Set()
  }

  val horizontal = Set((1,0),(1,1),(1,2))

  "a blinker should blink" in {
    Life(horizontal).iterate.board must_== Set((1,1), (2,1), (0,1))
    Life(horizontal).iterate.iterate.board must_== horizontal    
  }

  "a block should be still" in {
    val block = Set( (0,0),(0,1),(1,0),(1,1) )
    Life(block).iterate.board must_== block
  }

  /*
   * I'm not really sure if this is the correct behaviour: shouldn't behaviour
   * at a given position for a given set of coordinates be the same regardless
   * of the game board dimensions?
   *
   * To make that work you'd need to have a notion of object identity on the game
   * board
   */
  "a blinker at a board's edge should become two blocks and then vanish" in {
    val withoutDimensions = Life(horizontal).transposeX(1).iterate
    withoutDimensions.board.size must_== 3
    withoutDimensions.board must_== Set((2,1), (3,1), (1,1))

    val ngame = GameOfLife(Some(3))
    val first = ngame.Life(horizontal).transposeX(1).iterate
    first.board.size must_== 2
    first.board must_== Set((2,1), (1,1))
    first.iterate.board must_== Set()
   // it.iterate must_== it
  }
  
  
}
