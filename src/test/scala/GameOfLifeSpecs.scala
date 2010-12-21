import org.specs._

object GameOfLifeSpecs extends Specification {

  import ConwaysGameOfLife._

  "get neighbours should work" in {
    Life(Set((0,0))).neighbours((1,1)).size must_== 1
    Life(Set((0,0))).neighbours((1,1)) mustContain(0,0)
  }

  "should be able to query dimensions" in {
    blinker.inDimensions(30,30) must beTrue
    blinker.transposeX(40).inDimensions(30,30) must beFalse
    
  }

  "a blinker should blink" in {
    val horizontal = Set((1,0),(1,1),(1,2))
    Life(horizontal).iterate.board must_== Set((1,1), (2,1), (0,1))
    Life(horizontal).iterate.iterate.board must_== horizontal    
  }

  "a block should be still" in {
    val block = Set( (0,0),(0,1),(1,0),(1,1) )
    Life(block).iterate.board must_== block
  }
  
}
