package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
      }
  }

  trait Level1 extends SolutionChecker {
    /* terrain for level 1*/

    val level =
      """ooo-------
        |oSoooo----
        |ooooooooo-
        |-ooooooooo
        |-----ooToo
        |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0, 0)), "0,0")
      assert(!terrain(Pos(0, 5)), "0,0")
      assert(!terrain(Pos(4, 11)), "4,11")
      assert(!terrain(Pos(11, 4)), "11,4")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1, 1))
    }
  }

  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }

  test("isStanding") {
    new Level1 {
      assert(Block(Pos(0, 0), Pos(0, 0)).isStanding)
      assert(!Block(Pos(0, 0), Pos(0, 1)).isStanding)
      assert(!Block(Pos(0, 0), Pos(1, 0)).isStanding)
    }
  }

  test("isLegal") {
    new Level1 {
      assert(Block(Pos(0, 0), Pos(0, 0)).isLegal)
      assert(Block(Pos(0, 0), Pos(1, 0)).isLegal)
      assert(Block(Pos(0, 0), Pos(0, 1)).isLegal)
      assert(Block(Pos(1, 0), Pos(2, 0)).isLegal)
      assert(!Block(Pos(2, 0), Pos(3, 0)).isLegal)
      assert(!Block(Pos(3, 0), Pos(4, 0)).isLegal)
    }
  }

  test("startBlock") {
    new Level1 {
      assert(startBlock == Block(Pos(1, 1), Pos(1, 1)))
    }
  }
}
