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

  trait Level0 extends SolutionChecker {
    /* terrain for level 0*/

    val level =
      """------
        |--ST--
        |--oo--
        |--oo--
        |------""".stripMargin

    val optsolution = List(Down, Right, Up)
  }


  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0, 0)), "0,0")
      assert(!terrain(Pos(0, 5)), "0,0")
      assert(!terrain(Pos(4, 11)), "4,11")
      assert(!terrain(Pos(11, 4)), "11,4")
      assert(!terrain(Pos(-1, 0)), "-1,0")
      assert(!terrain(Pos(0, -1)), "0,-1")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1, 1))
    }
  }

  test("optimal solution for level 0") {
    new Level0 {
      assert(solve(solution) === Block(goal, goal))
    }
  }

  test("optimal solution length for level 0") {
    new Level0 {
      assert(solution.length === optsolution.length)
    }
  }

  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) === Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length === optsolution.length)
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
      assert(startBlock === Block(Pos(1, 1), Pos(1, 1)))
    }
  }

  test("neighbors") {
    new Level1 {
      assert(Block(Pos(0, 2), Pos(0, 2)).neighbors === List(
        (Block(Pos(0, 0), Pos(0, 1)), Left),
        (Block(Pos(0, 3), Pos(0, 4)), Right),
        (Block(Pos(-2, 2), Pos(-1, 2)), Up),
        (Block(Pos(1, 2), Pos(2, 2)), Down)
      ))
    }
  }

  test("legalNeighbors") {
    new Level1 {
      assert(Block(Pos(0, 2), Pos(0, 2)).legalNeighbors === List(
        (Block(Pos(0, 0), Pos(0, 1)), Left),
        (Block(Pos(1, 2), Pos(2, 2)), Down)
      ))
    }
  }

  test("done") {
    new Level1 {
      assert(done(Block(Pos(4, 7), Pos(4, 7))))
      assert(!done(Block(Pos(4, 6), Pos(4, 7))))
    }
  }

  test("neighborsWithHistory - empty history") {
    new Level1 {
      assert(neighborsWithHistory(Block(Pos(1, 1), Pos(1, 1)), Nil) === Stream(
        (Block(Pos(1, 2), Pos(1, 3)), List(Right)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Down))
      ))
    }
  }

  test("neighborsWithHistory - with history") {
    new Level1 {
      assert(neighborsWithHistory(Block(Pos(1, 1), Pos(1, 1)), List(Left, Up)) === Stream(
        (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
      ))
    }
  }

  test("newNeighborsOnly") {
    new Level1 {
      assert(newNeighborsOnly(
        Set(
          (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
          (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
        ).toStream,
        Set(Block(Pos(1, 2), Pos(1, 3)), Block(Pos(1, 1), Pos(1, 1)))
      ) === Set(
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
      ).toStream)
    }
  }

  test("from level 0") {
    new Level0 {
      assert(from(Stream((startBlock, List())), Set(startBlock)).toList === List(
        (Block(Pos(1, 2), Pos(1, 2)), List()),
        (Block(Pos(2, 2), Pos(3, 2)), List(Down)),
        (Block(Pos(2, 3), Pos(3, 3)), List(Right, Down)),
        (Block(Pos(1, 3), Pos(1, 3)), List(Up, Right, Down))
      ))
    }
  }

  test("pathsFromStart level 0") {
    new Level0 {
      assert(pathsFromStart.toList === List(
        (Block(Pos(1, 2), Pos(1, 2)), List()),
        (Block(Pos(2, 2), Pos(3, 2)), List(Down)),
        (Block(Pos(2, 3), Pos(3, 3)), List(Right, Down)),
        (Block(Pos(1, 3), Pos(1, 3)), List(Up, Right, Down))
      ))
    }
  }

  test("pathsToGoal level 0") {
    new Level0 {
      assert(pathsToGoal.toList === List(
        (Block(Pos(1, 3), Pos(1, 3)), List(Up, Right, Down))
      ))
    }
  }
}
