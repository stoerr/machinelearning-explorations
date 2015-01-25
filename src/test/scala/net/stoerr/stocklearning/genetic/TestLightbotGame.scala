package net.stoerr.stocklearning.genetic

import org.scalatest.FunSuite

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 25.01.2015
 */
class TestLightbotGame extends FunSuite {

  test("Run Lightbot") {
    val setup = "E e l "
    val board = LightbotGame(setup)
    println(board)
    val program = List(board.Forward, board.Forward, board.Switch)
    println(program)
    val result: Option[board.BoardState] = board.run(program)
    println(result)
    assert(result.get.finished)
  }

  test("Create program") {
    val setup =
      """e e l
        |E e e
        |e l e""".stripMargin
    val board = LightbotGame(setup)
    // board.allPrograms take 20 foreach println
    val solution = board.allPrograms find (board.solvedBy(_))
    // println(solution.get)
    assert("List(R, F, L, F, S, F, L, F, F, S)" == solution.get.toString)
  }

}
