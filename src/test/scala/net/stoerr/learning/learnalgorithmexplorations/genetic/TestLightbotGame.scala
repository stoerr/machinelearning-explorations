package net.stoerr.learning.learnalgorithmexplorations.genetic

import org.scalatest.FunSuite

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 25.01.2015
 */
class TestLightbotGame extends FunSuite {

  test("Run Lightbot") {
    val setup = "E e1l2"
    val board = LightbotGame(setup)
    println(board)
    val program = List(board.Jump, board.Jump, board.Switch)
    // println(program)
    val result: Option[board.BoardState] = board.run(program)
    // println(result)
    assert(result.get.finished)
  }

  ignore("Create program") {
    val setup =
      """E e l2
        |e l e1""".stripMargin
    val board = LightbotGame(setup)
    // board.allPrograms take 20 foreach println
    val solution = board.allPrograms find (board.solvedBy(_))
    // println(board.run(solution.get))
    println(solution.get.mkString)
    assert("RFLFSJLJS" == solution.get.mkString)
  }

}
