package net.stoerr.stocklearning.genetic

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 24.01.2015
 */
object LightbotGame {

  def apply(description: String): LightbotGame = {
    var startBot: Location = null
    val locations: Map[Location, Field] = description.split("\r\n").zipWithIndex.flatMap { case (row, rownum) =>
      val lineAdjusted = if (row.length % 2 == 0) row else row + " "
      lineAdjusted.toCharArray.grouped(2).zipWithIndex.map { case (Array(ftype, heightChar), column) =>
        val height: Int = if (heightChar == ' ') 0 else heightChar.toInt
        val loc = Location(column, rownum)
        if (ftype == 'E' || ftype == 'L') startBot = loc
        ftype match {
          case 'e' | 'E' => (loc, Empty(height))
          case 'l' | 'L' => (loc, Light(height))
          case other => error("Invalid character " + other + " at " +(rownum, column) + " in \n" + description)
        }
      }
    }.toMap
    val startLights = locations.filter(_._2.isInstanceOf[Light]).keys.map(_ -> false).toMap
    LightbotGame(locations, startBot, startLights)
  }

}

case class Direction(x: Int, y: Int) {
  def left = Direction(y, -x)

  def right = Direction(-y, x)
}

case class Location(x: Int, y: Int) {
  def +(d: Direction) = Location(x + d.x, y + d.y)
}

sealed trait Field {
  val height: Int
}

case class Empty(height: Int) extends Field

case class Light(height: Int) extends Field

case class LightbotGame(val locations: Map[Location, Field], startBot: Location, startLights: Map[Location, Boolean]) {

  sealed trait Action {
    def apply(state: BoardState): Option[BoardState]
  }

  case class BoardState(botLocation: Location, direction: Direction, lights: Map[Location, Boolean]) {
    def currentField: Field = locations(botLocation)

    def finished: Boolean = lights.values.forall(on => on)
  }

  def startState = BoardState(startBot, Direction(1, 0), startLights)

  def run(program: Seq[Action]) = program.foldLeft[Option[BoardState]](Some(startState))((state, action) => state.flatMap(action(_)))

  def solvedBy(program: Seq[Action]): Boolean = run(program).map(_.finished).getOrElse(false)

  object Forward extends Action {
    override def toString = "F"

    override def apply(state: BoardState): Option[BoardState] = {
      val nextLocation = state.botLocation + state.direction
      locations.get(nextLocation).filter(state.currentField.height == _.height).map(_ => state.copy(botLocation = nextLocation))
    }
  }

  object Right extends Action {
    override def toString = "R"

    override def apply(state: BoardState): Option[BoardState] = Some(state.copy(direction = state.direction.right))
  }

  object Left extends Action {
    override def toString = "L"

    override def apply(state: BoardState): Option[BoardState] = Some(state.copy(direction = state.direction.left))
  }

  object Switch extends Action {
    override def toString = "S"

    override def apply(state: BoardState): Option[BoardState] = state.lights.get(state.botLocation) map { lightstate =>
      state.copy(lights = state.lights.updated(state.botLocation, !lightstate))
    }
  }

  val allActions: Seq[Action] = Array(Forward, Switch, Left, Right)

  val allPrograms: Stream[List[Action]] = allActions.map(List(_)).toStream #::: allPrograms.flatMap(p => allActions.map(_ :: p))

}

