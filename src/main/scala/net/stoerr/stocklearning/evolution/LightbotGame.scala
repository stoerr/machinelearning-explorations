package net.stoerr.stocklearning.evolution

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 24.01.2015
 */
object LightbotGame {

}

case class Direction(x: Int, y: Int) {
  def left = Direction(-y, x)

  def right = Direction(y, -x)
}

case class Location(x: Int, y: Int) {
  def +(d: Direction) = Location(x + d.x, y + d.y)
}

sealed trait Field {
  val height: Int
}

case class Empty(height: Int) extends Field

// case class Light(id: Int, height: Int) extends Field

case class LightbotGame(val locations: Map[Location, Field]) {

  sealed trait Action {
    def apply(state: BoardState): Option[BoardState]
  }

  case class BoardState(botLocation: Location, direction: Direction, lights: Map[Location, Boolean]) {
    def currentField: Field = locations(botLocation)
  }

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


}

