package day1

object Application {

  sealed trait Direction extends Product with Serializable
  case object North extends Direction
  case object South extends Direction
  case object East extends Direction
  case object West extends Direction

  sealed trait Command extends Product with Serializable
  case object Left extends Command
  case object Right extends Command

  val trans = Map(
  (North, Left) -> West,
    (North, Right) -> East,
    (South, Left) -> East,
    (South, Right) -> West,
    (East, Left) -> North,
    (East, Right) -> South,
    (West, Left) -> South,
    (West, Right) -> North)

  def main(args: Array[String]): Unit = {
    val init: Direction = North
    val input = io.Source.fromInputStream(Application.getClass.getClassLoader.getResourceAsStream("day1.txt")).mkString

    val commands = input.split(",").toList.map(_.trim)
    val interpreted = commands.map { cmd => interpret(cmd) }

    val reduced = interpreted.foldLeft((0,0,init)){ case ((a,b,d), (cmd, count)) => {
      trans((d, cmd)) match {
        case North => (a+count, b, North)
        case South => (a - count, b, South)
        case East => (a, b + count, East)
        case West => (a, b - count, West)
      }
    }}

    println(s"We are ${Math.abs(reduced._1) + Math.abs(reduced._2)} steps away.")

  }

  def interpret(command:String): (Command, Int) = {
    val pattern = "(L|R)([0-9]+)".r
    val pattern(dir, count) = command
    val d = dir match {
      case "L" => Left
      case "R" => Right
      case _ => throw new IllegalArgumentException("Unknown direction")
    }
    (d, count.toInt)
  }
}
