package whole

/**
 * Class modelling a player
 */

case class Player(name: String, index: Int) {
  val rand = new scala.util.Random(System.nanoTime())
  val id = rand.nextInt()
  val turnTimes = scala.collection.mutable.Buffer[Int]()
  val lapTimes = scala.collection.mutable.Buffer[Int]()
  var wallCrashes = 0
  var carCollisions = 0
   
  override def toString: String = s"Player $index: \"$name\""
}
