package whole

/**
 * Class modelling a race car
 */

import scala.io._
import scala.math._

case class RaceCar(player: Player, name: String, carCharacter: Char){
    /**
     * Class representing a race car and its associated set of moves: {Right, Left, Forward} for carDirection moves
     * and {Gear1, Gear2, Gear3, Gear4, Gear5, noShift} for gear moves
     */
    val index = player.index
    var gear = 1
    var gearState: GearState = Gear1
    var carDirection: Vector2D = Vector2D(0, 1)   // forward carDirection of race car in gear 1
    var applicableDirections = Array(Vector2D(-1,1), Vector2D(0,1), Vector2D(1,1))   // left(), forward(), right()
    var carPosition: Vector2D = Vector2D(0, 0).toRowCol  // grid coordinates of race car; (0,0) is the bottom-left corner of the grid;

    // defining get methods
    def getPlayer = player
    def getCarName = name
    def getCarPosition = carPosition
    def getGear = gearState.name
    def getGearState = gearState
    def getCarStatus = s"Gear state: \"${gearState.name}\"\n" +
      s"            Position vector (row, column): \"${carPosition}\"\n" +
      s"            Direction vector (x, y): \"${carDirection}\"\n" +
      s"            Applicable direction vectors (x, y): \"${applicableDirections(0)}, ${applicableDirections(1)}, ${applicableDirections(2)}\"\n"

    def setGearState(gs: GearState) = gearState = gs
    // whole.State design pattern: Methods work differently depending on how the race car is orientated and in which gear the car is in
    def shiftUp() = gearState.shiftUp()
    def shiftDown() = gearState.shiftDown()
    def noShift() = gearState.noShift()

    def right() = gearState.right()
    def left() = gearState.left()
    def forward() = gearState.forward()

    // In some game modes, the car may break down after some time if a pit stop to replace gears is not made in time
    def break() = gearState.break()

    override def toString: String = carCharacter.toString

    sealed trait GearState {
        // define attributes
        val name: String
        val canShiftUp = true
        val canShiftDown = true
        val canGoRight = true
        val canGoLeft = true
        val canGoForward = true
        val broken = false
        var candidateDirections = Array[Vector2D]()

        def shiftUp() = {
            gear += 1
            // Find the new direction candidates
            val sgnX = if (carDirection.x < 0) - 1 else 1
            val sgnY = if (carDirection.y < 0) - 1 else 1
            if (carDirection.x.abs == gear && carDirection.y.abs != gear){ 
                  candidateDirections = Array(Vector2D(carDirection.x + sgnX, carDirection.y), Vector2D(carDirection.x + sgnX,
                                        carDirection.y + sgnY))
            }
            else if (carDirection.x.abs != gear && carDirection.y.abs == gear) {
                candidateDirections = Array(Vector2D(carDirection.x, carDirection.y + sgnY), Vector2D(carDirection.x + sgnX,
                                      carDirection.y + sgnY))
            }
            else {
                candidateDirections = Array(Vector2D(carDirection.x + sgnX, carDirection.y + sgnY))
            }
            // Update direction to the correct new direction among candidate directions
            try {
                val atans = candidateDirections.map(vec => (vec, (atan(vec.y/vec.x) - atan(carDirection.y/carDirection.x)).abs)).toMap
                // Correct new direction has least arctan difference with old direction
                val newCarDir = atans.filter({case (k, v) => v == atans.valuesIterator.min}).keySet.head
                carDirection = newCarDir
            }
            catch {
                // Division by 0 for x = 0
                case e: ArithmeticException => {
                    carDirection = Vector2D(carDirection.x, carDirection.y + sgnY)
                }
            }
            // Update applicable directions
            if (carDirection.x.abs == carDirection.y.abs){
                applicableDirections = Array(Vector2D(carDirection.x - sgnX, carDirection.y), carDirection,
                                       Vector2D(carDirection.x, carDirection.y - sgnY))
            }
            else if (carDirection.x.abs > carDirection.y.abs){
                applicableDirections = Array(Vector2D(carDirection.x, carDirection.y - 1) , carDirection,
                                       Vector2D(carDirection.x, carDirection.y + 1))
            }
            else {
                applicableDirections = Array(Vector2D(carDirection.x - 1, carDirection.y) , carDirection,
                                       Vector2D(carDirection.x + 1, carDirection.y))
            }
        }

        def shiftDown() = {
            gear -= 1
            // Find the new direction candidates
            val sgnX = if (carDirection.x < 0) - 1 else 1
            val sgnY = if (carDirection.y < 0) - 1 else 1
            if (carDirection.x.abs == gear && carDirection.y.abs != gear){
                  candidateDirections = Array(Vector2D(carDirection.x - sgnX, carDirection.y), Vector2D(carDirection.x - sgnX,
                                        carDirection.y - sgnY))
            }
            else if (carDirection.x.abs != gear && carDirection.y.abs == gear) {
                candidateDirections = Array(Vector2D(carDirection.x, carDirection.y - sgnY), Vector2D(carDirection.x - sgnX,
                                      carDirection.y - sgnY))
            }
            else {
                candidateDirections = Array(Vector2D(carDirection.x - sgnX, carDirection.y - sgnY))
            }
            // Update direction to the correct new direction among candidate directions
            try {
                val atans = candidateDirections.map(vec => (vec, (atan(vec.y/vec.x) - atan(carDirection.y/carDirection.x)).abs)).toMap
                // Correct new direction has least arctan difference with old direction
                val newCarDir = atans.filter({case (k, v) => v == atans.valuesIterator.min}).keySet.head
                carDirection = newCarDir
            }
            catch {
                // Division by 0 for x = 0
                case e: ArithmeticException => {
                    carDirection = Vector2D(carDirection.x, carDirection.y - sgnY)
                }
            }
            // Update applicable directions
            if (carDirection.x.abs == carDirection.y.abs){
                applicableDirections = Array(Vector2D(carDirection.x - sgnX, carDirection.y), carDirection,
                                       Vector2D(carDirection.x, carDirection.y - sgnY))
            }
            else if (carDirection.x.abs > carDirection.y.abs){
                applicableDirections = Array(Vector2D(carDirection.x, carDirection.y - 1) , carDirection,
                                       Vector2D(carDirection.x, carDirection.y + 1))
            }
            else {
                applicableDirections = Array(Vector2D(carDirection.x - 1, carDirection.y) , carDirection,
                                       Vector2D(carDirection.x + 1, carDirection.y))
            }
        }

        def noShift() = {}
        
        def left() = {
            // Update car position
            carPosition = carPosition + applicableDirections(0).toRowCol
            // Update car carDirection
            carDirection = applicableDirections(0)
            // Update applicable directions
            val sgnX = if (carDirection.x < 0) - 1 else 1
            val sgnY = if (carDirection.y < 0) - 1 else 1
            if (carDirection.x.abs == gear && carDirection.y.abs != gear) {
                applicableDirections = Array(Vector2D(applicableDirections(0).x, applicableDirections(0).y + sgnX),
                                       applicableDirections(0), applicableDirections(1))
            }
            else if (carDirection.y.abs == gear && carDirection.x.abs != gear) {
                applicableDirections = Array(Vector2D(applicableDirections(0).x - sgnY, applicableDirections(0).y),
                                       applicableDirections(0), applicableDirections(1))
            }
            else if (carDirection.y.abs == gear && carDirection.x.abs == gear) {
              if (sgnX == sgnY) {
                  applicableDirections = Array(Vector2D(applicableDirections(0).x - sgnY, applicableDirections(0).y),
                                         applicableDirections(0), applicableDirections(1))
              }
              else {
                  applicableDirections = Array(Vector2D(applicableDirections(0).x, applicableDirections(0).y + sgnX),
                                         applicableDirections(0), applicableDirections(1))
              }
            }
        }

        def forward() = {
            // Update car position
            carPosition = carPosition + applicableDirections(1).toRowCol
            // Update car carDirection
            carDirection = applicableDirections(1)
            // Possible directions remain constant
        }
      
        def right() = {
            // Update car position
            carPosition = carPosition + applicableDirections(2).toRowCol
            // Update car carDirection
            carDirection = applicableDirections(2)
            // Update possible directions
            val sgnX = if (carDirection.x < 0 ) - 1 else 1
            val sgnY = if (carDirection.y < 0 ) - 1 else 1
            if (carDirection.x.abs == gear && carDirection.y.abs != gear) {
                applicableDirections = Array(applicableDirections(1) , applicableDirections(2),
                                       Vector2D(applicableDirections(2).x, applicableDirections(2).y - sgnX))
            }
            else if (carDirection.y.abs == gear && carDirection.x.abs != gear) {
                applicableDirections = Array(applicableDirections(1), applicableDirections(2),
                                       Vector2D(applicableDirections(2).x + sgnY, applicableDirections(2).y))
            }
            else if (carDirection.y.abs == gear && carDirection.x.abs == gear) {
                if (sgnX == sgnY) {
                    applicableDirections = Array(applicableDirections(1) , applicableDirections(2),
                                           Vector2D(applicableDirections(2).x, applicableDirections(2).y - sgnX))
                }
                else {
                    applicableDirections = Array(applicableDirections(1) , applicableDirections(2),
                                           Vector2D(applicableDirections(2).x + sgnY, applicableDirections(2).y))
                }
            }
        }
        
        def break() = gearState = GearBroken

    }

    case object Gear1 extends GearState {
        val name = "Gear1"
        val possibleDirections = Array(Vector2D(-1,1), Vector2D(0,1), Vector2D(1,1),
                                       Vector2D(-1,0), Vector2D(0,0), Vector2D(1,0),
                                       Vector2D(-1,-1), Vector2D(0,-1), Vector2D(1,-1))
        override def shiftUp() = {
            super.shiftUp()
            gearState = Gear2
        }
        override def shiftDown() = {
            throw IllegalMove("Cannot shift down from Gear 1\n")   
        }
        override val canShiftDown = false
    }

    case object Gear2 extends GearState {
        val name = "Gear2"
        val possibleDirections = Array(Vector2D(-2,2), Vector2D(-1,2), Vector2D(0,2), Vector2D(1,2), Vector2D(2,2),
                                       Vector2D(-2,1),                                               Vector2D(2,1),
                                       Vector2D(-2,0),                 Vector2D(0,0),                Vector2D(2,0),
                                       Vector2D(-2,-1),                                              Vector2D(2,-1),
                                       Vector2D(-2,-2), Vector2D(-1,-2), Vector2D(0,-2), Vector2D(1,-2),  Vector2D(2,-2))
        override def shiftUp() = {
            super.shiftUp()
            gearState = Gear3
        }
        override def shiftDown() = {
            super.shiftDown()
            gearState = Gear1
        }
    }

    case object Gear3 extends GearState {
        val name = "Gear3"
        val possibleDirections = Array(Vector2D(-3,3), Vector2D(-2,3), Vector2D(-1,3), Vector2D(0,3), Vector2D(1,3), Vector2D(2,3), Vector2D(3,3),
                                       Vector2D(-3,2),                                                                              Vector2D(3,2),
                                       Vector2D(-3,1),                                                                              Vector2D(3,1),
                                       Vector2D(-3,0),                                 Vector2D(0,0),                               Vector2D(3,0),
                                       Vector2D(-3,-1),                                                                             Vector2D(3,-1),
                                       Vector2D(-3,-2),                                                                             Vector2D(3,-2),
                                       Vector2D(-3,-3), Vector2D(-2,-3), Vector2D(-1,-3), Vector2D(0,-3), Vector2D(1,-3), Vector2D(2,-3), Vector2D(3,-3))
        override def shiftUp() = {
            super.shiftUp()
            gearState = Gear4
        }
        override def shiftDown() = {
            super.shiftDown()
            gearState = Gear2
        }
    }

    case object Gear4 extends GearState {
        val name = "Gear4"
        val possibleDirections = Array(Vector2D(-4,4), Vector2D(-3,4), Vector2D(-2,4), Vector2D(-1,4), Vector2D(0,4), Vector2D(1,4), Vector2D(2,4), Vector2D(3,4), Vector2D(4,4),
                                       Vector2D(-4,3),                                                                                                             Vector2D(4,3),
                                       Vector2D(-4,2),                                                                                                             Vector2D(4,2),
                                       Vector2D(-4,1),                                                                                                             Vector2D(4,1),
                                       Vector2D(-4,0),                                                 Vector2D(0,0),                                              Vector2D(4,0),
                                       Vector2D(-4,-1),                                                                                                            Vector2D(4,-1),
                                       Vector2D(-4,-2),                                                                                                            Vector2D(4,-2),
                                       Vector2D(-4,-3),                                                                                                            Vector2D(4,-3),
                                       Vector2D(-4,-4), Vector2D(-3,-4), Vector2D(-2,-4), Vector2D(-1,-4), Vector2D(0,-4), Vector2D(1,-4), Vector2D(2,-4), Vector2D(3,-4), Vector2D(4,-4))
        override def shiftUp() = {
            super.shiftUp()
            gearState = Gear5
        }
        override def shiftDown() = {
            super.shiftDown()
            gearState = Gear3
        }
    }

    case object Gear5 extends GearState {
        val name = "Gear5"
        val possibleDirections = Array(Vector2D(-5,5), Vector2D(-4,5), Vector2D(-3,5), Vector2D(-2,5), Vector2D(-1,5), Vector2D(0,5), Vector2D(1,5), Vector2D(2,5), Vector2D(3,5), Vector2D(4,5), Vector2D(5,5),
                                       Vector2D(-5,4),                                                                                                                                            Vector2D(5,4),
                                       Vector2D(-5,3),                                                                                                                                            Vector2D(5,3),
                                       Vector2D(-5,2),                                                                                                                                            Vector2D(5,2),
                                       Vector2D(-5,1),                                                                                                                                            Vector2D(5,1),
                                       Vector2D(-5,0),                                                                 Vector2D(0,0),                                                             Vector2D(5,0),
                                       Vector2D(-5,-1),                                                                                                                                           Vector2D(5,-1),
                                       Vector2D(-5,-2),                                                                                                                                           Vector2D(5,-2),
                                       Vector2D(-5,-3),                                                                                                                                           Vector2D(5,-3),
                                       Vector2D(-5,-4),                                                                                                                                           Vector2D(5,-4),
                                       Vector2D(-5,-5), Vector2D(-4,-5), Vector2D(-3,-5), Vector2D(-2,-5), Vector2D(-1,-5), Vector2D(0,-5), Vector2D(1,-5), Vector2D(2,-5), Vector2D(3,-5), Vector2D(4,-5), Vector2D(5,-5))
        override def shiftUp() = {
            throw IllegalMove("Cannot shift up from Gear 5\n")   
        }
        override def shiftDown() = {
            super.shiftDown()
            gearState = Gear4
        }
        override val canShiftUp = false
    }

    case object GearBroken extends GearState {
        val name = "GearBroken"
        val possibleDirections: Array[Vector2D] = Array()
        override def shiftUp() = {}
        override def shiftDown() = {}
        override val canShiftUp = false
        override val canShiftDown = false
        override val canGoRight = false
        override val canGoLeft = false
        override val canGoForward = false
    }
}

// Class for illegal move exceptions
case class IllegalMove(description: String) extends Exception(description: String)

// Class for a 2-dimensional vector defining x, y coordinates
case class Vector2D(x: Int, y: Int){
    def magnitude = scala.math.sqrt(x*x + y*y)
    def +(that: Vector2D): Vector2D = Vector2D(x+that.x, y+that.y)
    def -(that: Vector2D): Vector2D = Vector2D(x-that.x, y-that.y)
    def *(scalar: Int): Vector2D = Vector2D(x*scalar, y*scalar)
    def dot(that: Vector2D): Int = {x*that.x + y*that.y}
    def equals(that: Vector2D): Boolean = (x == that.x) && (y == that.y)
    def toTuple = Tuple2(x, y)
    // 2-dimensional vector defining row, column coordinates
    def toRowCol = Vector2D(y, x)
    def deepcopy() = Vector2D(x, y)
}

