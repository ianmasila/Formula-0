package whole

/**
 * Class modelling a formula race state
 */

import scala.collection._
import scala.io._

// Every state of the game is associated with a winning status, actions, and successor states.
abstract class State {

    def successorState: Formula0

    def isWinner: Array[String]

    def toString: String
}

// Game state class
case class Formula0(player0: Player, car0: RaceCar, player1: Player, car1: RaceCar,
                    track: Array[String], wallTile: Set[Char], finishLine: Char, maxLap: Int = 5) extends State {

    // Attributes of a game state
    // players
    val players = Array(player0, player1)
    val cars = Array(car0, car1)
    // instantiate a grid
    val gameGrid: Grid = Grid()
    // set the grid
    gameGrid.setGrid(track, wallTile, finishLine)
    val grid: Map[Tuple2[Int, Int], Char] = gameGrid.grid
    val gridViewLen = gameGrid.toString.length
    // valid states where racing is possible
    val states: Map[Tuple2[Int, Int], Char] = gameGrid.states
    val statesKeySet = states.keySet

    // Variables

    // current player
    var playerIdx = 0
    // current car
    var car = car0
    var otherCar = car1
    var car0Pos = Vector2D(car0.carPosition.x, car0.carPosition.y).deepcopy()
    var car1Pos = Vector2D(car1.carPosition.x, car1.carPosition.y).deepcopy()
    var car0PosGridView = 0
    var car1PosGridView = 1
    var lap = Array.fill[Int](2)(0)
    var turn = 0
    var moveMade = Array.fill[String](2)("")
    var startTime = 0
    var endTime = 0
    var takenTime = 0
    
    // defining get methods

    def getTurn: Int = turn

    def getCurrentPlayer: Int = {
        // Return the index of the current player
        playerIdx
    }

    def getPlayerNames: Array[String] = {
        // Return the name of the players
        Array(players(0).name, players(1).name)
    }
    
    def getPlayerFromName: Map[String, Player] = {
        (getPlayerNames zip players).toMap
    }

    def getCarFromPlayer: Map[Player, RaceCar] = {
        (players zip Array(car0, car1)).toMap
    }  

    def getLap: Map[String, Int] = {
        (getPlayerNames zip lap).toMap
    }

    // Computing successor states

    def getNextState: Formula0 = {
        // make a deep copy of current state
        val nextState = deepcopy()
        // Switch players
        nextState.playerIdx = 1 - playerIdx
        // Switch cars and their grid position views
        nextState.car = if (playerIdx == 0) car1 else car0
        nextState.otherCar = if (playerIdx == 0) car0 else car1
        // Update turn
        nextState.turn += 1
        nextState
    }

    def successorState: Formula0 = {
        // Base next state before moving car
        val nextState = getNextState
        val nextStateBackup = getNextState

        // Action given
        def userGearInput(): String = {
            val gearShiftInputRaw = StdIn.readLine("Please select a gear shift...\n" +
              "[W] ShiftUp \t [S] ShiftDown \t [0]  NoShift \n")
            val gearShiftInput = gearShiftInputRaw.trim.toUpperCase
            if (!(Set("W", "S", "0") contains gearShiftInput)) {
                print(s"\u001B[31mInvalid input '$gearShiftInputRaw' received...\u001b[0m\n")
                userGearInput()
            }
            else gearShiftInput
        }

        def shiftGear(): Unit = {
            userGearInput() match {
                case "W" => {
                    try nextState.otherCar.shiftUp() catch {
                        case IllegalMove(description) => {
                            print("\u001B[31mIllegalMove: " + description + "\u001b[0m")
                            shiftGear()
                        }
                    }
                    finally moveMade(0) = "[W] ShiftUp"
                }
                case "S" => {
                    try nextState.otherCar.shiftDown() catch {
                        case IllegalMove(description) => {
                            print("\u001B[31mIllegalMove: " + description + "\u001b[0m")
                            shiftGear()
                        }
                    }
                    finally moveMade(0) = "[S] ShiftDown"
                }
                case "0" => {
                    nextState.otherCar.noShift()
                    moveMade(0) = "[0] NoShift"
                }
            }
        }

        def userDirInput(): String = {
            val dirInputRaw = StdIn.readLine("Please select a direction to move...\n" +
              "[A] Left \t [W] Forward \t [D] right\n")
            val dirInput = dirInputRaw.trim.toUpperCase
            if (!(Set("W", "A", "D") contains dirInput)) {
                print(s"\u001B[31mInvalid input '$dirInputRaw' received...\u001b[0m\n")
                userDirInput()
            }
            else dirInput
        }

        def moveDir(): Unit = {
            userDirInput() match {
                case "W" => {
                    nextState.otherCar.forward()
                    moveMade(1) = "[W] Forward"
                }
                case "A" => {
                    nextState.otherCar.left()
                    moveMade(1) = "[A] Left"
                }
                case "D" => {
                    nextState.otherCar.right()
                    moveMade(1) = "[D] Right"
                }
            }
        }

        // Move the car
        startTime = (System.nanoTime()/(1000000000)).toInt
        shiftGear()
        moveDir()
        endTime = (System.nanoTime()/(1000000000)).toInt
        takenTime = endTime - startTime

        // Add takenTime to current player turn times buffer
        players(playerIdx).turnTimes += takenTime

        // Update otherCarPosGridView for next state
        if (playerIdx == 0){
            nextState.car0PosGridView = (gridViewLen - 1) - ((car0.carPosition.x * (track(0).length + 1)) +
                                        (track(0).length - car0.carPosition.y))
        }
        else {
            nextState.car1PosGridView = (gridViewLen - 1) - ((car1.carPosition.x * (track(0).length + 1)) +
                                        (track(0).length - car1.carPosition.y))
        }

        // Update lap and lap time buffer
        if (finishLine == states.getOrElse(nextState.otherCar.carPosition.toTuple, ' ')) {
            lap(playerIdx) += 1
            players(playerIdx).lapTimes += players(playerIdx).turnTimes.sum
            players(playerIdx).turnTimes.clear()
        }

        // Check if car is in valid state: not in wall tile
        if (!(statesKeySet contains nextState.otherCar.carPosition.toTuple)){
            nextState.otherCar.player.wallCrashes += 1
            gameGrid.gameCrashScreen(nextState.otherCar.carCharacter)
            print(s"\u001b[31mYou veered off track to position ${nextState.otherCar.carPosition.toTuple}. " +
              s"Off grid: ${!(grid contains nextState.otherCar.carPosition.toTuple)}!\n" +
              s"Move made: ${moveMade.mkString("\t")}\n" +
              s"Resetting \"${nextState.otherCar.carCharacter.toString}\" to previous location...\u001b[0m\n")
            Thread.sleep(1600)

//            // Stationary direction change possible
//            // Return car to previous direction
//            if (moveMade(1).startsWith("[A]")){
//                nextState.otherCar.right()
//            }
//            else if (moveMade(1).startsWith("[D]")){
//                nextState.otherCar.left()
//            }
//            // Return car to previous gear state
//            if (moveMade(0).startsWith("[W]")){
//                nextState.otherCar.shiftDown()
//            }
//            else if (moveMade(0).startsWith("[S]")){
//                nextState.otherCar.shiftUp()
//            }

            // Return car to previous position allowing stationary gear changes and direction changes
            nextState.otherCar.carPosition = if (playerIdx == 0) car0Pos else car1Pos
            // Return to previous screen
            nextStateBackup
        }
        // Check no collision has occurred
        else if (nextState.otherCar.carPosition == otherCar.carPosition){
            nextState.otherCar.player.carCollisions += 1
            gameGrid.gameCrashScreen(nextState.otherCar.carCharacter)
            print(s"\u001b[31mYou crashed into \"${otherCar.carCharacter.toString}\"!\n" +
              s"Resetting \"${nextState.otherCar.carCharacter.toString}\" to previous location...\u001b[0m\n")
            Thread.sleep(1600)

//            // Stationary direction change possible
//            // Return car to previous direction
//            if (moveMade(1).startsWith("[A]")){
//                nextState.otherCar.right()
//            }
//            else if (moveMade(1).startsWith("[D]")){
//                nextState.otherCar.left()
//            }
//            // Return car to previous gear state
//            if (moveMade(0).startsWith("[W]")){
//                nextState.otherCar.shiftDown()
//            }
//            else if (moveMade(0).startsWith("[S]")){
//                nextState.otherCar.shiftUp()
//            }

            // Return car to previous position allowing stationary gear changes and direction changes
            nextState.otherCar.carPosition = if (playerIdx == 0) car0Pos else car1Pos
            // Return to previous screen
            nextStateBackup
        }
        else nextState
    }


    def isWinner: Array[String] = {

        val playerLapMap = getLap
        val leaderLap = playerLapMap.valuesIterator.max
        val leaderMap = playerLapMap.filter({case (k, v) => v == leaderLap})
        if (leaderLap == maxLap) {
            if (leaderMap.size > 1){
                Array(player0.name, player1.name)
            }
            Array(leaderMap.keySet.head)
        }
        Array[String]()
    }

    def deepcopy(): Formula0 = {
        val cloneState = Formula0(player0, car0, player1, car1,
                    track, wallTile, finishLine, maxLap)
        // Update relevant variables
        cloneState.playerIdx = playerIdx
        cloneState.car = car
        cloneState.otherCar = otherCar
        cloneState.turn = turn
        cloneState.lap = lap
        cloneState.car0PosGridView = car0PosGridView
        cloneState.car1PosGridView = car1PosGridView
        cloneState
    }
    
    override def toString = {
        // Visualise the game state as a string that can be printed on screen
        var gridView = gameGrid.toString  // track itself as a single string
        gridView = gridView.slice(0, car1PosGridView) + car1.toString + gridView.slice(car1PosGridView + 1, gridView.length)
        gridView = gridView.slice(0, car0PosGridView) + car0.toString + gridView.slice(car0PosGridView + 1, gridView.length)
        gridView
    }
}

