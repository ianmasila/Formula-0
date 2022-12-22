package whole

object Main {

/**
 * Game class modelling gameplay
 */

import java.io.{PrintWriter, FileWriter, File, BufferedReader, BufferedWriter, IOException, InputStreamReader, OutputStreamWriter}
import scala.io.{StdIn, Source}
import scala.collection.mutable._
import java.util.Calendar

class Game() {
    // IO
    // Writing results
    val pwTracks = new PrintWriter(new FileWriter("Tracks.txt", true))
    val pwGameHistory = new PrintWriter(new FileWriter("GameHistory.txt", true))
    // Reading tracks
    val filePath = "Tracks.txt"
    val file = new File(filePath)
    val fin1 = Source.fromFile(file)
    val fin2 = Source.fromFile(file)
    val data: Iterator[String] = fin1.getLines
    val dataToPrint:  Iterator[String] = fin2.getLines
    val dataArray = data.toArray
    val trackContent = dataArray.filter(_.startsWith(s"\""))
    val trackNames = dataArray.filter(_.startsWith("name")).map(x => x.splitAt(5)._2.trim.toLowerCase())
    val trackBuffer = Buffer[Buffer[String]]()

    // Fill buffer of buffer of strings for tracks
    var trackNumber = 0
    trackBuffer += Buffer[String]()
    trackContent.foreach(line => {
        if (!(line.startsWith(s"\"END"))) {
            trackBuffer(trackNumber) += line.slice(1, line.length - 1)
        }
        else {
            trackNumber += 1
            trackBuffer += Buffer[String]()
        }
    })

    // Map track names to track arrays
    val trackMap = trackNames.zip(trackBuffer.map(_.toArray)).toMap

    // Random generator
    val rand = new scala.util.Random(System.nanoTime())

    // Calendar instance
    val calendar = Calendar.getInstance

    // Default track 20 x 42
    val defaultTrack = Array(
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX",
            "XXXXXXXXXXXXXXXXX          XXXXXXXXXXXXXXX",
            "XXXXXXXXXXXXXX                XXXXXXXXXXXX",
            "XXXXXXXXXXX        XXXXX        XXXXXXXXXX",
            "XXXXXXXXX       XXXXXXXXXXX       XXXXXXXX",
            "XXXXXXX       XXXXXXXXXXXXXXX      XXXXXXX",
            "XXXXX        XXXXXXXXXXXXXXXXXX      XXXXX",
            "XXXXX       XXXXXXXXXXXXXXXXX        XXXXX",
            "XXXXXX     XXXXXXXXXXXXXXXXX        XXXXXX",
            "XXXXXXXX     XXXXXXXXXXXXX         XXXXXXX",
            "XXXXXXXXX      XXXXXXXXXX         XXXXXXXX",
            "XXXXXXXXXX      XXXXXXXXXX########XXXXXXXX",
            "XXXXXXXXXX     XXXXXXXXXXX       XXXXXXXXX",
            "XXXXXXXXX        XXXXXXX       XXXXXXXXXXX",
            "XXXXXXXXXX        XXXXX      XXXXXXXXXXXXX",
            "XXXXXXXXXXXX       XXX     XXXXXXXXXXXXXXX",
            "XXXXXXXXXXXXXX      X     XXXXXXXXXXXXXXXX",
            "XXXXXXXXXXXXXXXX          XXXXXXXXXXXXXXXX",
            "XXXXXXXXXXXXXXXXXX       XXXXXXXXXXXXXXXXX",
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")

    var trackName = "defaultTrack"
    var track = defaultTrack
    var player0 = Player("Anonymous0", 0)
    var car0 = RaceCar(player0, "Anonymous0", '0')
    var player1 = Player("Anonymous1", 1)
    var car1 = RaceCar(player1, "Anonymous1", '1')

    // Decorative game screens
    def gameLoadingScreen() = {
        (1 to 100).foreach(i => {
             val dots = "." * ((i-1)/2)
             print(s"Game loading$dots$i%\r")
             Thread.sleep(12)
        })
    }

    def gamePlaySelectScreen(): Unit = {
        print("\u001b[1mFORMULA O\n\n")
        print("\u001b[1m\u001b[42;1 [1] Play\n\u001b[41;1 [0] Exit\n")
        print(s"\u001b[0m\n")
    }

    def gameTrackSelectScreen(): Unit = {
        // read tracks file (includes fastest lap times info) and choose a track: Array[String]
        println("\u001b[1m\nTRACK SELECT\u001b[0m")
        Thread.sleep(500)
        // Print Tracks.txt for user
        dataToPrint.foreach(println)
        fin2.close()
    }

    def gameWinnerScreen(name: String): Unit = {
        val gg = s"$name Wins" * ((20*42)/(name.length + 5))
        val ggGrouped = gg.grouped(84).toArray
        ggGrouped.foreach(l => {
            print(s"\u001b[32;1m$l\n")
            Thread.sleep(60)
        })
        print(s"\u001b[0m\n")
    }

    def gameResultsScreen(results: Array[String]): Unit = {
        // display results as a string that can be written to to gameHistory file (save and load driver information (name, lap times etc.))
        results.foreach(entry => {
            val split = entry.split(":")
            print(s"\u001b[32;1m${split(0)}\u001b[31;1m:\u001b[0m${split(1)}\n")
            Thread.sleep(30)
        })
        print(s"\u001b[0m\n")
    }

    def gameOverScreen(): Unit = {
        // Visualize a game over screen (default track is 20 x 42)
        val gg = "GAMEOVER" * ((20*42)/8)
        val ggGrouped = gg.grouped(84).toArray
        ggGrouped.foreach(l => {
            print(s"\u001b[31m$l\n")
            Thread.sleep(30)
        })
        print(s"\u001b[0m\n")
    }

    // Load initial game state
    def playSelect(): Unit = {
        val playSelectionRaw = StdIn.readLine("Press 1 to Play\nPress 0 to Exit...\n")
        playSelectionRaw.trim match {
            case "1" => //gameTrackSelectionScreen
            case "0" => {
                gameOverScreen()
                System.exit(0)
            }
            case _ => {
                print(s"\u001B[31mInvalid input '$playSelectionRaw' received...\u001b[0m\n")
                playSelect()
            }
        }
    }

    def carCharSelect(): Char = {
        val carChar = StdIn.readLine(s"Please enter your unique car's single character, e.g. @, & (how it will look on screen)...\n")
        carChar.length match {
            case 1 => carChar(0)
            case _ => {
                print(s"\u001B[31mInvalid input '$carChar' received...\u001b[0m\n")
                carCharSelect()
            }
        }
    }

    def car1CharSelect(car0Char: Char): Char = {
        val sel = carCharSelect()
        if (sel == car0Char) {
            print(s"\u001B[31mInvalid input '$car0Char' matching Player 0's car character received...\u001b[0m\n")
            car1CharSelect(car0Char)
        }
        else sel
    }

    def trackSelect(): Tuple2[String, Array[String]] = {
        val trackChosenRaw = StdIn.readLine("Please select a track by typing in the name of the track, e.g. defaultTrack ...\n")
        val trackChosen = trackChosenRaw.trim.toLowerCase()
        if (!(trackNames contains trackChosen)) {
            print(s"\u001B[31mInvalid input '$trackChosenRaw' received...\u001b[0m\n")
            trackSelect()
        }
        else {
            val track = trackMap(trackChosen)
            // Close Source
            fin1.close()
            (trackChosen, track)
        }
    }

    def lapSelect(): Int = {
        val maxLap = StdIn.readLine("Please enter the number of laps for this game, e.g. 3...\n")
        val res = {
            try (maxLap.toInt)
            catch {
                case _: Throwable => {
                      print(s"\u001B[31mInvalid input '$maxLap' received...\u001b[0m\n")
                      lapSelect()
                }
            }
        }
    res
    }

    def load(): Formula0 = {
        // Game loading
        gameLoadingScreen()
        // Play or Exit
        gamePlaySelectScreen()
        playSelect()
        // Player 0
        val p0Name = StdIn.readLine("Player 0, please enter your name...\n")
        val car0Name = StdIn.readLine("Player 0, please enter your car's name...\n")
        val car0Character = carCharSelect()
        player0 = Player(p0Name, 0)
        car0 = RaceCar(player0, car0Name, car0Character)
        // Player 1
        val p1Name = StdIn.readLine("Player 1, please enter your name...\n")
        val car1Name = StdIn.readLine("Player 1, please enter your car's name...\n")
        val car1Character = car1CharSelect(car0Character)
        player1 = Player(p1Name, 1)
        car1 = RaceCar(player1, car1Name, car1Character)
        // Max Lap Selection
        val maxLap = lapSelect()
        // Track Selection
        gameTrackSelectScreen()
        val trackSelection = trackSelect()
        trackName = trackSelection._1
        track = trackSelection._2
        val trackRows = track.length
        val trackCols = track(0).length
        val wallTile = track(0).toArray.toSet
        val finishLine = '#'

        // Initial Formula 0 Game State
        val initGameState = Formula0(player0, car0, player1, car1,
                            track, wallTile, finishLine, maxLap)

        // Grid coordinates of finish line characters as a set of tuples
        val finLineCoordinatesSet = initGameState.grid.filter({case (k, v) => v == finishLine}).keySet
        val finLineCoordinates = finLineCoordinatesSet.toArray.map(x => Vector2D(x._1, x._2))

        // Set cars on starting line
        initGameState.car0.carPosition = finLineCoordinates(rand.nextInt(finLineCoordinatesSet.size))
        initGameState.car1.carPosition = finLineCoordinates(rand.nextInt(finLineCoordinatesSet.size))
        // coordinates on grid to index on gridView (track as a single string)
        val gridViewLen = initGameState.gridViewLen
        initGameState.car0PosGridView = (gridViewLen - 1) - ((car0.carPosition.x * (track(0).length + 1)) +
                                        (track(0).length - car0.carPosition.y))
        initGameState.car1PosGridView = (gridViewLen - 1) - ((car1.carPosition.x * (track(0).length + 1)) +
                                        (track(0).length - car1.carPosition.y))

        print("\u001b[1mR A C E\u001b[0m\n")
        Thread.sleep(1000)
        initGameState
    }


    // Play game
    def play(): Unit = {
      // Game ID
      val gameID = rand.nextInt
      // Game Date
      val date = calendar.getTime.toString
      var gameState = load()
      while(true) {
          // Print current game state
          print(gameState.toString)
          // Check if there is a winner on even turns
          var isWinner = if (gameState.turn % 2 == 0) gameState.isWinner else Array[String]()
          // Check for a tie or no winner state
          isWinner = if (isWinner.length == 2) {
                        print("\u001b[32mWe have a tie! Tossing coin to determine winner...\u001b[0m\n")
                        Array(isWinner(rand.nextInt(2)))
                     } else isWinner

          // Display winner if one exists
          isWinner match {
            case Array(name) => {
                gameWinnerScreen(name)
                val winnerPlayer = gameState.getPlayerFromName(name)
                val loserPlayer = gameState.getPlayerFromName.filter({case (k, v) => k != name}).values.head
                val results = Array(
                    s"\"id\":$gameID",
                    s"\"date\":$date",
                    s"\"trackName\":$trackName",
                    s"\"winner\":${winnerPlayer.name}",
                    s"\"winnerCarName\":${gameState.getCarFromPlayer(winnerPlayer).name}",
                    s"\"winnerCarCharacter\":${gameState.getCarFromPlayer(winnerPlayer).toString}",
                    s"\"winnerLapTimes\":${winnerPlayer.lapTimes}" ,
                    s"\"winnerBestLapTime\":${winnerPlayer.lapTimes.min}",
                    s"\"winnerWallCrashes\": ${winnerPlayer.wallCrashes}",
                    s"\"winnerCarCollisions\": ${winnerPlayer.carCollisions}",
                    s"\"notWinner\":${loserPlayer.name}",
                    s"\"notWinnerCarName\":${gameState.getCarFromPlayer(loserPlayer).name}",
                    s"\"notWinnerCarCharacter\":${gameState.getCarFromPlayer(loserPlayer).toString}",
                    s"\"notWinnerLapTimes\":${loserPlayer.lapTimes}" ,
                    s"\"notWinnerBestLapTime\":${loserPlayer.lapTimes.min}",
                    s"\"notWinnerWallCrashes\": ${loserPlayer.wallCrashes}",
                    s"\"notWinnerCarCollisions\": ${loserPlayer.carCollisions}",
                    s"\"totalRunTime\": ${winnerPlayer.lapTimes.sum + loserPlayer.lapTimes.sum}")

                // Write game result to gameHistory file
                results.foreach(entry => pwGameHistory.println(entry))
                pwGameHistory.println()
                pwGameHistory.close()

                // Print results on screen
                gameResultsScreen(results)

                // Restart or Exit
                play()
            }
            case Array() => {
                // Play into successor state
                print(s"Turn ${gameState.getTurn}: ${gameState.players(gameState.getCurrentPlayer).toString} playing " +
                      s"${gameState.cars(gameState.getCurrentPlayer).toString}\n" +
                      s"Car status: ${gameState.cars(gameState.getCurrentPlayer).getCarStatus}" +
                      s"Lap status: ${gameState.getLap.toString.slice(3, gameState.getLap.toString.length)}\n")
                gameState = gameState.successorState
            }
          }
      }
    }
}

    // Main Method
    def main(args: Array[String]): Unit = {
        val game = new Game()
        game.play()
    }
}
