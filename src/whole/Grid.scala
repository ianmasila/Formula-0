package whole

/**
 * This class models a race track.
 */

import scala.collection._
import scala.io.StdIn

case class Grid() {
  /**
    Represents a grid on which racing happens.
    
    // General information
    The grid is an n x n square represented as a list of strings, with each character representing a grid cell.
    We approach the grid as a state space transition system with each grid cell (a pair of row and column) being a state in the grid system.    
    Valid states are those tiles that are not wall tiles.

    // Transitions/ Movement
    There are four basic move actions: going north (decrease row index), south 
    (increase row index), east (increase column index), or west (decrease column
    index). The fifth possible action is to remain in place. 
    
    An action is applicable if the move does not end up in a state (grid location)
    with a wall tile.

    */
  /**
   * Representing the grid as an internal map of coordinate tuples (row, column) and the associated character from a 2-dimensional array of characters.
   * Initialised as an empty map
   */

  var grid = scala.collection.mutable.Map[Tuple2[Int, Int], Char]()
  var gridTrack = Array[String]()
  var wallTile = Set('X')
  var startingLine = '#'
  var gridDim = (0, 0)
  // Valid states (row, column) pairs where racing is possible
  var states = scala.collection.mutable.Map[Tuple2[Int, Int], Char]()
  // Decorations for screens
  val backColours: LazyList[String] = (LazyList continually Array("\u001b[47;1m", "\u001b[43;1m", "\u001b[46;1m", "\u001b[42;1m", "\u001b[41;1m", "\u001b[44;1m")).flatten

  // set the grid given a track layout and a set of wall tiles
  def setGrid(track: Array[String], wall_tile: Set[Char] = Set('X'), starting_line: Char = '#'): Unit = {
    /*
        Create a grid given some track, wall tile, and a start/finish line.
        
        Parameters
        ----------
        track: Array[String]
           Each string denotes a row, and each character in the string represents the 
           tile on the corresponding column
        wall_tiles : set of characters for invalid states
           Any character in this set denotes a wall tile. Default is 'X'
        starting_line : set of characters for the start/finish line
           Any character in this set denotes the start/finish line. Default is '#'
        
        */
    // fill the empty grid with the given layout
    val n_rows = track.length
    val n_cols = track(0).length
    for (r <- Range(0, n_rows)) {
      for (c <- Range(0, n_cols)) {
        grid((r, c)) = track(n_rows - 1 - r)(c)  // this ensures the bottom left corner of the track is (0,0) on the grid
      }
    }
    // update grid dimensions
    gridDim = (n_rows, n_cols)
    // update track
    gridTrack = track
    // update wallTile
    wallTile = wall_tile
    // update startingLine
    startingLine = starting_line
    // update states
    states = grid.filter({ case (k, v) => !(wallTile.contains(v))})
  }
    
  // Decorative screens

  def gameCrashScreen(crashChar: Char): Unit = {
       // Visualize a crash on screen
      val gg = crashChar.toString * (gridDim._1 * gridDim._2)
      val ggGrouped = gg.grouped(gridDim._2).toArray
      val newLines = Array.fill[String](ggGrouped.length)("\n")
      val oldView = (ggGrouped zip backColours).map(x => x._2 + x._1 + "\n").mkString
      ggGrouped.foreach(l => {
          print(s"\u001b[31m$l\n")
          Thread.sleep(60)
      })
      print(s"\u001b[0m\n")
  }

  override def toString: String = {
    // Visualize the track as a grid

    gridTrack.mkString("\n") + "\n"
//    for (r <- Range(0, gridDim._1)) {
//      view = view + gridTrack(r) + "\n"
//    }
//    view
  }
}
