package com.maze.path.shortest

import java.io.{FileNotFoundException, IOException}

import scala.collection.mutable.{ListBuffer, Queue}
import com.typesafe.scalalogging.Logger

import scala.io.Source
import scala.util.control.Breaks

/**
  * Finding the shortest path of maze start to end using Lee Algorithm(BFS)
  * Simply A-Maze-ing!
  * Author - BK_Vasanth (bkvasanth@msn.com)
  * References - http://users.eecs.northwestern.edu/~haizhou/357/lec6.pdf
  */
object BFSMazeSolution {
  val logger = Logger("BFSMazeSolution")

  case class Combo(cellValue: Int, featureCode: Int) //*** Combo representions combined output - cell value and featurecode

  implicit def extractValue(combo: Combo) = {
    if ((combo.cellValue & combo.featureCode) == 0) false else true
  }

  case class Node(coordinates: (Int, Int), parentList: Option[List[Node]], value: Int)

  case class Maze(start: (Int, Int), end: (Int, Int), data: Array[Array[Node]])

  val UP = 1;
  val RIGHT = 2;
  val DOWN = 4;
  val LEFT = 8;
  val START = 16;
  val END = 32;
  val MINE = 64
  val directionMap = Map(UP -> (-1, 0), RIGHT -> (0, 1), DOWN -> (1, 0), LEFT -> (0, -1))

  //will move it to args in future
  val filename = "mazes.txt"
  val lifelines = 3

  val mazeHeightWidthPattern = "[(]([0-9]+),([0-9]+)[)]".r

  /**
    * To retrieve the maze with start and end coordinates
    *
    * @param rowSize
    * @param colSize
    * @param mazeInpArr
    * @return the Maze instance, which contains start, end and Matrix of Node instances
    */
  def retrieveMaze(rowSize: Int, colSize: Int, mazeInpArr: Array[String]): Maze = {
    val nodeArray = Array.ofDim[Node](rowSize, colSize) // Two Dimensional Array
    var counter: Int = -1
    var start: Option[(Int, Int)] = None;
    var end: Option[(Int, Int)] = None;

    for {
      i <- 0 to rowSize - 1
      j <- 0 to colSize - 1
    } {
      counter = counter + 1;
      val node = Node((i, j), None, mazeInpArr(counter).toInt)

      if (!start.isDefined && Combo(mazeInpArr(counter).toInt, START)) start = Some((i, j))
      if (!end.isDefined && Combo(mazeInpArr(counter).toInt, END)) end = Some((i, j))

      nodeArray(i)(j) = node
    }

    Maze(start.get, end.get, nodeArray)
  }

  /**
    * finds the shortest path from the source to destination.
    *
    * @param maze
    * @return
    */
  def shortestPath(maze: Maze): List[(Int, Int)] = {
    val queue = new Queue[Node]
    val finalPath = ListBuffer[Node]()
    val output = ListBuffer[(Int, Int)]()

    val mazeData = maze.data
    val visited: Array[Array[Boolean]] = Array.ofDim[Boolean](mazeData.length, mazeData(0).length)


    queue.enqueue(mazeData(maze.start._1)(maze.start._2)) //Add the start node to the queue
    visited(maze.start._1)(maze.start._2) = true

    val solutionBreak = new Breaks
    val interimBreak = new Breaks

    solutionBreak.breakable { //will get used, When the solution found

      while (!queue.isEmpty) { // Run till queue is not empty
        interimBreak.breakable { //will be used, when there is no way to move forward in this path

          val node = queue.dequeue()
          val pList: List[Node] = (node.parentList.getOrElse(List[Node]()))
          val parentList = ListBuffer[Node]().++=(pList)

          if (Combo(node.value, MINE) && getCurrentMinesInPath(parentList) == lifelines - 1) interimBreak.break // when the lifelines are maxed out

          if (compareTuples(node.coordinates, maze.end)) {
            parentList += node //end node to be added in the parent list
            finalPath ++= parentList //move the parent list to output
            solutionBreak.break //*** found the right path !!! YAY ;)
          }

          var isParentAdded = false

          directionMap.map { direction => // iterate all 4 possible directions - UP, LEFT, RIGHT, DOWN
            if (isAllowedToTravel(mazeData, visited, node.coordinates, direction._1)) { //node.coordinates, (node.coordinates._1 + keyVal._2._1, node.coordinates._2 + keyVal._2._2)
              if (!isParentAdded) {
                parentList.+=(node)
                isParentAdded = true
              }
              val neighbourRow = node.coordinates._1 + direction._2._1
              val neighborCol = node.coordinates._2 + direction._2._2

              visited(neighbourRow)(neighborCol) = true
              queue.enqueue(Node((neighbourRow, neighborCol), Some(parentList.toList), (mazeData(neighbourRow)(neighborCol)).value))
            }
          }
        }
      }
    }

    if (finalPath.size > 0) {
      finalPath.map(parentNode => output.+=(parentNode.coordinates))
    } else {
      logger.info("Destination can't be reached from source !!")
    }

    output.toList
  }

  def getCurrentMinesInPath(parentList: ListBuffer[Node]) = {
    parentList.map(node => if (Combo(node.value, MINE)) 1 else 0).reduce(_ + _)
  }

  /**
    *
    * @param md        - input maze data
    * @param visited   - visited node array
    * @param currentRC - coordinates of current Row and Column
    * @param direction - UP = 1; RIGHT = 2; DOWN = 4; LEFT = 8;
    * @return whether able to travel to neighbour node from the current node
    */
  def isAllowedToTravel(md: Array[Array[Node]], visited: Array[Array[Boolean]], currentRC: (Int, Int), direction: Int): Boolean = {
    val nextCoordinates = directionMap(direction)
    val nextRowCol = ((currentRC._1 + nextCoordinates._1), (currentRC._2 + nextCoordinates._2)) // Find co-ordinates of neighbor
    val currentNode = md(currentRC._1)(currentRC._2)
    val isMoveAllowed: Boolean = Combo(currentNode.value, direction)

    if (isMoveAllowed && (nextRowCol._1 >= 0) && (nextRowCol._1 < md.length) && //Current Row position greater than 0 and lesser than the length
      (nextRowCol._2 >= 0) && (nextRowCol._2 < md(0).length) && //Current Column position greater than 0 and lesser than the length
      !visited(nextRowCol._1)(nextRowCol._2)) {
      true
    } else {
      false
    }
  }

  /**
    * To find the directions and generate formatted ouput from the list of path(x,y) from source to destination
    *
    * @param outputPath
    * @return the directions from start coordinates to end coordinate :)
    */
  def generateFormattedOutput(outputPath: List[(Int, Int)]): String = {
    var prevNode = (-1, -1)
    // initialize with start node
    val directionList = ListBuffer.empty[String]
    for (i <- outputPath) {
      if (compareTuples(prevNode, (-1, -1))) { //If it is not a start node
        prevNode = i
      } else {
        directionList.+=(findDirection((i._1 - prevNode._1, i._2 - prevNode._2)))
        prevNode = i
      }
    }

    new StringBuilder().append("['").append(directionList.toList.mkString("', '")).append("']").toString()
  }

  /**
    * helps to find direction(up, right, down, left) from the coordinate
    *
    * @param coordinateDiff -
    * @return
    */
  def findDirection(coordinateDiff: (Int, Int)): String = {
    val directionMap = Map("up" -> (-1, 0), "right" -> (0, 1), "down" -> (1, 0), "left" -> (0, -1))
    var output = ""
    directionMap.map { elem =>
      if (compareTuples(coordinateDiff, elem._2)) {
        output = elem._1
      }
    }
    output
  }

  /**
    * jus to compare the tuples without any ordering hassle
    * @param i - tuple1
    * @param j - tuple2
    * @return - true if both the tuples contains the same value
    */
  def compareTuples(i: (Int, Int), j: (Int, Int)): Boolean = implicitly[Ordering[Tuple2[Int, Int]]].compare(i, j) == 0

  def main(args:Array[String]) = {
    try {
      for (line <- Source.fromResource(filename).getLines()) {
        val arrInfoData = line.split("-")
        val mazeHeightWidthPattern(rows, cols) = arrInfoData(0)
        val mazeInpArr = arrInfoData(1).substring(1, arrInfoData(1).length()-1).split(",")
        val mazeData = retrieveMaze(rows.toInt, cols.toInt, mazeInpArr)
        //Find the shortest path for the maze data
        val outputPath = shortestPath(mazeData)
        val destinationDirections = generateFormattedOutput(outputPath)
        logger.info(destinationDirections)
      }
    } catch {
      case ex: FileNotFoundException => logger.debug("File not found exception", ex)
      case ex: IOException => logger.debug("Got an IOException!", ex)
      case ex: IndexOutOfBoundsException => logger.debug("Got an IndexOutOfBoundsException!", ex)
      case ex: Exception => logger.debug("Got an Exception !", ex)
    }
  }
}
