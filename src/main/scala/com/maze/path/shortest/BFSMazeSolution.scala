package com.maze.path.shortest
import java.io.{FileNotFoundException, IOException}

import com.typesafe.scalalogging.Logger

import scala.io.Source

/**
  * Finding the shortest path of maze start to end using Breadth First Search Algorithm
  * Author - BK_Vasanth (bkvasanth@msn.com)
  */
object BFSMazeSolution {
  val logger = Logger("BFSMazeSolution")

  case class Node(moveUp:Boolean, moveRight:Boolean, moveDown:Boolean, moveLeft: Boolean, isStart:Boolean, isEnd: Boolean, isMine:Boolean)
  case class Combo(cellValue: String, featureCode: Int)
  case class Maze(start:(Int,Int), end:(Int,Int), data: Array[Array[Node]])

  implicit def extractValue(combo: Combo) = if((combo.cellValue.toInt & combo.featureCode) == 0) false else true

  val UP = 1; val RIGHT = 2; val DOWN = 4; val LEFT = 8; val START = 16; val END = 32; val MINE = 64

  //todo - move to argument
  val filename = "/Users/m_215670/IdeaProjects/MazeFun/src/main/resources/mazes.txt"
  val mazeHeightWidthPattern = "[(]([0-9]+),([0-9]+)[)]".r

  def retrieveMaze(rowSize: Int, colSize: Int, mazeData: String): Maze = {
    val mazeInpArr = mazeData.substring(1, mazeData.length()-1).split(",") //One-Dimensional Array
    val mazeOutArray = Array.ofDim[Node](rowSize, colSize) // Two Dimensional Array
    var counter:Int = -1
    var start = (0,0); var end = (rowSize-1,colSize-1);

    for {
      i <- 0 to rowSize-1
      j <- 0 to colSize-1
    } {
      counter=counter+1;
      val node = Node(Combo(mazeInpArr(counter), UP),
         Combo(mazeInpArr(counter), RIGHT), Combo(mazeInpArr(counter), DOWN),
         Combo(mazeInpArr(counter), LEFT), Combo(mazeInpArr(counter), START),
         Combo(mazeInpArr(counter), END), Combo(mazeInpArr(counter), MINE)
      )
      if(node.isStart) start = (i,j)
      if(node.isEnd) end = (i,j)
      mazeOutArray(i)(j) = node
    }

    Maze(start, end, mazeOutArray)
    }

  def main(args:Array[String]) = {
    try {
      for (line <- Source.fromFile(filename).getLines()) {
        val arrInfoData = line.split("-")
        val mazeHeightWidthPattern(rows, cols) = arrInfoData(0)

        val mazeData = retrieveMaze(rows.toInt, cols.toInt, arrInfoData(1))

        logger.debug(rows + ", " + cols)
        logger.debug(arrInfoData(1))
        logger.info(mazeData.toString)
      }
    } catch {
      case ex: FileNotFoundException => logger.debug("File not found exception", ex)
      case ex: IOException => logger.debug("Got an IOException!", ex)
      case ex: IndexOutOfBoundsException => logger.debug("Got an IndexOutOfBoundsException!", ex)
    }
  }
}
