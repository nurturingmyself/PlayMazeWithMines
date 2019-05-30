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

  //todo - move to argument
  val filename = "C:\\Git_Projects\\PlayMazeWithMines\\src\\main\\resources\\mazes.txt"
  val mazeHeightWidthPattern = "[(]([0-9]+),([0-9]+)[)]".r

  def loadMazeData(rowSize: Int, colSize: Int, mazeData: String):Array[Array[Int]] = {
    val mazeMultiDimArr = Array.ofDim[Int](rowSize, colSize)
    val mazeDataArr = mazeData.substring(1, mazeData.length()-1).split(",")
    var counter:Int = -1

    for {

      i <- 0 to rowSize-1
      j <- 0 to colSize-1
    }{counter=counter+1; mazeMultiDimArr(i)(j) = mazeDataArr(counter).toInt}

    mazeMultiDimArr
    }

  def main(args:Array[String]) = {
    try {
      for (line <- Source.fromFile(filename).getLines()) {
        val arrInfoData = line.split("-")
        val mazeHeightWidthPattern(rows, cols) = arrInfoData(0)

        val multiDimMazeData = loadMazeData(rows.toInt, cols.toInt, arrInfoData(1))
        logger.debug(rows + ", " + cols)
        logger.debug(arrInfoData(1))
        logger.info(multiDimMazeData.toString)
      }
    } catch {
      case ex: FileNotFoundException => logger.debug("File not found exception", ex)
      case ex: IOException => logger.debug("Got an IOException!", ex)
      case ex: IndexOutOfBoundsException => logger.debug("Got an IndexOutOfBoundsException!", ex)
    }
  }
}
