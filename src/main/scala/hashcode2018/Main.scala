package hashcode2018

import java.io.{BufferedWriter, File, FileWriter}

import scala.io.Source

case class Input(vehicleCount: Int, rides: Vector[Ride], bonus: Int, maxSteps: Int)
case class Ride(id: Int, from: (Int, Int), to: (Int, Int), minStart: Int, maxEnd: Int)

case class Output(vehicleAssigments: Vector[Vector[Int]])

object Main extends App {

  def readInput(inputName: String): Input = {
    val headerLine :: rideLines = Source.fromFile(s"inputs/$inputName.in").getLines().toList
    val _ :: _ :: vehicleCount :: _ :: bonus :: maxSteps :: Nil = headerLine.split(" ").toList.map(_.toInt)

    val rides = rideLines.map(_.split(" ").toList.map(_.toInt)).zipWithIndex.map {
      case (fromX :: fromY :: toX :: toY :: minStart :: maxEnd :: Nil, id) =>
        Ride(id, (fromX, fromY), (toX, toY), minStart, maxEnd)
    }.toVector

    Input(vehicleCount, rides, bonus, maxSteps)
  }

  def writeOutput(output: Output): Unit = {
    val res = output.vehicleAssigments.map { ass => s"${ass.length} ${ass.mkString(" ")}" }.mkString("\n") + "\n"
    val file = new File("output")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(res)
    bw.close()
  }

  def doMagic(input: Input): Output = {
    Greedy.go(input)
  }

  writeOutput(doMagic(readInput("a_example")))
}
