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
      case _ => throw new Exception("illegal input")
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

  def dist(p1: (Int, Int), p2: (Int, Int)) = {
    math.abs(p1._1 - p2._1) + math.abs(p1._2 - p2._2)
  }

  def evaluate(input: Input, output: Output): Long = {
    evaluate(input, output, { (points, _, _, _) =>
      println("this doesn't make sense!!!")
      points
    })
  }

  def evaluate(input: Input, output: Output, onDumbness: (Long, Int, (Int, Int), Ride) => Long): Long = {
    output.vehicleAssigments.zipWithIndex.map { case (ass, i) =>
      val vehiclePoints = ass.map(input.rides).foldLeft((0L, 0, (0, 0))) {
        case ((points, t, xy), ride) =>
          val rideDist = dist(ride.from, ride.to)
          val rideStartT = t + dist(xy, ride.from)
          val rideEndT = math.max(ride.minStart, rideStartT) + rideDist

          // println(s"Vehicle $i: doing ride $ride")

          val newPoints = if(rideStartT > ride.maxEnd - rideDist) {
            onDumbness(points, t, xy, ride)
          } else {
            val bonus = if(rideStartT <= ride.minStart) input.bonus else 0
            points + bonus + rideDist
          }

          // println(s"rideDist: $rideDist, rideStartT: $rideStartT, rideEndT: $rideEndT, points: $newPoints")
          (newPoints, rideEndT, ride.to)
      }._1

      // println(s"Vehicle $i achieved $vehiclePoints points")
      vehiclePoints

    }.sum
  }

  def doMagic(input: Input): Output = {
    println(input)
    println(evaluate(input, Output(Vector(
      Vector(0),
      Vector(2, 1)))))

    Greedy.go(input)
  }

  writeOutput(doMagic(readInput("a_example")))
}
