package hashcode2018

import java.io.{BufferedWriter, File, FileWriter}

import scala.io.Source

case class Input(vehicleCount: Int, rides: Vector[Ride], bonus: Int, maxSteps: Int) {
  lazy val rideMap = rides.map(r => r.id -> r).toMap
}
case class Ride(id: Int, from: (Int, Int), to: (Int, Int), minStart: Int, maxEnd: Int) {
  val dist = Main.dist(from, to)
}

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

  def writeOutput(output: Output, outputFile: String): Unit = {
    val res = output.vehicleAssigments.map { ass => s"${ass.length} ${ass.mkString(" ")}" }.mkString("\n") + "\n"
    val file = new File(outputFile)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(res)
    bw.close()
  }

  def dist(p1: (Int, Int), p2: (Int, Int)) = {
    math.abs(p1._1 - p2._1) + math.abs(p1._2 - p2._2)
  }

  def evaluate(input: Input, output: Output): Long = {
    evaluate(input, output, { (points, _, _, _) =>
      // println("this doesn't make sense!!!")
      points
    })
  }

  def evaluate(input: Input, output: Output, onDumbness: (Long, Int, (Int, Int), Ride) => Long): Long = {
    output.vehicleAssigments.map { ass => evaluate(input, ass, onDumbness) }.sum
  }

  def evaluate(input: Input, assignment: Seq[Int], onDumbness: (Long, Int, (Int, Int), Ride) => Long): Long = {
    val vehiclePoints = assignment.map(input.rides).foldLeft((0L, 0, (0, 0))) {
      case ((points, t, xy), ride) =>
        val rideStartT = t + dist(xy, ride.from)
        val rideEndT = math.max(ride.minStart, rideStartT) + ride.dist

        // println(s"Vehicle $i: doing ride $ride")

        val newPoints = if(rideStartT > ride.maxEnd - ride.dist) {
          onDumbness(points, t, xy, ride)
        } else {
          val bonus = if(rideStartT <= ride.minStart) input.bonus else 0
          points + bonus + ride.dist
        }

        // println(s"rideDist: $rideDist, rideStartT: $rideStartT, rideEndT: $rideEndT, points: $newPoints")
        (newPoints, rideEndT, ride.to)
    }._1

    // println(s"Vehicle $i achieved $vehiclePoints points")
    vehiclePoints
  }

  def doMagic(input: Input): Output = {
    /*val initial = None // Some(Greedy.go(input))
    val avgDist = input.rides.map(_.dist).sum.toDouble / input.rides.length
    SimulatedAnnealing(input, initial).go(avgDist * 100.0, avgDist * 0.001, 100000)*/
    Simulator.go(input)
  }

  val inputs = List("a_example", "b_should_be_easy", "c_no_hurry", "d_metropolis", "e_high_bonus")

  val totalScore = inputs.map { input =>
    val in = readInput(input)
    val res = doMagic(in)

    val impossibleRides = in.rides.filter({ r => dist((0, 0), r.from) + dist(r.from, r.to) > r.maxEnd }).map(_.id).toSet
    val ridesAssigned = res.vehicleAssigments.flatten.toSet
    val ridesToAssign = in.rides.indices.toSet -- ridesAssigned -- impossibleRides
    val score = evaluate(in, res)

    val averageDistance = in.rides.map(r => dist(r.from, r.to)).sum / in.rides.length

    println("")
    println(s"$input -> $score")
    println(s"assigned   = ${ridesAssigned.size}")
    println(s"impossible = ${impossibleRides.size}")
    println(s"missing    = ${ridesToAssign.size}")
    println(s"avg dist   = $averageDistance")

    writeOutput(res, input + "_output")

    score
  }.sum

  println("")
  println(s"total score = $totalScore")
}
