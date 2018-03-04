package hashcode2018

import scala.collection.mutable

object Simulator {
  case class RideState(ride: Ride, score: Long, distTo: Long, timeTo: Long, inBonus: Boolean, totalDist: Long)

  def dist(from: (Int, Int), to: (Int, Int)): Int = math.abs(to._1 - from._1) + math.abs(to._2 - from._2)

  implicit val ord = Ordering.by[State, Int](-_.time)
  sealed trait State { def time: Int }
  case class Free(time: Int, vehicleId: Int) extends State

  sealed trait RiderType
  case object LongDistanceRunner extends RiderType
  case object BonusCatcher extends RiderType
  case object Regular extends RiderType
  case object ShortDistanceRunner extends RiderType

  def getBestRide(riderType: RiderType, availableRides: Vector[RideState]): Ride = {
    riderType match {
      case Regular =>
        availableRides.minBy(_.timeTo).ride
      case LongDistanceRunner =>
        availableRides.maxBy(v => (v.totalDist, -v.timeTo)).ride
      case BonusCatcher =>
        val haveBonus = availableRides.filter(_.inBonus)
        if (haveBonus.length > 0) {
          haveBonus.minBy(_.timeTo).ride
        } else
          availableRides.minBy(_.timeTo).ride
      case ShortDistanceRunner =>
        availableRides.minBy(_.totalDist).ride
    }
  }

  def go(input: Input): Output = {
    var assignments = (0 until input.vehicleCount).map(_ => Vector[Int]()).toVector
    val positions = Array.fill(input.vehicleCount)((0, 0))
    val vehicleTypes: Array[RiderType] = Array.fill(input.vehicleCount)(BonusCatcher)
    val assignedRides = mutable.Set[Int]()
    val pq = mutable.PriorityQueue[State]()
    (0 until input.vehicleCount).foreach { i => pq.enqueue(Free(0, i)) }

    /*if (input.vehicleCount >= 10) {
      val distRunners = (0.1 * input.vehicleCount).toInt
      val bonusCatchers = (0.1 * input.vehicleCount).toInt
      val shortDistanceRunners = (0.1 * input.vehicleCount).toInt
      (0 until distRunners).foreach { i => vehicleTypes(i) = LongDistanceRunner }
      (distRunners until (distRunners + bonusCatchers)).foreach { i => vehicleTypes(i) = BonusCatcher }
      ((distRunners + bonusCatchers) until (distRunners + bonusCatchers + shortDistanceRunners)).foreach {
        i => vehicleTypes(i) = ShortDistanceRunner
      }
    }*/

    while (pq.nonEmpty) {
      val event = pq.dequeue()
      event match {
        case Free(t, vId) =>
          val availableRides = input.rides.filter(r => !assignedRides.contains(r.id)).flatMap { ride =>
            val startT = math.max(t + dist(positions(vId), ride.from), ride.minStart)
            val endT = startT + dist(ride.from, ride.to)
            if (endT > ride.maxEnd)
              None
            else {
              val score = dist(ride.from, ride.to) + (if (startT == ride.minStart) input.bonus else 0)
              Some(RideState(ride, score, dist(positions(vId), ride.from), startT, startT == ride.minStart, dist(ride.from, ride.to)))
            }
          }
          if (availableRides.length > 0) {
            val bestRide = getBestRide(vehicleTypes(vId), availableRides)
            val nextT = math.max(t + dist(positions(vId), bestRide.from), bestRide.minStart) + dist(bestRide.from, bestRide.to)
            val nextPos = bestRide.to
            positions(vId) = nextPos
            assignments = assignments.updated(vId, assignments(vId) :+ bestRide.id)
            assignedRides += bestRide.id
            pq.enqueue(Free(nextT, vId))
          }
      }
    }

    Output(assignments)
  }
}
