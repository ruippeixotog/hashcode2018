package hashcode2018

import scala.collection.mutable

object Simulator {
  case class RideState(ride: Ride, score: Long, distTo: Long, timeTo: Long, inBonus: Boolean, totalDist: Long, nSkips: Int)

  def dist(from: (Int, Int), to: (Int, Int)): Int = math.abs(to._1 - from._1) + math.abs(to._2 - from._2)

  implicit val ord = Ordering.by[State, Int](-_.time)
  sealed trait State { def time: Int }
  case class Free(time: Int, vehicleId: Int) extends State

  sealed trait RiderType
  case object Regular extends RiderType
  case object FastFinisher extends RiderType

  def getBestRide(riderType: RiderType, availableRides: Vector[RideState]): Ride = {
    riderType match {
      case Regular =>
        val skips = availableRides.filter(_.nSkips > 50)
        val haveBonus = availableRides.filter(_.inBonus)
        val ridesToConsider =
          if (skips.length > 0) skips
          else if (haveBonus.length > 0) haveBonus
          else availableRides
        ridesToConsider.minBy(v => (v.timeTo, -v.totalDist)).ride
      case FastFinisher =>
        val haveBonus = availableRides.filter(_.inBonus)
        val ridesToConsider = if (haveBonus.length > 0) haveBonus else availableRides
        ridesToConsider.minBy(v => (v.timeTo + v.totalDist)).ride
    }
  }

  def go(input: Input): Output = {
    var assignments = (0 until input.vehicleCount).map(_ => Vector[Int]()).toVector
    val positions = Array.fill(input.vehicleCount)((0, 0))
    val vehicleTypes: Array[RiderType] = Array.fill(input.vehicleCount)(Regular)
    val assignedRides = mutable.Set[Int]()
    val pq = mutable.PriorityQueue[State]()
    (0 until input.vehicleCount).foreach { i => pq.enqueue(Free(0, i)) }

    val ridesSkip = mutable.Map[Int, Int]().withDefaultValue(0)

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
              Some(RideState(ride, score, dist(positions(vId), ride.from), startT, startT == ride.minStart, dist(ride.from, ride.to), ridesSkip(ride.id)))
            }
          }
          if (availableRides.length > 0) {
            val bestRide = getBestRide(vehicleTypes(vId), availableRides)
            availableRides.foreach { r =>
              ridesSkip(r.ride.id) = ridesSkip(r.ride.id) + 1
            }
            ridesSkip(bestRide.id) = ridesSkip(bestRide.id) - 1
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
