package hashcode2018

object Greedy {
  case class VehicleState(id: Int, ride: Int, time: Int, score: Int)

  def dist(from: (Int, Int), to: (Int, Int)): Int = math.abs(to._1 - from._1) + math.abs(to._2 - from._2)

  def bestByTime(curr: Option[VehicleState], next: VehicleState): VehicleState =
    curr match {
      case None => next
      case Some(v) if (next.time < v.time) => next
      case Some(v) => v
    }

  def bestByScore(curr: Option[VehicleState], next: VehicleState): VehicleState =
    curr match {
      case None => next
      case Some(v) if (next.score > v.score) => next
      case Some(v) => v
    }

  def bestForRide(ride: Ride, vehicles: Vector[Vector[VehicleState]], ridesMap: Map[Int, Ride], bonus: Int): Option[VehicleState] = {
    vehicles.zipWithIndex.foldLeft[Option[VehicleState]](None) { case (curr, (ass, vId)) =>
      val lastRide = ass.lastOption.map(_.ride)
      val lastT = ass.lastOption.map(_.time).getOrElse(0)
      val lastS = ass.lastOption.map(_.score).getOrElse(0)
      val pos = lastRide.map(ridesMap).map(_.to).getOrElse((0, 0))
      val nextT = lastT + dist(pos, ride.from) + dist(ride.from, ride.to)
      val nextS = lastS + dist(ride.from, ride.to) + (if (nextT == ride.minStart) bonus else 0)
      val next = VehicleState(vId, ride.id, nextT, nextS)
      if (next.time <= ride.maxEnd)
        Some(bestByScore(curr, next))
      else
        curr
    }
  }

  def assignAux(rides: List[Ride], vehicles: Vector[Vector[VehicleState]], ridesMap: Map[Int, Ride], bonus: Int): Vector[Vector[Int]] = rides match {
    case Nil => vehicles.map(_.map(_.ride))
    case r :: other =>
      bestForRide(r, vehicles, ridesMap, bonus) match {
        case Some(v) => assignAux(other, vehicles.updated(v.id, vehicles(v.id) :+ v), ridesMap, bonus)
        case None => assignAux(other, vehicles, ridesMap, bonus)
      }
  }

  def assign(rides: List[Ride], nVehicles: Int, ridesMap: Map[Int, Ride], bonus: Int): Vector[Vector[Int]] =
    assignAux(rides, (0 until nVehicles).map(_ => Vector[VehicleState]()).toVector, ridesMap, bonus: Int)

  def go(input: Input): Output = {
    val rides = input.rides.sortBy(_.minStart).toList
    val ridesMap = input.rides.map(r => r.id -> r).toMap
    Output(assign(rides, input.vehicleCount, ridesMap, input.bonus))
  }
}
