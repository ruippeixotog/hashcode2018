package hashcode2018

object Greedy {
  case class VehicleState(ride: Int, distance: Int, score: Int)

  def dist(from: (Int, Int), to: (Int, Int)): Int = math.abs(to._1 - from._1) + math.abs(to._2 - from._2)

  def bestForRide(ride: Ride, vehicles: Vector[Vector[VehicleState]], ridesMap: Map[Int, Ride]): Option[(Int, VehicleState)] = {
    vehicles.zipWithIndex.foldLeft[Option[(Int, VehicleState)]](None) { case (curr, (ass, vId)) =>
      val lastRide = ass.lastOption.map(_.ride)
      val lastT = ass.lastOption.map(_.distance).getOrElse(0)
      val pos = lastRide.map(ridesMap).map(_.to).getOrElse((0, 0))
      val d = lastT + dist(pos, ride.from) + dist(ride.from, ride.to)
      if (d <= ride.maxEnd) {
        curr match {
          case Some((_, vs)) =>
            if (d < vs.distance)
              Some(vId -> VehicleState(ride.id, d, 0))
            else
              curr
          case None =>
            Some(vId -> VehicleState(ride.id, d, 0))
        }
      } else
        curr
    }
  }

  def assignAux(rides: List[Ride], vehicles: Vector[Vector[VehicleState]], ridesMap: Map[Int, Ride]): Vector[Vector[Int]] = rides match {
    case Nil => vehicles.map(_.map(_.ride))
    case r :: other =>
      bestForRide(r, vehicles, ridesMap) match {
        case Some((i, v)) => assignAux(other, vehicles.updated(i, vehicles(i) :+ v), ridesMap)
        case None => assignAux(other, vehicles, ridesMap)
      }
  }

  def assign(rides: List[Ride], nVehicles: Int, ridesMap: Map[Int, Ride]): Vector[Vector[Int]] =
    assignAux(rides, (0 until nVehicles).map(_ => Vector[VehicleState]()).toVector, ridesMap)

  def go(input: Input): Output = {
    val rides = input.rides.sortBy(_.minStart).toList
    val ridesMap = input.rides.map(r => r.id -> r).toMap
    Output(assign(rides, input.vehicleCount, ridesMap))
  }
}
