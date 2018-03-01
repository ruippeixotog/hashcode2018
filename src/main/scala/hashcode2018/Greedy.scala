package hashcode2018

object Greedy {
  def dist(from: (Int, Int), to: (Int, Int)): Int = math.abs(to._1 - from._1) + math.abs(to._2 - from._2)

  def bestForRide(ride: Ride, vehicles: Vector[Vector[(Int, Int)]], ridesMap: Map[Int, Ride]): Option[(Int, Int)] = {
    vehicles.zipWithIndex.foldLeft[Option[(Int, Int)]](None) { case (curr, (ass, vId)) =>
      val lastRide = ass.lastOption.map(_._1)
      val lastT = ass.lastOption.map(_._2).getOrElse(0)
      val pos = lastRide.map(ridesMap).map(_.to).getOrElse((0, 0))
      val d = lastT + dist(pos, ride.from) + dist(ride.from, ride.to)
      if (d <= ride.maxEnd) {
        curr match {
          case Some((_, cd)) =>
            if (d < cd)
              Some(vId -> d)
            else
              curr
          case None =>
            Some(vId -> d)
        }
      } else
        curr
    }
  }

  def assign(rides: List[Ride], vehicles: Vector[Vector[(Int, Int)]], ridesMap: Map[Int, Ride]): Vector[Vector[Int]] = rides match {
    case Nil => vehicles.map(_.map(_._1))
    case r :: other =>
      bestForRide(r, vehicles, ridesMap) match {
        case Some((i, nt)) => assign(other, vehicles.updated(i, vehicles(i) :+ (r.id -> nt)), ridesMap)
        case None => assign(other, vehicles, ridesMap)
      }
  }

  def go(input: Input): Output = {
    val rides = input.rides.sortBy(_.minStart).toList
    val ridesMap = input.rides.map(r => r.id -> r).toMap
    val vehicles = (0 until input.vehicleCount).map(_ => Vector[(Int, Int)]()).toVector
    Output(assign(rides, vehicles, ridesMap))
  }
}
