package hashcode2018

import scala.util.Random

case class SimulatedAnnealing(input: Input, initalSol: Option[Output] = None) {

  val globalStartTime: Double = System.currentTimeMillis()

  def toOutput(sol: Array[Int]): Output = {
    val mapping = sol.zipWithIndex
      .groupBy(_._1)
      .mapValues(_.map(_._2).sortBy(input.rides(_).minStart).toVector)
      .withDefaultValue(Vector.empty)

    Output(Vector.tabulate(input.vehicleCount)(mapping))
  }

  def toOutput(sol: Array[Int], vehicleFilter: Set[Int], rideOverride: Map[Int, Int]): Output = {
    val mapping = sol.zipWithIndex
      .collect { case (vehicle, ride) if vehicleFilter(vehicle) => (rideOverride.getOrElse(ride, vehicle), ride) }
      .groupBy(_._1)
      .mapValues(_.map(_._2).sortBy(input.rides(_).minStart).toVector)
      .withDefaultValue(Vector.empty)

    Output(Vector.tabulate(input.vehicleCount)(mapping))
  }

  case class Mutation(sol: Array[Int]) {
    val ride = Random.nextInt(sol.length)
    def oldVehicle = sol(ride)
    val newVehicle = Random.nextInt(input.vehicleCount + 1) - 1

    val oldScore = Main.evaluate(input, toOutput(sol, Set(oldVehicle, newVehicle), Map.empty))
    val newScore = Main.evaluate(input, toOutput(sol, Set(oldVehicle, newVehicle), Map(ride -> newVehicle)))
    val delta = newScore - oldScore

    def apply(): Unit = sol(ride) = newVehicle
  }

  def go(maxT: Double, minT: Double, finishAtTime: Double): Output = {

    // sync means calculating current temperature from current work time
    val ITERS_PER_SYNC = 16       // must be power of two
    // sometimes we dump current progress to stderr
    val ITERS_PER_DUMP = 0x40000  // must be power of two

    // number of iterations done so far
    var iters = 0
    // part of the full cooling schedule passed
    var done = 0.0
    // best solution so far
    var best: Array[Int] = initalSol match {
      case None => input.rides.map(_ => Random.nextInt(input.vehicleCount)).toArray
      case Some(out) => input.rides.indices.map { i => out.vehicleAssigments.indexWhere(_.contains(i)) }.toArray
    }
    var bestPoints = Main.evaluate(input, toOutput(best))

    // number of accepted mutations overall
    var accMuts = 0

    // time when the SA begins
    val startTime = System.currentTimeMillis()

    // time dedicated for SA processing
    val hasTime = finishAtTime - (startTime - globalStartTime)

    var currSol = best.clone()
    var currSolPoints = bestPoints

    def loop(): Boolean = {
      // dump stats so that you can watch the progress of SA
      if ((iters & (ITERS_PER_DUMP - 1)) == 0)
        printf("Iteration:%6d  Acc:%6d  Temp:%7.3f  Score:%1.5f\n",
          iters, accMuts, maxT * math.pow(minT / maxT, done),
          currSolPoints.toDouble)

      // synchronize the temperature with time
      if ((iters & (ITERS_PER_SYNC - 1)) == 0) {
        done = (System.currentTimeMillis() - startTime) / hasTime
        if (done >= 1.0) return true
      }

      // create mutation for current solution
      val mut = Mutation(currSol)

      //if mutated solution is better, accept it
      var move = false
      if (mut.delta >= 0) move = true
      else {
        // otherwise calculate current temperature
        val temp = maxT * math.pow(minT / maxT, done)
        // and accept with the tricky probability
        move = Random.nextDouble() < math.exp(mut.delta / temp)
      }

      // if mutation is accepted, apply it to the solution
      if (move) { accMuts += 1; mut.apply(); currSolPoints += mut.delta }

      // do not forget to store the best solution
      if (currSolPoints > bestPoints) { best = currSol.clone(); bestPoints = currSolPoints }
      iters += 1
      false
    }

    while(!loop()) {}

    //return the best solution as the result
    printf("Simulated annealing made %d iterations (accepted: %d)\n", iters, accMuts)
    printf("Accepted score: %d\n", bestPoints)

    assert(Main.evaluate(input, toOutput(best)) == bestPoints,
      s"${Main.evaluate(input, toOutput(best))} != $bestPoints")

    toOutput(best)
  }
}
