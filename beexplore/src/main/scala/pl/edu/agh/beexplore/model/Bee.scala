package pl.edu.agh.beexplore.model

import com.avsystem.commons.MMap
import pl.edu.agh.beexplore.config.BeexploreConfig
import pl.edu.agh.xinuk.model.Energy

import scala.util.Random

final case class Bee(
                      tripNumber: Int,
                      maxTripDuration: Long, //lifespan -> maxTripDuration
                      discoveredFlowerPatches: collection.mutable.Map[Id, (Int, Int)],
                      destination: (Int, Int),
                      vectorFromColony: (Int, Int),
                      lastMoveVector: (Int, Int)
                    )

object Bee {
//  private val random = new Random(System.nanoTime())
  def create()(implicit config: BeexploreConfig):
  Bee = Bee(
    1,
    config.beeTripDuration,
    MMap.empty[Id, (Int, Int)],
    (Int.MinValue, Int.MinValue), // not real destination - bee can freely fly
    (0, 0),
    (0, 0)
  )
}