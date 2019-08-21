package pl.edu.agh.beexplore.model

import com.avsystem.commons.MMap
import pl.edu.agh.beexplore.config.BeexploreConfig
import pl.edu.agh.xinuk.model.Energy

import scala.util.Random

final case class Bee(
                      energy: Energy,
                      maxTripDuration: Long, //lifespan -> maxTripDuration
                      discoveredFlowerPatches: collection.mutable.Map[Id, (Int, Int)],
                      destination: (Int, Int)
                    )

object Bee {
//  private val random = new Random(System.nanoTime())
  def create()(implicit config: BeexploreConfig):
  Bee = Bee(
    config.foraminiferaStartEnergy,
    config.beeTripDuration,
    MMap.empty[Id, (Int, Int)],
//    (random.nextInt(config.gridSize) - 1, random.nextInt(config.gridSize) - 1)
    (-1, -1)
  )
}