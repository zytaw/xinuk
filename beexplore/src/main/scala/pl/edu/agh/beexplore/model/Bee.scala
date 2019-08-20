package pl.edu.agh.beexplore.model

import com.avsystem.commons.MMap
import pl.edu.agh.beexplore.config.BeexploreConfig
import pl.edu.agh.xinuk.model.Energy

final case class Bee(
                      energy: Energy,
                      lifespan: Long,   //lifespan -> maxTripDuration
                      discoveredFlowerPatches: collection.mutable.Map[Id, (Int, Int)]
                    )

object Bee {
  def create()(implicit config: BeexploreConfig):
  Bee = Bee(
    config.foraminiferaStartEnergy,
    config.beeTripDuration,
    MMap.empty[Id, (Int, Int)]
  )
}