package pl.edu.agh.beexplore.model

import pl.edu.agh.beexplore.config.BeexploreConfig
import pl.edu.agh.xinuk.model.Energy

final case class Bee(energy: Energy, lifespan: Long)

object Bee {
  def create()(implicit config: BeexploreConfig): Bee = Bee(config.foraminiferaStartEnergy, 100)
}