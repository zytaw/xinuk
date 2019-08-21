package pl.edu.agh.beexplore.model

import com.avsystem.commons.MMap
import pl.edu.agh.beexplore.config.BeexploreConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{Cell, Energy, SmellingCell}

final case class BeeColony(
                            coordinates: (Int, Int),
                            smell: SmellArray,
                            bees: Vector[Bee],
                            visitedCoords: Vector[(Int, Int)],
                            discoveredFlowerPatchCoords: collection.mutable.Map[Id, (Int, Int)],
                            discoveredFlowerPatchMetrics: collection.mutable.Map[Id, Int]
                          ) extends SmellingCell {
  override type Self = BeeColony

  override def withSmell(smell: SmellArray): BeeColony = copy(smell = smell)
}

object BeeColony {
  def create(bees: Vector[Bee] = Vector.empty)(
    implicit config: BeexploreConfig,
  ):
  BeeColony = BeeColony (
    (config.beeColonyCoordinateX, config.beeColonyCoordinateY),
    Cell.emptySignal,
    bees,
    Vector.empty,
    MMap.empty[Id, (Int, Int)],
    MMap.empty[Id, Int]
  )
}