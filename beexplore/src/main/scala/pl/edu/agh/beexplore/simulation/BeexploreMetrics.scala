package pl.edu.agh.beexplore.simulation

import pl.edu.agh.beexplore.model.Id
import pl.edu.agh.xinuk.simulation.Metrics
import com.avsystem.commons.MMap


final case class BeexploreMetrics(beeCount: Int,
                                  flowerPatchCount: Int,
                                  firstTripFlowerPatchCount: collection.mutable.Map[Id, (Int, Double)],
                                  discoveredFlowerPatchCount: collection.mutable.Map[Id, (Int, Double)],
                                  beeMoves: Long,
                                  beeTrips: Long,
                                  ) extends Metrics {

  override def log: String = {
    s"$beeCount;$flowerPatchCount;$discoveredFlowerPatchCount;$beeMoves;$beeTrips"
  }

  override def series: Vector[(String, Double)] = Vector(
    "Bees" -> beeCount,
    "FlowerPatches" -> flowerPatchCount
  )

  override def +(other: Metrics): BeexploreMetrics = {
    other match {
      case BeexploreMetrics.EMPTY => this
      case BeexploreMetrics(otherBeeCount, otherFlowerPatchCount, _, otherDiscoveredFlowerPatchCount, otherBeeMoves,
      otherBeeTrips) =>
        BeexploreMetrics(
          beeCount + otherBeeCount,
          flowerPatchCount + otherFlowerPatchCount,
          firstTripFlowerPatchCount,
          discoveredFlowerPatchCount ++ otherDiscoveredFlowerPatchCount,
          beeMoves + otherBeeMoves,
          beeTrips + otherBeeTrips
        )
      case null => this
      case _ => throw new UnsupportedOperationException(s"Cannot add: non-BeexploreMetrics to BeexploreMetrics")
    }
  }
}

object BeexploreMetrics {
  private val EMPTY = BeexploreMetrics(0, 0, MMap.empty[Id, (Int, Double)], MMap.empty[Id, (Int, Double)], 0, 0)

  def empty(): BeexploreMetrics = EMPTY
}