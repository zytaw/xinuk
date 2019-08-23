package pl.edu.agh.beexplore.simulation

import pl.edu.agh.xinuk.simulation.Metrics

final case class BeexploreMetrics(beeCount: Long,
                                  flowerPatchCount: Long,
                                  discoveredFlowerPatchCount: Long,
                                  beeMoves: Long,
                                  beeTrips: Long,
                                  ) extends Metrics {

  override def log: String = {
    s"$beeCount;$flowerPatchCount;$discoveredFlowerPatchCount;$beeMoves;$beeTrips"
  }

  override def series: Vector[(String, Double)] = Vector(
    "Foraminifera" -> beeCount,
    "Algae" -> flowerPatchCount
  )

  override def +(other: Metrics): BeexploreMetrics = {
    other match {
      case BeexploreMetrics.EMPTY => this
      case BeexploreMetrics(otherBeeCount, otherFlowerPatchCount, otherDiscoveredFlowerPatchCount, otherBeeMoves,
      otherBeeTrips) =>
        BeexploreMetrics(
          beeCount + otherBeeCount,
          flowerPatchCount + otherFlowerPatchCount,
          discoveredFlowerPatchCount + otherDiscoveredFlowerPatchCount,
          beeMoves + otherBeeMoves,
          beeTrips + otherBeeTrips
        )
      case null => this
      case _ => throw new UnsupportedOperationException(s"Cannot add: non-BeexploreMetrics to BeexploreMetrics")
    }
  }
}

object BeexploreMetrics {
  private val EMPTY = BeexploreMetrics(0, 0, 0, 0, 0)

  def empty(): BeexploreMetrics = EMPTY
}