package pl.edu.agh.beexplore.simulation

import pl.edu.agh.xinuk.simulation.Metrics

final case class BeexploreMetrics(foraminiferaCount: Long,
                                 algaeCount: Double,
                                 foraminiferaDeaths: Long,
                                 foraminiferaTotalEnergy: Double,
                                 foraminiferaReproductionsCount: Long,
                                 consumedAlgaeCount: Double,
                                 foraminiferaTotalLifespan: Long,
                                 foraminiferaMoves: Long) extends Metrics {

  override def log: String = {
    s"$foraminiferaCount;$algaeCount;$foraminiferaDeaths;$foraminiferaTotalEnergy;$foraminiferaReproductionsCount;$consumedAlgaeCount;$foraminiferaTotalLifespan;$foraminiferaMoves"
  }

  override def series: Vector[(String, Double)] = Vector(
    "Foraminifera" -> foraminiferaCount,
    "Algae" -> algaeCount
  )

  override def +(other: Metrics): BeexploreMetrics = {
    other match {
      case BeexploreMetrics.EMPTY => this
      case BeexploreMetrics(otherForaminiferaCount, otherAlgaeCount, otherForaminiferaDeaths,
      otherForaminiferaTotalEnergy, otherForaminiferaReproductionsCount, otherConsumedAlgaeCount,
      otherForaminiferaTotalLifespan, otherForaminiferaMoves) =>
        BeexploreMetrics(foraminiferaCount + otherForaminiferaCount, algaeCount + otherAlgaeCount,
          foraminiferaDeaths + otherForaminiferaDeaths, foraminiferaTotalEnergy + otherForaminiferaTotalEnergy,
          foraminiferaReproductionsCount + otherForaminiferaReproductionsCount,
          consumedAlgaeCount + otherConsumedAlgaeCount, foraminiferaTotalLifespan + otherForaminiferaTotalLifespan,
          foraminiferaMoves + otherForaminiferaMoves)
      case null => this
      case _ => throw new UnsupportedOperationException(s"Cannot add: non-FortwistMetrics to FortwistMetrics")
    }
  }
}

object BeexploreMetrics {
  private val EMPTY = BeexploreMetrics(0, 0, 0, 0, 0, 0, 0, 0)

  def empty(): BeexploreMetrics = EMPTY
}