package pl.edu.agh.beexplore

import java.awt.Color

import pl.edu.agh.beexplore.algorithm.BeexploreMovesController
import pl.edu.agh.beexplore.config.BeexploreConfig
import pl.edu.agh.beexplore.model.BeexploreCell
import pl.edu.agh.beexplore.model.parallel.BeexploreConflictResolver
import pl.edu.agh.xinuk.Simulation
import pl.edu.agh.xinuk.model.DefaultSmellPropagation

object BeexploreMain {
  private val configPrefix = "beexplore"
  private val metricHeaders = Vector(
    "beeCount",
    "beeTotalTrips",
    "beeTotalTripDuration",
    "beeMoves",
    "flowerPatchCount",
    "exploredFlowerPatchCount",
    "a",
    "b"
  )

  private def cellToColor(cell: BeexploreCell): Color = {
    val hue = 0.11f
    val saturation = 0.69f
    //val luminance = cell.algae.value.floatValue()
    val luminance = cell.bees.size / 10f
    Color.getHSBColor(hue, saturation, luminance)
  }

  def main(args: Array[String]): Unit = {
    import pl.edu.agh.xinuk.config.ValueReaders._
    new Simulation[BeexploreConfig](
      configPrefix,
      metricHeaders,
      BeexploreConflictResolver,
      DefaultSmellPropagation.calculateSmellAddendsStandard,
      BeexploreCell.create()
    )(
        new BeexploreMovesController(_)(_),
      { case cell: BeexploreCell => cellToColor(cell) }
    ).start()
  }
}