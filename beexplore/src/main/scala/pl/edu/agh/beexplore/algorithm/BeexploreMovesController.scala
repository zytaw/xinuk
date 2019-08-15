package pl.edu.agh.beexplore.algorithm

import pl.edu.agh.beexplore.config.BeexploreConfig
import pl.edu.agh.beexplore.model.{Bee, BeexploreCell, Id}
import pl.edu.agh.beexplore.simulation.BeexploreMetrics
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model.{BufferCell, Cell, EmptyCell, Grid, GridPart}

import scala.collection.immutable.TreeSet
import scala.util.Random

class BeexploreMovesController(bufferZone: TreeSet[(Int, Int)])
                              (implicit config: BeexploreConfig) extends MovesController {
  private var grid: Grid = _
  private val random = new Random(System.nanoTime())

  override def initialGrid: (Grid, BeexploreMetrics) = {
    grid = Grid.empty(bufferZone, BeexploreCell(Cell.emptySignal, Vector.empty, Id.Start))

    grid.cells(config.gridSize / 2)(config.gridSize / 2) = BeexploreCell.create(Vector.fill(10)(Bee.create()))

    // TODO: fill metrics
    val metrics = BeexploreMetrics(
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0
    )
    (grid, metrics)
  }

  override def makeMoves(iteration: Long, grid: Grid): (Grid, BeexploreMetrics) = {

//    val newGrid = Grid.empty(bufferZone, BeexploreCell.create())
//
//    newGrid.cells(x)(y) = newGrid.cells(x)(y) match {
//      case cell: BeexploreCell => cell
//      case BufferCell(cell: BeexploreCell) => BufferCell(cell)
//    }
    // TODO: fill metrics
    val metrics = BeexploreMetrics(
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0
    )

    (grid, metrics)
  }
}
