package pl.edu.agh.beexplore.algorithm

import com.avsystem.commons._
import com.avsystem.commons.misc.Opt
import pl.edu.agh.beexplore.config.BeexploreConfig
import pl.edu.agh.beexplore.model.{Bee, BeexploreCell, Id}
import pl.edu.agh.beexplore.simulation.BeexploreMetrics
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model.{BufferCell, Cell, EmptyCell, Energy, Grid, GridPart, Obstacle, Signal}

import scala.collection.immutable.TreeSet
import scala.util.Random

class BeexploreMovesController(bufferZone: TreeSet[(Int, Int)])
                              (implicit config: BeexploreConfig) extends MovesController {
  import Cell._

  private var grid: Grid = _
  private val random = new Random(System.nanoTime())

  override def initialGrid: (Grid, BeexploreMetrics) = {
    grid = Grid.empty(bufferZone, BeexploreCell(Cell.emptySignal, Vector.empty, Id.Start))

    grid.cells(config.gridSize / 4)(config.gridSize / 4) = BeexploreCell.create(Vector.fill(10)(Bee.create()))

    println("grid initialized")

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

    this.grid = grid
    val newGrid = Grid.empty(bufferZone, BeexploreCell.create())

    def update(x: Int, y: Int)(op: BeexploreCell => BeexploreCell): Unit = {
      def updated(cell: BeexploreCell): BeexploreCell = {
        val afterOp = op(cell)
        val smellAdjustment = (config.foraminiferaInitialSignal * afterOp.bees.size) + (config.algaeSignalMultiplier * afterOp.flowerPatch.value)
        afterOp.copy(smell = afterOp.smell)
      }

      newGrid.cells(x)(y) = newGrid.cells(x)(y) match {
        case cell: BeexploreCell => updated(cell)
        case BufferCell(cell: BeexploreCell) => BufferCell(updated(cell))
      }
    }

    def calculateCell(x: Int, y: Int): Unit = {

      this.grid.cells(x)(y) match {
        case Obstacle =>

        case BufferCell(BeexploreCell(smell, _, _)) =>
          update(x, y)(cell => cell.copy(smell = cell.smell + smell))

        case BeexploreCell(smell, bees, flowerPatch) => {
          val (newBees: Iterator[Bee], moves: BMap[(Int, Int), Stream[Bee]], newFlowerPatch:Id) =
            bees.foldLeft(
              (
                Iterator[Bee](),
                MMap.empty[(Int, Int), Stream[Bee]].withDefaultValue(Stream.empty),
                flowerPatch
              )
          ) {
              case ((currentCellResult, pendingMoves, runningFlowerPatch), bee) =>
                val action = moveBee(bee, x, y, pendingMoves)
//                only 1 move in moves iterator
                action.moves.foreach {
                  case ((x, y), movingBee) =>
                    pendingMoves((x, y)) = pendingMoves((x, y)) :+ movingBee
                }
                (currentCellResult ++ action.currentCellResult, pendingMoves, runningFlowerPatch)
          }

          import Cell._
          update(x, y)(
            cell => cell.copy(
              smell = cell.smell + smell,
              bees = cell.bees ++ newBees
//            flowerPatch
            )
          )
          moves.foreach {
            case ((i, j), bees) =>
//            movesCount += bees.size
              update(i, j)(b => b.copy(bees = b.bees ++ bees))
          }
        }
      }
    }

    final case class BeeAction(
                                currentCellResult: Iterator[Bee],
                                moves: Iterator[((Int, Int), Bee)] = Iterator.empty
                              )

    def moveBee(bee: Bee, x: Int, y: Int, moves: BMap[(Int, Int), Stream[Bee]]): BeeAction = {

      var newX = x + random.nextInt(3) - 1
      var newY = y + random.nextInt(3) - 1
      if (grid.cells(newX)(newY) == Obstacle){
        newX = x
        newY = y
      }

      val updatedBee = bee.copy(
        energy = bee.energy - config.foraminiferaLifeActivityCost,
        lifespan = bee.lifespan + 1
      )

      val destination = Iterator(((newX, newY), updatedBee))

      BeeAction(Iterator.empty, moves = destination)
    }
//
//    def copyCells(x: Int, y: Int, cell: GridPart): Unit = {
//      newGrid.cells(x)(y) = cell
//    }

    // TODO: gather metrics like below:
//    for {
//      x <- 0 until config.gridSize
//      y <- 0 until config.gridSize
//    } {
//      this.grid.cells(x)(y) match {
//        case BeexploreCell(_, bees, flowerPatch) =>
//          foraminiferaTotalEnergy += bees.iterator.map(_.energy.value).sum
//          beeCount += bees.size
//        case BufferCell(BeexploreCell(_, foraminiferas, algae)) =>
//          foraminiferaTotalEnergy += foraminiferas.iterator.map(_.energy.value).sum
//          foraminiferaCount += foraminiferas.size
//        case _ =>
//      }
//    }

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
    } calculateCell(x, y)

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

    (newGrid, metrics)
  }
}
