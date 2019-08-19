package pl.edu.agh.beexplore.algorithm

import com.avsystem.commons._
import com.avsystem.commons.misc.Opt
import pl.edu.agh.beexplore.config.BeexploreConfig
import pl.edu.agh.beexplore.model.{Bee, BeeColony, BeexploreCell, Id}
import pl.edu.agh.beexplore.simulation.BeexploreMetrics
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model.{BufferCell, Cell, EmptyCell, Energy, Grid, GridPart, Obstacle, Signal, SmellingCell}
import util.control.Breaks._

import scala.collection.immutable.TreeSet
import scala.util.Random

class BeexploreMovesController(bufferZone: TreeSet[(Int, Int)])
                              (implicit config: BeexploreConfig) extends MovesController {
  import Cell._

  private var grid: Grid = _
  private val random = new Random(System.nanoTime())

  override def initialGrid: (Grid, BeexploreMetrics) = {
    grid = Grid.empty(bufferZone, BeexploreCell(Cell.emptySignal, Vector.empty, Id.Start))

//    coordinates are not absolute, now they're for each node
//    to calculate absoluteCoords, probably will need sth like absoluteX = x + nodeId(horizontally) * gridSize
    grid.cells(config.beeColonyCoordinateX)(config.beeColonyCoordinateY) = BeeColony.create(Vector.fill(config.beeNumber)(Bee.create()))

//    def getRandomElement(list: Seq[(Int, Int)], random: Random):
//    (Int, Int) = list(random.nextInt(list.length))

    for (i:Int <- 0 until config.flowerPatchNumber) {
      print("generating flowerPatch ", i)
      var x = random.nextInt(config.gridSize - 3) + 1  // TODO for now we don't want flower patches on bufferCell
      var y = random.nextInt(config.gridSize - 3) + 1

      while (grid.cells(x)(y).isInstanceOf[BeeColony]
        || grid.cells(x)(y).asInstanceOf[BeexploreCell].flowerPatch != Id.Start
        || grid.cells(x)(y) == Obstacle) {
        print(".")
        x = random.nextInt(config.gridSize - 3) + 1
        y = random.nextInt(config.gridSize - 3) + 1
      }
      grid.cells(x)(y) = BeexploreCell(Cell.emptySignal, Vector.empty, Id(i))

      val flowerPatchSize = random.nextInt(config.flowerPatchSizeMax - config.flowerPatchSizeMin) + config.flowerPatchSizeMin
      println(" | flowerpatch size: ", flowerPatchSize)

      for (j:Int <- 0 until flowerPatchSize) {
        val possibleNextFlower = Iterator(Random.shuffle(List((-1, 0), (0, -1), (0, 1), (1, 0)))).flatten

        var nextFlower = (0, 0)
        var newX = x + nextFlower._1
        var newY = y + nextFlower._2
        breakable {
          while (possibleNextFlower.hasNext) {
            nextFlower = possibleNextFlower.next()
            newX = x + nextFlower._1
            newY = y + nextFlower._2

            if (grid.cells(newX)(newY).asInstanceOf[BeexploreCell].flowerPatch == Id.Start
              && !grid.cells(newX)(newY).isInstanceOf[BeeColony]
              && grid.cells(newX)(newY)!= Obstacle
            ) {
              grid.cells(newX)(newY) = BeexploreCell(Cell.emptySignal, Vector.empty, Id(i))
              x = newX
              y = newY
              break
            }
          }
        }
      }
  }


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

    newGrid.cells(config.beeColonyCoordinateX)(config.beeColonyCoordinateY) = grid.cells(config.beeColonyCoordinateX)(config.beeColonyCoordinateY) match {
                  case cell: BeeColony => cell.copy(bees = Vector.empty)
                }

    def update(x: Int, y: Int)(op: BeexploreCell => BeexploreCell): Unit = {
      def updated(cell: BeexploreCell): BeexploreCell = {
        val afterOp = op(cell)
        val smellAdjustment = (config.foraminiferaInitialSignal * afterOp.bees.size) + (config.algaeSignalMultiplier * afterOp.flowerPatch.value)
        afterOp.copy(smell = afterOp.smell)
        afterOp.copy(flowerPatch = afterOp.flowerPatch)
      }

      newGrid.cells(x)(y) = newGrid.cells(x)(y) match {
        case cell: BeexploreCell => updated(cell)
        case cell: BeeColony => cell
        case BufferCell(cell: BeexploreCell) => BufferCell(updated(cell))
      }
    }

    def updateColony(x: Int, y: Int)(op: BeeColony => BeeColony): Unit = {
      def updatedColony(cell: BeeColony): BeeColony = {
        val afterOp = op(cell)
        afterOp.copy(smell = afterOp.smell)
      }

      newGrid.cells(x)(y) = newGrid.cells(x)(y) match {
        case cell: BeexploreCell => cell
        case cell: BeeColony => updatedColony(cell)
        case BufferCell(cell: BeexploreCell) => cell
      }
    }

    def calculateCell(x: Int, y: Int): Unit = {

      this.grid.cells(x)(y) match {
        case Obstacle =>

        case BufferCell(BeexploreCell(smell, _, _)) =>
          update(x, y)(cell => cell.copy(smell = cell.smell + smell))

        case BeeColony(_, smell, bees, visitedCoords, discoveredFlowerPatchCoords) => {
          val (newBees: Iterator[Bee], moves: BMap[(Int, Int), Stream[Bee]]) =
            bees.foldLeft(
              (
                Iterator[Bee](),
                MMap.empty[(Int, Int), Stream[Bee]].withDefaultValue(Stream.empty)
              )
            ) {
              case ((currentCellResult, pendingMoves), bee) =>
                val action = moveBee(bee, x, y, pendingMoves)
                //                only 1 move in moves iterator
                action.moves.foreach {
                  case ((x, y), movingBee) =>
                    pendingMoves((x, y)) = pendingMoves((x, y)) :+ movingBee
                }
                (currentCellResult ++ action.currentCellResult, pendingMoves)
            }

          moves.foreach {
            case ((i, j), bees) =>
              updateColony(i, j)(b => b.copy(bees = b.bees ++ bees))
              update(i, j)(b => b.copy(bees = b.bees ++ bees))
          }
        }

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
              bees = cell.bees ++ newBees,
              flowerPatch = grid.cells(x)(y).asInstanceOf[BeexploreCell].flowerPatch
            )
          )
          moves.foreach {
            case ((i, j), bees) =>
//            movesCount += bees.size
              update(i, j)(b => b.copy(bees = b.bees ++ bees, flowerPatch = b.flowerPatch))
              updateColony(i, j)(b => b.copy(bees = b.bees ++ bees))
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
      else if (newX == 0 && newY == 0){
        newX = 13
        newY = 13
      }

      val updatedBee = bee.copy(
        energy = bee.energy - config.foraminiferaLifeActivityCost,
        lifespan = bee.lifespan + 1
      )

      val destination = Iterator(((newX, newY), updatedBee))

      BeeAction(Iterator.empty, moves = destination)
    }


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
