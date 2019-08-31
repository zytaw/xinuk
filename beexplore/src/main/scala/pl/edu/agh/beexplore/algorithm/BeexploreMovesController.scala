package pl.edu.agh.beexplore.algorithm

import java.awt.Color

import com.avsystem.commons._
import com.avsystem.commons.misc.Opt
import pl.edu.agh.beexplore.config.BeexploreConfig
import pl.edu.agh.beexplore.model
import pl.edu.agh.beexplore.model.{Bee, BeeColony, BeexploreCell, Id}
import pl.edu.agh.beexplore.simulation.BeexploreMetrics
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model.{BufferCell, Cell, EmptyCell, Energy, Grid, GridPart, Obstacle, Signal, SmellingCell, WorkerId}

import util.control.Breaks._
import scala.collection.immutable.TreeSet
import scala.util.Random
import scala.math.signum

import java.io.File
import javax.imageio.ImageIO
import java.awt.image.BufferedImage

import Array._

class BeexploreMovesController(bufferZone: TreeSet[(Int, Int)], workerId: WorkerId)
                              (implicit config: BeexploreConfig) extends MovesController {
  import Cell._

  private var grid: Grid = _
  private val random = new Random(System.nanoTime())

  override def initialGrid: (Grid, BeexploreMetrics) = {
    println("[WORKER INIT] ", workerId)
    grid = Grid.empty(bufferZone, BeexploreCell(Cell.emptySignal, Vector.empty, Id.Start))

//    def similarTo(c: Color, flowerPatchColor: Color) = {
//      val distance = (c.getRed - flowerPatchColor.getRed) * (c.getRed - flowerPatchColor.getRed)
//      + (c.getGreen - flowerPatchColor.getGreen) * (c.getGreen - flowerPatchColor.getGreen)
//      + (c.getBlue - flowerPatchColor.getBlue) * (c.getBlue - flowerPatchColor.getBlue)
//
//      if (distance < 10) true
//      else false
//    }

    if (config.flowerPatchesFromFile) {
      val img = ImageIO.read(new File("map350.png"))

      val w = img.getWidth
      val h = img.getHeight

      val flowerPatchColor = new Color(53, 96, 232)

      var imgFlowerPatch = ofDim[Boolean](w, h)

      val out = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
      for (x <- 0 until w) {
        for (y <- 0 until h) {
          val c = new Color(img.getRGB(x, y))
          if (c == flowerPatchColor) {
            imgFlowerPatch(x)(y) = true
          }
        }
      }
//      ImageIO.write(out, "jpg", new File("test_bettermap.jpg"))

      def processNeighbourFlowerPatches (x: Int, y: Int, id: Int): Unit = {
        if (imgFlowerPatch(y)(x)) {
          val neighbourCellCoordinates = Grid.neighbourCellCoordinates(x, y)
          imgFlowerPatch(y)(x) = false
          grid.cells(x)(y) = BeexploreCell(Cell.emptySignal + config.flowerPatchSignalMultiplier, Vector.empty, Id(id))
          val neighboursToProcess = neighbourCellCoordinates
            .map {
              case (i, j) => (grid.cells(i)(j), i, j)
            }
            .filter(_._1 != Obstacle)
            .filter(r => imgFlowerPatch(r._3)(r._2))
            .foreach {
//              case null => println("ok")
              case (_, i, j) =>
                processNeighbourFlowerPatches(i, j, id)
            }
        }
      }

      var colonyNotSet = true
      var newFlowerPatchId = -1
      for (x <- 0 until w) {
        for (y <- 0 until h){
          val c = new Color(img.getRGB(y, x))
//          if (x > 0 && y > 0 && x < w-1 && y < h-1 && c == flowerPatchColor) {
          if (x > 0 && y > 0 && x < w-1 && y < h-1 && imgFlowerPatch(y)(x)) {
            newFlowerPatchId += 1
            processNeighbourFlowerPatches(x, y, newFlowerPatchId)
          }
          if (colonyNotSet && c == new Color(255, 0, 0)) {
            grid.cells(x)(y) = BeeColony.create(Vector.fill(config.beeNumber)(Bee.create()))
            println("beecolony coords ", x, y)
            colonyNotSet = false
          }
        }
      }

      println("flowerPatchId: ", newFlowerPatchId)

    }
    else {
      //    coordinates are not absolute, now they're for each node
      //    to calculate absoluteCoords, probably will need sth like absoluteX = x + nodeId(horizontally) * gridSize
      if (workerId.value == config.beeColonyWorkerId)
        grid.cells(config.beeColonyCoordinateX)(config.beeColonyCoordinateY) = BeeColony.create(Vector.fill(config.beeNumber)(Bee.create()))

      for (i:Int <- 0 until config.flowerPatchNumber) {
        var x = random.nextInt(config.gridSize - 3) + 1  // TODO for now we don't want flower patches on bufferCell
        var y = random.nextInt(config.gridSize - 3) + 1

        while (grid.cells(x)(y).isInstanceOf[BeeColony]
          || grid.cells(x)(y).isInstanceOf[BufferCell]
          || grid.cells(x)(y).asInstanceOf[BeexploreCell].flowerPatch != Id.Start
          || grid.cells(x)(y) == Obstacle) {
          print(".")
          x = random.nextInt(config.gridSize - 3) + 1
          y = random.nextInt(config.gridSize - 3) + 1
        }

        val flowerPatchId = (workerId.value - 1) * config.flowerPatchNumber + i
        grid.cells(x)(y) = BeexploreCell(Cell.emptySignal + config.flowerPatchSignalMultiplier, Vector.empty, Id(flowerPatchId))

        val flowerPatchSize = random.nextInt(config.flowerPatchSizeMax - config.flowerPatchSizeMin) + config.flowerPatchSizeMin
        println("generating flowerPatch ", flowerPatchId, " | flowerpatch size: ", flowerPatchSize)

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

              val cell = grid.cells(newX)(newY)
              if (cell != Obstacle
                && !cell.isInstanceOf[BufferCell]
                && !cell.isInstanceOf[BeeColony]
                && cell.asInstanceOf[BeexploreCell].flowerPatch == Id.Start
              ) {
                grid.cells(newX)(newY) = BeexploreCell(Cell.emptySignal + config.flowerPatchSignalMultiplier, Vector.empty, Id(flowerPatchId))
                x = newX
                y = newY
                break
              }
            }
          }
        }
      }
    }

    println("grid initialized")

    val metrics = BeexploreMetrics(
      config.flowerPatchNumber,
      config.beeNumber,
      MMap.empty[Id, (Int, Double)],
      MMap.empty[Id, Int],
      0,
      0
    )
    (grid, metrics)
  }

  override def makeMoves(iteration: Long, grid: Grid): (Grid, BeexploreMetrics) = {

    this.grid = grid
    val newGrid = Grid.empty(bufferZone, BeexploreCell.create())

    var firstTripFlowerPatchCount = MMap.empty[Id, (Int, Double)]
    var discoveredFlowerPatchCount = MMap.empty[Id, Int]
    var beeMoves = 0L
    var beeTrips = 0L

    newGrid.cells(config.beeColonyCoordinateX)(config.beeColonyCoordinateY) = grid.cells(config.beeColonyCoordinateX)(config.beeColonyCoordinateY) match {
                  case cell: BeeColony => cell.copy(bees = Vector.empty)
                  case cell: BeexploreCell => cell.copy(bees = Vector.empty)
                }

    def update(x: Int, y: Int)(op: BeexploreCell => BeexploreCell): Unit = {
      def updated(cell: BeexploreCell): BeexploreCell = {
        val afterOp = op(cell)
        var flowerPatchAdjustment = 0
        if (afterOp.flowerPatch.value != -1)
          flowerPatchAdjustment = 1
        val smellAdjustment = (config.beeInitialSignal * afterOp.bees.size) + (config.flowerPatchSignalMultiplier * flowerPatchAdjustment)
        afterOp.copy(smell = afterOp.smell + smellAdjustment)
      }

      newGrid.cells(x)(y) = newGrid.cells(x)(y) match {
        case cell: BeexploreCell => updated(cell)
        case cell: BeeColony => cell
        case BufferCell(cell: BeexploreCell) => BufferCell(updated(cell))
        case Obstacle => Obstacle
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
        case BufferCell(cell) => BufferCell(cell)
        case Obstacle => Obstacle
      }
    }

    def calculateCell(x: Int, y: Int): Unit = {

      this.grid.cells(x)(y) match {
        case Obstacle =>

        case BufferCell(BeexploreCell(smell, _, _)) =>
          update(x, y)(cell => cell.copy(smell = cell.smell + smell))

        case BeeColony(_, _, bees, _, _, _) => {
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
          val (newBees: Iterator[Bee], moves: BMap[(Int, Int), Stream[Bee]], _) =
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

    def smellBasedMoveCoords(x: Int, y: Int, moves:BMap[(Int, Int), Stream[Bee]]): (Int, Int) = {
      val neighbourCellCoordinates = Grid.neighbourCellCoordinates(x, y)
      val destination = Random.shuffle(Grid.SubcellCoordinates
        .map {
          case (i, j) =>
            grid.cells(x)(y).smell(i)(j) + moves.get((x, y)).map(
              bees => config.beeInitialSignal * bees.size).getOrElse(Signal.Zero)
        })
        .zipWithIndex
        .sorted(implicitly[Ordering[(Signal, Int)]].reverse)
        .iterator
        .map { case (_, idx) =>
          val (i, j) = neighbourCellCoordinates(idx)
//          println((i, j, grid.cells(i)(j)))
          (i, j, grid.cells(i)(j))
        }
        .filter(_._3 != Obstacle)
        .nextOpt match {
          case Opt((i, j, _)) =>
            (i, j)
          case Opt.Empty =>
            (x, y)
        }
      destination
    }

    def randomMoveCoords (x: Int, y: Int): (Int, Int) = {
      var newX = x + random.nextInt(3) - 1
      var newY = y + random.nextInt(3) - 1

      if (grid.cells(newX)(newY) == Obstacle){
        newX = x
        newY = y
      }

      if (newX == config.beeColonyCoordinateX && newY == config.beeColonyCoordinateY){
        randomMoveCoords(x, y)
      }
      else
        (newX, newY)
    }

    def desiredMoveCoords (x: Int, y: Int, destination: (Int, Int), vectorFromColony: (Int, Int)): (Int, Int) = {
      val newX = x + signum(destination._1 - vectorFromColony._1)
      val newY = y + signum(destination._2 - vectorFromColony._2)
      (newX, newY)
    }

    def turningAnglesCoords (x: Int, y: Int, lastMoveVector: (Int, Int)): (Int, Int) = {

      val turningVector = random.nextInt(214) match {
        case x if 0 until 58 contains x =>
          (0, 1) //fly straight
        case x if 58 until 97 contains x =>
          (1, 1) // 45 degrees
        case x if 97 until 133 contains x =>
          (1, 0) // 90 degrees
        case x if 133 until 178 contains x =>
          (1, -1) // 135 degrees
        case x if 178 until 214 contains x =>
          (0, -1) // 180 degrees - turn around
      }

      val (newX, newY) = (lastMoveVector._1, lastMoveVector._2 * -1) match {
        case (0, 1) =>
          (x + turningVector._1, y + turningVector._2)
        case (1, 0) =>
          (x + turningVector._2, y - turningVector._1)
        case (0, -1) =>
          (x - turningVector._1, y - turningVector._2)
        case (-1, 0) =>
          (x - turningVector._2, y + turningVector._1)
        case (1, 1) =>
          (x + math.signum(turningVector._1 + turningVector._2), y + math.signum(turningVector._2 - turningVector._1))
        case (-1, -1) =>
          (x - math.signum(turningVector._1 + turningVector._2), y - math.signum(turningVector._2 - turningVector._1))
        case (1, -1) =>
          (x + math.signum(turningVector._2 - turningVector._1), y - math.signum(turningVector._1 + turningVector._2))
        case (-1, 1) =>
          (x - math.signum(turningVector._1 + turningVector._2), y + math.signum(turningVector._2 - turningVector._1))
        case (0, 0) =>
          (x + turningVector._1, y + turningVector._2)
        case (_, _) =>
          (x, y)
      }

      if (grid.cells(newX)(newY) == Obstacle || (newX == config.beeColonyCoordinateX && newY == config.beeColonyCoordinateY)) {
//        println("smutno mi ", turningVector, lastMoveVector)
        (x, y)
      }
      else
        (newX, newY)
    }


    def moveBee(bee: Bee, x: Int, y: Int, moves: BMap[(Int, Int), Stream[Bee]]): BeeAction = {

      val (newX, newY) = bee.destination match {
        case (Int.MinValue, Int.MinValue) =>
          if (config.signalSpeedRatio > 0)
            smellBasedMoveCoords(x, y, moves)
          else if (bee.lastMoveVector == (Int.MinValue, Int.MinValue))
            randomMoveCoords(x, y)  // without smell
          else
            turningAnglesCoords(x, y, bee.lastMoveVector)

        case _ =>
          desiredMoveCoords(x, y, bee.destination, bee.vectorFromColony)
      }

      def getUpdatedBee (cell: GridPart, bee: Bee): Bee = {
        val moveVectorX = newX - x
        val moveVectorY = newY - y

        var tripNumber = bee.tripNumber
        var destination = bee.destination
        var maxTripDuration = bee.maxTripDuration - 1
        var discoveredFlowerPatches = bee.discoveredFlowerPatches
        var vectorFromColony = (bee.vectorFromColony._1 + moveVectorX, bee.vectorFromColony._2 + moveVectorY)
        beeMoves += 1
        val lastMoveVector = (moveVectorX, moveVectorY)

//        println("[BEE] ", bee, " moving from (",x, y, ") to (", newX, newY, ")")

         cell match {
          case BeexploreCell(_, _, flowerPatch) => {
            if (destination == vectorFromColony)
            // destination found - bee can fly wherever it wants
              destination = (Int.MinValue, Int.MinValue)
            if (flowerPatch != Id.Start && !bee.discoveredFlowerPatches.contains(flowerPatch))
              discoveredFlowerPatches += flowerPatch -> vectorFromColony
            if (maxTripDuration <= 0)
//              same as negated vectorFromColony (but then desiredMoveCoords wouldn't work)
              destination = (0, 0)
          }

          case BeeColony(_, _, _, firstTripDetections, discoveredFlowerPatchCoords, discoveredFlowerPatchMetrics) => {
//            println("bee in colony, discovered FlowerPatches: ", bee.discoveredFlowerPatches)
            // flowerPatch detection probabilities on 1st scouting trip
            if (bee.tripNumber == 1) {
              println("[BEE]", bee)
              for ((id, _) <- bee.discoveredFlowerPatches) {
                val discoveredFlowerPatchDistance = math.sqrt(math.pow(bee.discoveredFlowerPatches(id)._1, 2) + math.pow(bee.discoveredFlowerPatches(id)._2, 2))
                if (firstTripDetections.contains(id))
                  if (firstTripDetections(id)._2 > discoveredFlowerPatchDistance)
                    firstTripDetections(id) = (firstTripDetections(id)._1 + 1, discoveredFlowerPatchDistance)
                  else
                    firstTripDetections(id) = (firstTripDetections(id)._1 + 1, firstTripDetections(id)._2)
                else
                  firstTripDetections(id) = (1, discoveredFlowerPatchDistance)
              }
              firstTripFlowerPatchCount = firstTripDetections
            }

            // newest coord for each flowerPatch are kept in BeeColony (since potentially the environment could dynamically change)
            discoveredFlowerPatchCoords ++= bee.discoveredFlowerPatches
            for ((id, _) <- bee.discoveredFlowerPatches) {
              if (discoveredFlowerPatchMetrics.contains(id))
                discoveredFlowerPatchMetrics(id) = discoveredFlowerPatchMetrics(id) + 1
              else
                discoveredFlowerPatchMetrics(id) = 1
            }

            tripNumber += 1
            discoveredFlowerPatches.clear
            maxTripDuration = config.beeTripDuration
            vectorFromColony = (0, 0)

            config.beeSearchMode match {
              case 1 =>
                destination = (Int.MinValue, Int.MinValue)
              case 2 => {
                val possibleDestinations = discoveredFlowerPatchCoords.values.toList
                if (possibleDestinations.nonEmpty)
                  destination = possibleDestinations(random.nextInt(possibleDestinations.length))
                else
                  destination = (Int.MinValue, Int.MinValue)
              }
            }
            println("--firstTripDetections: ", firstTripDetections, "discoveredFlowerPatchMetrics: ", discoveredFlowerPatchMetrics)
            if (beeTrips < tripNumber)
              beeTrips = tripNumber
            discoveredFlowerPatchCount = discoveredFlowerPatchMetrics

          }

          case _ =>
        }

        bee.copy(
          tripNumber,
          maxTripDuration,
          discoveredFlowerPatches,
          destination,
          vectorFromColony,
          lastMoveVector
        )
      }

      val updatedBee = getUpdatedBee(this.grid.cells(newX)(newY), bee)

      val destination = Iterator(((newX, newY), updatedBee))

      BeeAction(Iterator.empty, moves = destination)
    }

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
    } calculateCell(x, y)

//    todo w każdym kroku zapewnić uzupełnienie metryk
    val metrics = BeexploreMetrics(
      beeCount = config.beeNumber,
      flowerPatchCount = config.flowerPatchNumber,
      firstTripFlowerPatchCount = firstTripFlowerPatchCount,
      discoveredFlowerPatchCount = discoveredFlowerPatchCount,
      beeMoves = beeMoves,
      beeTrips = beeTrips,
    )

    (newGrid, metrics)
  }
}
