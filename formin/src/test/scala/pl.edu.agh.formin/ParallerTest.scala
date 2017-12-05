package pl.edu.agh.formin

import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestProbe}
import org.scalatest.{FlatSpecLike, Matchers}
import org.scalatest.concurrent.{Eventually, ScalaFutures}
import pl.edu.agh.formin.config.{ForminConfig, GuiType}
import pl.edu.agh.formin.model.parallel.{Neighbour, NeighbourPosition}
import pl.edu.agh.xinuk.model._

class ParallerTest extends FlatSpecLike with Matchers with Eventually with ScalaFutures {
  implicit val config: ForminConfig = ForminConfig(
    foraminiferaStartEnergy = Energy(0.5),
    foraminiferaReproductionCost = Energy(0.2),
    foraminiferaReproductionThreshold = Energy(0.3),
    foraminiferaLifeActivityCost = Energy(0.1),
    algaeReproductionFrequency = 2,
    algaeEnergeticCapacity = Energy(0.4),
    signalSpeedRatio = 2,
    signalSuppressionFactor = 0.5,
    gridSize = 5,
    spawnChance = 0.1,
    foraminiferaSpawnChance = 0.5,
    foraminiferaInitialSignal = Signal(-1),
    algaeInitialSignal = Signal(1),
    guiType = GuiType.None,
    guiCellSize = 4,
    workersRoot = 3,
    iterationsNumber = 3,
    isSupervisor = true
  )

  trait Fixture {
    implicit val system: ActorSystem = ActorSystem("WorkerActorTest")
  }

  "A WorkerActors" should "have defined buffers correctly for worker surrounded by neighbours" in new Fixture {
    val workerRegion = TestProbe("worker1")
    val worker = TestActorRef(WorkerActor.props(config)) //system.actorOf(WorkerActor.props(config))
    worker ! WorkerActor.NeighboursInitialized(WorkerId(5),
      Vector(Neighbour(NeighbourPosition.Bottom),
        Neighbour(NeighbourPosition.BottomLeft),
        Neighbour(NeighbourPosition.Top),
        Neighbour(NeighbourPosition.TopLeft),
        Neighbour(NeighbourPosition.TopRight),
        Neighbour(NeighbourPosition.Left),
        Neighbour(NeighbourPosition.Right),
        Neighbour(NeighbourPosition.BottomRight)),
      workerRegion.ref)
    val grid: Grid = worker.underlyingActor.asInstanceOf[WorkerActor].grid

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if x == 0 || x == config.gridSize - 1 && y == 0 || y == config.gridSize - 1
    } {
      grid.cells(x)(y) shouldBe an[BufferCell]
    }
  }

  it should "have defined buffers correctly for worker in the left top corner of the grid" in new Fixture {
    val workerRegion = TestProbe("worker1")
    val worker = TestActorRef(WorkerActor.props(config))
    worker ! WorkerActor.NeighboursInitialized(WorkerId(5),
      Vector(Neighbour(NeighbourPosition.Bottom),
        Neighbour(NeighbourPosition.Right),
        Neighbour(NeighbourPosition.BottomRight)),
      workerRegion.ref)
    val grid: Grid = worker.underlyingActor.asInstanceOf[WorkerActor].grid

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if x == config.gridSize - 1 || y == config.gridSize - 1
    } {
      grid.cells(x)(y) shouldBe an[BufferCell]
    }
  }

  it should "have defined buffers correctly for worker in the left bottom corner of the grid" in new Fixture {
    val workerRegion = TestProbe("worker1")
    val worker = TestActorRef(WorkerActor.props(config))
    worker ! WorkerActor.NeighboursInitialized(WorkerId(5),
      Vector(Neighbour(NeighbourPosition.Top),
        Neighbour(NeighbourPosition.Right),
        Neighbour(NeighbourPosition.TopRight)),
      workerRegion.ref)
    val grid: Grid = worker.underlyingActor.asInstanceOf[WorkerActor].grid

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if x == 0 || y == config.gridSize - 1
    } {
      grid.cells(x)(y) shouldBe an[BufferCell]
    }
  }

  it should "have defined buffers correctly for worker in the right bottom corner of the grid" in new Fixture {
    val workerRegion = TestProbe("worker1")
    val worker = TestActorRef(WorkerActor.props(config))
    worker ! WorkerActor.NeighboursInitialized(WorkerId(5),
      Vector(Neighbour(NeighbourPosition.Top),
        Neighbour(NeighbourPosition.Left),
        Neighbour(NeighbourPosition.TopLeft)),
      workerRegion.ref)
    val grid: Grid = worker.underlyingActor.asInstanceOf[WorkerActor].grid

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if x == 0 || y == 0
    } {
      grid.cells(x)(y) shouldBe an[BufferCell]
    }
  }

  it should "have defined buffers correctly for worker in the right top corner of the grid" in new Fixture {
    val workerRegion = TestProbe("worker1")
    val worker = TestActorRef(WorkerActor.props(config))
    worker ! WorkerActor.NeighboursInitialized(WorkerId(5),
      Vector(Neighbour(NeighbourPosition.Bottom),
        Neighbour(NeighbourPosition.Left),
        Neighbour(NeighbourPosition.BottomLeft)),
      workerRegion.ref)
    val grid: Grid = worker.underlyingActor.asInstanceOf[WorkerActor].grid

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if x == config.gridSize - 1 || y == 0
    } {
      grid.cells(x)(y) shouldBe an[BufferCell]
    }
  }

  it should "have defined buffers correctly for worker in the top line of the grid not in the corner" in new Fixture {
    val workerRegion = TestProbe("worker1")
    val worker = TestActorRef(WorkerActor.props(config))
    worker ! WorkerActor.NeighboursInitialized(WorkerId(5),
      Vector(Neighbour(NeighbourPosition.Bottom),
        Neighbour(NeighbourPosition.Left),
        Neighbour(NeighbourPosition.Right),
        Neighbour(NeighbourPosition.BottomRight),
        Neighbour(NeighbourPosition.BottomLeft)),
      workerRegion.ref)
    val grid: Grid = worker.underlyingActor.asInstanceOf[WorkerActor].grid

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if x == 0 && y != 0 && y != config.gridSize - 1
    } {
      grid.cells(x)(y) shouldBe Obstacle
    }

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if x == config.gridSize - 1 && (y == 0 || y == config.gridSize - 1)
    } {
      grid.cells(x)(y) shouldBe an[BufferCell]
    }
  }

  it should "have defined buffers correctly for worker in the right line of the grid not in the corner" in new Fixture {
    val workerRegion = TestProbe("worker1")
    val worker = TestActorRef(WorkerActor.props(config))
    worker ! WorkerActor.NeighboursInitialized(WorkerId(5),
      Vector(Neighbour(NeighbourPosition.Bottom),
        Neighbour(NeighbourPosition.Left),
        Neighbour(NeighbourPosition.Top),
        Neighbour(NeighbourPosition.TopLeft),
        Neighbour(NeighbourPosition.BottomLeft)),
      workerRegion.ref)
    val grid: Grid = worker.underlyingActor.asInstanceOf[WorkerActor].grid

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if x != 0 && x != config.gridSize - 1 && y == config.gridSize - 1
    } {
      grid.cells(x)(y) shouldBe Obstacle
    }

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if x == config.gridSize - 1 || y == 0
    } {
      grid.cells(x)(y) shouldBe an[BufferCell]
    }
  }

  it should "have defined buffers correctly for worker in the left line of the grid not in the corner" in new Fixture {
    val workerRegion = TestProbe("worker1")
    val worker = TestActorRef(WorkerActor.props(config))
    worker ! WorkerActor.NeighboursInitialized(WorkerId(5),
      Vector(Neighbour(NeighbourPosition.Bottom),
        Neighbour(NeighbourPosition.Right),
        Neighbour(NeighbourPosition.Top),
        Neighbour(NeighbourPosition.TopRight),
        Neighbour(NeighbourPosition.BottomRight)),
      workerRegion.ref)
    val grid: Grid = worker.underlyingActor.asInstanceOf[WorkerActor].grid

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if x != 0 && x != config.gridSize - 1 && y == 0
    } {
      grid.cells(x)(y) shouldBe Obstacle
    }

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if x == config.gridSize - 1 || y == config.gridSize - 1
    } {
      grid.cells(x)(y) shouldBe an[BufferCell]
    }
  }

  it should "have defined buffers correctly for worker in the bottom line of the grid not in the corner" in new Fixture {
    val workerRegion = TestProbe("worker1")
    val worker = TestActorRef(WorkerActor.props(config))
    worker ! WorkerActor.NeighboursInitialized(WorkerId(5),
      Vector(Neighbour(NeighbourPosition.Left),
        Neighbour(NeighbourPosition.Right),
        Neighbour(NeighbourPosition.Top),
        Neighbour(NeighbourPosition.TopRight),
        Neighbour(NeighbourPosition.TopLeft)),
      workerRegion.ref)
    val grid: Grid = worker.underlyingActor.asInstanceOf[WorkerActor].grid

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if x == config.gridSize - 1 && y != 0 && y != config.gridSize - 1
    } {
      grid.cells(x)(y) shouldBe Obstacle
    }

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if x == 0 || y == config.gridSize - 1
    } {
      grid.cells(x)(y) shouldBe an[BufferCell]
    }
  }

}
