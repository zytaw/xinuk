package pl.edu.agh.beexplore.config

import pl.edu.agh.xinuk.config.{GuiType, XinukConfig}
import pl.edu.agh.xinuk.model.{Energy, Signal}

/*
FSE - foraminifera start energy; FSE ∈ [0,1] && FSE ∈ R.
FRC - foraminifera reproduction cost; FRC ∈ [0,1] && FRC ∈ R.
FRT - foraminifera reproduction threshold; FRT ∈ [0,1] && FRT ∈ R.
FLAC - foraminifera life activities(vegetation and movement) cost; FLAC ∈ [0,1] && FLAC ∈ R.
ARF - algae reproduction frequency; ARF ∈ N.
AEC - algae energetic capacity; AEC ∈ [0,1] && AEC ∈ R.
SSR - signal speed ratio; SSR ∈ N. Foraminifera speed is 1.
SPF - global suppression factor of the signal; SPF ∈ [0,1] && SPF ∈ R.
GS - grid size; GS ∈ N, where map size is GSxGS.
SC - spawn chance, SC ∈ [0,1] && SC ∈ R
FSC - foraminifera spawn chance; FAR ∈ N.
FSSV - foraminifera start signal value; FSSV ∈ [0,1] && FSSV ∈ R.
ASSV - algae start signal value; ASSV ∈ [0,1] && ASSV ∈ R.
 */

final case class BeexploreConfig(
                                 signalSpeedRatio: Int,
                                 signalSuppressionFactor: Double,
                                 signalAttenuationFactor: Double,
                                 gridSize: Int,
                                 beeInitialSignal: Signal,
                                 guiType: GuiType,
                                 guiCellSize: Int,
                                 workersRoot: Int,
                                 iterationsNumber: Long,
                                 isSupervisor: Boolean,
                                 shardingMod: Int,
                                 beeColonyCoordinateX: Int,
                                 beeColonyCoordinateY: Int,
                                 beeColonyWorkerId: Int,
                                 beeNumber: Int,
                                 beeTripDuration: Long,
                                 flowerPatchSizeMin: Int,
                                 flowerPatchSizeMax: Int,
                                 flowerPatchNumber: Int,
                                 flowerPatchesFromFile: Boolean,
                                 flowerPatchSignalMultiplier: Signal,
                                 beeSearchMode: Int,
                                 stepsWithNoSmellAfterPatchDiscovery: Int
                               ) extends XinukConfig