package vexiiriscv.misc

import spinal.core.Area
import spinal.core.fiber.Lock
import spinal.lib.misc.plugin.FiberPlugin
import spinal.lib.misc.pipeline
import spinal.lib.misc.pipeline.StageLink

class PipelineBuilderPlugin extends FiberPlugin{
  val elaborationLock = Lock()
  val logic = during build new Area{
    elaborationLock.await()
    val chunks = host.list[PipelineService]
    val links = chunks.flatMap(_.getLinks())
    pipeline.Builder(links)
  }
}
