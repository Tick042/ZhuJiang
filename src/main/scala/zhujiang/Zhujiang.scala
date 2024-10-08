package zhujiang

import chisel3._
import chisel3.experimental.{ChiselAnnotation, annotate}
import org.chipsalliance.cde.config.Parameters
import sifive.enterprise.firrtl.NestedPrefixModulesAnnotation
import xijiang.router.base.IcnBundle
import xijiang.Ring
import xijiang.c2c.C2cLinkPort
import zhujiang.device.bridge.axi.AxiBridge

class Zhujiang(implicit p: Parameters) extends ZJModule {
  require(p(ZJParametersKey).tfsParams.isEmpty)
  if(p(ZJParametersKey).modulePrefix != "") {
    val mod = this.toNamed
    annotate(new ChiselAnnotation {
      def toFirrtl = NestedPrefixModulesAnnotation(mod, p(ZJParametersKey).modulePrefix, true)
    })
  }
  private val localRing = Module(new Ring(true))
  private val csnRing = Module(new Ring(false))

  private def makeIOs(icns: Option[Seq[IcnBundle]], local: Boolean): Unit = {
    icns.foreach(_.foreach(icn => {
      val port = IO(icn.cloneType)
      port.suggestName(icn.node.icnStr)
      port <> icn
    }))
  }
  private val mem = localRing.icnSns.map(_.filter(hi => hi.node.mainMemory))
  private val dcus = localRing.icnSns.map(_.filterNot(hi => hi.node.mainMemory))
  makeIOs(localRing.icnCcs, true)
  makeIOs(localRing.icnRfs, true)
  makeIOs(localRing.icnRis, true)
  makeIOs(localRing.icnHfs, true)
  makeIOs(localRing.icnHis, true)
  makeIOs(dcus, true)

  makeIOs(csnRing.icnRfs, false)
  makeIOs(csnRing.icnHfs, false)

  if(mem.isDefined) {
    val chi2axi = Module(new AxiBridge(mem.get.head.node))
    chi2axi.icn <> mem.get.head
    val port = IO(chi2axi.axi.cloneType)
    port.suggestName("mem_axi")
    port <> chi2axi.axi
  }

  if(csnRing.c2cs.isDefined) {
    csnRing.c2cs.foreach(_.zipWithIndex.foreach({ case (c2c, idx) =>
      val port = IO(new C2cLinkPort)
      port.suggestName(s"c2c_link_$idx")
      port <> c2c
    }))
  }
  val io_chip = IO(Input(UInt(p(ZJParametersKey).nodeAidBits.W)))
  localRing.io_chip := io_chip
  csnRing.io_chip := io_chip
}
