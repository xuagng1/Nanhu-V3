package xiangshan.vector.vbackend.vregfile
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.{ExuOutput, XSBundle, XSModule}

class MoveReq(implicit p: Parameters) extends XSBundle{
  private val VRFSize = coreParams.vectorParameters.vPhyRegsNum
  val srcAddr = UInt(log2Up(VRFSize).W)
  val dstAddr = UInt(log2Up(VRFSize).W)
  val ma = Bool()
  val ta = Bool()
  val tailMask = UInt((VLEN/8).W)
}

class VrfReadPort(implicit p: Parameters) extends XSBundle{
  private val VRFSize = coreParams.vectorParameters.vPhyRegsNum
  val addr = Input(UInt(log2Up(VRFSize).W))
  val data = Output(UInt(VLEN.W))
}

class VRegfile(wbWkpNum:Int, wbNoWkpNum:Int, readPortNum:Int)(implicit p: Parameters) extends XSModule {
  private val size = coreParams.vectorParameters.vPhyRegsNum
  private val dataWidth = VLEN
  private val maskWidth = VLEN / 8
  val io = IO(new Bundle{
    val wbWakeup = Input(Vec(wbWkpNum, Valid(new ExuOutput)))
    val wbNoWakeup = Input(Vec(wbNoWkpNum, Valid(new ExuOutput)))
    val wakeups = Output(Vec(wbWkpNum, Valid(new ExuOutput)))
    val moveOldValReqs = Input(Vec(loadUnitNum, Valid(new MoveReq)))
    val readPorts = Vec(readPortNum, new VrfReadPort)
  })
  
  
  val mrf = Reg(Vec(size, UInt(maskWidth.W)))
  val vrf = Reg(Vec(size, UInt(dataWidth.W)))

  // read vector register file
  for (r <- io.readPorts) {
    r.data := vrf(r.addr)
  }
  // write vector register file
  for (i <- 0 until wbWkpNum) { 
    io.wakeups(i).bits := io.wbWakeup(i).bits
    when (io.wbWakeup(i).valid) {
      val vrfBytes = vrf(io.wbWakeup(i).bits.uop.pdest)asTypeOf(Vec(dataWidth/8, UInt(8.W)))
      val mrfBits = mrf(io.wbWakeup(i).bits.uop.pdest).asTypeOf(Vec(maskWidth, Bool()))
      for (j <- 0 until maskWidth) {
        when (io.wbWakeup(i).bits.writeDataMask(j) === 1.U) {
          vrfBytes(j) := io.wbWakeup(i).bits.data(j * 8 + 7, j * 8)
        }
        when (io.wbWakeup(i).bits.wakeupMask(j) === 1.U) {   
        mrfBits(j) := true.B
        }
    }
      vrf(io.wbWakeup(i).bits.uop.pdest) := vrfBytes.asUInt
      mrf(io.wbWakeup(i).bits.uop.pdest) := mrfBits.asUInt
    }
    // wakeup
    when (mrf(io.wbWakeup(i).bits.uop.pdest).andR) {      
      io.wakeups(i).valid := true.B
      mrf(io.wbWakeup(i).bits.uop.pdest) := RegNext(0.U)
    } .otherwise{
      io.wakeups(i).valid := false.B
    }
  }
  // not wakeup 
  for (i <- 0 until wbNoWkpNum) { 
    val vrfBytes = vrf(io.wbNoWakeup(i).bits.uop.pdest)asTypeOf(Vec(dataWidth/8, UInt(8.W)))
      for (j <- 0 until maskWidth) {
        when (io.wbNoWakeup(i).bits.writeDataMask(j) === 1.U) {
          vrfBytes(j) := io.wbNoWakeup(i).bits.data(j * 8 + 7, j * 8)
      }
    }
      vrf(io.wbNoWakeup(i).bits.uop.pdest) := vrfBytes.asUInt
  }
  // Move request
  for (i <- 0 until loadUnitNum) {
    when (io.moveOldValReqs(i).valid) {
      val dstMask = mrf(io.moveOldValReqs(i).bits.dstAddr)
      val srcBytes = vrf(io.moveOldValReqs(i).bits.srcAddr).asTypeOf(Vec(16, UInt(8.W)))
      // ta is true 
      when (io.moveOldValReqs(i).bits.ta) {
        val outBytes = io.moveOldValReqs(i).bits.tailMask.asBools.zipWithIndex.map{case (maskBit, index) =>
          val dataByte = Mux(maskBit, 255.U, srcBytes(index))
          dataByte 
        }
        vrf(io.moveOldValReqs(i).bits.dstAddr) := Cat(outBytes.reverse)
      // ma is true 
      } .elsewhen(io.moveOldValReqs(i).bits.ma) {
        val outBytes = (io.moveOldValReqs(i).bits.tailMask | dstMask).asBools.zipWithIndex.map{case (maskBit, index) =>
          val dataByte = Mux(!maskBit, 255.U, srcBytes(index))
          dataByte 
        }
      vrf(io.moveOldValReqs(i).bits.dstAddr) := Cat(outBytes.reverse)
    }
      // write to mask register file rmf
      mrf(io.moveOldValReqs(i).bits.dstAddr) := io.moveOldValReqs(i).bits.tailMask | dstMask
    }
  }
}