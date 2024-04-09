/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.frontend.icache

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.ClientStates._
import freechips.rocketchip.tilelink.TLPermissions._
import freechips.rocketchip.tilelink._
import xiangshan._
import huancun.{AliasKey, DirtyKey}
import xiangshan.cache._
import utils._
import xs.utils._
import difftest._
import xs.utils.perf.HasPerfLogging


abstract class ICacheMissUnitModule(implicit p: Parameters) extends XSModule
  with HasICacheParameters

abstract class ICacheMissUnitBundle(implicit p: Parameters) extends XSBundle
  with HasICacheParameters

class ICacheMissReq(implicit p: Parameters) extends ICacheBundle
{
    val paddr      = UInt(PAddrBits.W)
    val vaddr      = UInt(VAddrBits.W)
    val waymask   = UInt(nWays.W)


    def getVirSetIdx = get_idx(vaddr)
    def getPhyTag    = get_phy_tag(paddr)
}


class ICacheMissResp(implicit p: Parameters) extends ICacheBundle
{
    val data     = UInt(blockBits.W)
    val corrupt  = Bool()
}

class ICacheMissBundle(implicit p: Parameters) extends ICacheBundle{
    val req       =   Vec(2, Flipped(DecoupledIO(new ICacheMissReq)))
    val resp      =   Vec(2,ValidIO(new ICacheMissResp))
    val flush     =   Input(Bool())
}


class ICacheMissEntry(edge: TLEdgeOut, id: Int)(implicit p: Parameters) extends ICacheMissUnitModule
  with MemoryOpConstants with HasPerfLogging
{
  val io = IO(new Bundle {
    val id = Input(UInt(log2Ceil(PortNumber).W))

    val req = Flipped(DecoupledIO(new ICacheMissReq))
    val resp = ValidIO(new ICacheMissResp)

    //tilelink channel
    val mem_acquire = DecoupledIO(new TLBundleA(edge.bundle))
    val mem_grant = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))

    val meta_write = DecoupledIO(new ICacheMetaWriteBundle)
    val data_write = DecoupledIO(new ICacheDataWriteBundle)

    val victimInfor    =  Output(new ICacheVictimInfor())

    val ongoing_req    = Output(new FilterInfo)
    val fencei      = Input(Bool())

  })

  /** default value for control signals */
  io.resp := DontCare
  io.mem_acquire.bits := DontCare
  io.mem_grant.ready := true.B
  io.meta_write.bits := DontCare
  io.data_write.bits := DontCare

  val s_idle  :: s_send_mem_aquire :: s_wait_mem_grant :: s_write_back :: s_wait_resp :: Nil = Enum(5)
  val state = RegInit(s_idle)
  val state_dup = Seq.fill(5)(RegInit(s_idle))
  /** control logic transformation */
  //request register
  val req = Reg(new ICacheMissReq)
  val req_idx = req.getVirSetIdx //virtual index
  val req_tag = req.getPhyTag //physical tag
  val req_waymask = req.waymask
  val release_id  = Cat(MainPipeKey.U, id.U)
  val req_corrupt = RegInit(false.B)

  io.victimInfor.valid := state_dup(0) === s_write_back || state_dup(0) === s_wait_resp
  io.victimInfor.vidx  := req_idx

  val (_, _, refill_done, refill_address_inc) = edge.addr_inc(io.mem_grant)

  val needflush_r = RegInit(false.B)
  when (state === s_idle) { needflush_r := false.B }
  when (state =/= s_idle && io.fencei) { needflush_r := true.B }
  val needflush = needflush_r | io.fencei

  //cacheline register
  val readBeatCnt = Reg(UInt(log2Up(refillCycles).W))
  val respDataReg = Reg(Vec(refillCycles, UInt(beatBits.W)))

  //initial
  io.resp.bits := DontCare
  io.mem_acquire.bits := DontCare
  io.mem_grant.ready := true.B
  io.meta_write.bits := DontCare
  io.data_write.bits := DontCare

  io.ongoing_req.valid := (state =/= s_idle)
  io.ongoing_req.paddr :=  addrAlign(req.paddr, blockBytes, PAddrBits)

  io.req.ready := (state === s_idle)
  io.mem_acquire.valid := (state_dup(1) === s_send_mem_aquire)


 // io.toPrefetch.valid := (state_dup(2) =/= s_idle)
 // io.toPrefetch.bits  :=  addrAlign(req.paddr, blockBytes, PAddrBits)
  // io.toPrefetch.valid := DontCare
  // io.toPrefetch.bits := DontCare
  val grantack = RegEnable(edge.GrantAck(io.mem_grant.bits), io.mem_grant.fire)
  val grant_param = Reg(UInt(TLPermissions.bdWidth.W))
  val is_dirty = RegInit(false.B)
  val is_grant = RegEnable(edge.isRequest(io.mem_grant.bits), io.mem_grant.fire)

  //state change
  switch(state) {
    is(s_idle) {
      when(io.req.fire) {
        readBeatCnt := 0.U
        state := s_send_mem_aquire
        state_dup.map(_ := s_send_mem_aquire)
        req := io.req.bits
      }
    }

    // memory request
    is(s_send_mem_aquire) {
      when(io.mem_acquire.fire) {
        state := s_wait_mem_grant
        state_dup.map(_ := s_wait_mem_grant)
      }
    }

    is(s_wait_mem_grant) {
      when(edge.hasData(io.mem_grant.bits)) {
        when(io.mem_grant.fire) {
          readBeatCnt := readBeatCnt + 1.U
          respDataReg(readBeatCnt) := io.mem_grant.bits.data
          req_corrupt := io.mem_grant.bits.corrupt
        //  grant_param := io.mem_grant.bits.param
       //   is_dirty    := io.mem_grant.bits.echo.lift(DirtyKey).getOrElse(false.B)
          when(readBeatCnt === (refillCycles - 1).U) {
      //      assert(refill_done, "refill not done!")
            state := s_write_back
            state_dup.map(_ := s_write_back)
          }
        }
      }
    }

    is(s_write_back) {
      state := Mux(io.meta_write.fire && io.data_write.fire || needflush, s_wait_resp, s_write_back)
      state_dup.map(_ := Mux(io.meta_write.fire && io.data_write.fire, s_wait_resp, s_write_back))
    }

    is(s_wait_resp) {
      io.resp.bits.data := respDataReg.asUInt
      io.resp.bits.corrupt := req_corrupt
      when(io.resp.fire) {
        state := s_idle
        state_dup.map(_ := s_idle)
      }
    }
  }

  /** refill write and meta write */
  val acquireBlock = edge.Get(
    fromSource = io.id,
    toAddress = addrAlign(req.paddr, blockBytes, PAddrBits),
    lgSize = (log2Up(cacheParams.blockBytes)).U,
  )._2
  io.mem_acquire.bits := acquireBlock
  // resolve cache alias by L2
  io.mem_acquire.bits.user.lift(AliasKey).foreach(_ := req.vaddr(12))
  require(nSets <= 256) // icache size should not be more than 128KB

  //resp to ifu
  io.resp.valid := state === s_wait_resp

  io.meta_write.valid := (state === s_write_back) && !needflush
  io.meta_write.bits.generate(tag = req_tag, idx = req_idx, waymask = req_waymask, bankIdx = req_idx(0))

  io.data_write.valid := (state === s_write_back) && !needflush
  val dataWriteEn = Wire(Vec(4, Bool()))
  dataWriteEn.zipWithIndex.map{ case(wen,i) => 
    wen := state_dup(i) === s_write_back
  }
  io.data_write.bits.generate(data = respDataReg.asUInt, idx = req_idx, waymask = req_waymask, bankIdx = req_idx(0), writeEn = dataWriteEn, paddr = req.paddr)

  XSPerfAccumulate(
    "entryPenalty" + Integer.toString(id, 10),
    BoolStopWatch(
      start = io.req.fire,
      stop = io.resp.valid,
      startHighPriority = true)
  )
  XSPerfAccumulate("entryReq" + Integer.toString(id, 10), io.req.fire)

}


class ICacheMissUnit(edge: TLEdgeOut)(implicit p: Parameters) extends ICacheMissUnitModule with HasPerfLogging
{
  val io = IO(new Bundle{
    val hartId      = Input(UInt(8.W))
    val req         = Vec(2, Flipped(DecoupledIO(new ICacheMissReq)))
    val resp        = Vec(2, ValidIO(new ICacheMissResp))

    val mem_acquire = DecoupledIO(new TLBundleA(edge.bundle))
    val mem_grant   = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))

    val fdip_acquire = Flipped(DecoupledIO(new TLBundleA(edge.bundle)))
    val fdip_grant   = DecoupledIO(new TLBundleD(edge.bundle))
    
    val meta_write  = DecoupledIO(new ICacheMetaWriteBundle)
    val data_write  = DecoupledIO(new ICacheDataWriteBundle)

    val fencei      = Input(Bool())
    val ICacheMissUnitInfo = new ICacheMissUnitInfo
    val victimInfor = Vec(PortNumber, Output(new ICacheVictimInfor()))

  //   val prefetch_req          =  Flipped(DecoupledIO(new PIQReq))
     val prefetch_check        =  Vec(PortNumber,ValidIO(UInt(PAddrBits.W)))


  })
  // assign default values to output signals
  io.mem_grant.ready := false.B

  val meta_write_arb = Module(new Arbiter(new ICacheMetaWriteBundle,  PortNumber))
  val refill_arb     = Module(new Arbiter(new ICacheDataWriteBundle,  PortNumber))

  io.mem_grant.ready := true.B

  val entries = (0 until PortNumber) map { i =>
    val entry = Module(new ICacheMissEntry(edge, i))

    entry.io.id := i.U
    println(s"miss entry ID: ${i}")

    // entry req
    entry.io.req.valid := io.req(i).valid
    entry.io.req.bits  := io.req(i).bits
    io.req(i).ready    := entry.io.req.ready

    // entry resp
    meta_write_arb.io.in(i)     <>  entry.io.meta_write
    refill_arb.io.in(i)         <>  entry.io.data_write

    entry.io.mem_grant.valid := false.B
    entry.io.mem_grant.bits  := DontCare
    when (io.mem_grant.bits.source === i.U) {
      entry.io.mem_grant <> io.mem_grant
    }

    io.resp(i) <> entry.io.resp
    io.ICacheMissUnitInfo.mshr(i) <> entry.io.ongoing_req
    entry.io.fencei := io.fencei

    io.victimInfor(i) := entry.io.victimInfor
  //  io.prefetch_check(i) <> entry.io.toPrefetch
    io.prefetch_check(i) <> DontCare

    XSPerfAccumulate(
      "entryPenalty" + Integer.toString(i, 10),
      BoolStopWatch(
        start = entry.io.req.fire,
        stop = entry.io.resp.fire,
        startHighPriority = true)
    )
    XSPerfAccumulate("entryReq" + Integer.toString(i, 10), entry.io.req.fire)

    entry
  }

  io.fdip_grant.valid := false.B
  io.fdip_grant.bits  := DontCare
  when (io.mem_grant.bits.source === PortNumber.U) {
    io.fdip_grant <> io.mem_grant
  }
  // val alloc = Wire(UInt(log2Ceil(nPrefetchEntries).W))

  // val prefEntries = (iprefetchUnitIDStart until iprefetchUnitIDStart + nPrefetchEntries) map { i =>
  //   val prefetchEntry = Module(new IPrefetchEntry(edge, PortNumber))

  //   prefetchEntry.io.mem_hint_ack.valid := false.B
  //   prefetchEntry.io.mem_hint_ack.bits := DontCare

  //   when(io.mem_grant.bits.source === PortNumber.U) {
  //     prefetchEntry.io.mem_hint_ack <> io.mem_grant
  //   }

  //   prefetchEntry.io.req.valid := io.prefetch_req.valid && ((i-PortNumber).U === alloc)
  //   prefetchEntry.io.req.bits  := io.prefetch_req.bits

  //   prefetchEntry.io.id := i.U
  //   println(s"prefetch entry ID: ${i}")

  //   prefetchEntry
  // }

  // alloc := PriorityEncoder(prefEntries.map(_.io.req.ready))
  // io.prefetch_req.ready := ParallelOR(prefEntries.map(_.io.req.ready))
  // val tl_a_chanel = entries.map(_.io.mem_acquire) ++ prefEntries.map(_.io.mem_hint)
 // io.prefetch_req.ready := DontCare

   /**
    ******************************************************************************
    * Register 2 cycle meta write info for IPrefetchPipe filter
    ******************************************************************************
    */
  val meta_write_buffer = InitQueue(new FilterInfo, size = 2)
  meta_write_buffer(0).valid := io.meta_write.fire
  meta_write_buffer(0).paddr := io.data_write.bits.paddr
  meta_write_buffer(1)       := meta_write_buffer(0)
  (0 until 2).foreach (i => {
    io.ICacheMissUnitInfo.recentWrite(i) := meta_write_buffer(i)
  })
  val tl_a_chanel = entries.map(_.io.mem_acquire) :+ io.fdip_acquire
  TLArbiter.lowest(edge, io.mem_acquire, tl_a_chanel:_*)


  io.meta_write     <> meta_write_arb.io.out
  io.data_write     <> refill_arb.io.out

  if (env.EnableDifftest) {
    val difftest = DifftestModule(new DiffRefillEvent)
    difftest.coreid := io.hartId
    difftest.index := 0.U
    difftest.idtfr := 0.U
    difftest.valid := refill_arb.io.out.valid
    difftest.addr := refill_arb.io.out.bits.paddr
    difftest.data := refill_arb.io.out.bits.data.asTypeOf(difftest.data)
  }

  (0 until nWays).map{ w =>
    XSPerfAccumulate("line_0_refill_way_" + Integer.toString(w, 10),  entries(0).io.meta_write.valid && OHToUInt(entries(0).io.meta_write.bits.waymask)  === w.U)
    XSPerfAccumulate("line_1_refill_way_" + Integer.toString(w, 10),  entries(1).io.meta_write.valid && OHToUInt(entries(1).io.meta_write.bits.waymask)  === w.U)
  }

}



