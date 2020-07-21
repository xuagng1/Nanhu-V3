package xiangshan.frontend

import chisel3._
import chisel3.util._
import utils._
import xiangshan._

class LoopBuffer extends XSModule {
  val io = IO(new Bundle() {
    val flush = Input(Bool())
    val redirect = Input(Bool())
    val in = Flipped(DecoupledIO(new FetchPacket))
    val out = Vec(DecodeWidth, DecoupledIO(new CtrlFlow))
    val loopPC = ValidIO(UInt(VAddrBits.W))
  })

  class LBufEntry extends XSBundle {
    val inst = UInt(32.W)
    val pc = UInt(VAddrBits.W)
    val fetchOffset = UInt((log2Up(FetchWidth * 4)).W)
    val pnpc = UInt(VAddrBits.W)
    val hist = UInt(HistoryLength.W)
    val btbPredCtr = UInt(2.W)
    val btbHitWay = Bool()
    val tageMeta = new TageMeta
    val rasSp = UInt(log2Up(RasSize).W)
    val rasTopCtr = UInt(8.W)
    val exceptionVec = Vec(16, Bool())
    val intrVec = Vec(12, Bool())
    val isRVC = Bool()
    val isBr = Bool()
    val crossPageIPFFix = Bool()

    // val valid = Bool()
    val isTaken = Bool()
  }

  // ignore
  for(i <- 0 until DecodeWidth) {
    io.out(i).bits.exceptionVec := DontCare
    io.out(i).bits.intrVec := DontCare
    io.out(i).bits.isBr := DontCare
    io.out(i).bits.crossPageIPFFix := DontCare
  }

  // Check Short Backward Branch
  def isSBB(inst: UInt): Bool = {
    inst === BitPat("b1111_???????_111111111_?????_1101111") || inst === BitPat("b1111???_?????_?????_???_????1_1100011")
  }

  // Get sbb target
  def SBBOffset(inst: UInt): UInt = {
    val isJal = inst === BitPat("b1111_???????_111111111_?????_1101111")
    val isCon = inst === BitPat("b1111???_?????_?????_???_????1_1100011")
    Mux(isJal, inst(27, 21), Mux(isCon, Cat(inst(27,25), inst(11,8)), 0.U(7.W)))
  }

  def isJal(inst: UInt): Bool = {
    inst === BitPat("b????????????????????_?????_1101111") || 
    inst === BitPat("b????????????????????_?????_1100111") || 
    inst === BitPat("b???????_?????_?????_???_?????_1100011")
    //    false.B
  }

  //Count Register
  val offsetCounter = Reg(UInt((log2Up(IBufSize)+2).W))
  val tsbbPC = Reg(UInt(VAddrBits.W))

  // def isFull(ptr1: UInt, ptr2: UInt): Bool = ptr1 === ptr2 && lbuf_valid(ptr2)
  // def isEmpty(ptr1: UInt, ptr2: UInt): Bool = ptr1 === ptr2 && !lbuf_valid(ptr1)
  def isOverflow(ptr: UInt): Bool = lbuf_valid(ptr)

  // Loop Buffer define
  val lbuf = Reg(Vec(IBufSize, new LBufEntry))
  val lbuf_valid = RegInit(VecInit(Seq.fill(IBufSize)(false.B)))
  val out_isTaken = Reg(Vec(DecodeWidth, Bool()))
  val head_ptr = RegInit(0.U(log2Up(IBufSize).W))
  val tail_ptr = RegInit(0.U(log2Up(IBufSize).W))

  val loop_str = RegInit(0.U(log2Up(IBufSize).W))
  val loop_end = RegInit(0.U(log2Up(IBufSize).W))
  val loop_ptr = RegInit(0.U(log2Up(IBufSize).W))

  // val hasSBB = (0 until DecodeWidth).map(i => lbuf_valid(head_ptr + i.U) && isSBB(lbuf(head_ptr + i.U).inst)).reduce(_||_)
  val hasSBB = ParallelOR((0 until DecodeWidth).map(i => io.out(i).fire && isSBB(io.out(i).bits.instr))).asBool()
  val hasTSBB = ParallelOR((0 until DecodeWidth).map(i => io.out(i).fire && io.out(i).bits.pc === tsbbPC)).asBool()
  val sbbIdx = OHToUInt(HighestBit(VecInit((0 until DecodeWidth).map(i => isSBB(io.out(i).bits.instr))).asUInt, DecodeWidth))
  val tsbbIdx = OHToUInt((0 until DecodeWidth).map(i => lbuf(head_ptr + i.U).pc === tsbbPC))
  val sbbTaken = lbuf(head_ptr + sbbIdx).isTaken
  val tsbbTaken = io.redirect
  //  val tsbbTaken = lbuf(head_ptr + tsbbIdx).isTaken

  val s_idle :: s_fill :: s_active :: Nil = Enum(3)
  val LBstate = RegInit(s_idle)

  def flush() = {
    XSDebug("Loop Buffer Flushed.\n")
    LBstate := s_idle
    for(i <- 0 until IBufSize) {
      lbuf(i).inst := 0.U // Delete can improve performance?
      lbuf(i).pc := 0.U // Delete can improve performance?
      lbuf_valid(i) := false.B
    }
    head_ptr := 0.U
    tail_ptr := 0.U
  }

  // clean invalid insts in LB when out FILL state
  def cleanFILL(str: UInt, end: UInt): Unit = {
    when(str <= end) {
      for(i <- 0 until IBufSize) {
        lbuf_valid(i) := (str > i.U || i.U >= end) && lbuf_valid(i)
      }
    }.otherwise {
      for(i <- 0 until IBufSize) {
        lbuf_valid(i) := (str <= i.U && i.U < end) && lbuf_valid(i)
      }
    }
  }

  /*---------------*/
  /*    Dequeue    */
  /*---------------*/
  var deq_idx = WireInit(0.U(log2Up(DecodeWidth+2).W))

  io.loopPC.valid := LBstate === s_active

  when(LBstate =/= s_active) {
    for(i <- 0 until DecodeWidth) {
    //   io.out(i).valid := !isEmpty(head_ptr + deq_idx, tail_ptr) && lbuf_valid(head_ptr + deq_idx)
      io.out(i).valid := lbuf_valid(head_ptr + deq_idx)

      when(io.out(i).fire){
        io.out(i).bits.instr := lbuf(head_ptr + deq_idx).inst
        io.out(i).bits.pc := lbuf(head_ptr + deq_idx).pc
        io.out(i).bits.fetchOffset := lbuf(head_ptr + deq_idx).fetchOffset
        io.out(i).bits.pnpc := lbuf(head_ptr + deq_idx).pnpc
        io.out(i).bits.hist := lbuf(head_ptr + deq_idx).hist
        io.out(i).bits.btbPredCtr := lbuf(head_ptr + deq_idx).btbPredCtr
        io.out(i).bits.btbHitWay := lbuf(head_ptr + deq_idx).btbHitWay
        io.out(i).bits.tageMeta := lbuf(head_ptr + deq_idx).tageMeta
        io.out(i).bits.rasSp := lbuf(head_ptr + deq_idx).rasSp
        io.out(i).bits.rasTopCtr := lbuf(head_ptr + deq_idx).rasTopCtr
        io.out(i).bits.isRVC := false.B
        lbuf_valid(head_ptr + deq_idx) := (lbuf_valid(head_ptr + deq_idx) && LBstate === s_fill) || (hasSBB && sbbTaken && i.U > sbbIdx)
        out_isTaken(i) := lbuf(head_ptr + deq_idx).isTaken
      }.otherwise {
        io.out(i).bits <> DontCare
      }

      XSDebug("deq_idx=%d\n", deq_idx)
      deq_idx = deq_idx + (lbuf_valid(head_ptr + deq_idx) && io.out(i).fire)
    }

    head_ptr := head_ptr + deq_idx

    io.loopPC.bits := DontCare
  }.otherwise {
    XSDebug("You shouldn't see this info\n")
    deq_idx = 0.U
    for(i <- 0 until DecodeWidth) {
      io.out(i).valid := deq_idx =/= DecodeWidth.U + 1.U && lbuf(loop_ptr + deq_idx).pc <= tsbbPC

      when(io.out(i).fire) {
        io.out(i).bits.instr := lbuf(loop_ptr + deq_idx).inst
        io.out(i).bits.pc := lbuf(loop_ptr + deq_idx).pc
        io.out(i).bits.fetchOffset := lbuf(loop_ptr + deq_idx).fetchOffset
        io.out(i).bits.pnpc := lbuf(loop_ptr + deq_idx).pnpc
        io.out(i).bits.hist := lbuf(loop_ptr + deq_idx).hist
        io.out(i).bits.btbPredCtr := lbuf(loop_ptr + deq_idx).btbPredCtr
        io.out(i).bits.btbHitWay := lbuf(loop_ptr + deq_idx).btbHitWay
        io.out(i).bits.tageMeta := lbuf(loop_ptr + deq_idx).tageMeta
        io.out(i).bits.rasSp := lbuf(loop_ptr + deq_idx).rasSp
        io.out(i).bits.rasTopCtr := lbuf(loop_ptr + deq_idx).rasTopCtr
        io.out(i).bits.isRVC := false.B
      }.otherwise {
        io.out(i).bits <> DontCare
      }

      deq_idx = Mux(deq_idx === DecodeWidth.U + 1.U || loop_ptr + deq_idx === loop_end, DecodeWidth.U + 1.U, deq_idx + io.out(i).fire)
    }

    val next_loop_ptr = Mux(deq_idx === DecodeWidth.U + 1.U, loop_str, loop_ptr + deq_idx)
    loop_ptr := next_loop_ptr
    //    XSDebug("deq_idx = %d\n", deq_idx)
    //    XSDebug("loop_ptr = %d\n", Mux(deq_idx === DecodeWidth.U, loop_str, loop_ptr + deq_idx))
    io.loopPC.bits := lbuf(next_loop_ptr).pc
  }

  val offsetCounterWire = WireInit(offsetCounter + (PopCount((0 until DecodeWidth).map(io.out(_).fire())) << 1).asUInt)
  offsetCounter := offsetCounterWire
  XSDebug("countReg=%b\n", offsetCounterWire)

  /*---------------*/
  /*    Enqueue    */
  /*---------------*/
  var enq_idx = 0.U(log2Up(FetchWidth+1).W)

  io.in.ready := LBstate =/= s_active && !isOverflow(tail_ptr + FetchWidth.U - 1.U)

  when(io.in.fire()){
    for(i <- 0 until FetchWidth) {
      lbuf(tail_ptr + enq_idx).inst := io.in.bits.instrs(i)
      lbuf(tail_ptr + enq_idx).pc := io.in.bits.pc + (enq_idx << 2).asUInt
      lbuf(tail_ptr + enq_idx).pnpc := io.in.bits.pnpc(i)
      lbuf(tail_ptr + enq_idx).fetchOffset := (enq_idx<<2).asUInt
      lbuf(tail_ptr + enq_idx).hist := io.in.bits.hist(i)
      lbuf(tail_ptr + enq_idx).btbPredCtr := io.in.bits.predCtr(i)
      lbuf(tail_ptr + enq_idx).btbHitWay := io.in.bits.btbHitWay
      lbuf(tail_ptr + enq_idx).tageMeta := io.in.bits.tageMeta(i)
      lbuf(tail_ptr + enq_idx).rasSp := io.in.bits.rasSp
      lbuf(tail_ptr + enq_idx).rasTopCtr := io.in.bits.rasTopCtr

      lbuf_valid(tail_ptr + enq_idx) := io.in.bits.mask(i<<1) // FIXME: need fix me when support RVC
      // FIXME: This is wrong
      lbuf(tail_ptr + enq_idx).isTaken := (io.in.bits.pc + (enq_idx << 2).asUInt + 4.U) =/= io.in.bits.pnpc(i) // isTaken can reduce to LBufSize/FetchWidth
      // lbuf(tail_ptr + enq_idx).isTaken := false.B // isTaken can reduce to LBufSize/FetchWidth

      enq_idx = enq_idx + io.in.bits.mask(i<<1)
    }
    tail_ptr := tail_ptr + enq_idx
  }

  //Loop Buffer FSM
  switch(LBstate) {
    is(s_idle) {
      // To FILL
      // 检测到sbb且跳转，sbb成为triggrting sbb
      XSDebug(hasSBB, "SBB detected\n")
      when(hasSBB && sbbTaken) {
        LBstate := s_fill
        XSDebug("State change: FILL\n")
        offsetCounter := Cat("b1".U, SBBOffset(lbuf(head_ptr + sbbIdx).inst)) + ((DecodeWidth.U - sbbIdx)<<1).asUInt
        tsbbPC := lbuf(head_ptr + sbbIdx).pc
        loop_str := head_ptr + sbbIdx + 1.U
        XSDebug("loop_str=%d\n", head_ptr + sbbIdx + 1.U)
      }
    }
    is(s_fill) {
      when(offsetCounterWire((log2Up(IBufSize)+2)-1) === 0.U && hasTSBB) {
        when(sbbTaken) {
          // To ACTIVE
          // triggering sbb造成cof
          LBstate := s_active
          XSDebug("State change: ACTIVE\n")
          loop_end := head_ptr + sbbIdx
          XSDebug("loop_end=%d\n", head_ptr + sbbIdx)
          loop_ptr := loop_str
        }.otherwise {
          // triggering sbb不跳转
          // To IDLE
          LBstate := s_idle
          cleanFILL(loop_str, head_ptr + PopCount((0 until DecodeWidth).map(io.out(_).fire())))
          XSDebug("State change: IDLE\n")
        }
      }

      // 非triggering sbb造成的cof
      // 遇到过一个周期内跳转为ACTIVE后又跳转为IDLE，无法重现
      when(ParallelOR((0 until DecodeWidth).map(i => io.out(i).valid && !isSBB(io.out(i).bits.instr) && isJal(io.out(i).bits.instr) && lbuf(tail_ptr + i.U).isTaken)).asBool()) {
        // To IDLE
        LBstate := s_idle
        cleanFILL(loop_str, head_ptr + PopCount((0 until DecodeWidth).map(io.out(_).fire())))
        XSDebug("State change: IDLE\n")
      }
    }
    is(s_active) {
      // To IDLE
      // triggering sbb不跳转
      when(hasTSBB && !tsbbTaken) {
        // To IDLE
        XSDebug("tsbb not taken, State change: IDLE\n")
        flush()
      }

      // 非triggering sbb造成的cof
      when(ParallelOR((0 until DecodeWidth).map(i => io.out(i).valid && io.out(i).bits.pc =/= tsbbPC && isJal(io.out(i).bits.instr) && io.redirect)).asBool()) {
        // To IDLE
        XSDebug("cof by other inst, State change: IDLE\n")
        flush()
      }
    }
  }

  // flush
  when(io.flush) {
    flush()
  }

  // Debug Info
  // XSDebug(io.in.fire(), p"PC= ${Hexadecimal(io.in.bits.pc)}\n")
  XSDebug(io.flush, "Loop Buffer Flushed\n")

  XSDebug(LBstate === s_idle, "Current state: IDLE\n")
  XSDebug(LBstate === s_fill, "Current state: FILL\n")
  XSDebug(LBstate === s_active, "Current state: ACTIVE\n")

  when(io.in.valid) {
    XSDebug("Enque:\n")
    XSDebug(p"PC=${Hexadecimal(io.in.bits.pc)}\n")
    for(i <- 0 until FetchWidth){
        XSDebug(p"${Hexadecimal(io.in.bits.instrs(i))}  v=${io.in.valid}  r=${io.in.ready}\n")
    }
  }

// when((0 until DecodeWidth).map(i => io.out(i).ready).reduce(_||_)){
    XSDebug("Deque:\n")
    for(i <- 0 until DecodeWidth){
        XSDebug(p"${Hexadecimal(io.out(i).bits.instr)}  PC=${Hexadecimal(io.out(i).bits.pc)}  v=${io.out(i).valid}  r=${io.out(i).ready}\n")
    }
// }

XSDebug(p"last_head_ptr=$head_ptr  last_tail_ptr=$tail_ptr\n")

// Print loop buffer
for(i <- 0 until IBufSize/8) {
    XSDebug("%x v:%b | %x v:%b | %x v:%b | %x v:%b | %x v:%b | %x v:%b | %x v:%b | %x v:%b\n",
    lbuf(i*8+0).inst, lbuf_valid(i*8+0),
      lbuf(i*8+1).inst, lbuf_valid(i*8+1),
      lbuf(i*8+2).inst, lbuf_valid(i*8+2),
      lbuf(i*8+3).inst, lbuf_valid(i*8+3),
      lbuf(i*8+4).inst, lbuf_valid(i*8+4),
      lbuf(i*8+5).inst, lbuf_valid(i*8+5),
      lbuf(i*8+6).inst, lbuf_valid(i*8+6),
      lbuf(i*8+7).inst, lbuf_valid(i*8+7)
    )
  }
}
