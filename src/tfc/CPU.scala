package tfc

class CPU(val mem: Memory = new Memory, val alu: ALU = new ALU, val debug: Int => Unit = println _, var trace: Boolean = true) {
  
  if (trace) println("TRACE ON")
  
  var ic = 0
  var sp = mem.size
  var acc = 0
  var halted = false
  def accSigned = acc.toShort.toInt
  
  def pop() = {
    val it = mem.read16(sp)
    sp = sp + 2
    it
  }
  def push(it: Int) = {
    sp = sp - 2
    mem.write16(sp, it)
  }
  private def printbyte(it: Int) = {
    print(((it >> 4) & 0xF).toHexString)
    print((it & 0xF).toHexString)
  }
  def step() = {
    if (halted) throw new Exception("execution halted")
    mem.trace = trace
    if (trace) print(ic.formatted("%4x") + " | ")
    val instr = mem.read8(ic)
    if (trace) {
      printbyte(instr)
      print(" ")
    }
    instr match {
      // 0x0 data handling
      case 0x00 => // push
        if (trace) print(" push ")
        push(acc)
      case 0x01 => // pop
        if (trace) print("  pop ")
        acc = pop()
      case 0x02 => // const/clear
        if (trace) print("  ccl ")
        ic += 1
        val data = mem.read8(ic).toByte.toInt & 0xFFFF
        acc = data
      case 0x03 => // const/shift
        if (trace) print("  csh ")
        ic += 1
        val data = mem.read8(ic)
        acc = (acc << 8) & 0xFF00 | data
      case 0x04 => // grab
        if (trace) print(" grab ")
        push(mem.read16(sp + accSigned))
      case 0x05 => // stash
        if (trace) print("stash ")
        val stashed = pop()
        mem.write16(sp + accSigned, stashed)
      case 0x06 => // lookup
        if (trace) print(" lkup ")
        push(mem.read16(pop()))
      case 0x07 => // set
        if (trace) print("  set ")
        val value = pop()
        val addr = pop()
        mem.write16(addr, value)
      // 0x1 control flow
      case 0x10 => // nop
        if (trace) print("  nop ")
      case 0x11 => // halt
        if (trace) print(" halt ")
        halted = true
      case 0x12 => // jump/conditional [relative]
        if (trace) print("   jc ")
        // -1 because ic gets incremented at end of step
        if (pop() != 0) ic = ic + accSigned - 1
      case 0x13 => // jump/nonlocal
        if (trace) print("  jnl ")
        ic = acc - 1
      case 0x1F => // debug
        if (trace) print("  dbg ")
        debug(acc)
      // 0x2 arithmetic
      case _ if (instr & 0xF0) == 0x20 =>
        if (trace) print("  op2 ")
        val op = instr & 0xF
        val b = pop(); val a = pop()
        val c = alu.calc2(a, b, op)
        push(c)
      // 0x3 single-param arithmetic
      case _ if (instr & 0xF0) == 0x30 =>
        if (trace) print("  op1 ")
        val op = instr & 0xF
        val a = pop()
        val c = alu.calc1(a, op)
        push(c)
      case _ => throw new Exception("invalid instruction " + instr.toHexString + " at " + ic)
    }
    ic += 1
    if (trace) println(" | -> " + ic.formatted("%x4"))
  }
  def load(instrs: Array[Byte]) = {
    for (i <- 0 until instrs.length) {
      mem.write8(i, instrs(i))
    }
  }
  
}

class ALU {
  def calc2(a: Int, b: Int, instr: Int): Int = {
    val result = instr match {
      case 0x0 => a+b
      case 0x1 => a-b
      case 0x2 => a*b
      case 0x3 =>
        val x = a.toShort; val y = b.toShort
        Math.floorDiv(x, y)
      case 0x4 =>
        val x = a.toShort; val y = b.toShort
        Math.floorMod(x, y)
      case 0x5 => // compare
        val lessSigned = if (a.toShort < b.toShort) 1 else 0
        val lessUnsigned = if (a < b) 1 else 0
        val equal = if (a == b) 1 else 0
        (equal << 2) | (lessUnsigned << 1) | lessSigned
      case 0x6 => a&b
      case 0x7 => a|b
      case 0x8 => a^b
      case 0x9 => a >>> b
      case 0xA => a << b
      case 0xB => a >> b
      case 0xC => if (a != 0 && b != 0) -1 else 0
      case _ => throw new Exception("invalid arithmetic operation")
    }
    result & 0xFFFF
  }
  def calc1(a: Int, instr: Int): Int = {
    val result = instr match {
      case 0x0 => -a.toShort
      case 0x1 => ~a
      case 0x2 => // less than - signed
        if ((a & 1) != 0) 1 else 0
      case 0x3 => // less than or equal to - signed
        if ((a & 1 | a & 4) != 0) 1 else 0
      case 0x4 => // greater than - signed
        if ((a & 1 | a & 4) == 0) 1 else 0
      case 0x5 => // greater than or equal to - signed
        if ((a & 1) == 0) 1 else 0
      case 0x6 => // equal to
        if ((a & 4) != 0) 1 else 0
      case 0x7 => // not equal
        if ((a & 4) == 0) 1 else 0
      case 0x8 => // less than - unsigned
        if ((a & 2) != 0) 1 else 0
      case 0x9 => // less than or equal to - unsigned
        if ((a & 2 | a & 4) != 0) 1 else 0
      case 0x10 => // greater than - unsigned
        if ((a & 2 | a & 4) == 0) 1 else 0
      case 0x11 => // greater than or equal to - unsigned
        if ((a & 2) == 0) 1 else 0
    }
    result & 0xFFFF
  }
}

class Memory(val size: Int = Short.MaxValue + 1) {
  
  private[tfc] var trace = false
  if (size > Short.MaxValue + 1) throw new Exception("inaccessible memory size")
  
  val mem = new Array[Byte](size)
  
  def read8(pos: Int) = mem(pos) & 0xFF
  def read16(pos: Int) = (read8(pos) << 8) | (read8(pos+1) & 0xFF)
  def write8(pos: Int, value: Int) = {
    if (value > 0xFF) throw new Exception("invalid write")
    mem(pos) = value.toByte
  }
  def write16(pos: Int, value: Int) = {
    if (trace) print("M[" + pos + "]=" + value)
    write8(pos, value >> 8)
    write8(pos + 1, value & ~0xFF00)
  }
  
}