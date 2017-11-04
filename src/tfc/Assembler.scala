package tfc

import java.io.File

object AssemblyParser {
  import parsers._
  
  sealed trait Token
  sealed trait AccRef { self: Token => }
  object T {
    case class Label(name: String) extends Token with AccRef
    case class Const(value: Int) extends Token with AccRef
    case object Colon extends Token
  }
  object Kv {
    case object Acc extends Token with AccRef
    sealed trait CodeTok extends Token
    case object Ld extends CodeTok
    case object Push extends CodeTok
    case object Pop extends CodeTok with AccRef
    case object Ccl extends CodeTok
    case object Csh extends CodeTok
    case object Grab extends CodeTok
    case object Stash extends CodeTok
    case object Lkup extends CodeTok
    case object Set extends CodeTok
    val dataManipulation_0 = Vector(Push, Pop, Ccl, Csh, Grab, Stash, Lkup, Set)
    case object Nop extends CodeTok
    case object Halt extends CodeTok
    case object Jc extends CodeTok
    case object Jnl extends CodeTok
    case object Dbg extends CodeTok
    val controlFlow_1 = Vector(Nop, Halt, Jc, Jnl, Dbg)
    sealed trait Arith2Tok extends CodeTok
    case object Add extends Arith2Tok
    case object Sub extends Arith2Tok
    case object Mul extends Arith2Tok
    case object Div extends Arith2Tok
    case object Mod extends Arith2Tok
    case object Cmp extends Arith2Tok
    case object Band extends Arith2Tok
    case object Bor extends Arith2Tok
    case object Bxor extends Arith2Tok
    case object Shr extends Arith2Tok
    case object Shl extends Arith2Tok
    case object Sshr extends Arith2Tok
    val arith2_2 = Vector(Add, Sub, Mul, Div, Mod, Cmp, Band, Bor, Bxor ,Shr, Shl, Sshr)
    sealed trait Arith1Tok extends CodeTok
    case object Neg extends Arith1Tok
    case object Inv extends Arith1Tok
    case object L extends Arith1Tok
    case object Le extends Arith1Tok
    case object G extends Arith1Tok
    case object Ge extends Arith1Tok
    case object Eq extends Arith1Tok
    case object Ne extends Arith1Tok
    case object Lu extends Arith1Tok
    case object Leu extends Arith1Tok
    case object Gu extends Arith1Tok
    case object Geu extends Arith1Tok
    val arith1_3 = Vector(Neg, Inv, L, Le, G, Ge, Eq, Ne, Lu, Leu, Gu, Geu)
    def minor(ops: Vector[CodeTok]) = ops.zipWithIndex.map {
      case (Dbg, _) => Dbg -> 0xF
      case (a, b) => a -> b
    }.toMap
    def major(ops: Vector[Token], major: Int) = ops.map(_ -> major).toMap
    val ops: Vector[CodeTok] = dataManipulation_0 ++ controlFlow_1 ++ arith2_2 ++ arith1_3
    val minors = minor(dataManipulation_0) ++ minor(controlFlow_1) ++ minor(arith2_2) ++ minor(arith1_3)
    val majors = major(dataManipulation_0, 0) ++ major(controlFlow_1, 1) ++ major(arith2_2, 2) ++ major(arith1_3, 3)
    val codes = ops.map(code => code -> ((majors(code) << 4) | minors(code)).toByte).toMap
    val kvs = ops ++ Vector(Acc, Ld)
    val advancedOps = Vector(
        Ccl, Csh,// simplified into Ld
        Ld,// special handling
        Push, Grab, Stash, Jc, Jnl, Dbg) // use acc
    val accOps = Vector(Push, Grab, Stash, Jc, Jnl, Dbg)
    val basicOps = ops.filterNot(advancedOps contains _)
    val kvMap = kvs.map(kv => kv.toString.toLowerCase -> kv).toMap
  }
  
  def in(set: Set[Char]) = char withFilter (set contains _)
  lazy val idparser = {
    val validIdStartChar = (('a' to 'z') ++ ('A' to 'Z') ++ Set('_')).toSet
    val validIdChar = (validIdStartChar ++ ('0' to '9')).toSet
    (in(validIdStartChar) ** in(validIdChar).star).map {
      case a ** bs => a + bs.mkString
    }
  }
  lazy val kv = idparser map {
    case x if Kv.kvMap.contains(x) => Kv.kvMap(x)
    case x => T.Label(x)
  }
  lazy val colon = lit(":") replace T.Colon
  lazy val _tok: Parser[Token] = kv ++ const ++ colon
  lazy val whitespace = {
    lazy val spacechar = char withFilter (_.isWhitespace)
    spacechar.star replace ()
  }
  lazy val tok = whitespace *> _tok *< whitespace
  lazy val basicOp = for {
    t <- tok
    if Kv.basicOps.contains(t)
  } yield Assembly.BasicInstr(t.asInstanceOf[Kv.CodeTok])
  lazy val const = {
    val validNumlitChar = ('0' to '9').toSet + '-'
    in(validNumlitChar).plus.map {
      case ds => T.Const(ds.mkString.toInt)
    }
  }
  lazy val acc = {
    val a = for (Kv.Acc <- tok) yield Kv.Acc
    val l = for (T.Label(l) <- tok) yield T.Label(l)
    val p = for (Kv.Pop <- tok) yield Kv.Pop
    const ++ a ++ l ++ p
  }
  lazy val accOp = for {
    t <- tok
    if Kv.accOps.contains(t)
    a <- acc
  } yield Assembly.AccInstr(t.asInstanceOf[Kv.CodeTok], a)
  lazy val load = for {
    Kv.Ld <- tok
    k <- const
  } yield Assembly.LdInstr(k)
  lazy val op = basicOp ++ accOp ++ load
  lazy val label = for {
    T.Label(l) <- tok
    T.Colon <- tok
  } yield Assembly.Label(l)
  lazy val part = op ++ label
  def parser: Parser[Vector[Assembly.Part]] = part.star
  
}

object Assembly {
  sealed trait Part {
    def size: Int
  }
  case class Label(name: String) extends Part {
    val size = 0
    override def toString = name + ":"
  }
  sealed trait Instr extends Part
  case class BasicInstr(op: AssemblyParser.Kv.CodeTok) extends Instr {
    val size = 1
    override def toString = "    " + op.toString.toLowerCase
  }
  case class AccInstr(op: AssemblyParser.Kv.CodeTok, acc: AssemblyParser.AccRef) extends Instr {
    val size = acc match {
      case AssemblyParser.T.Const(k) =>
        if (k < 128 && k >= -128) 3
        else 5
      case AssemblyParser.T.Label(_) => 5 // conservative estimate
      case AssemblyParser.Kv.Acc => 1
      case AssemblyParser.Kv.Pop => 2
    }
    override def toString = "    " + op.toString.toLowerCase + " " + (acc match {
      case AssemblyParser.T.Const(k) => k.toString
      case AssemblyParser.T.Label(l) => l
      case x => x.toString.toLowerCase
    })
  }
  case class LdInstr(const: AssemblyParser.T.Const) extends Instr {
    val size =
      if (const.value < 128 && const.value >= -128) 2
      else 4
    override def toString = "    ld " + const.value
  }
}

class Assembler {
  import Assembly._
  
  def assemble(assembly: Vector[Part]) = {
    val labelIxes = assembly.zipWithIndex.collect { case (Label(name), ix) => Label(name) -> ix }.toMap
    // estimate the byte position of each instruction
    val lengthEst0 = assembly.scanLeft(0)(_ + _.size)
    def dist0(src: Int, tgt: Int) = lengthEst0(tgt) - lengthEst0(src)
    // improve estimate
    import AssemblyParser._
    val sizeEst1 = assembly.zipWithIndex.map {
      case (AccInstr(Kv.Jc, T.Label(l)), src) =>
        val tgt = labelIxes(Label(l))
        val diff = dist0(src + 1, tgt) + 1
        if (diff < 128 && diff >= -128) 3
        else 5
      case (AccInstr(_, T.Label(l)), src) =>
        val tgt = labelIxes(Label(l))
        val diff = dist0(0, tgt)
        if (diff < 128 && diff >= -128) 3
        else 5
      case (x, _) => x.size
    }
    val lengthEst1 = sizeEst1.scanLeft(0)(_ + _)
    def dist1(src: Int, tgt: Int) = lengthEst1(tgt) - lengthEst1(src)
    val parts: Vector[Array[Byte]] = {
      assembly.zipWithIndex.map {
        case (Label(_), _) => Array[Byte]()
        case (BasicInstr(op), _) => Array(Kv.codes(op))
        case (AccInstr(op, T.Const(k)), _) =>
          if (k < 128 && k >= -128)
            Array(Kv.codes(Kv.Ccl), k.toByte,
                Kv.codes(op))
          else Array(Kv.codes(Kv.Ccl), (k >> 8).toByte,
              Kv.codes(Kv.Csh), (k & 0xFF).toByte,
              Kv.codes(op))
        case (AccInstr(Kv.Jc, T.Label(l)), src) =>
          val tgt = labelIxes(Label(l))
          val d0 = dist0(src + 1, tgt) + 1
          val d1 = dist1(src + 1, tgt) + 1
          if (d0 < 128 && d0 >= -128)
            Array(Kv.codes(Kv.Ccl), d1.toByte, Kv.codes(Kv.Jc))
          else
            Array(Kv.codes(Kv.Ccl), (d1 >> 8).toByte, Kv.codes(Kv.Csh), (d1 & 0xFF).toByte, Kv.codes(Kv.Jc))
        case (AccInstr(op, T.Label(l)), _) =>
          val tgt = labelIxes(Label(l))
          val d0 = dist0(0, tgt)
          val d1 = dist1(0, tgt)
          if (d0 < 128 && d0 >= -128)
            Array(Kv.codes(Kv.Ccl), d1.toByte, Kv.codes(op))
          else
            Array(Kv.codes(Kv.Ccl), (d1 >> 8).toByte, Kv.codes(Kv.Csh), (d1 & 0xFF).toByte, Kv.codes(op))
        case (AccInstr(op, Kv.Acc), _) =>
          Array(Kv.codes(op))
        case (AccInstr(op, Kv.Pop), _) =>
          Array(Kv.codes(Kv.Pop), Kv.codes(op))
        case (LdInstr(k), _) =>
          if (k.value < 128 && k.value >= -128) Array(Kv.codes(Kv.Ccl), k.value.toByte)
          else Array(Kv.codes(Kv.Ccl), (k.value >> 8).toByte, Kv.codes(Kv.Csh), (k.value & 0xFF).toByte)
      }
    }
    for ((((begin, part), instr), ix) <- lengthEst1.zip(parts).zip(assembly).zipWithIndex) {
      print(begin.formatted("%4x") + " | ")
      for (i <- 0 until 5) {
        if (i < part.length) {
          print(((part(i) >> 4) & 0xF).toHexString)
          print((part(i) & 0xF).toHexString)
          print(" ")
        }
        else print("   ")
      }
      println(" | " + ix.formatted("%5d") + " " + instr)
    }
    parts.toArray.flatten
  }
}
object Assembler extends Assembler {
  def main(args: Array[String]) = {
    val file = new File("TailcalledFantasyConsole/code")
    val asm = parsers.parse(AssemblyParser.parser, file)
    val bytes = assemble(asm)
    val cpu = new CPU(trace = false)
    cpu.load(bytes)
    while (!cpu.halted) {
      cpu.step()
    }
    ()
  }
}