package aoc2020.day8

import aoc2020.Util._
import cats.Eval
import cats.data.State

object Day8 {
  
  enum Instruction(val arg: Int) {
    case Acc(override val arg: Int) extends Instruction(arg)
    case Jmp(override val arg: Int) extends Instruction(arg)
    case Nop(override val arg: Int) extends Instruction(arg)
  }

  object Instruction {
    def fromString(s: String)(arg: Int): Instruction =
      s match {
        case "acc" => Acc(arg)
        case "jmp" => Jmp(arg)
        case "nop" => Nop(arg)
      }
  }

  case class ProgramState(val acc: Long = 0, val history: List[Int] = List())
  
  enum ProgramResult{
    case InfiniteLoop(lastState: Option[ProgramState] = None) extends ProgramResult
    case Terminated(endState: ProgramState) extends ProgramResult
  }
  
  def run(
      program: Vector[Instruction],
      programState: ProgramState = ProgramState()
  )(pos: Int = 0): ProgramResult = {
    if (programState.history.contains(pos)) ProgramResult.InfiniteLoop(Some(programState))
    else if (pos == program.size) ProgramResult.Terminated(programState)
    else program(pos) match {
        case Instruction.Acc(arg) =>
          run(program, programState.copy(acc = programState.acc + arg, history = programState.history :+ pos))(pos + 1)
        case Instruction.Jmp(arg) => 
          run(program, programState.copy(history = programState.history :+ pos))(pos + arg)
        case Instruction.Nop(arg) => 
          run(program, programState.copy(history = programState.history :+ pos))(pos + 1)
      }
  }

  def main(args: Array[String]): Unit = {
    val program = readLines("input/day8.txt")
      .map {
        case s"$instruction $arg" =>
          Instruction.fromString(instruction)(arg.toInt)
      }.toVector
    
    val ProgramResult.InfiniteLoop(Some(ProgramState(p1, _))) = run(program)()

    val allRuns = for (i <- 0 until program.size)
      yield program(i) match {
        case Instruction.Jmp(arg) => run(program.updated(i, Instruction.Nop(arg)))()
        case Instruction.Nop(arg) => run(program.updated(i, Instruction.Jmp(arg)))()
        case _ => ProgramResult.InfiniteLoop()
      } 
    
    val Some(ProgramState(p2, _)) = allRuns.collectFirst { case ProgramResult.Terminated(state) => state }
    
    println(p1)
    println(p2)
  }
}
