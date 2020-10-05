//package com.evolutiongaming.bootcamp.basics

import javax.naming.spi.DirStateFactory.Result

import scala.io.Source
import scala.util.{Try, Success, Failure}

object ControlStructuresHomework {
  // Homework

  // Create a command line application that reads various "commands" from the
  // stdin, evaluates them, and writes output to stdout.

  // Commands are:

  //   divide 4 5
  // which should output "4 divided by 5 is 0.8"

  //   sum 5 5 6 8.5
  // which should output "the sum of 5 5 6 8.5 is 24.5"

  //   average 4 3 8.5 4
  // which should output "the average of 4 3 8.5 4 is 4.875"

  //   min 4 -3 -17
  // which should output "the minimum of 4 -3 -17 is -17"

  //   max 4 -3 -17
  // which should output "the maximum of 4 -3 -17 is 4"

  // In case of commands that cannot be parsed or calculations that cannot be performed,
  // output a single line starting with "Error: "

  sealed trait Command
  object Command {
    final case class Divide(dividend: Double, divisor: Double) extends Command
    final case class Sum(numbers: List[Double]) extends Command
    final case class Average(numbers: List[Double]) extends Command
    final case class Min(numbers: List[Double]) extends Command
    final case class Max(numbers: List[Double]) extends Command
  }

  final case class ErrorMessage(value: String)

  sealed trait Result {
    val value: String
  }
  final case class ChangeMe(value: String) extends Result // adjust Result as required to match requirements

  def parseCommand(x: String): Either[ErrorMessage, Command] = {
    // implement this method
    // Implementation hints:
    // You can use String#split, convert to List using .toList, then pattern match on
    val commandList: List[String] = x.split(" ").toList
    val numbers: List[Double] = commandList.tail
                                    .map(s => Try(s.toDouble))
                                    .collect { case Success(x) => x }
    commandList.head match {
      case "divide" => Right(Command.Divide(commandList(1).toDouble, commandList(2).toDouble))
      case "sum" => Right(Command.Sum(numbers))
      case "average" => Right(Command.Average(numbers))
      case "min" => Right(Command.Min(numbers))
      case "max" => Right(Command.Max(numbers))
      case _ => Left(ErrorMessage(s"Command '${commandList.head}' not implemented"))
    }
    // Consider how to handle extra whitespace gracefully (without errors).
  }

  // should return an error (using `Left` channel) in case of division by zero and other
  // invalid operations
  def calculate(x: Command): Either[ErrorMessage, Result] = {
    x match {
      case Command.Divide(dividend: Double, divisor: Double) =>
        Right(ChangeMe(s"divide $dividend $divisor ${(dividend / divisor).toString}"))
      case Command.Sum(numbers: List[Double]) =>
        Right(ChangeMe(s"sum ${numbers.mkString(" ")} ${numbers.sum}"))
      case Command.Average(numbers: List[Double]) =>
        Right(ChangeMe(s"average ${numbers.mkString(" ")} ${numbers.sum / numbers.size}"))
      case Command.Min(numbers: List[Double]) =>
        Right(ChangeMe(s"min ${numbers.mkString(" ")} ${numbers.min}"))
      case Command.Max(numbers: List[Double]) =>
              Right(ChangeMe(s"max ${numbers.mkString(" ")} ${numbers.max}"))
      case _ => Left(ErrorMessage("calculate not implemented yet")) // implement this method
    }
  }

  def renderResult(x: Result): String = {
    val res: List[String] = x.value.split(" ").toList
    res.head match {
      case "divide" => s"${res(1)} divided by ${res(2)} is ${res(3)}}"
      case "sum" => s"the sum of ${res.slice(1, res.size - 1).mkString(" ")} is ${res.last}"
      case "average" => s"the average of ${res.slice(1, res.size - 1).mkString(" ")} is ${res.last}"
      case "min" => s"the minimum of ${res.slice(1, res.size - 1).mkString(" ")} is ${res.last}"
      case "max" => s"the maximum of ${res.slice(1, res.size - 1).mkString(" ")} is ${res.last}"
      case _ => s"Could not renderResult '${x.value}''"
    }
  }

  def process(x: String): String = {
    //import cats.implicits._
    // the import above will enable useful operations on Either-s such as `leftMap`
    // (map over the Left channel) and `merge` (convert `Either[A, A]` into `A`),
    // but you can also avoid using them using pattern matching.

    // implement using a for-comprehension

    val command: Either[ErrorMessage, Command] = parseCommand(x)

    val result: Either[ErrorMessage, Result] = command match {
      case Left(errorMessage) => Left(errorMessage)
      case Right(command) => calculate(command)
    }

    val answer: String = result match {
      case Left(errorMessage) => errorMessage.value
      case Right(resultNumber) => renderResult(resultNumber)
    }

    return answer
  }

  // This `main` method reads lines from stdin, passes each to `process` and outputs the return value to stdout
  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}
