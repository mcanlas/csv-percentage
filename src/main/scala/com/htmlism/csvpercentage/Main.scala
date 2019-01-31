package com.htmlism.csvpercentage

import scala.io.Source

import mouse.any._

object Main {
  private val scrubDict = Set("android", "iPhone", "iPad")

  def main(args: Array[String]): Unit = {
    Source
      .fromFile(args(0))
      .getLines()
      .map(toCells)
      .map(_.map(scrub(scrubDict)))
      .toList
      .tail |> Analyzer.analyze // tail used to burn header
  }

  def toCells(s: String): Row =
    s
      .replace("\"", "")
      .split(",")
      .toList

  def scrub(stack: Set[String])(s: String): String =
    if (stack.isEmpty)
      s
    else {
      val smashWord = stack.head // random lul

      if (s.contains(smashWord))
        scrub(stack - smashWord)(smashWord)
      else
        scrub(stack - smashWord)(s)
    }
}
