package com.joehalliwell.sp.io

class ConsoleSimple(var prompt: String = "") extends Console {
  def setPrompt(prompt: String): Unit = this.prompt = prompt
  def println(value: Any): Unit       = scala.Console.println(value.toString)
  def print(value: Any): Unit         = scala.Console.print(value)
  def readLine(): String              = scala.Console.in.readLine()
}
