package com.joehalliwell.sp.io

import jline.console.ConsoleReader

class ConsoleImpl extends Console {
  val device = new ConsoleReader()

  def setPrompt(prompt: String): Unit = device.setPrompt(prompt)
  def println(value: Any): Unit       = device.println(value.toString)
  def print(value: Any): Unit         = device.print(value.toString)
  def readLine(): String              = device.readLine()

}
