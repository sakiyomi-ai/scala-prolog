package com.joehalliwell.sp.io

trait Console {
  def setPrompt(prompt: String): Unit
  def println(value: Any): Unit
  def print(value: Any): Unit
  def readLine(): String
}
