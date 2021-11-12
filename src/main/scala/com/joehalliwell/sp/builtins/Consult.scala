package com.joehalliwell.sp

package builtin {
  object Consult extends Builtin {
    def exec(state: State, args: Seq[Term])(implicit context: Prolog): Option[State] = {
      consult(args.head.toString)
      state.push()
    }

    def consult(filename: String)(implicit context: Prolog) = {
      context.console.println("Loading " + filename)
      var lineNumber = -1;
      for ((line, index) <- scala.io.Source
             .fromFile(filename)
             .getLines()
             .zipWithIndex
             .filter(!_._1.isEmpty)
             .filter(!_._1.startsWith("%"))) {
        // Save the line number in case of error
        lineNumber = index
        context.parser.parse(line.trim) match {
          case Right(fact) => context.assert(fact)
          case Left(emsg)  => context.console.println("Syntax error in line " + (lineNumber + 1) + ": " + emsg)
        }
      }
    }
  }
}
