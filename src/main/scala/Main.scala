import io.quarkus.runtime.QuarkusApplication
import io.quarkus.runtime.annotations.QuarkusMain

import com.joehalliwell.sp.REPL

@QuarkusMain
class Main extends QuarkusApplication {

  @Override
  def run(args: String*): Int = {
    REPL.main(args.toArray)
    0
  }

}
