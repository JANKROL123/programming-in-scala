val helloStr = "Hello, World!"

@main
def main_1: Unit = {
  println(helloStr)
}

@main
def main_2(n: Int): Unit = {
  println(s"Commandline argument: $n")
}

def bar: String = "-" * 80

@main
def main_3(name: String, age: Int, playsFootball: Boolean, rest: String*): Unit = {
  val playerStatus = if playsFootball then "plays" else "doesn't play"
  println(bar)
  println(s"$name is $age years old and $playerStatus football.")
  if (rest.length > 0) {
    println(bar)
    println("Some extra (string) arguments:")
    for (str <- rest) println(str)
    println(bar)
  }
  println("That's all for today ;)")
  println(bar)
}
