@main
def zadanie_33: Unit = {
  val dane = io.Source.fromFile("nazwiska.txt").getLines().toList

  val difficultFName = dane.map(n => n.split(" ").toList.head.toLowerCase.toSet.size).max

  val shortLName = dane.map(n => n.split(" ").toList).filter(n => n match {
    case hd::_ => hd.toLowerCase.toSet.size == difficultFName
  }).map(n => n.tail.mkString(" ").length).min

  val result = dane.map(n => n.split(" ").toList match {
    case List(a,b,c) => (a.toLowerCase.toSet.size, (b+c).length, n)
    case List(a,b) => (a.toLowerCase.toSet.size, b.length, n)
  }).sortBy(n => (-n._1, n._2)).takeWhile(n => n._2 == shortLName)
  .map(n => n match {case (_,_,name) => name})


  println(result)
}


@main
def histogram(max:Int): Unit = {
  val data = io.Source.fromFile("ogniem_i_mieczem.txt").getLines().toList

  val result = data.flatMap(n => n.toList).collect({case n if n.isLetter => n.toLower})
               .groupMapReduce(n => n)(n => 1)(_+_).toList.sortBy(n => -n._2)

  val myMax = result.maxBy(n => n._2)._2

  val factor = max.toDouble / myMax
  
  result.foreach(n => n match {
    case (letter,occurences) => println(s"${letter}: ${"*"*(occurences*factor).toInt}")
  })
}
