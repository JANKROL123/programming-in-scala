def sumOpts(l:List[Option[Double]]): Option[Double] = {
  val init:Option[Double] = None
  l.foldLeft(init)((acum,item) => (acum,item) match {
    case (None,_) => item
    case (Some(x),Some(y)) => Some(x+y)
    case (_,None) => acum
  })
}
def position[A](l:List[A], el:A): Option[Int] = {
  l.zipWithIndex.collectFirst({
    case n if n._1 == el => n._2
  })
}
def indices[A](l:List[A], el:A): Set[Int] = {
  l.zipWithIndex.collect({
    case n if n._1 == el => n._2
  }).toSet
}
def swap[A](l:List[A]): List[A] = {
  l.grouped(2).toList.map(n => n match {
    case List(a,b) => List(b,a)
    case List(a) => List(a)
  }).flatten
}
def freq[A](l:List[A]): List[(A,Int)] = {
  //l.groupMapReduce(n => n)(n => 1)(_+_).toList List[(A,Int)]
  l.groupBy(n => n).mapValues(n => n.map(m => 1).sum).toList
}
@main
def zadanie_24: Unit = {
  val lista = List(Some(5.4), Some(-2.0), Some(1.0), None, Some(2.6))
  println(sumOpts(lista))
  println(sumOpts(List()))
  println(sumOpts(List(None,None,None)))
}
@main
def zadanie_25: Unit = {
  val lista = List(2, 1, 1, 5)
  println(position(lista,1))
  println(position(lista,3))
  println(position(lista,2))
  println(position(lista,5))
}
@main
def zadanie_26: Unit = {
  val lista = List(1, 2, 1, 1, 5)
  println(indices(lista, 1)) 
  println(indices(lista, 7)) 
}
@main
def zadanie_27: Unit = {
  val lista = List(1, 2, 3, 4, 5)
  println(swap(lista))
}
@main
def zadanie_28: Unit = {
  val strefy: List[String] = java.util.TimeZone.getAvailableIDs.toList
  val result28 = strefy.collect({
    case n if n.startsWith("Europe/") => n.stripPrefix("Europe/")
  }).sortBy(n => (n.length, n))
  println(result28)
}
@main
def zadanie_29: Unit = {
  val lista = List('a','b','a','c','c','a')
  println(freq(lista))
}