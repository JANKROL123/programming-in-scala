def subseq[A](list:List[A], begIdx:Int, endIdx:Int): List[A] = list.take(endIdx+1).drop(begIdx)
def pairPosNeg(list:List[Double]): (List[Double], List[Double]) = list.filter(n => n != 0).partition(n => n < 0)
def deStutter[A](list:List[A]): List[A] = {
  list.foldLeft(List[A]())((acum,item) => acum match {
    case Nil => item::acum
    case hd::_ if hd != item => item::acum
    case _ => acum
  }).reverse
}
def remElems[A](list:List[A], k:Int): List[A] = list.zipWithIndex.filter(n => n._2 != k).map(n => n._1)
def freqMax[A](list:List[A]): (Set[A],Int) = {
  list match {
    case Nil => (Set(), 0)
    case _ => 
      val set = list.toSet.map(n => (n, list.count(m => m == n)))
      val myMax = set.maxBy(n => n._2)._2
      (set.collect({
        case n if n._2 == myMax => n._1 
      }), myMax)
  }
}
@main
def zadanie_19: Unit = {
  val lista = List(1,2,3,4,5,6,7,8) 
  println(subseq(lista, 2, 3))
  println(subseq(lista, 2, 1))
  println(subseq(lista, -1, 10))
}
@main
def zadanie_20: Unit = {
  val lista: List[Double] = List(1, -2, 0, 4, 5, 0, -7, 8)
  println(pairPosNeg(lista))
}
@main
def zadanie_21: Unit = {
  val l = List(1, 1, 2, 4, 4, 4, 1, 3)
  println(deStutter(l))
  println(deStutter(List(1,1,1,1,1)))
  println(deStutter(List(1,1,1,1,1,2)))
  println(deStutter(List(1,1,2,1,1,1)))
  println(deStutter(List()))
}
@main
def zadanie_22: Unit = {
  val l = List(1, 1, 2, 4, 4, 1, 3)
  println(remElems(l, 2)) 
  println(remElems(l, -1)) 
  println(remElems(l, 15)) 
}
@main
def zadanie_23: Unit = {
  println(freqMax(List(1, 1, 2, 4, 4, 3, 4, 1, 3)))
  println(freqMax(List(1, 2, 4, 4, 3, 4, 1, 3)))
  println(freqMax(List(1, 1, 2, 4, 4, 3, 4, 1, 3, 1)))
  println(freqMax(List(1, 1, 2, 4, 4, 3, 4, 1, 3, 3)))
  println(freqMax(List()))
}