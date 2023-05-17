def sumuj(l:List[Option[Double]]): Option[Double] = {
  @annotation.tailrec
  def recSumuj(l:List[Option[Double]], acum:Option[Double]): Option[Double] = {
    (l,acum) match {
      case (Some(hd)::tl, None) if hd > 0 => recSumuj(tl, Some(hd))
      case (Some(hd)::tl, Some(x)) if hd > 0 => recSumuj(tl, Some(x+hd))
      case (_::tl, _) => recSumuj(tl, acum)
      case (Nil, _) => acum
    }
  } 
  recSumuj(l, None)
}
def maksimum(l1:List[Double], l2:List[Double]): List[Double] = {
  @annotation.tailrec
  def recMaksimum(l1:List[Double], l2:List[Double], acum:List[Double]): List[Double] = {
    (l1, l2) match {
      case (hd1::tl1, hd2::tl2) if hd1 >= hd2 => recMaksimum(tl1, tl2, acum :+ hd1)
      case (hd1::tl1, hd2::tl2) if hd1 < hd2 => recMaksimum(tl1, tl2, acum :+ hd2)
      case (Nil, _) => acum ++ l2
      case (_, Nil) => acum ++ l1
    }
  }
  recMaksimum(l1, l2, List())
}
def usun[A](l:List[A], el:A): List[A] = {
  @annotation.tailrec
  def recUsun[A](l:List[A], el:A, acum:List[A]): List[A] = {
    l match {
      case hd::tl if hd != el => recUsun(tl, el, acum :+ hd)
      case hd::tl if hd == el => recUsun(tl, el, acum)
      case Nil => acum
    }
  }
  recUsun(l, el, List())
}
def divide[A](l:List[A]): (List[A], List[A]) = {
  @annotation.tailrec
  def recDivide[A](l:List[A], idx:Int, even:List[A], odd:List[A]): (List[A], List[A]) = {
    idx == l.length match {
      case true => (even,odd)
      case false => idx % 2 == 0 match {
        case true => recDivide(l, idx+1, even :+ l(idx), odd)
        case false => recDivide(l, idx+1, even, odd :+ l(idx))
      }
    }
  }
  recDivide(l, 0, List(), List())
}
type Pred[A] = A => Boolean
def and[A](p:Pred[A], q:Pred[A]): Pred[A] = {
  a => p(a) && q(a)
}
def or[A](p:Pred[A], q:Pred[A]): Pred[A] = {
  a => p(a) || q(a)
}
def not[A](p:Pred[A]): Pred[A] = {
  a => !p(a)
}
def imp[A](p:Pred[A], q:Pred[A]): Pred[A] = {
  a => !p(a) || q(a)
}
@main
def zadanie_10: Unit = {
  val lista = List(Some(4.0), Some(-3.0), None, Some(1.0), Some(0.0))
  val wynik = sumuj(lista)
  println(wynik)
}
@main
def zadanie_11: Unit = {
  val lista1 = List(2.0, -1.6, 3.2, 5.4, -8.4)
  val lista2 = List(3.3, -3.1, 3.2, -4.1, -0.4, 5.5)
  val wynik = maksimum(lista1, lista2) 
  println(wynik)
}
@main
def zadanie_12: Unit = {
  val lista = List(2, 1, 4, 1, 3, 3, 1, 2)
  val wynik = usun(lista, 1) 
  println(wynik)
}
@main
def zadanie_13: Unit = {
  val lista = List(1, 3, 5, 6, 7)
  val wynik = divide(lista) 
  println(wynik)
}
@main
def zadanie_14: Unit = {
  val isEven = (n:Int) => n % 2 == 0
  val biggerThanFive = (n:Int) => n > 5 
  val tryAND = and(isEven, biggerThanFive)
  val tryOR = or(isEven, biggerThanFive)
  val tryNOT = not(isEven)
  val tryIMP = imp(isEven, biggerThanFive)
  println(tryAND(7))
  println(tryOR(7))
  println(tryNOT(7))
  println(tryIMP(7))
}