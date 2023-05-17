def oczyść[A](l:List[A]): List[A] = {
  @annotation.tailrec
  def recOczyść(l:List[A], acum:List[A]): List[A] = {
    (l,acum) match {
      case (Nil,_) => acum.reverse
      case (hd::tl,Nil) => recOczyść(tl, hd::acum)
      case (hd::tl,acumHD::_) if hd == acumHD => recOczyść(tl,acum)
      case (hd::tl,acumHD::_) if hd != acumHD => recOczyść(tl,hd::acum)
    }
  }
  recOczyść(l, List())
}
def skompresuj[A](l:List[A]): List[(A,Int)] = {
  @annotation.tailrec
  def recSkompresuj(l:List[A], bigAcum:List[(A,Int)], smallAcum:List[A]): List[(A,Int)] = {
    (l,smallAcum) match {
      case (hd::tl,Nil) => recSkompresuj(tl, bigAcum, smallAcum:+hd)
      case (hd::tl,acumHD::_) if hd == acumHD => recSkompresuj(tl, bigAcum, smallAcum:+hd)
      case (hd::_,acumHD::_) if hd != acumHD => recSkompresuj(l, bigAcum:+(acumHD,smallAcum.length), List())
      case (Nil,acumHD::_) => bigAcum :+ (acumHD,smallAcum.length)
      case (Nil,_) => bigAcum
    }
  }
  recSkompresuj(l, List(), List())
}
def isOrdered[A](leq:(A,A) => Boolean)(l:List[A]): Boolean = {
  @annotation.tailrec
  def recIsOrdered(l:List[A], acum:Option[A]): Boolean = {
    (l,acum) match {
      case (hd::tl,None) => recIsOrdered(tl,Some(hd))
      case (hd::tl,Some(x)) => leq(x,hd) match {
        case true => recIsOrdered(tl,Some(hd))
        case false => false
      }
      case (Nil,_) => true
    }
  }
  recIsOrdered(l, None)
}
def applyForAll[A,B](l:List[A])(f:A => B): List[B] = {
  @annotation.tailrec
  def recApplyForAll(l:List[A], acum:List[B]): List[B] = {
    l match {
      case hd::tl => recApplyForAll(tl, acum:+f(hd))
      case Nil => acum
    }
  }
  recApplyForAll(l,List())
}
@main
def zadanie_15: Unit = {
  val lista = List(1, 1, 2, 4, 4, 4, 1, 3)
  println(oczyść(lista))
}
@main
def zadanie_16: Unit = {
  val lista = List('a', 'a', 'b', 'c', 'c', 'c', 'a', 'a', 'b', 'd')
  println(skompresuj(lista))
}
@main
def zadanie_17: Unit = {
  val lt = (m: Int, n: Int) => m < n
  val lte = (m: Int, n: Int) => m <= n
  val lista = List(1, 2, 2, 5)
  println(isOrdered(lt)(lista))
  println(isOrdered(lte)(lista))
}
@main
def zadanie_18: Unit = {
  val lista = List(1, 3, 5)
  val f = (n: Int) => n + 3
  println(applyForAll(lista)(f))
}