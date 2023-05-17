def reverse(str:String): String = {
  @annotation.tailrec
  def recReverse(str:List[Char], acum:String): String = {
    str match {
      case hd::tl => recReverse(tl, s"$hd$acum")
      case Nil => acum
    }
  }
  recReverse(str.toList, "")
}
def pierwsza(n:Int): Boolean = {
  @annotation.tailrec
  def recPierwsza(n:Int, tab:List[Int], status:Boolean): Boolean = {
    tab match {
      case hd::tl => recPierwsza(n, tl, status && (n%hd != 0))
      case Nil => status
    }
  }
  if (n < 0) false 
  else {
    n match {
      case 1 => false
      case 2 => true
      case _ => recPierwsza(n, (2 until n).toList, true)
    }
  }
}
def ciąg(n:Int): Int = {
  @annotation.tailrec
  def recCiag(n:Int, idx:Int, a:Int, b:Int): Int = {
    idx == n match {
      case true => a+b 
      case false => recCiag(n, idx+1, b, a+b)
    }
  }
  n match {
    case 0 => 2 
    case 1 => 1
    case _ => recCiag(n, 2, 2, 1)
  }
}
def tasuj(l1:List[Int], l2:List[Int]): List[Int] = {
  @annotation.tailrec
  def recTasuj(l1:List[Int], l2:List[Int], acum:List[Int]): List[Int] = {
    (l1,l2,acum) match {
      case (hd1::tl1, hd2::tl2, Nil) if hd1 > hd2 => recTasuj(l1, tl2, acum:+hd2)
      case (hd1::tl1, hd2::tl2, Nil) if hd1 < hd2 => recTasuj(tl1, l2, acum:+hd1)
      case (hd1::tl1, hd2::tl2, Nil) if hd1 == hd2 => recTasuj(l1, tl2, acum:+hd2)
      case (Nil, hd2::tl2, Nil) => recTasuj(l1, tl2, acum:+hd2)
      case (Nil, hd2::tl2, Nil) => recTasuj(l1, tl2, acum)
      case (hd1::tl1, Nil, Nil) => recTasuj(tl1, l2, acum:+hd1)
      case (hd1::tl1, Nil, Nil) => recTasuj(tl1, l2, acum)
      case (hd1::tl1, hd2::tl2, _) if hd1 > hd2 && acum.last != hd2 => recTasuj(l1, tl2, acum:+hd2)
      case (hd1::tl1, hd2::tl2, _) if hd1 < hd2 && acum.last != hd1 => recTasuj(tl1, l2, acum:+hd1)
      case (hd1::tl1, hd2::tl2, _) if hd1 > hd2 && acum.last == hd2 => recTasuj(l1, tl2, acum)
      case (hd1::tl1, hd2::tl2, _) if hd1 < hd2 && acum.last == hd1 => recTasuj(tl1, l2, acum)
      case (hd1::tl1, hd2::tl2, _) if hd1 == hd2 => recTasuj(l1, tl2, acum)
      case (Nil, hd2::tl2, _) if acum.last != hd2 => recTasuj(l1, tl2, acum:+hd2)
      case (Nil, hd2::tl2, _) if acum.last == hd2 => recTasuj(l1, tl2, acum)
      case (hd1::tl1, Nil, _) if acum.last != hd1 => recTasuj(tl1, l2, acum:+hd1)
      case (hd1::tl1, Nil, _) if acum.last == hd1 => recTasuj(tl1, l2, acum)
      case (Nil, Nil, _) => acum
    }
  }
  recTasuj(l1, l2, List())
}
@main
def zadanie_06(arg:String): Unit = {
  println(reverse(arg))
}
@main
def zadanie_07(arg:Int): Unit = {
  println(pierwsza(arg))
}
@main
def zadanie_08(arg:Int): Unit = {
  println(ciąg(arg))
}
@main
def zadanie_09: Unit = {
  val lista1 = List(2, 4, 3, 5)
  val lista2 = List(1, 2, 2, 3, 1, 5)
  println(tasuj(lista1, lista2))
}