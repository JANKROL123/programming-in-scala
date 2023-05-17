import akka.actor.{ActorSystem, Actor, ActorRef, Props}

case class Init(liczbaPracownikow: Int)
case class Zlecenie(tekst: List[String])
case object Zacznij
case class Wykonaj(podzadanie: String)
case class Wynik(wyn: Int)

class Szef extends Actor {
  def receive: Receive = {
    case Init(liczbaPracownikow) => 
      val pracownicy = (0 until liczbaPracownikow).toList.map(n => context.actorOf(Props[Pracownik](), s"pracownik$n"))
      context.become(zPracownikami(pracownicy))
  }
  def zPracownikami(pracownicy: List[ActorRef]): Receive = {
    case Zlecenie(tekst) => 
      context.become(zeZleceniem(pracownicy, tekst, tekst.length, Nil))
      self ! Zacznij
  }
  def zeZleceniem(pracownicy: List[ActorRef], tekst: List[String], rozmiarPoczatkowy: Int, odpowiedzi: List[Int]): Receive = {
    case Zacznij => tekst match {
      case Nil => println(s"[${self.path.name}]: 0")
      case hd::tl => 
        context.become(zeZleceniem(pracownicy, tl, rozmiarPoczatkowy, odpowiedzi))
        pracownicy.head ! Wykonaj(hd)
    }
    case Wynik(wyn) => (tekst, rozmiarPoczatkowy == odpowiedzi.length + 1) match {
      case (_, true) => 
        println(s"[${self.path.name}]: ${(wyn::odpowiedzi).sum}")
        context.become(zPracownikami(pracownicy))
      case (Nil, _) => context.become(zeZleceniem(pracownicy, tekst, rozmiarPoczatkowy, wyn::odpowiedzi))
      case (hd::tl, _) => 
        val nowyIDX = (pracownicy.indexOf(sender()) + 1) % (pracownicy.length)
        context.become(zeZleceniem(pracownicy, tl, rozmiarPoczatkowy, wyn::odpowiedzi))
        pracownicy(nowyIDX) ! Wykonaj(hd)
    }

  }
}

class Pracownik extends Actor {
  def receive: Receive = {
    case Wykonaj(podzadanie) => 
      println(s"[${self.path.name}]: dostałem podzadanie: $podzadanie")
      val slowa = podzadanie.split(" ").toList.length
      sender() ! Wynik(slowa)
  }
}

@main
def zadanie_39: Unit = {
  // poniższą listę napisów wyślij do „szefa” za pomocą komunikatu tu „Zlecenie”
  val lista = io.Source
      .fromResource("ogniem_i_mieczem.txt")
      .getLines
      .toList
  val system = ActorSystem("system")
  val szef = system.actorOf(Props[Szef](), "szef")
  szef ! Init(10)
  szef ! Zlecenie(lista)
}