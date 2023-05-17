package jp1.akka.lab13.model

import akka.actor.{Actor, ActorRef, Props}

val akkaPathAllowedChars = ('a' to 'z').toSet union
  ('A' to 'Z').toSet union
  "-_.*$+:@&=,!~';.)".toSet

object Organizator {
  case object Start
  // rozpoczynamy zawody – losujemy 50 osób, tworzymy z nich zawodników
  // i grupę eliminacyjną
  case object Runda
  // polecenie rozegrania rundy (kwalifikacyjnej bądź finałowej) –  wysyłamy Grupa.Runda
  // do aktualnej grupy
  case object Wyniki
  // polecenie wyświetlenia klasyfikacji dla aktualnej grupy
  case class Wyniki(w: Map[ActorRef, Option[Ocena]])
  // wyniki zwracane przez Grupę
  case object Stop
  // kończymy działanie
}

class Organizator extends Actor {
  // importujemy komunikaty na które ma reagować Organizator
  import Organizator._
  val sort1 = (n: Ocena) => n.nota1 + n.nota2 + n.nota3
  val sort2 = (n: Ocena) => n.nota1
  val sort3 = (n: Ocena) => n.nota3
  val segregujWyniki = (wyniki: Map[ActorRef, Option[Ocena]]) => wyniki.map(n => (n._1, n._2.getOrElse(Ocena(0,0,0))))
  def receive: Receive = {
    case Start =>
      // tworzenie 50. osób, opdowiadających im Zawodników
      // oraz Grupy eliminacyjnej
      val zawodnicy = List.fill(50) {
        val o = Utl.osoba()
        context.actorOf(Props(Zawodnik(o)), s"${o.imie}-${o.nazwisko}" filter akkaPathAllowedChars)
      }
      val grupaEliminacyjna = context.actorOf(Props[Grupa](new Grupa(zawodnicy)), "grupaEliminacyjna")
      context.become(doEliminacji(grupaEliminacyjna))

    
  }
  def doEliminacji(grupa: ActorRef): Receive = {
    case Runda => 
      grupa ! Grupa.Runda
    
    case Wyniki(wyniki) => 
      sender() ! Grupa.Koniec
      val finalisci = segregujWyniki(wyniki).toList.sortBy(n => (sort1(n._2), sort2(n._2), sort3(n._2))).reverse.take(20)
      val grupaFinalowa = context.actorOf(Props[Grupa](new Grupa(finalisci.map(n => n._1))), "grupaFinalowa")
      println("Eliminacje przeprowadzone")
      context.become(doFinalu(grupaFinalowa, finalisci))
  }
  def doFinalu(grupa: ActorRef, finalisci: List[(ActorRef, Ocena)]): Receive = {
    case Runda => 
      grupa ! Grupa.Runda
    case Wyniki(wyniki) => 
      sender() ! Grupa.Koniec
      val obecneWyniki = segregujWyniki(wyniki)
      val ostateczne = finalisci.foldLeft(obecneWyniki)((acum,item) => {
        val obecnaOcena = acum.getOrElse(item._1, Ocena(0,0,0))
        val poprzedniaOcena = item._2
        val nowaOcena = Ocena(poprzedniaOcena.nota1 + obecnaOcena.nota1, poprzedniaOcena.nota2 + obecnaOcena.nota2, poprzedniaOcena.nota3 + obecnaOcena.nota3)
        acum + ((item._1, nowaOcena))
      }).toList.sortBy(n => (sort1(n._2), sort2(n._2), sort3(n._2))).reverse.zipWithIndex.map(n => n match {
        case ((zawodnik, wynik), idx) => (idx, zawodnik, wynik)
      }).foldLeft(List[(Int, ActorRef, Ocena)]())((acum,item) => (acum,item) match {
        case (Nil, (0, zawodnik, miejsce)) => (1, zawodnik, miejsce)::acum
        case ((miejsce1, _, Ocena(a, b, c))::_, (_, zawodnik2, Ocena(x, y, z))) if a+b+c == x+y+z && a == x && c == z => (miejsce1, zawodnik2, Ocena(x,y,z))::acum
        case ((_, _, _)::_, (miejsce2, zawodnik2, ocena2)) => (miejsce2+1, zawodnik2, ocena2)::acum
      }).reverse
      println("Finał przeprowadzony")
      context.become(poFinale(ostateczne))
  }
  def poFinale(ostateczne: List[(Int, ActorRef, Ocena)]): Receive = {
    case Wyniki => 
      println("\n")
      ostateczne.foreach(i => i match {
        case (miejsce, daneOsobowe, Ocena(x,y,z)) => 
          println(s"$miejsce. ${daneOsobowe.path.name.replace("-", " ")} - $x-$y-$z = ${x+y+z}")
      })
    case Stop =>
      println("kończymy zawody...")
      context.system.terminate()
  }
}
