package jp1.akka.lab13.model

import akka.actor.{Actor, ActorRef, PoisonPill}

object Grupa {
  case object Runda
  // Zawodnicy mają wykonać swoje próby – Grupa
  // kolejno (sekwencyjnie) informuje zawodników
  // o konieczności wykonania próby i „oczekuje”
  // na ich wynik (typu Option[Ocena])
  case object Wyniki
  // Polecenie zwrócenia aktualnego rankingu Grupy
  // Oczywiście klasyfikowani są jedynie Zawodnicy,
  // którzy pomyślnie ukończyli swoją próbę
  case class Wynik(ocena: Option[Ocena])
  // Informacja o wyniku Zawodnika (wysyłana przez Zawodnika do Grupy)
  // np. Wynik(Some(Ocena(10, 15, 14)))
  // Jeśli zawodnik nie ukończy próby zwracana jest wartość Wynik(None)
  case object Koniec
  // Grupa kończy rywalizację
}
class Grupa(zawodnicy: List[ActorRef]) extends Actor {
  def receive: Receive = {
    case Grupa.Runda => 
      zawodnicy.foreach(_ ! Zawodnik.Próba)
      context.become(kolekcjonujWyniki(Map()))
  }
  def kolekcjonujWyniki(zestawienie: Map[ActorRef, Option[Ocena]]): Receive = {
    case Grupa.Wynik(ocena) => 
      val noweZestawienie = zestawienie + ((sender(), ocena))
      if noweZestawienie.size == zawodnicy.length then context.parent ! Organizator.Wyniki(noweZestawienie)
      else context.become(kolekcjonujWyniki(noweZestawienie))
    case Grupa.Koniec => 
      self ! PoisonPill
  }
}
