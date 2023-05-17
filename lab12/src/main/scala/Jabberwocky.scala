import akka.actor.{ActorSystem, Actor, Props, ActorRef, PoisonPill, Terminated}
import scala.concurrent.duration._
val r = util.Random
/*
  W konfiguracji projektu wykorzystana została wtyczka
  sbt-revolver. W związku z tym uruchamiamy program poleceniem

    reStart

  a zatrzymujemy pisząc (mimo przesuwających się komunikatów)

     reStop

  i naciskając klawisz ENTER. Jeśli czynności powyższe
  już wykonywaliśmy to możemy też przywołać poprzednie
  polecenia używając strzałek góra/dół na klawiaturze.
*/


object SiłaWyższa {
  case object Strzelać
}

case class OtoZamki(z1: ActorRef, z2: ActorRef)
case class PrzygotujSie(liczbaObr: Int, z: ActorRef)
case object Strzala
case class CzyZgine(liczbaObr: Int)

class SiłaWyższa extends Actor {
  import SiłaWyższa._
  def receive = {
    case OtoZamki(z1, z2) =>
      context.become(zZamkami(z1, z2))
  }
  def zZamkami(zamek1: ActorRef, zamek2: ActorRef): Receive = {
    case Strzelać => 
      zamek1 ! Strzelać
      zamek2 ! Strzelać
  }
}

class Zamek extends Actor {
  import SiłaWyższa.{Strzelać}
  def receive: Receive = {
    case PrzygotujSie(liczbaObr, wrogiZamek) =>
      val obroncy = (0 until liczbaObr).toList.map(n => context.actorOf(Props[Obrońca](new Obrońca(wrogiZamek)), s"${self.path.name}obronca$n"))
      obroncy.foreach(context.watch(_))
      context.become(gotowoscBojowa(obroncy))
  }
  def gotowoscBojowa(obroncy: List[ActorRef]): Receive = {
    case Strzelać => 
      obroncy.foreach(_ ! Strzelać)
    case Strzala =>
      val idx = r.nextInt(obroncy.length)
      obroncy(idx) ! CzyZgine(obroncy.length)
    case Terminated(ob) => 
      println(s"[${self.path.name}]: trafiony, moje siły: ${obroncy.length-1}")
      val nowiObroncy = obroncy.filter(n => n != ob)
      if nowiObroncy.length != 0 then context.become(gotowoscBojowa(nowiObroncy))
      else {
        println(s"[${self.path.name}]: PORAŻKA")
        context.system.terminate()
      }
  }
}

class Obrońca(wrogiZamek: ActorRef) extends Actor {
  import SiłaWyższa.{Strzelać}
  def receive: Receive = {
    case Strzelać => 
      wrogiZamek ! Strzala
    case CzyZgine(n) => 
      if r.nextFloat() < n.toDouble / 200 then self ! PoisonPill
      println(s"[${context.parent.path.name}]: pudło, moje siły: ${n}")

  }
}

@main
def bitwa: Unit = {
  val system = ActorSystem("Jabberwocky")
  import system.dispatcher
  import SiłaWyższa._
  // UWAGA: „nazwy”, tworzące ścieżkę do aktora muszą być zapisywane
  // z użyciem znaków znaków ASCII (a nie np. UTF8) – stąd „SilaWyzsza”


  val siłaWyższa = system.actorOf(Props[SiłaWyższa](), "SilaWyzsza")
  val zamek1 = system.actorOf(Props[Zamek](), "zamek1")
  val zamek2 = system.actorOf(Props[Zamek](), "zamek2")
  siłaWyższa ! OtoZamki(zamek1, zamek2)
  zamek1 ! PrzygotujSie(100, zamek2)
  zamek2 ! PrzygotujSie(100, zamek1)


  // Do „animacji” SiłyWyższej wykorzystamy „Planistę” (Scheduler)
  val pantaRhei = system.scheduler.scheduleWithFixedDelay(
    Duration.Zero,     // opóźnienie początkowe
    1000.milliseconds, // odstęp pomiedzy kolejnymi komunikatami
    siłaWyższa,        // adresat „korespondencji”
    SiłaWyższa.Strzelać     // komunikat
  )

  // Oczywiście zatrzymanie symulacji NIE MOŻE się odbyć tak, jak poniżej
  /*
  Thread.sleep(3000)
  val res = if pantaRhei.cancel()
    then "Udało się zakończyć „cykanie”"
    else "Coś poszło nie tak – dalej „cyka”"
  println(res)
  system.terminate()
  */
}
