
import akka.actor.{ActorSystem, Actor, ActorRef, Props}

case object Piłeczka
case class Graj35(przeciwnik: ActorRef)
case class Graj36(przeciwnik: ActorRef, maks:Int)
case class Graj37(przeciwnik: ActorRef)
case class Graj38(listaGraczy: List[ActorRef], pierwszy: ActorRef)
case object InnerInfo


class Gracz35 extends Actor {
  def receive: Receive = {
    case Graj35(przeciwnik) => context.become(zPrzeciwnikiem(przeciwnik))
  }
  def zPrzeciwnikiem(przeciwnik:ActorRef): Receive = {
    case Piłeczka => 
      println(s"[${self.path.name}]: dostałem piłeczkę od ${sender().path.name}")
      przeciwnik ! Piłeczka
  }
}



class Gracz36 extends Actor {
  def receive: Receive = {
    case Graj36(przeciwnik, maks) => context.become(zPrzeciwnikiem(przeciwnik, maks))
  }
  def zPrzeciwnikiem(przeciwnik: ActorRef, maks:Int): Receive = {
    case Piłeczka if maks != 0 => 
      println(s"[${self.path.name}]: dostałem piłeczkę od ${sender().path.name}")
      przeciwnik ! Piłeczka
      context.become(zPrzeciwnikiem(przeciwnik, maks-1))
    case Piłeczka if maks == 0 => context.system.terminate()
  }
}



class Gracz37 extends Actor {
  def receive: Receive = {
    case Graj37(przeciwnik) => context.become(zPrzeciwnikiem(przeciwnik))
  }
  def zPrzeciwnikiem(przeciwnik: ActorRef): Receive = {
    case Piłeczka => 
      println(s"[${self.path.name}]: dostałem piłeczkę od ${sender().path.name}")
      przeciwnik ! Piłeczka
  }
}


class Gracz38 extends Actor {
  def receive: Receive = {
    case Graj38(hd::tl, pierwszy) => 
      context.become(zPrzeciwnikiem(hd))
      hd ! Graj38(tl, pierwszy)
    case Graj38(Nil, pierwszy) => 
      context.become(zPrzeciwnikiem(pierwszy))
      pierwszy ! Piłeczka
  }
  def zPrzeciwnikiem(przeciwnik: ActorRef): Receive = {
    case Piłeczka => 
      println(s"[${self.path.name}]: dostalem pileczke od ${sender().path.name}")
      przeciwnik ! Piłeczka
  }
}



@main
def zadanie_35: Unit = {
  val system = ActorSystem("system")
  val gracz1 = system.actorOf(Props[Gracz35](), "gracz1")
  val gracz2 = system.actorOf(Props[Gracz35](), "gracz2")
  gracz1 ! Graj35(gracz2)
  gracz2 ! Graj35(gracz1)
  gracz1 ! Piłeczka
}

@main
def zadanie_36: Unit = {
  val system = ActorSystem("system")
  val gracz3 = system.actorOf(Props[Gracz36](), "gracz3")
  val gracz4 = system.actorOf(Props[Gracz36](), "gracz4")
  gracz3 ! Graj36(gracz4, 15)
  gracz4 ! Graj36(gracz3, 15)
  gracz3 ! Piłeczka
}

@main
def zadanie_37: Unit = {
  val system = ActorSystem("system")
  val pinger = system.actorOf(Props[Gracz37](), "pinger")
  val panger = system.actorOf(Props[Gracz37](), "panger")
  val ponger = system.actorOf(Props[Gracz37](), "ponger")
  pinger ! Graj37(panger)
  panger ! Graj37(ponger)
  ponger ! Graj37(pinger)
  pinger ! Piłeczka
}

@main
def zadanie_38(maks: Int): Unit = {
  val system = ActorSystem("system")
  val gracze = (0 until maks).toList.map(n => system.actorOf(Props[Gracz38](), s"gracz$n"))
  gracze.head ! Graj38(gracze.tail, gracze.head)
}
