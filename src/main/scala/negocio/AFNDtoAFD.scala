package negocio
import concepto.Automata

object AFNDtoAFD {
  def convertir(AFND: Automata): Automata = {
    val casi2QnEstados = dosQnEstados(AFND)
    val estadosAFD = casi2QnEstados.foldLeft(Set[Int]())((x, y) => x.+(traducirConjuntoAUnicoEntero(y)))
    val estadosFinalesAFD = finales(casi2QnEstados, AFND.estadosFinales)
    val transicionesAFD = AFND.alfabeto.foldLeft(List[(Int, Char, Int)]())((list, char) => list.++(
      casi2QnEstados.foldLeft(List[(Int, Char, Int)]())((res, estado) => res.::(tuplaCorrespondienteA(estado, char, AFND)))))

    println("Casi2Qn: "+casi2QnEstados)
    println("Estados AFD: "+estadosAFD)
    println("Estados Finales: "+estadosFinalesAFD)
    println("Transiciones: "+transicionesAFD)

    Automata(estadosAFD, AFND.alfabeto, transicionesAFD, AFND.estadoInicial, estadosFinalesAFD)
  }

  def dosQnEstados(AFND: Automata): Set[Set[Int]] = {
    def go(AFND: Automata, conjuntoActual: Set[Set[Int]], input: Char, indice: Int): Set[Set[Int]] = {
      if(!alAgregarAlgoCambia(AFND, conjuntoActual, input, indice))
        conjuntoActual
      else
        AFND.alfabeto.foldLeft(Set[Set[Int]]())((x, y) => x++go(AFND, conjuntoActual++(dadoConjuntoEInputConcatenoADondeVa(conjuntoActual.toList(indice), y, AFND)), y, indice+1))
    }
    go(AFND, Set(Set(1)), AFND.alfabeto.head, 0)
  }

  def finales(conj: Set[Set[Int]], conjFinales: Set[Int]): Set[Int] = {
    conj.fold(Set[Int]())((x, y) =>
      if(y.exists(conjFinales.contains(_)))
        x.+(traducirConjuntoAUnicoEntero(y))
      else
        x )
  }

  def alAgregarAlgoCambia(AFND: Automata, t: Set[Set[Int]], input: Char, indice: Int): Boolean = {
    t.size != (t++(dadoConjuntoEInputConcatenoADondeVa(t.toList(indice), input, AFND))).size
  }

  def tuplaCorrespondienteA(estados: Set[Int], input: Char, A: Automata): (Int, Char, Int) = {
    val resultadoFCDeEstados = obtenerTranscisiones(estados, input, A)
    (traducirConjuntoAUnicoEntero(estados), input, traducirConjuntoAUnicoEntero(resultadoFCDeEstados))
  }

  def dadoConjuntoEInputConcatenoADondeVa(conj: Set[Int], input: Char, A: Automata): List[Set[Int]] = {
    val conjuntoResultante = obtenerTranscisiones(conj, input, A)
    List(conjuntoResultante).::(conj)
  }

  def obtenerTranscisiones(estado: Int, input: Char, A: Automata): Set[Int] = {
    A.transiciones.foldRight(Set[Int]())((x, y) =>
      if(x._1 == estado && x._2 == input)
        y.+(x._3)
      else
        y.+(-1) )
  }

  def obtenerTranscisiones(estados: Set[Int], input: Char, A: Automata): Set[Int] = {
    if(estados.contains(-1)) Set.empty
    else
        estados.foldRight(Set[Int]())((x, y) => y.++(obtenerTranscisiones(x, input, A)))
  }

  def traducirConjuntoAUnicoEntero(conjunto: Set[Int]): Int = {
    val ordenado = conjunto.toList.sorted
    println(ordenado)
    if(ordenado.isEmpty) -1
    else
        Integer2int(Integer.valueOf(ordenado.foldLeft("")(_.toString+_.toString)))
  }
  /*
  def main (args: Array[String]): Unit = {
    val trancisiones = List((1, 'a', 3), (1, 'a', 5), (1, 'a', 4), (1, 'd', 4))
    val alfabeto = Set('a', 'b', 'c', 'd')
    val estados = Set(1, 2, 3, 4, 5)
    val A = Automata(estados, alfabeto, trancisiones, 1, Set(1, 3))
    AFNDtoAFD.convertir(A)
  }
  */
}