package concepto
import parser.ParserAutomata

case class Automata(estados: Set[Int], alfabeto: Set[Char], transiciones: List[(Int, Char, Int)], estadoInicial: Int, estadosFinales: Set[Int] ) { }


object Automata {

  def convertirAFNDaAFD (AFND: Automata): Automata =
    {
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
          y.+(-1))
    }

    def obtenerTranscisiones(estados: Set[Int], input: Char, A: Automata): Set[Int] = {
      estados.foldRight(Set[Int]())((x, y) => y.++(obtenerTranscisiones(x, input, A)))
    }

  def traducirConjuntoAUnicoEntero(conjunto: Set[Int]): Int = {
    val ordenado = conjunto.toList.sorted
    Integer2int(Integer.valueOf(ordenado.foldLeft("")(_.toString+_.toString)))
  }
}
/*
object Automata{
  def procesar( cadena : String) : Boolean ={
    //-----------------------------------------------------------------------
    // Evaluar si el formato del Archivo es correcto. (Mensaje de Error)
    //-----------------------------------------------------------------------

    //-----------------------------------------------------------------------
    // Procesar el archivo.txt aqui: Obtener las listas que harcodemos abajo.
    //-----------------------------------------------------------------------

    val alfabeto = List('j','a','v','i','e','r','d','g','o')
    val cantidadEstados = 10
    val estadosFinales = List(1,5,7,8,10)
    val transiciones: List[(Int,Char,Int)] = List((1,'j',2),(2,'a',3),(3,'v',4),(4,'a',5),(1,'d',4),(4,'i',6),(6,'e',7),(7,'r',8),(7,'g',9),(9,'o',10))

    val AFND = new Automata(ParserAutomata.estados(cantidadEstados), alfabeto.toSet, transiciones, 0,  estadosFinales.toSet)

    //-----------------------------------------------------------------------
    // Evaluar si la cadena es correcta caracteres que no pertenescan al
    // alfabeto del automata. (Mensaje de Error)
    //-----------------------------------------------------------------------

    val estadosResultantes: List[Int] = List(1)
    
    @annotation.tailrec
    def ciclo(posicionCadena: Int, estadosResultantes : List[Int]) : Boolean =
    {
      if(cadena.size==posicionCadena)
      {
        if(contieneElementosFinales(estadosResultantes, AFND.estadosFinales)) true
        else false
      }
      else
      {
        val newEstadosResultantes: List[Int] = evaluarTransiciones(cadena(posicionCadena), estadosResultantes, AFND.transiciones)
        if(newEstadosResultantes.isEmpty) false
        ciclo(posicionCadena+1, newEstadosResultantes)
      }
    }
    ciclo(0, estadosResultantes)
  }
  
  def contieneElementosFinales(estadosResultantes : List[Int], estadosFinales : Set[Int]) : Boolean =
  {
    @annotation.tailrec
    def ciclo(posicion: Int) : Boolean =
    {
      if(estadosResultantes.size==posicion)
      {
        false
      }
      else
      {
        if(estadosFinales.contains(estadosResultantes(posicion))) true
        else ciclo(posicion+1)
      }
    }
    ciclo(0)
  }
  
  def evaluarTransiciones(input : Char, estadosResultantes : List[Int], transiciones : List[(Int,Char,Int)]) : List[Int] = 
  {
    val newEstadosResultantes: List[Int] = List()
    
    @annotation.tailrec
    def ciclo(posicion: Int, estadosResultantesPost : List[Int]) : List[Int] =
    {
      if(estadosResultantes.size==posicion)
      {
        estadosResultantesPost
      }
      else
      {
        val newEstadosResultantes: List[Int] = estadosResultantesPost ::: buscarTransiciones(estadosResultantes(posicion), input, transiciones)
        ciclo(posicion+1, newEstadosResultantes)
      }
    }
    ciclo(0, newEstadosResultantes)
  }
  
  def buscarTransiciones(origen : Int, input : Char, transiciones: List[(Int,Char,Int)]) : List[Int] =
  {
    val listReturn: List[Int] = List()
    @annotation.tailrec
    def ciclo(posicion: Int, listaReturn : List[Int]) : List[Int] =
    {
      if(transiciones.size==posicion)
      {
        return listaReturn
      }
      else
      {
        if(transiciones(posicion)._1.equals(origen) && transiciones(posicion)._2.equals(input))
        {
          val newLista: List[Int] = listaReturn ::: List(transiciones(posicion)._3)
          ciclo(posicion+1, newLista)
        }
        else
        {
          ciclo(posicion+1, listaReturn)  
        }        
      }
    }
    ciclo(0, listReturn)
  }

  def procesarAFD(AFD: Automata, w: String): Boolean = {
    @annotation.tailrec
    def ciclo(AFD: Automata, estadoActual: Int, w: String): Boolean = {
        if(w.isEmpty && AFD.estadosFinales.contains(estadoActual))
            true
        else if (w.isEmpty)
            false
        else if(estadoActual == -1)
            false
        else
            ciclo(AFD, obtenerTransicion(AFD, estadoActual, w.head), w.tail)
    }
    ciclo(AFD, AFD.estadoInicial, w)
  }

  def obtenerTransicion(AFD: Automata, estado: Int, a: Char): Int = {
     val ret = AFD.transiciones.filter((x: (Int, Char, Int)) => (x._1 == estado && x._2 == a))
     if(ret.isEmpty) -1
     else ret.head._3
  }
  
}
*/