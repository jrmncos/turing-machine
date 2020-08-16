package concepto

final class MaquinaTuring(
    estados: Set[Int], 
    alfabeto: Set[Int], 
    alfabetoCinta: Set[Int], 
    transiciones: Map[(Estado, Char), (Estado, Char, Direccion)], 
    estadoInicial: Int, 
    simboloBlanco: Char, 
    estadosFinales: Set[Int])
{
 def procesar(inicio: (Estado, Cinta), transiciones: Map[(Estado, Char), (Estado, Char, Direccion)], estadosFinales: List[String]): (Estado, Cinta) =
  {
    val puntero = obtenerPuntero(inicio._2)
    
    @annotation.tailrec
    def ciclo(punteroCinta : Int, estadoActual : Estado, cinta : Cinta) : (Estado, Cinta) =
    {
      if(estadosFinales.contains(estadoActual.state))
      {
        return (estadoActual, cinta)
      }
      else
      {
        println("---------------------------------------------------")
        println("puntero ---> "+punteroCinta)
        println("elemento de cinta ---> "+cinta.tape(punteroCinta))
        println("estado actual ---> "+estadoActual.state)
        val transicion = transiciones(estadoActual, cinta.tape(punteroCinta))
        if (transicion._3.sentido.equals("R"))
        {
          //se mueve hacia la derecha
          val newCinta = new Cinta(obtenerListMod(punteroCinta,cinta,transicion._2))
          println(newCinta.tape)
          ciclo(punteroCinta+1, transicion._1, newCinta)
        }
        else
        {
          //se mueve hacia la izquierda
          val newCinta = new Cinta(obtenerListMod(punteroCinta,cinta,transicion._2))
          println(newCinta.tape)
          ciclo(punteroCinta-1, transicion._1, newCinta)
        }
      }
    }
    ciclo(puntero, inicio._1, inicio._2)
  }
  
  def obtenerListMod(punteroCinta : Int, cinta : Cinta, elemReempl : Char) : List[Char] =
  {
    val listReturn: List[Char] = List()
    
    @annotation.tailrec
    def ciclo(puntero : Int, lista : List[Char]) : List[Char] =
    {
      if (cinta.tape.size==puntero)
      {
        lista
      }
      else
      {
        if (punteroCinta==puntero)
        {
          val newLista: List[Char] = lista ::: List(elemReempl)
          ciclo(puntero+1, newLista)          
        }
        else
        {
          val newLista : List[Char] = lista ::: List(cinta.tape(puntero))
          ciclo(puntero+1, newLista)
        }
      }
    }
    ciclo(0, listReturn)
  } 
  
  def obtenerPuntero(cinta: Cinta) : Int =
  {
    @annotation.tailrec
    def ciclo(puntero : Int) : Int =
    {
      if(cinta.tape(puntero)!='B')
      {
        puntero
      }
      else
      {
        ciclo(puntero+1)  
      }
    }
    ciclo(0)
  }
 
}

case class Estado(state: String) 
{
  
}

case class Cinta(tape: List[Char])
{
  
}

case class Direccion(sentido: String) 
{
  
}