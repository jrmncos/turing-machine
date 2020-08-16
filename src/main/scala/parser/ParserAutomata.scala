package parser
import scala.util.matching.Regex
import concepto.Automata
import scala.io.Source

object ParserAutomata
{
  def cantEstados(s: String): Int = 
  {
   Integer2int(Integer.valueOf(s)) 
  }
  
  def estados(cantEstadosFinales: Int ): Set[Int] = 
  {
    val ret = List.range(1, cantEstadosFinales+1).toSet
    ret.toSet
  }
  
  def estadosFinales(s: String): Set[Int] = 
  {
    val charRegEx: Regex = "\\d".r
    val ret = charRegEx.findAllIn(s).toSet
    ret.map(s =>Integer2int(Integer.valueOf(s)))
  }
  
  def alfabeto(s: String): Set[Char] = 
  { 
    val charRegEx: Regex = "\\d|\\w".r
    val ret = charRegEx.findAllIn(s).toSet
    ret.map(_.head)
  }

  def fcTransicion(s: Iterator[String]): List[(Int, Char, Int)] = 
  {
    @annotation.tailrec
    def go(s1: Iterator[String], ret: List[(Int, Char, Int)]): List[(Int, Char, Int)] = 
    {
      if(!s1.hasNext) { ret }
      else
      {
        val tupla = obtenerTupla(s1.next)
      	go(s1, ret.+:(tupla))
      }
    }
    go(s, List())
  }
  
  def obtenerTupla(s: String): (Int, Char, Int) = 
  {
    val regex: Regex = "\\d|\\w".r
    val e = regex.findAllIn(s)
    (Integer2int(Integer.valueOf(e.next)), e.next.head, Integer2int(Integer.valueOf(e.next)))
  }
  
  def automataFromFile(input: Iterator[String]): Automata = {
    val _alfabeto = alfabeto(input.next)
    val _cEstados = cantEstados(input.next)
    val _estados = estados(_cEstados)
    val _estadoFinales = estadosFinales(input.next)
    val _transiciones = fcTransicion(input)
    
    Automata(_estados, _alfabeto, _transiciones, 1, _estadoFinales)
  }
  /*
  def main (args: Array[String]): Unit = {
    val rutaFile = System.getProperty("user.dir")+"\\files\\AutomataTP.txt";
    val s = Source.fromFile(rutaFile).getLines()
    println("Alfabeto: " + alfabeto(s.next))
    val cEstados = cantEstados(s.next)
    println("Cantidad de estados: " + cEstados)
    println("Estados: " + estados(cEstados))
    println("Estados finales: " + estadosFinales(s.next))
    println("Funcion de transicion: " + fcTransicion(s))
  }
  */
}