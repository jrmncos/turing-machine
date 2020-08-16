import concepto._
import parser.ParserGramatica
import org.scalactic.source.Position.apply

class GramaticaTest extends UnitSpec 
{
  "La Gramatica: S → cAd, A → ab, A → a" must "aceptar el string cad" in {
    val terminales = ParserGramatica.terminales("c, a, b, d")
    val variables = ParserGramatica.variables("S, A")
    val producciones = ParserGramatica.producciones(List("S → cAd", "A → ab", "A → a"))
    val G1 = Gramatica(producciones, terminales, variables, Variable("S"))
    val result = Gramatica.procesar(G1, "cad")
    
    result should equal (true) 
  }
  
    "La Gramatica: S → cAd, A → ab, A → a" must "aceptar el string cabd" in {
    val terminales = ParserGramatica.terminales("c, a, b, d")
    val variables = ParserGramatica.variables("S, A")
    val producciones = ParserGramatica.producciones(List("S → cAd", "A → ab", "A → a"))
    val G1 = Gramatica(producciones, terminales, variables, Variable("S"))
    val result = Gramatica.procesar(G1, "cabd")
    
    result should equal (true) 
  }

  "La Gramatica: S → cAd, A → ab, A → a" must "rechazar el string caabd" in {
    val terminales = ParserGramatica.terminales("c, a, b, d")
    val variables = ParserGramatica.variables("S, A")
    val producciones = ParserGramatica.producciones(List("S → cAd", "A → ab", "A → a"))
    val G1 = Gramatica(producciones, terminales, variables, Variable("S"))
    val result = Gramatica.procesar(G1, "caabd")
    
    result should equal (false) 
  }   
  
  "La Gramatica: S → cAd, A → ab, A → a" must "rechazar el string ε" in {
    val terminales = ParserGramatica.terminales("c, a, b, d")
    val variables = ParserGramatica.variables("S, A")
    val producciones = ParserGramatica.producciones(List("S → cAd", "A → ab", "A → a"))
    val G1 = Gramatica(producciones, terminales, variables, Variable("S"))
    val result = Gramatica.procesar(G1, "ε")
    
    result should equal (false) 
  } 

  "La Gramatica: S → 01, S → 0S1, S → ε" must "aceptar el string 01" in {
    val terminales = ParserGramatica.terminales("0, 1")
    val variables = ParserGramatica.variables("S")
    val producciones = ParserGramatica.producciones(List("S → 01", "S → 0S1", "S → ε"))
    val G1 = Gramatica(producciones, terminales, variables, Variable("S"))
    val result = Gramatica.procesar(G1, "01")
    
    result should equal (true) 
  }
  
  "La Gramatica: S → 01, S → 0S1, S → ε" must "aceptar el string 0000011111" in {
    val terminales = ParserGramatica.terminales("0, 1")
    val variables = ParserGramatica.variables("S")
    val producciones = ParserGramatica.producciones(List("S → 01", "S → 0S1", "S → ε"))
    val G1 = Gramatica(producciones, terminales, variables, Variable("S"))
    val result = Gramatica.procesar(G1, "0000011111")
    
    result should equal (true) 
  }
  
  "La Gramatica: S → 01, S → 0S1, S → ε" must "aceptar el string ε" in {
    val terminales = ParserGramatica.terminales("0, 1")
    val variables = ParserGramatica.variables("S")
    val producciones = ParserGramatica.producciones(List("S → 01", "S → 0S1", "S → ε"))
    val G1 = Gramatica(producciones, terminales, variables, Variable("S"))
    val result = Gramatica.procesar(G1, "ε")
    
    result should equal (true) 
  }
  
  "La Gramatica: S → 01, S → 0S1, S → ε" must "aceptar el string εεεεεε" in {
    val terminales = ParserGramatica.terminales("0, 1")
    val variables = ParserGramatica.variables("S")
    val producciones = ParserGramatica.producciones(List("S → 01", "S → 0S1", "S → ε"))
    val G1 = Gramatica(producciones, terminales, variables, Variable("S"))
    val result = Gramatica.procesar(G1, "εεεεεε")
    
    result should equal (true) 
  }
  
  "La Gramatica: S → 01, S → 0S1, S → ε" must "rechazar el string 0111" in {
    val terminales = ParserGramatica.terminales("0, 1")
    val variables = ParserGramatica.variables("S")
    val producciones = ParserGramatica.producciones(List("S → 01", "S → 0S1", "S → ε"))
    val G1 = Gramatica(producciones, terminales, variables, Variable("S"))
    val result = Gramatica.procesar(G1, "0111")
    
    result should equal (false) 
  }
  
}