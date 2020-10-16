package swam
package code_analysis
package coverage
package utils

/**
    @author Javier Cabrera-Arteaga on 2020-10-15
  */
class FunctionMetadata(val name: String, val params: Vector[ValType], val r: Vector[ValType])

class CoverageMetadaDTO(val totalInstructions: Int, val totalBasicBlocks: Int) {}
