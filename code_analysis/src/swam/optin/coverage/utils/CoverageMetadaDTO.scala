package swam
package code_analysis
package coverage
package utils

import swam.code_analysis.coverage.instrument.InstrumentationType

/**
  *@author Javier Cabrera-Arteaga on 2020-10-15
  */
class FunctionMetadata(val name: String, val params: Vector[ValType], val r: Vector[ValType])

class CoverageMetadaDTO(val totalInstructions: Int,
                        val totalBasicBlocks: Int,
                        val tpe: Int = 0 /*0 for JS 1 for inner callback*/,
                        val AFLMemOffset: Int = 0,
                        val AFLMemSize: Int = 0,
                        val blockCoverageMemOffset: Int = 0,
                        val blockCoverageMemSize: Int = 0) {}
