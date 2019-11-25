/*
 * Slumps team
 */

package swam
package slumps
package config

import squants.information._

import enumeratum._
import enumeratum.EnumEntry._

/** Holds all the
  * configurable values.
  *
  * @param souperBinPath Path to souper bin.
  */
case class SlumpsConfiguration(souperBinPath: String)
