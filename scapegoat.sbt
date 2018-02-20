scapegoatVersion := "1.1.0"

scapegoatDisabledInspections := Seq("RedundantFinalModifierOnCaseClass",
                                    "AsInstanceOf", /* false positive */
                                    "TraversableHead") /* fix traversable head */

import com.sksamuel.scapegoat.sbt.ScapegoatSbtPlugin.autoImport._

scapegoatReports := Seq("html")