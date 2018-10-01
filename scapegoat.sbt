scapegoatVersion in ThisBuild := "1.3.8"

scapegoatDisabledInspections := Seq("RedundantFinalModifierOnCaseClass",
                                    "AsInstanceOf", /* false positive */
                                    "TraversableHead") /* fix traversable head */

import com.sksamuel.scapegoat.sbt.ScapegoatSbtPlugin.autoImport._

scapegoatReports := Seq("html")